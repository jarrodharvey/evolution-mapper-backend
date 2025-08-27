# Evolution Mapper - Distributed Backend API
# R Plumber API for serving phylogenetic tree data

#* @filter cors
function(req, res) {
  # Get allowed origins from environment variable, fallback to localhost
  cors_origins <- Sys.getenv("CORS_ALLOWED_ORIGINS", "http://localhost:3000")
  allowed_origins <- trimws(strsplit(cors_origins, ",")[[1]])
  
  # Get the origin from the request
  origin <- req$HTTP_ORIGIN
  
  # Set CORS headers if origin is allowed
  if (!is.null(origin) && origin %in% allowed_origins) {
    res$setHeader("Access-Control-Allow-Origin", origin)
  } else if (is.null(origin) && length(allowed_origins) > 0) {
    # Fallback for requests without origin header (like Postman)
    res$setHeader("Access-Control-Allow-Origin", allowed_origins[1])
  }
  
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Requested-With, X-API-Key")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  forward()
}

# Source tree generation functions
source("functions/rotl_tree_generation.R")
source("functions/datelife_tree_generation.R")
source("functions/hybrid_tree_generation.R")
source("functions/wikipedia_api.R")

# Required libraries
library(DBI)
library(RSQLite)
library(jsonlite)
library(httr)

# Shared function for parsing species input (used by both APIs)
parse_species_input <- function(input) {
  if (is.character(input)) {
    if (startsWith(input, "[") && endsWith(input, "]")) {
      return(jsonlite::fromJSON(input))
    } else {
      return(trimws(strsplit(input, ",")[[1]]))
    }
  } else {
    return(input)
  }
}

# Clean scientific names by removing parenthetical addendums
clean_scientific_names <- function(scientific_names) {
  # Remove parenthetical addendums like "(species in domain Eukaryota)"
  # but preserve the main binomial name
  cleaned <- gsub("\\s*\\([^)]+\\)\\s*$", "", scientific_names)
  cleaned <- trimws(cleaned)  # Remove any trailing whitespace
  return(cleaned)
}

# Simple in-memory rate limiting - tracks requests per IP
rate_limit_storage <- new.env()
rate_limit_window <- 60  # seconds
rate_limit_max <- 60     # requests per window

# API Key configuration from .Renviron file
# Load API keys from environment variable (set in .Renviron)
api_keys_env <- Sys.getenv("EVOLUTION_API_KEYS")

if (nzchar(api_keys_env)) {
  valid_api_keys <- trimws(strsplit(api_keys_env, ",")[[1]])
} else {
  # No fallback keys for security - require proper configuration
  stop("EVOLUTION_API_KEYS environment variable not found. Please configure API keys in .Renviron file.")
}

#* @apiTitle Evolution Mapper API
#* @apiDescription API for generating phylogenetic trees and species data

#* @plumber
function(pr) {
  pr %>% pr_set_docs("swagger")
}

#* API Key authentication filter
#* @filter apikey
function(req, res) {
  # Skip authentication for health check endpoint
  if (req$PATH_INFO == "/api/health") {
    forward()
    return()
  }
  
  # Get API key from header only (more secure than query parameters)
  api_key <- req$HTTP_X_API_KEY
  
  # Check if API key is provided and valid
  if (is.null(api_key) || !api_key %in% valid_api_keys) {
    res$status <- 401  # Unauthorized
    return(list(
      success = FALSE,
      error = "Invalid or missing API key. Include your API key in the 'X-API-Key' header.",
      documentation = "Contact the API administrator for access credentials."
    ))
  }
  
  # Store API key in request for potential logging/tracking
  req$api_key <- api_key
  
  forward()
}

#* Rate limiting filter - prevents API abuse
#* @filter ratelimit
function(req, res) {
  # Use IP address as the identifier for rate limiting
  client_ip <- req$REMOTE_ADDR %||% "unknown"
  current_time <- as.numeric(Sys.time())
  
  # Get or initialize request history for this IP
  if (!exists(client_ip, envir = rate_limit_storage)) {
    rate_limit_storage[[client_ip]] <- list()
  }
  
  # Get request timestamps for this IP
  ip_requests <- rate_limit_storage[[client_ip]]
  
  # Remove requests outside the time window
  cutoff_time <- current_time - rate_limit_window
  ip_requests <- ip_requests[ip_requests > cutoff_time]
  
  # Check if rate limit exceeded
  if (length(ip_requests) >= rate_limit_max) {
    res$status <- 429  # Too Many Requests
    return(list(
      success = FALSE,
      error = "Rate limit exceeded. Maximum 60 requests per minute allowed.",
      retry_after = 60
    ))
  }
  
  # Add current request timestamp
  ip_requests <- c(ip_requests, current_time)
  rate_limit_storage[[client_ip]] <- ip_requests
  
  forward()
}

#* Health check endpoint
#* @get /api/health
function() {
  list(
    status = "ok",
    message = "Evolution Mapper API is running",
    timestamp = Sys.time()
  )
}

#* Get legend information for tree visualization colors
#* @get /api/legend
function() {
  source("functions/color_config.R")
  
  list(
    success = TRUE,
    legend = get_legend_data()
  )
}

#* Echo test endpoint
#* @param msg The message to echo back
#* @get /api/echo
function(msg = "Hello World") {
  list(
    echo = msg,
    timestamp = Sys.time()
  )
}

#* Search species by name with optional limit for frontend picker
#* @param search Optional search term to filter species names
#* @param limit Optional limit for number of results (default 50, max 100)
#* @get /api/species
function(search = NULL, limit = 50) {
  tryCatch({
    # Validate and sanitize limit parameter
    limit <- as.numeric(limit)
    if (is.na(limit) || limit < 1) {
      limit <- 50
    } else if (limit > 100) {
      limit <- 100  # Prevent excessive results
    }
    
    # Query database directly
    db_path <- "data/species.sqlite"
    species_db <- dbConnect(SQLite(), db_path)
    
    if (is.null(search) || search == "") {
      query <- paste0(
        "SELECT common, scientific, ott FROM (",
        "  SELECT common, scientific, ott, ",
        "  ROW_NUMBER() OVER (PARTITION BY common ORDER BY RANDOM()) as rn ",
        "  FROM species ",
        "  WHERE ott IS NOT NULL AND ott != '' AND common IS NOT NULL",
        ") ranked ",
        "WHERE rn = 1 ",
        "ORDER BY LENGTH(common), common LIMIT ", limit
      )
    } else {
      query <- paste0(
        "SELECT common, scientific, ott FROM (",
        "  SELECT common, scientific, ott, ",
        "  ROW_NUMBER() OVER (PARTITION BY common ORDER BY RANDOM()) as rn ",
        "  FROM species ",
        "  WHERE ott IS NOT NULL AND ott != '' AND common IS NOT NULL ",
        "  AND (common LIKE '%", gsub("'", "''", search), "%' OR scientific LIKE '%", gsub("'", "''", search), "%')",
        ") ranked ",
        "WHERE rn = 1 ",
        "ORDER BY LENGTH(common), common LIMIT ", limit
      )
    }
    
    species_data <- dbGetQuery(species_db, query)
    dbDisconnect(species_db)
    
    list(
      success = TRUE,
      count = nrow(species_data),
      search_term = search,
      limit_applied = limit,
      species = species_data
    )
  }, error = function(e) {
    list(
      success = FALSE,
      error = paste("Error searching species:", conditionMessage(e))
    )
  })
}

#* Generate phylogenetic tree from list of species with both common and scientific names
#* @param common_names A JSON array of species common names
#* @param scientific_names A JSON array of species scientific names (must match common_names length)
#* @post /api/tree
function(req, common_names = NULL, scientific_names = NULL) {
  if (is.null(common_names) || is.null(scientific_names)) {
    return(list(
      success = FALSE,
      error = "Missing required parameters 'common_names' and 'scientific_names'",
      note = "Both parameters must be provided as equal-length arrays"
    ))
  }
  
  # Parse both input parameters using shared function
  common_list <- parse_species_input(common_names)
  scientific_list <- parse_species_input(scientific_names)
  
  if (length(common_list) != length(scientific_list)) {
    return(list(
      success = FALSE,
      error = "common_names and scientific_names must have the same length"
    ))
  }
  
  if (length(common_list) < 2) {
    return(list(
      success = FALSE,
      error = "At least 2 species required for tree generation"
    ))
  }
  
  result <- generate_tree_html_paired(common_list, scientific_list)
  return(result)
}

#* Get random species names for frontend picker
#* @param count Number of species (3-20, default random)
#* @get /api/random-species
function(count = NULL) {
  if (!is.null(count)) {
    count <- as.numeric(count)
    if (is.na(count) || count < 3 || count > 20) {
      return(list(
        success = FALSE,
        error = "Count must be between 3 and 20"
      ))
    }
  } else {
    count <- sample(3:7, 1)
  }
  
  tryCatch({
    # Get random species from database directly
    db_path <- "data/species.sqlite"
    species_db <- dbConnect(SQLite(), db_path)
    
    query <- paste0(
      "SELECT common, scientific FROM species ",
      "WHERE ott IS NOT NULL AND ott != '' AND common IS NOT NULL ",
      "ORDER BY RANDOM() LIMIT ", count
    )
    
    random_species_data <- dbGetQuery(species_db, query)
    dbDisconnect(species_db)
    
    return(list(
      success = TRUE,
      selected_species = list(
        common_names = random_species_data$common,
        scientific_names = random_species_data$scientific
      )
    ))
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error getting random species:", conditionMessage(e))
    ))
  })
}

#* Generate random phylogenetic tree for testing
#* @param count Number of species (2-7, default random)
#* @get /api/random-tree
function(count = NULL) {
  if (!is.null(count)) {
    count <- as.numeric(count)
    if (is.na(count) || count < 2 || count > 7) {
      return(list(
        success = FALSE,
        error = "Count must be between 2 and 7"
      ))
    }
  } else {
    count <- sample(3:7, 1)
  }
  
  tryCatch({
    # Get random species from database directly
    db_path <- "data/species.sqlite"
    species_db <- dbConnect(SQLite(), db_path)
    
    query <- paste0(
      "SELECT common, scientific, ott FROM species ",
      "WHERE ott IS NOT NULL AND ott != '' AND common IS NOT NULL ",
      "ORDER BY RANDOM() LIMIT ", count
    )
    
    random_species_data <- dbGetQuery(species_db, query)
    dbDisconnect(species_db)
    
    if (nrow(random_species_data) < 2) {
      return(list(
        success = FALSE,
        error = "Could not find enough valid species in database"
      ))
    }
    
    random_species <- random_species_data$common
    result <- generate_tree_html(random_species)
    
    # Always include selected species for debugging with both common and scientific names
    result$selected_species <- list(
      common_names = random_species_data$common,
      scientific_names = random_species_data$scientific
    )
    
    return(result)
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating random tree:", conditionMessage(e)),
      selected_species = if(exists("random_species_data") && nrow(random_species_data) > 0) {
        list(
          common_names = random_species_data$common,
          scientific_names = random_species_data$scientific
        )
      } else {
        list(common_names = "unknown", scientific_names = "unknown")
      }
    ))
  })
}

#* Generate dated phylogenetic tree using DateLife chronograms
#* @param common_names A JSON array of species common names
#* @param scientific_names A JSON array of species scientific names (must match common_names length)
#* @param allow_partial_response Boolean to allow partial coverage trees (default false)
#* @post /api/dated-tree
function(req, common_names = NULL, scientific_names = NULL, allow_partial_response = FALSE) {
  if (is.null(common_names) || is.null(scientific_names)) {
    return(list(
      success = FALSE,
      error = "Missing required parameters 'common_names' and 'scientific_names'",
      note = "Both parameters must be provided as equal-length arrays"
    ))
  }
  
  # Parse both input parameters using shared function
  common_list <- parse_species_input(common_names)
  scientific_list <- parse_species_input(scientific_names)
  
  if (length(common_list) != length(scientific_list)) {
    return(list(
      success = FALSE,
      error = "common_names and scientific_names must have the same length"
    ))
  }
  
  if (length(common_list) < 2) {
    return(list(
      success = FALSE,
      error = "At least 2 species required for tree generation"
    ))
  }
  
  # Parse allow_partial_response parameter
  allow_partial <- FALSE
  if (!is.null(allow_partial_response)) {
    if (is.logical(allow_partial_response)) {
      allow_partial <- allow_partial_response
    } else if (is.character(allow_partial_response)) {
      allow_partial <- tolower(allow_partial_response) %in% c("true", "1", "yes")
    }
  }
  
  tryCatch({
    # Load DateLife
    library(datelife)
    library(ape)
    
    # Clean scientific names to remove parenthetical addendums that can cause phylocom parsing issues
    cleaned_scientific_list <- clean_scientific_names(scientific_list)
    cat("Cleaned scientific names for DateLife query:\n")
    for (i in seq_along(scientific_list)) {
      if (scientific_list[i] != cleaned_scientific_list[i]) {
        cat("  ", scientific_list[i], " -> ", cleaned_scientific_list[i], "\n")
      }
    }
    
    # Try DateLife with the cleaned scientific names with timeout
    cat("Attempting DateLife with species:", paste(cleaned_scientific_list, collapse = ", "), "\n")
    
    # Implement timeout wrapper for DateLife to prevent hanging
    datelife_result <- tryCatch({
      # Set timeout (60 seconds for DateLife query)
      setTimeLimit(cpu = 60, elapsed = 60, transient = TRUE)
      
      # Call DateLife with cleaned names
      result <- get_datelife_result(input = cleaned_scientific_list)
      
      # Reset timeout
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
      
      result
    }, error = function(e) {
      # Reset timeout on error
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
      
      if (grepl("timeout|time limit", e$message)) {
        cat("DateLife query timed out after 60 seconds\n")
        return(NULL)  # Return NULL to trigger timeout error response
      } else {
        stop(e)  # Re-throw other errors
      }
    })
    
    # Check if DateLife timed out
    if (is.null(datelife_result)) {
      return(list(
        success = FALSE,
        coverage = "timeout",
        error = "DateLife query timed out after 60 seconds. This can happen with complex species combinations.",
        input_common_names = common_list,
        input_scientific_names = scientific_list,
        note = "Try the regular /api/tree endpoint for topology-only trees or reduce the number of species"
      ))
    }
    
    if (length(datelife_result) == 0) {
      return(list(
        success = FALSE,
        coverage = "none",
        error = "No chronogram data available for any of the input species",
        input_common_names = common_list,
        input_scientific_names = scientific_list,
        note = "Try the regular /api/tree endpoint for topology-only trees"
      ))
    }
    
    # Check which species are covered
    first_matrix <- datelife_result[[1]]
    covered_species <- rownames(first_matrix)
    missing_indices <- which(!scientific_list %in% gsub("_", " ", covered_species))
    
    # Initialize missing species variables
    missing_scientific <- c()
    missing_common <- c()
    
    if (length(missing_indices) > 0) {
      missing_scientific <- scientific_list[missing_indices]
      missing_common <- common_list[missing_indices]
      
      if (!allow_partial) {
        # Return error response for partial coverage
        return(list(
          success = FALSE,
          coverage = "partial",
          error = "Some species not found in chronogram database",
          input_common_names = common_list,
          input_scientific_names = scientific_list,
          covered_species = gsub("_", " ", covered_species),
          missing_common_names = missing_common,
          missing_scientific_names = missing_scientific,
          note = "DateLife can only generate trees for species with published chronogram data"
        ))
      }
      
      # When allow_partial is true, continue to generate tree with available species
      # Missing species info will be included in the final response
      cat("Partial coverage allowed! Generating tree with", length(covered_species), "covered species...\n")
    }
    
    # All species are covered - generate the dated tree
    cat("All species covered! Generating dated tree...\n")
    
    # Create median consensus matrix
    median_matrix <- datelife_result_median_matrix(datelife_result)
    
    # Convert to phylo object
    phylo_tree <- summary_matrix_to_phylo(median_matrix)
    
    # Get node ages
    node_depths <- node.depth.edgelength(phylo_tree)
    root_age <- max(node_depths)
    
    # Convert to CollapsibleTree-compatible format with ages using both names
    result <- generate_dated_tree_html_paired(phylo_tree, median_matrix, common_list, scientific_list)
    
    if (result$success) {
      result$datelife_info <- list(
        chronograms_used = length(datelife_result),
        root_age_mya = round(root_age, 1),
        covered_species = gsub("_", " ", covered_species),
        data_source = "DateLife chronogram database"
      )
      
      # Add coverage information (always include missing species fields for frontend compatibility)
      result$missing_common_names <- missing_common
      result$missing_scientific_names <- missing_scientific
      result$input_common_names <- common_list
      result$input_scientific_names <- scientific_list
      
      if (length(missing_indices) > 0) {
        result$coverage <- "partial"
        result$datelife_info$coverage_note <- paste0(
          "Partial coverage: ", length(covered_species), " of ", 
          length(scientific_list), " species included"
        )
      } else {
        result$coverage <- "complete"
      }
    }
    
    return(result)
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("DateLife processing error:", conditionMessage(e)),
      input_common_names = common_list,
      input_scientific_names = scientific_list
    ))
  })
}

#* Generate hybrid phylogenetic tree with complete ROTL structure and DateLife ages where available
#* @param common_names A JSON array of species common names
#* @param scientific_names A JSON array of species scientific names (must match common_names length)
#* @post /api/full-tree-dated
function(req, common_names = NULL, scientific_names = NULL) {
  if (is.null(common_names) || is.null(scientific_names)) {
    return(list(
      success = FALSE,
      error = "Missing required parameters 'common_names' and 'scientific_names'",
      note = "Both parameters must be provided as equal-length arrays"
    ))
  }
  
  # Parse both input parameters using shared function
  common_list <- parse_species_input(common_names)
  scientific_list <- parse_species_input(scientific_names)
  
  if (length(common_list) != length(scientific_list)) {
    return(list(
      success = FALSE,
      error = "common_names and scientific_names must have the same length"
    ))
  }
  
  if (length(common_list) < 2) {
    return(list(
      success = FALSE,
      error = "At least 2 species required for tree generation"
    ))
  }
  
  result <- generate_hybrid_tree_html(common_list, scientific_list)
  return(result)
}

#* Get truncated Wikipedia introduction for taxonomic group
#* @param taxonomic_group The taxonomic group name to look up on Wikipedia
#* @param truncate_length Optional maximum length of introduction (default 300)
#* @get /api/wikipedia_truncated_intro
function(taxonomic_group = NULL, truncate_length = 300) {
  if (is.null(taxonomic_group) || taxonomic_group == "") {
    return(list(
      success = FALSE,
      error = "Missing required parameter 'taxonomic_group'",
      note = "Provide the name of a taxonomic group (e.g., 'Mammalia', 'Primates', 'Canidae')"
    ))
  }
  
  # Validate and sanitize truncate_length parameter
  if (!is.null(truncate_length)) {
    truncate_length <- as.numeric(truncate_length)
    if (is.na(truncate_length) || truncate_length < 50) {
      truncate_length <- 300  # Default fallback
    } else if (truncate_length > 1000) {
      truncate_length <- 1000  # Maximum limit to prevent abuse
    }
  } else {
    truncate_length <- 300
  }
  
  # Call Wikipedia API function
  result <- get_wikipedia_intro(taxonomic_group, truncate_length)
  return(result)
}

#* Get citations for all currently attached R packages
#* @get /api/citations
function() {
  tryCatch({
    # Get all currently attached packages
    attached_packages <- sub("^package:", "", search()[grepl("^package:", search())])
    
    # Create a named list to store citations
    citation_map <- list()
    
    # Iterate through each package and capture its citation as text
    for (pkg in attached_packages) {
      cit <- capture.output(print(citation(pkg), style = "text"))
      cit_text <- paste(cit, collapse = "\n")
      
      # Use the citation text as key in the list
      if (cit_text %in% names(citation_map)) {
        citation_map[[cit_text]] <- c(citation_map[[cit_text]], pkg)
      } else {
        citation_map[[cit_text]] <- pkg
      }
    }
    
    # Convert to structured response format
    citations_list <- list()
    for (cit_text in names(citation_map)) {
      packages <- citation_map[[cit_text]]
      citations_list <- append(citations_list, list(list(
        packages = packages,
        citation = cit_text
      )))
    }
    
    return(list(
      success = TRUE,
      count = length(citations_list),
      attached_packages = attached_packages,
      citations = citations_list
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error retrieving citations:", conditionMessage(e))
    ))
  })
}

#* Debug endpoint - returns data structure instead of HTML tree
#* @param count Number of species (2-7, default 3)
#* @get /api/debug-tree
function(count = 3) {
  count <- as.numeric(count)
  if (is.na(count) || count < 2 || count > 7) {
    return(list(
      success = FALSE,
      error = "Count must be between 2 and 7"
    ))
  }
  
  tryCatch({
    random_species <- get_random_species(count)
    
    # Get the hierarchy data without generating HTML
    hierarchy_data <- convert_rotl_to_hierarchy(random_species)
    
    if (is.null(hierarchy_data)) {
      return(list(
        success = FALSE,
        error = "Unable to generate hierarchy data"
      ))
    }
    
    # Create the same color mapping logic as in generate_tree_html
    hierarchy_cols <- names(hierarchy_data)[names(hierarchy_data) != "Species"]
    hierarchy_cols <- c(hierarchy_cols, "Species")
    
    # Collect all unique nodes
    all_unique_nodes <- c("Common ancestor - click me!")
    for (col in hierarchy_cols[hierarchy_cols != "Species"]) {
      unique_vals <- unique(hierarchy_data[[col]][!is.na(hierarchy_data[[col]])])
      all_unique_nodes <- c(all_unique_nodes, unique_vals)
    }
    all_unique_nodes <- c(all_unique_nodes, hierarchy_data$Species)
    
    # Create color mapping
    node_colors <- sapply(all_unique_nodes, function(node) {
      if (node == "Common ancestor - click me!") {
        return("#E74C3C")  # Red for root
      } else if (node == "Ancestor") {
        return("#3498DB")  # Blue for ancestor nodes
      } else if (node %in% hierarchy_data$Species) {
        return("#27AE60")  # Green for species
      } else {
        return("#F39C12")  # Orange for taxonomic groups
      }
    })
    
    # Also show what the network data would look like
    # Convert hierarchy to parent-child relationships (same logic as in generate_tree_html)
    network_data <- data.frame(
      from = character(0),
      to = character(0),
      NodeType = character(0),
      stringsAsFactors = FALSE
    )
    
    root_name <- "Common ancestor - click me!"
    
    # Only process if hierarchy_data exists and has rows
    if (!is.null(hierarchy_data) && nrow(hierarchy_data) > 0) {
      for (i in 1:nrow(hierarchy_data)) {
        path_levels <- c()
        
        for (col in hierarchy_cols) {
          if (col == "Species") {
            path_levels <- c(path_levels, hierarchy_data[[col]][i])
          } else if (!is.na(hierarchy_data[[col]][i])) {
            level_name <- hierarchy_data[[col]][i]
            if (nchar(level_name) > 50) {
              level_name <- paste0(substr(level_name, 1, 47), "...")
            }
            path_levels <- c(path_levels, level_name)
          }
        }
        
        if (length(path_levels) > 0 && path_levels[1] != root_name) {
          path_levels <- c(root_name, path_levels)
        }
        
        if (length(path_levels) < 2) next
        
        for (j in 1:(length(path_levels) - 1)) {
          parent <- path_levels[j]
          child <- path_levels[j + 1]
          
          if (any(network_data$from == parent & network_data$to == child)) next
          
          if (child == root_name) {
            child_type <- "root"
          } else if (trimws(child) == "Ancestor") {  # Trim whitespace
            child_type <- "ancestor"
          } else if (child %in% hierarchy_data$Species) {
            child_type <- "species"
          } else {
            child_type <- "taxonomic"
          }
          
          network_data <- rbind(network_data, data.frame(
            from = parent,
            to = child,
            NodeType = child_type,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Add colors if we have data
      if (nrow(network_data) > 0) {
        network_data$Color <- sapply(network_data$NodeType, function(type) {
          switch(type,
            "root" = "#E74C3C",      # Red
            "ancestor" = "#3498DB",   # Blue  
            "species" = "#27AE60",    # Green
            "taxonomic" = "#F39C12"   # Orange
          )
        })
      }
    }
    
    return(list(
      success = TRUE,
      selected_species = random_species,
      hierarchy_data = hierarchy_data,
      hierarchy_cols = hierarchy_cols,
      all_unique_nodes = all_unique_nodes,
      node_colors = node_colors,
      network_data = network_data,
      species_count = length(random_species)
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating debug data:", conditionMessage(e))
    ))
  })
}