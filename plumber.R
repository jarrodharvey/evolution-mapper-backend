# Evolution Mapper - Distributed Backend API
# R Plumber API for serving phylogenetic tree data

library(plumber)

# Source tree generation functions
source("functions/tree_generation.R")

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
  # Fallback keys if .Renviron not found (not recommended for production)
  warning("No EVOLUTION_API_KEYS environment variable found. Using fallback demo keys.")
  valid_api_keys <- c("demo-key-12345", "research-key-67890", "dev-key-abcde")
}

#* @apiTitle Evolution Mapper API
#* @apiDescription API for generating phylogenetic trees and species data

#* API Key authentication filter
#* @filter apikey
function(req, res) {
  # Skip authentication for health check endpoint
  if (req$PATH_INFO == "/api/health") {
    plumber::forward()
    return()
  }
  
  # Get API key from header or query parameter
  api_key <- req$HTTP_X_API_KEY %||% req$args$api_key
  
  # Check if API key is provided and valid
  if (is.null(api_key) || !api_key %in% valid_api_keys) {
    res$status <- 401  # Unauthorized
    return(list(
      success = FALSE,
      error = "Invalid or missing API key. Include your API key in the 'X-API-Key' header or 'api_key' query parameter.",
      documentation = "Contact the API administrator for access credentials."
    ))
  }
  
  # Store API key in request for potential logging/tracking
  req$api_key <- api_key
  
  plumber::forward()
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
  
  plumber::forward()
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
    
    species_data <- search_species(search, limit)
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

#* Generate phylogenetic tree from list of species common names
#* @param species A JSON array of species common names
#* @post /api/tree
function(req, species = NULL) {
  if (is.null(species)) {
    return(list(
      success = FALSE,
      error = "Missing required parameter 'species'"
    ))
  }
  
  # Handle both JSON array and comma-separated string formats
  if (is.character(species)) {
    # If it's a single string, try parsing as JSON or split by comma
    if (startsWith(species, "[") && endsWith(species, "]")) {
      species_list <- jsonlite::fromJSON(species)
    } else {
      species_list <- trimws(strsplit(species, ",")[[1]])
    }
  } else {
    species_list <- species
  }
  
  if (length(species_list) < 2) {
    return(list(
      success = FALSE,
      error = "At least 2 species required for tree generation"
    ))
  }
  
  result <- generate_tree_html(species_list)
  return(result)
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
    random_species <- get_random_species(count)
    result <- generate_tree_html(random_species)
    
    if (result$success) {
      result$selected_species <- random_species
    }
    
    return(result)
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating random tree:", conditionMessage(e))
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