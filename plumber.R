# Evolution Mapper - Distributed Backend API
# R Plumber API for serving phylogenetic tree data

library(plumber)

# Source tree generation functions
source("functions/tree_generation.R")

# Simple in-memory rate limiting - tracks requests per IP
rate_limit_storage <- new.env()
rate_limit_window <- 60  # seconds
rate_limit_max <- 60     # requests per window

#* @apiTitle Evolution Mapper API
#* @apiDescription API for generating phylogenetic trees and species data

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