# Evolution Mapper - Distributed Backend API
# R Plumber API for serving phylogenetic tree data

library(plumber)

# Source tree generation functions
source("functions/tree_generation.R")

#* @apiTitle Evolution Mapper API
#* @apiDescription API for generating phylogenetic trees and species data

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

#* Get all unique species common names for frontend picker
#* @get /api/species
function() {
  tryCatch({
    species_names <- get_all_species_names()
    list(
      success = TRUE,
      count = length(species_names),
      species = species_names
    )
  }, error = function(e) {
    list(
      success = FALSE,
      error = paste("Error fetching species:", conditionMessage(e))
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