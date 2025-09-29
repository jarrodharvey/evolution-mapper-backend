# DateLife Efficiency Optimization Functions
# Leverages database has_datelife column for performance improvements

library(RSQLite)
library(DBI)
# All required functions are sourced at startup in plumber.R

#' Get species from database with DateLife availability information
#' @param common_names Vector of common names provided by user
#' @param scientific_names Vector of scientific names provided by user
#' @return Data frame with ott, common, scientific, and has_datelife columns
get_species_with_datelife_info <- function(common_names, scientific_names) {
  db_path <- "data/species.sqlite"
  species_db <- dbConnect(SQLite(), db_path)
  
  species_data <- data.frame(
    ott = integer(length(common_names)),
    common = character(length(common_names)),
    scientific = character(length(common_names)),
    has_datelife = integer(length(common_names)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(common_names)) {
    common_name <- common_names[i]
    scientific_name <- scientific_names[i]
    
    # Try to find exact match with both common and scientific names
    sql_string <- sprintf("SELECT ott, common, scientific, has_datelife FROM species WHERE common = '%s' AND scientific = '%s'", 
                         gsub("'", "''", common_name), gsub("'", "''", scientific_name))
    query_result <- dbSendQuery(species_db, sql_string)
    species_df <- dbFetch(query_result)
    dbClearResult(query_result)
    
    if (nrow(species_df) > 0) {
      # Found exact match - use it
      species_data[i, ] <- species_df[1, ]
    } else {
      # No exact match - try scientific name only (more reliable for OTT lookup)
      sql_string <- sprintf("SELECT ott, common, scientific, has_datelife FROM species WHERE scientific = '%s'", 
                           gsub("'", "''", scientific_name))
      query_result <- dbSendQuery(species_db, sql_string)
      species_df <- dbFetch(query_result)
      dbClearResult(query_result)
      
      if (nrow(species_df) > 0) {
        # Found by scientific name - use provided common name but database OTT, scientific, and DateLife info
        species_data[i, ] <- data.frame(
          ott = species_df$ott[1],
          common = common_name,  # Use user-provided common name
          scientific = species_df$scientific[1],
          has_datelife = species_df$has_datelife[1]
        )
      } else {
        # No match found - create placeholder with no DateLife availability
        species_data[i, ] <- data.frame(
          ott = NA,
          common = common_name,
          scientific = scientific_name,
          has_datelife = 0  # Assume no DateLife data if not in database
        )
      }
    }
  }
  
  dbDisconnect(species_db)
  return(species_data)
}

#' Count species with DateLife availability
#' @param species_data Data frame with has_datelife column (from get_species_with_datelife_info)
#' @return Integer count of species with DateLife data
count_datelife_available_species <- function(species_data) {
  # Count species that are valid (have OTT IDs) and have DateLife data
  valid_species <- species_data[!is.na(species_data$ott), ]
  datelife_species_count <- sum(valid_species$has_datelife == 1, na.rm = TRUE)
  return(datelife_species_count)
}

#' Filter species to only those with DateLife availability
#' @param species_data Data frame with has_datelife column (from get_species_with_datelife_info)
#' @return Data frame containing only species with DateLife data
filter_datelife_available_species <- function(species_data) {
  # Filter to valid species with DateLife data
  valid_species <- species_data[!is.na(species_data$ott), ]
  datelife_species <- valid_species[valid_species$has_datelife == 1, ]
  return(datelife_species)
}

#' Check if DateLife processing should be skipped for efficiency
#' @param species_data Data frame with has_datelife column (from get_species_with_datelife_info)
#' @param request_id Optional request ID for logging
#' @return List with should_skip (boolean), reason (string), and datelife_count (integer)
should_skip_datelife_processing <- function(species_data, request_id = NULL) {
  if (is.null(request_id)) {
    request_id <- "datelife_check"
  }
  
  # Count valid species (have OTT IDs)
  valid_species <- species_data[!is.na(species_data$ott), ]
  valid_count <- nrow(valid_species)
  
  # Count species with DateLife data
  datelife_count <- count_datelife_available_species(species_data)
  
  api_log_info(paste("[", request_id, "] DateLife availability check: ", datelife_count, "/", valid_count, " species have DateLife data", sep=""))
  
  # Skip if 0 or 1 species have DateLife data (can't form a tree)
  if (datelife_count <= 1) {
    reason <- paste("Insufficient DateLife coverage (", datelife_count, "/", valid_count, " species) - need at least 2 species with DateLife data", sep="")
    api_log_info(paste("[", request_id, "] SKIPPING DateLife processing: ", reason, sep=""))
    return(list(
      should_skip = TRUE,
      reason = reason,
      datelife_count = datelife_count,
      valid_count = valid_count
    ))
  }
  
  api_log_info(paste("[", request_id, "] DateLife processing will proceed with ", datelife_count, " species", sep=""))
  return(list(
    should_skip = FALSE,
    reason = "Sufficient DateLife coverage available",
    datelife_count = datelife_count,
    valid_count = valid_count
  ))
}

#' Get scientific names for DateLife processing (filtered to only DateLife-available species)
#' @param species_data Data frame with has_datelife column (from get_species_with_datelife_info)
#' @param request_id Optional request ID for logging
#' @return Vector of scientific names that have DateLife data
get_datelife_scientific_names <- function(species_data, request_id = NULL) {
  if (is.null(request_id)) {
    request_id <- "datelife_names"
  }
  
  datelife_species <- filter_datelife_available_species(species_data)
  scientific_names <- datelife_species$scientific
  
  api_log_info(paste("[", request_id, "] Filtered to ", length(scientific_names), " scientific names for DateLife processing", sep=""))
  
  if (length(scientific_names) > 0) {
    api_log_info(paste("[", request_id, "] DateLife species: ", paste(head(scientific_names, 3), collapse = ', '), if(length(scientific_names) > 3) '...' else '', sep=""))
  }
  
  return(scientific_names)
}

#' Get summary statistics for DateLife efficiency optimization
#' @param species_data Data frame with has_datelife column (from get_species_with_datelife_info)
#' @param request_id Optional request ID for logging
#' @return List with efficiency statistics
get_datelife_efficiency_stats <- function(species_data, request_id = NULL) {
  if (is.null(request_id)) {
    request_id <- "efficiency_stats"
  }
  
  total_input <- nrow(species_data)
  valid_species <- species_data[!is.na(species_data$ott), ]
  valid_count <- nrow(valid_species)
  datelife_count <- count_datelife_available_species(species_data)
  
  # Calculate efficiency metrics
  datelife_percentage <- if (valid_count > 0) round((datelife_count / valid_count) * 100, 1) else 0
  skip_efficiency <- if (datelife_count <= 1) "100% (complete skip)" else paste0(round(100 - (datelife_count / valid_count) * 100, 1), "% (partial filtering)")
  
  stats <- list(
    total_input_species = total_input,
    valid_species_count = valid_count,
    datelife_available_count = datelife_count,
    datelife_percentage = datelife_percentage,
    processing_efficiency = skip_efficiency,
    can_skip_completely = (datelife_count <= 1)
  )
  
  api_log_info(paste("[", request_id, "] Efficiency stats - Input: ", total_input, ", Valid: ", valid_count, ", DateLife: ", datelife_count, " (", datelife_percentage, "%), Efficiency: ", skip_efficiency, sep=""))
  
  return(stats)
}