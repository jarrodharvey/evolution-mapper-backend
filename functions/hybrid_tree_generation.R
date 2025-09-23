# Hybrid Tree Generation Functions
# Combines ROTL's complete topology with DateLife's partial age data
# Provides "best of both worlds" - complete species coverage with age data where available

library(rotl)
library(ape)
library(datelife)
library(collapsibleTree)
library(RSQLite)
library(DBI)
library(dplyr)
library(memoise)

# Source shared logging configuration
source("functions/logging_config.R")

# Source progress tracking functions
source("functions/progress_tracking.R")

source("functions/rotl_tree_generation.R")
source("functions/datelife_tree_generation.R")  # For optimized DateLife functions
source("functions/datelife_efficiency.R")  # For DateLife efficiency optimizations
source("functions/color_config.R")

#' Clean scientific names by removing parenthetical addendums
#' @param scientific_names Vector of scientific names
#' @return Vector of cleaned scientific names
clean_scientific_names <- function(scientific_names) {
  # Remove parenthetical addendums like "(species in domain Eukaryota)"
  # but preserve the main binomial name
  cleaned <- gsub("\\s*\\([^)]+\\)\\s*$", "", scientific_names)
  cleaned <- trimws(cleaned)  # Remove any trailing whitespace
  return(cleaned)
}

#' Assess calibration quality for reliable root age estimation
#' @param phylo_tree ROTL phylo tree  
#' @param calibrations Calibration points data frame
#' @param species_data Species data with DateLife availability
#' @param request_id Optional request ID for logging
#' @return List with quality assessment results
assess_calibration_quality <- function(phylo_tree, calibrations, species_data, request_id = "quality_check") {
  
  api_log_info(paste("[", request_id, "] Assessing calibration quality for root age reliability"))
  
  if (is.null(calibrations) || nrow(calibrations) == 0) {
    api_log_warn(paste("[", request_id, "] No calibrations available - root age unreliable"))
    return(list(
      sufficient_quality = FALSE,
      reason = "No calibration points available",
      recommendation = "Display 'Uncalibrated' for root"
    ))
  }
  
  n_tips <- length(phylo_tree$tip.label)
  n_calibrations <- nrow(calibrations)
  
  api_log_info(paste("[", request_id, "] Calibration assessment: ", n_calibrations, " calibrations for ", n_tips, " species"))
  
  # Quality metric 1: Minimum calibration threshold
  # Need at least 2 calibrations for any meaningful root age estimation
  if (n_calibrations < 2) {
    api_log_warn(paste("[", request_id, "] Insufficient calibrations (", n_calibrations, " < 2) - root age unreliable"))
    return(list(
      sufficient_quality = FALSE,
      reason = paste("Only", n_calibrations, "calibration point(s) available"),
      recommendation = "Need ≥2 calibrations for root age estimation"
    ))
  }
  
  # Quality metric 2: Deep lineage coverage
  # Check if calibrations span different major lineages from the root
  # Get the sister groups that branch directly from the root
  root_children <- phylo_tree$edge[phylo_tree$edge[, 1] == (n_tips + 1), 2]
  
  if (length(root_children) < 2) {
    api_log_warn(paste("[", request_id, "] Tree topology issue - root has < 2 children"))
    return(list(
      sufficient_quality = FALSE,
      reason = "Unusual tree topology",
      recommendation = "Root age calculation not applicable"
    ))
  }
  
  # Check if we have calibrations in different sister lineages from the root
  calibrated_lineages <- 0
  for (child in root_children) {
    # Get all descendants of this root child
    if (child <= n_tips) {
      # Direct tip descendant
      descendants <- child
    } else {
      # Internal node - get all tip descendants  
      descendants <- extract.clade(phylo_tree, child)$tip.label
      descendants <- match(descendants, phylo_tree$tip.label)
    }
    
    # Check if any calibrations involve species from this lineage
    lineage_has_calibration <- FALSE
    for (i in 1:nrow(calibrations)) {
      calib_species <- c(calibrations$species1[i], calibrations$species2[i])
      # Clean tip labels to match calibration species format
      clean_tips <- gsub("_ott\\d+", "", phylo_tree$tip.label[descendants])
      clean_tips <- gsub("_", " ", clean_tips)
      
      if (any(calib_species %in% clean_tips)) {
        lineage_has_calibration <- TRUE
        break
      }
    }
    
    if (lineage_has_calibration) {
      calibrated_lineages <- calibrated_lineages + 1
    }
  }
  
  api_log_info(paste("[", request_id, "] Deep lineage coverage: ", calibrated_lineages, "/", length(root_children), " root sister groups have calibrations"))
  
  # Quality metric 3: Coverage ratio
  # What percentage of total species have DateLife data?
  datelife_coverage_pct <- (sum(!is.na(species_data$datelife_available) & species_data$datelife_available) / nrow(species_data)) * 100
  
  api_log_info(paste("[", request_id, "] DateLife coverage: ", round(datelife_coverage_pct, 1), "% of species"))
  
  # Decision criteria based on Gemini's research
  sufficient_quality <- FALSE
  reason <- ""
  recommendation <- ""
  
  if (calibrated_lineages >= 2) {
    # Good: Calibrations in multiple sister lineages from root
    sufficient_quality <- TRUE
    reason <- paste("Strong calibration quality:", calibrated_lineages, "root sister lineages calibrated")
    recommendation <- "Root age display scientifically justified"
    api_log_info(paste("[", request_id, "] QUALITY PASS: Root age is scientifically defensible"))
  } else {
    # Poor: Calibrations clustered in single lineage - root age is unreliable extrapolation
    sufficient_quality <- FALSE
    reason <- paste("Insufficient deep coverage - only", calibrated_lineages, "root sister lineage(s) calibrated")
    recommendation <- "Root age would be unreliable extrapolation - display 'Insufficient calibration data'"
    api_log_warn(paste("[", request_id, "] QUALITY FAIL: Root age not scientifically defensible"))
  }
  
  return(list(
    sufficient_quality = sufficient_quality,
    reason = reason, 
    recommendation = recommendation,
    n_calibrations = n_calibrations,
    calibrated_lineages = calibrated_lineages,
    total_lineages = length(root_children),
    datelife_coverage_pct = round(datelife_coverage_pct, 1)
  ))
}

#' Get current OTT ID for scientific name using TNRS with tree validation and disk caching
#' @param scientific_name Scientific name to look up
#' @param request_id Request ID for logging
#' @return Current OTT ID that is confirmed to be in the tree, or NULL if not found
get_current_ott_id_cached <- memoise(
  function(scientific_name, request_id = "cache") {
    tryCatch({
      api_log_info(paste("[", request_id, "] TNRS lookup for:", scientific_name))
      
      # Use ROTL's Taxonomic Name Resolution Service
      tnrs_result <- tnrs_match_names(names = scientific_name, 
                                      context_name = NULL,  # Use all contexts
                                      do_approximate_matching = TRUE,
                                      include_suppressed = FALSE)
      
      if (nrow(tnrs_result) > 0) {
        # Check all matches, not just the first one
        for (i in 1:nrow(tnrs_result)) {
          if (!is.na(tnrs_result$ott_id[i])) {
            candidate_ott <- tnrs_result$ott_id[i]
            match_score <- tnrs_result$score[i]
            unique_name <- tnrs_result$unique_name[i]
            flags <- tnrs_result$flags[i]
            
            api_log_info(paste("[", request_id, "] Checking candidate OTT ID:", candidate_ott, "for", unique_name, "(score:", match_score, ", flags:", flags, ")"))
            
            # Validate that this OTT ID is actually in the current tree
            if (is_in_tree(candidate_ott)) {
              api_log_info(paste("[", request_id, "] TNRS found valid OTT ID:", candidate_ott, "for", scientific_name, "->", unique_name))
              return(candidate_ott)
            } else {
              api_log_warn(paste("[", request_id, "] TNRS candidate OTT ID", candidate_ott, "is not in tree (", unique_name, ")"))
            }
          }
        }
        
        # No valid OTT IDs found
        api_log_error(paste("[", request_id, "] TNRS found", nrow(tnrs_result), "matches but none are in the current tree for:", scientific_name))
        return(NULL)
      } else {
        api_log_warn(paste("[", request_id, "] TNRS could not find any matches for:", scientific_name))
        return(NULL)
      }
    }, error = function(e) {
      api_log_error(paste("[", request_id, "] TNRS lookup failed for", scientific_name, ":", conditionMessage(e)))
      return(NULL)
    })
  },
  cache = cache_filesystem("cache/tnrs_cache")
)

#' Attempt to recover from pruned OTT IDs using TNRS
#' @param error_message ROTL error message
#' @param valid_species Data frame with common, scientific, ott columns
#' @param request_id Request ID for logging  
#' @return List with updated valid_species data frame and dropped species info
recover_from_pruned_ott_ids <- function(error_message, valid_species, request_id) {
  api_log_info(paste("[", request_id, "] Attempting recovery from pruned OTT IDs..."))
  
  # Extract pruned OTT IDs from error message
  # Example error: "node_id 'ott426117' was not found!list(ott426117 = \"pruned_ott_id\")"
  pruned_otts <- regmatches(error_message, gregexpr("ott[0-9]+", error_message))[[1]]
  
  if (length(pruned_otts) == 0) {
    api_log_warn(paste("[", request_id, "] Could not extract pruned OTT IDs from error message"))
    return(NULL)
  }
  
  api_log_info(paste("[", request_id, "] Found", length(pruned_otts), "pruned OTT IDs:", paste(pruned_otts, collapse = ", ")))
  
  # Convert to numeric (remove 'ott' prefix)
  pruned_ott_nums <- as.numeric(gsub("ott", "", pruned_otts))
  
  # Find affected species
  affected_species <- valid_species[valid_species$ott %in% pruned_ott_nums, ]
  
  if (nrow(affected_species) == 0) {
    api_log_warn(paste("[", request_id, "] No matching species found for pruned OTT IDs"))
    return(NULL)
  }
  
  api_log_info(paste("[", request_id, "] Attempting TNRS lookup for", nrow(affected_species), "affected species"))
  
  # Try to get current OTT IDs using TNRS
  updated_species <- valid_species
  species_to_remove <- c()
  
  for (i in 1:nrow(affected_species)) {
    old_ott <- affected_species$ott[i]
    scientific_name <- affected_species$scientific[i]
    common_name <- affected_species$common[i]
    
    api_log_info(paste("[", request_id, "] Recovering OTT ID for:", common_name, "(", scientific_name, ") - old OTT:", old_ott))
    
    new_ott <- get_current_ott_id_cached(scientific_name, request_id)
    
    if (!is.null(new_ott)) {
      # Update the OTT ID in our data frame
      updated_species[updated_species$ott == old_ott, "ott"] <- new_ott
      api_log_info(paste("[", request_id, "] Updated", common_name, "from OTT", old_ott, "to OTT", new_ott))
    } else {
      api_log_warn(paste("[", request_id, "] Could not recover OTT ID for", common_name, "(", scientific_name, ") - will remove from tree"))
      species_to_remove <- c(species_to_remove, old_ott)
    }
  }
  
  # Remove species that couldn't be recovered and track dropped species info
  dropped_common <- c()
  dropped_scientific <- c()
  
  if (length(species_to_remove) > 0) {
    # Get info about dropped species before removing them
    dropped_species_info <- updated_species[updated_species$ott %in% species_to_remove, ]
    dropped_common <- dropped_species_info$common
    dropped_scientific <- dropped_species_info$scientific
    
    updated_species <- updated_species[!updated_species$ott %in% species_to_remove, ]
    api_log_info(paste("[", request_id, "] Removed", length(species_to_remove), "species that could not be recovered"))
  }
  
  # Check if we still have enough species for tree generation (minimum 2)
  if (nrow(updated_species) < 2) {
    api_log_error(paste("[", request_id, "] Insufficient species remaining after recovery (", nrow(updated_species), "< 2)"))
    return(list(
      updated_species = NULL,
      dropped_common_names = dropped_common,
      dropped_scientific_names = dropped_scientific
    ))
  }
  
  recovered_count <- nrow(affected_species) - length(species_to_remove)
  api_log_info(paste("[", request_id, "] TNRS recovery completed:", recovered_count, "recovered,", length(species_to_remove), "removed,", nrow(updated_species), "total remaining"))
  
  return(list(
    updated_species = updated_species,
    dropped_common_names = dropped_common,
    dropped_scientific_names = dropped_scientific
  ))
}

# Calculate dynamic link length for hybrid trees with age information
calculate_dynamic_link_length_hybrid <- function(network_data, base_length = 100, char_multiplier = 4) {
  # Get all node names (including age information in ancestor labels)
  all_names <- c(network_data$from, network_data$to)
  
  # Find the longest label
  max_chars <- max(nchar(all_names), na.rm = TRUE)
  
  # Calculate dynamic length with extra space for age info: base + (characters * multiplier)
  # Use higher base and multiplier than standard trees since we have age annotations
  dynamic_length <- base_length + (max_chars * char_multiplier)
  
  # Set reasonable bounds (minimum 140, maximum 300 for hybrid trees)
  dynamic_length <- max(140, min(300, dynamic_length))
  
  return(dynamic_length)
}

#' Generate hybrid tree HTML combining ROTL topology with DateLife ages
#' @param common_names Vector of common names provided by user
#' @param scientific_names Vector of scientific names provided by user
#' @param request_id Optional request ID for logging correlation
#' @return List with success status and HTML or error message
generate_hybrid_tree_html <- function(common_names, scientific_names, request_id = NULL, progress_token = NULL, throttle_secs = 0, expansion_speed = 750, as_json = FALSE) {
  if (is.null(request_id)) {
    request_id <- paste0("hybrid_", format(Sys.time(), "%H%M%S"))
  }
  
  # Helper function to update progress if token provided
  update_progress_internal <- function(step_name, status = "completed", additional_data = NULL) {
    if (!is.null(progress_token) && progress_token != "") {
      # Call the global update_progress function
      update_progress(progress_token, step_name, status, additional_data)
      
      # Add throttle delay if specified (for testing progress monitoring)
      if (throttle_secs > 0) {
        Sys.sleep(throttle_secs)
      }
    }
  }
  
  tryCatch({
    # Configure logging to write to same log file
    if (exists("log_appender", mode = "function")) {
      # Ensure logs directory exists
      if (!dir.exists("logs")) {
        dir.create("logs", recursive = TRUE)
      }
      # Configure file appender for logging
      log_appender(appender_file("logs/api.log", append = TRUE), namespace = "evolution.api")
      log_layout(layout_simple, namespace = "evolution.api")
      log_threshold(INFO, namespace = "evolution.api")
    }
    
    api_log_info(paste("[", request_id, "] === STARTING HYBRID TREE GENERATION ===", sep=""))
    api_log_info(paste("[", request_id, "] Input:", length(common_names), "species"))
    api_log_info(paste("[", request_id, "] Common names:", paste(head(common_names, 5), collapse = ', '), if(length(common_names) > 5) '...' else '', sep=" "))
    api_log_info(paste("[", request_id, "] Scientific names:", paste(head(scientific_names, 5), collapse = ', '), if(length(scientific_names) > 5) '...' else '', sep=" "))
    
    # Initialize dropped species tracking variables
    dropped_common_names <- c()
    dropped_scientific_names <- c()
    
    # Step 1: Get species data with DateLife availability information
    step_start <- Sys.time()
    api_log_info(paste("[", request_id, "] STEP 1: Getting species data with DateLife availability from database...", sep=""))
    
    species_data <- get_species_with_datelife_info(common_names, scientific_names)
    valid_species <- species_data[!is.na(species_data$ott), ]
    
    # Get efficiency statistics
    efficiency_stats <- get_datelife_efficiency_stats(species_data, request_id)
    
    step_duration <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Database lookup completed - Found", nrow(valid_species), "/", nrow(species_data), "valid OTT IDs - Duration:", round(step_duration, 3), "s"))
    api_log_info(paste("[", request_id, "] DateLife availability: ", efficiency_stats$datelife_available_count, "/", efficiency_stats$valid_species_count, " species (", efficiency_stats$datelife_percentage, "%)", sep=""))
    
    update_progress_internal("database_lookup", "completed", 
                           list(valid_species = nrow(valid_species), 
                                total_species = nrow(species_data),
                                datelife_available = efficiency_stats$datelife_available_count,
                                duration_seconds = round(step_duration, 3)))
    
    if (nrow(valid_species) < 2) {
      # Find missing species for both OTT ID lookup and consistency with /api/dated-tree format
      missing_indices <- which(is.na(species_data$ott))
      missing_common <- if (length(missing_indices) > 0) species_data$common[missing_indices] else c()
      missing_scientific <- if (length(missing_indices) > 0) species_data$scientific[missing_indices] else c()
      
      api_log_error(paste("[", request_id, "] INSUFFICIENT VALID SPECIES - Only", nrow(valid_species), "species with valid OTT IDs (need ≥2)"))
      api_log_error(paste("[", request_id, "] Missing OTT IDs for:", paste(missing_common, collapse = ', ')))
      
      return(list(
        success = FALSE,
        error = "Insufficient species with valid OTT IDs for tree generation",
        missing_species = missing_common,  # Keep for backwards compatibility
        # Add missing species fields for frontend compatibility (like /api/dated-tree)
        missing_common_names = missing_common,
        missing_scientific_names = missing_scientific,
        input_common_names = common_names,
        input_scientific_names = scientific_names,
        coverage = "insufficient_data"
      ))
    }
    
    # Step 2: Check if DateLife processing should be skipped for efficiency
    step_start <- Sys.time()
    datelife_skip_check <- should_skip_datelife_processing(species_data, request_id)
    
    if (datelife_skip_check$should_skip) {
      api_log_info(paste("[", request_id, "] STEP 2: SKIPPING DateLife processing (efficiency optimization) - ", datelife_skip_check$reason, sep=""))
      api_log_info(paste("[", request_id, "] STEP 2: Running ROTL query only...", sep=""))
      
      # Run ROTL only since DateLife would be ineffective
      rotl_result <- tryCatch({
        rotl_tree <- tol_induced_subtree(ott_ids = valid_species$ott)
        list(success = TRUE, result = rotl_tree)
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })
      
      # Set empty DateLife results
      datelife_result_wrapper <- list(success = TRUE, result = list())
      
    } else {
      api_log_info(paste("[", request_id, "] STEP 2: Running ROTL and filtered DateLife queries sequentially...", sep=""))
      update_progress_internal("sequential_queries", "in_progress", 
                             list(step = "ROTL and DateLife queries"))
      
      # Get filtered scientific names for DateLife (efficiency optimization)
      datelife_scientific_names <- get_datelife_scientific_names(species_data, request_id)
      cleaned_datelife_names <- clean_scientific_names(datelife_scientific_names)
      
      api_log_info(paste("[", request_id, "] Scientific name cleaning for DateLife subset:", sep=""))
      cleaned_count <- 0
      for (i in seq_along(datelife_scientific_names)) {
        if (datelife_scientific_names[i] != cleaned_datelife_names[i]) {
          api_log_info(paste("[", request_id, "]  ", datelife_scientific_names[i], " -> ", cleaned_datelife_names[i], sep=""))
          cleaned_count <- cleaned_count + 1
        }
      }
      if (cleaned_count == 0) {
        api_log_info(paste("[", request_id, "]   No names required cleaning", sep=""))
      } else {
        api_log_info(paste("[", request_id, "]   Cleaned", cleaned_count, "scientific names"))
      }
    
      # Run queries sequentially
      sequential_start <- Sys.time()
      
      # Task 1: ROTL query
      api_log_info(paste("[", request_id, "] Starting ROTL query with", nrow(valid_species), "OTT IDs...", sep=""))
      update_progress_internal("rotl_query", "in_progress", 
                             list(step = "ROTL phylogenetic tree query"))
      
      rotl_start <- Sys.time()
      rotl_result <- tryCatch({
        rotl_tree <- tol_induced_subtree(ott_ids = valid_species$ott)
        list(success = TRUE, result = rotl_tree, task = "rotl")
      }, error = function(e) {
        list(success = FALSE, error = e$message, task = "rotl")
      })
      rotl_duration <- as.numeric(difftime(Sys.time(), rotl_start, units = "secs"))
      
      if (rotl_result$success) {
        api_log_info(paste("[", request_id, "] ROTL query completed successfully - Duration:", round(rotl_duration, 3), "s"))
      } else {
        api_log_error(paste("[", request_id, "] ROTL query failed - Duration:", round(rotl_duration, 3), "s -", rotl_result$error))
      }
      
      update_progress_internal("rotl_query", "completed", 
                             list(duration_seconds = round(rotl_duration, 3),
                                  success = rotl_result$success))
      
      # Task 2: DateLife query
      api_log_info(paste("[", request_id, "] Starting DateLife query with", length(cleaned_datelife_names), "filtered scientific names...", sep=""))
      update_progress_internal("datelife_query", "in_progress", 
                             list(step = "DateLife chronogram query"))
      
      datelife_start <- Sys.time()
      datelife_result_wrapper <- tryCatch({
        # Use original DateLife function with filtered names
        datelife_result <- get_datelife_result(input = cleaned_datelife_names, get_spp_from_taxon = FALSE, reference_taxonomy = 'opentree')
        list(success = TRUE, result = datelife_result, task = "datelife")
      }, error = function(e) {
        list(success = FALSE, error = e$message, task = "datelife", result = list())
      })
      datelife_duration <- as.numeric(difftime(Sys.time(), datelife_start, units = "secs"))
      
      if (datelife_result_wrapper$success) {
        api_log_info(paste("[", request_id, "] DateLife query completed successfully - Duration:", round(datelife_duration, 3), "s"))
        api_log_info(paste("[", request_id, "] Found", length(datelife_result_wrapper$result), "chronograms"))
      } else {
        api_log_error(paste("[", request_id, "] DateLife query failed - Duration:", round(datelife_duration, 3), "s -", datelife_result_wrapper$error))
      }
      
      update_progress_internal("datelife_query", "completed", 
                             list(duration_seconds = round(datelife_duration, 3),
                                  success = datelife_result_wrapper$success,
                                  chronograms_found = if(datelife_result_wrapper$success) length(datelife_result_wrapper$result) else 0))
      
      sequential_duration <- as.numeric(difftime(Sys.time(), sequential_start, units = "secs"))
    }
    
    step_duration <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Step 2 completed - Duration:", round(step_duration, 3), "s", sep=""))
    
    update_progress_internal("sequential_queries", "completed", 
                           list(duration_seconds = round(step_duration, 3)))
    
    api_log_info(paste("[", request_id, "] Processing query results...", sep=""))
    
    # Handle ROTL result
    if (!rotl_result$success || is.null(rotl_result$result)) {
      api_log_error(paste("[", request_id, "] ROTL QUERY FAILED:", rotl_result$error))
      
      # Check if this is a pruned OTT ID error and attempt recovery
      if (grepl("was not found.*pruned_ott_id", rotl_result$error)) {
        api_log_info(paste("[", request_id, "] Detected pruned OTT ID error - attempting TNRS recovery"))
        
        recovery_result <- recover_from_pruned_ott_ids(rotl_result$error, valid_species, request_id)
        
        if (!is.null(recovery_result$updated_species)) {
          api_log_info(paste("[", request_id, "] TNRS recovery successful - retrying ROTL query with updated OTT IDs"))
          
          # Retry ROTL query with updated OTT IDs
          retry_result <- tryCatch({
            rotl_tree <- tol_induced_subtree(ott_ids = recovery_result$updated_species$ott)
            list(success = TRUE, result = rotl_tree)
          }, error = function(e) {
            list(success = FALSE, error = e$message)
          })
          
          if (retry_result$success) {
            api_log_info(paste("[", request_id, "] ROTL retry successful after TNRS recovery"))
            rotl_tree <- retry_result$result
            # Update valid_species with recovered OTT IDs for downstream processing
            valid_species <- recovery_result$updated_species
            # Store dropped species information for final response
            dropped_common_names <- recovery_result$dropped_common_names
            dropped_scientific_names <- recovery_result$dropped_scientific_names
            
            # CRITICAL FIX: Recalculate DateLife scientific names after recovery
            # This ensures the DateLife query uses the same species as the recovered ROTL tree
            api_log_info(paste("[", request_id, "] Recalculating DateLife species list after recovery"))
            
            # Create updated species_data matching the recovered valid_species
            recovered_species_data <- species_data[species_data$scientific %in% valid_species$scientific, ]
            
            # Recalculate DateLife scientific names with the recovered species
            datelife_scientific_names <- get_datelife_scientific_names(recovered_species_data, request_id)
            cleaned_datelife_names <- clean_scientific_names(datelife_scientific_names)
            
            api_log_info(paste("[", request_id, "] Updated DateLife species count:", length(cleaned_datelife_names)))
            
            # Re-run DateLife query with updated species list
            api_log_info(paste("[", request_id, "] Re-running DateLife query with recovered species list..."))
            datelife_start <- Sys.time()
            datelife_result_wrapper <- tryCatch({
              datelife_result <- get_datelife_result(input = cleaned_datelife_names, get_spp_from_taxon = FALSE, reference_taxonomy = 'opentree')
              list(success = TRUE, result = datelife_result, task = "datelife")
            }, error = function(e) {
              list(success = FALSE, error = e$message, task = "datelife", result = list())
            })
            datelife_duration <- as.numeric(difftime(Sys.time(), datelife_start, units = "secs"))
            
            if (datelife_result_wrapper$success) {
              api_log_info(paste("[", request_id, "] Updated DateLife query completed - Duration:", round(datelife_duration, 3), "s"))
              api_log_info(paste("[", request_id, "] Found", length(datelife_result_wrapper$result), "chronograms"))
            } else {
              api_log_error(paste("[", request_id, "] Updated DateLife query failed - Duration:", round(datelife_duration, 3), "s -", datelife_result_wrapper$error))
            }
          } else {
            api_log_error(paste("[", request_id, "] ROTL retry failed even after TNRS recovery:", retry_result$error))
            return(list(
              success = FALSE,
              error = paste("Failed to get tree from Open Tree of Life even after TNRS recovery:", retry_result$error)
            ))
          }
        } else {
          api_log_error(paste("[", request_id, "] TNRS recovery failed"))
          return(list(
            success = FALSE,
            error = paste("Failed to get tree from Open Tree of Life (TNRS recovery also failed):", rotl_result$error)
          ))
        }
      } else {
        # Non-pruned error, return original error
        return(list(
          success = FALSE,
          error = paste("Failed to get tree from Open Tree of Life:", rotl_result$error)
        ))
      }
    } else {
      rotl_tree <- rotl_result$result
    }
    api_log_info(paste("[", request_id, "] ROTL query successful - Tree has", length(rotl_tree$tip.label), "tips and", rotl_tree$Nnode, "internal nodes"))
    
    # Handle DateLife result
    if (datelife_result_wrapper$success) {
      datelife_result <- datelife_result_wrapper$result
      api_log_info(paste("[", request_id, "] DateLife query successful - Found", length(datelife_result), "chronograms"))
    } else {
      datelife_result <- list()
      api_log_warn(paste("[", request_id, "] DateLife query failed:", datelife_result_wrapper$error))
      api_log_info(paste("[", request_id, "] Proceeding with topology-only tree (no age data will be available)", sep=""))
    }
    
    # Step 3: Create age mapping from DateLife data
    step_start <- Sys.time()
    api_log_info(paste("[", request_id, "] STEP 3: Processing DateLife age data...", sep=""))
    update_progress_internal("datelife_processing", "in_progress", 
                           list(step = "Processing DateLife age data"))
    
    datelife_phylo <- NULL
    datelife_species <- c()
    ancestor_ages <- list()
    age_assignment_method <- "none"  # Default method when no age data available
    
    if (length(datelife_result) > 0) {
      api_log_info(paste("[", request_id, "] Processing", length(datelife_result), "DateLife chronograms..."))
      
      # Extract species that have DateLife data
      for (i in seq_along(datelife_result)) {
        study_species <- rownames(datelife_result[[i]])
        datelife_species <- unique(c(datelife_species, study_species))
      }
      api_log_info(paste("[", request_id, "] Extracted", length(datelife_species), "unique species from DateLife chronograms"))
      
      # Use modern chronos approach instead of problematic summary_matrix_to_phylo
      tryCatch({
        api_log_info(paste("[", request_id, "] Using modern chronos approach for DateLife age calibration...", sep=""))
        
        # Source the modern age mapping functions
        source("functions/modern_age_mapping.R")
        
        # Create species data frame for chronos
        species_data_for_chronos <- data.frame(
          common = common_names,
          scientific = scientific_names,
          stringsAsFactors = FALSE
        )
        
        chronos_start <- Sys.time()
        chronos_result <- generate_dated_tree_chronos(rotl_tree, datelife_result, species_data_for_chronos, request_id)
        chronos_duration <- as.numeric(difftime(Sys.time(), chronos_start, units = "secs"))
        
        if (chronos_result$success) {
          api_log_info(paste("[", request_id, "] Modern chronos approach successful - Duration:", round(chronos_duration, 3), "s"))
          api_log_info(paste("[", request_id, "] Calibrations used:", nrow(chronos_result$calibrations_used)))
          api_log_info(paste("[", request_id, "] Pairwise ages found:", chronos_result$pairwise_ages_found))
          
          # Use the ages from chronos result
          ancestor_ages <- chronos_result$node_ages
          age_assignment_method <- chronos_result$method  # Track method: "chronos" or "direct_pairwise_fallback"
          
          # Get root age from the dated tree
          branching_times_tree <- branching.times(chronos_result$dated_tree)
          root_age <- max(branching_times_tree)
          api_log_info(paste("[", request_id, "] Root age from chronos:", round(root_age, 1), "Mya"))
          
          # CRITICAL: Assess calibration quality before including root age
          calibration_quality <- assess_calibration_quality(rotl_tree, chronos_result$calibrations_used, species_data, request_id)
          
          # Store quality assessment for later use in network conversion
          attr(ancestor_ages, "root_quality_check") <- calibration_quality
          
          api_log_info(paste("[", request_id, "] Extracted ancestor ages for", length(ancestor_ages), "internal nodes from modern chronos"))
          
        } else {
          api_log_warn(paste("[", request_id, "] Modern chronos approach failed:", chronos_result$error))
          api_log_info(paste("[", request_id, "] Proceeding with ROTL tree only (age data will be unavailable)"))
          ancestor_ages <- list()
          age_assignment_method <- "none"  # No age data available
        }
      }, error = function(e) {
        api_log_error(paste("[", request_id, "] Could not create DateLife phylo tree:", conditionMessage(e)))
        # Don't reset age_assignment_method here - it may have been set correctly by successful fallback
        if (!exists("age_assignment_method")) {
          age_assignment_method <<- "none"  # Only set to none if not already set
        }
      })
    } else {
      api_log_info(paste("[", request_id, "] No DateLife chronograms available - proceeding without age data", sep=""))
      age_assignment_method <- "none"  # No age data available
    }
    
    step_duration <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Step 3 completed - Duration:", round(step_duration, 3), "s"))
    
    update_progress_internal("datelife_processing", "completed", 
                           list(duration_seconds = round(step_duration, 3),
                                chronograms_found = if(!is.null(datelife_result_wrapper$result) && length(datelife_result_wrapper$result) > 0) length(datelife_result_wrapper$result) else 0))
    
    # Step 4: Convert ROTL tree to network format with hybrid age information
    step_start <- Sys.time()
    api_log_info(paste("[", request_id, "] STEP 4: Converting ROTL tree to network format with age information...", sep=""))
    update_progress_internal("network_conversion", "in_progress", 
                           list(step = "Converting ROTL tree to network format"))
    
    network_data <- convert_phylo_to_network_hybrid(rotl_tree, valid_species, datelife_species, ancestor_ages, age_assignment_method, request_id)
    
    if (is.null(network_data) || nrow(network_data) == 0) {
      api_log_error(paste("[", request_id, "] Failed to convert tree to network format", sep=""))
      return(list(
        success = FALSE,
        error = "Failed to convert tree to network format"
      ))
    }
    
    step_duration <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Network conversion completed -", nrow(network_data), "edges created - Duration:", round(step_duration, 3), "s"))
    
    # Count nodes with age data
    nodes_with_ages <- sum(network_data$HasAge, na.rm = TRUE)
    total_nodes <- nrow(network_data)
    api_log_info(paste("[", request_id, "] Age coverage:", nodes_with_ages, "/", total_nodes, "nodes have age information"))
    
    update_progress_internal("network_conversion", "completed", 
                           list(duration_seconds = round(step_duration, 3),
                                edges_created = nrow(network_data),
                                nodes_with_ages = nodes_with_ages,
                                total_nodes = total_nodes))
    
    # Step 5: Create output (visualization or JSON)
    step_start <- Sys.time()
    if (as_json) {
      api_log_info(paste("[", request_id, "] STEP 5: Creating hybrid tree JSON...", sep=""))
      update_progress_internal("creating_output", "in_progress",
                             list(step = "Creating hybrid tree JSON structure"))

      tree_output <- create_hybrid_tree_json(network_data, request_id, progress_token)
      output_type <- "JSON"
    } else {
      api_log_info(paste("[", request_id, "] STEP 5: Creating hybrid tree visualization...", sep=""))
      update_progress_internal("creating_output", "in_progress",
                             list(step = "Creating hybrid tree visualization"))

      tree_output <- create_hybrid_tree_visualization(network_data, request_id, progress_token, expansion_speed)
      output_type <- "HTML"
    }

    step_duration <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    api_log_info(paste("[", request_id, "] ", output_type, " output created - Duration:", round(step_duration, 3), "s", sep=""))

    update_progress_internal("creating_output", "completed",
                           list(duration_seconds = round(step_duration, 3), output_type = output_type))
    
    # Determine which species are missing age data (similar to /api/dated-tree)
    api_log_info(paste("[", request_id, "] Analyzing age data coverage...", sep=""))
    # Need to normalize species names for comparison (DateLife uses underscores, input uses spaces)
    datelife_species_normalized <- gsub("_", " ", datelife_species)
    species_without_ages_scientific <- c()
    species_without_ages_common <- c()
    
    # Check each species that made it into the final tree to see if it has age data
    # Use valid_species (post-recovery) instead of original input to avoid counting dropped species
    for (i in seq_along(valid_species$scientific)) {
      sci_name <- valid_species$scientific[i]
      common_name <- valid_species$common[i]
      
      # Check if this species is in DateLife (normalize both formats for comparison)
      has_datelife_data <- sci_name %in% datelife_species_normalized || 
                          gsub(" ", "_", sci_name) %in% datelife_species
      
      if (!has_datelife_data) {
        species_without_ages_scientific <- c(species_without_ages_scientific, sci_name)
        species_without_ages_common <- c(species_without_ages_common, common_name)
      }
    }
    
    # Calculate coverage based on species that actually made it into the tree
    final_species_count <- nrow(valid_species)
    species_with_ages <- final_species_count - length(species_without_ages_scientific)
    coverage_type <- if (length(species_without_ages_scientific) == 0) "complete" else "partial"

    # Determine appropriate legend type based on age data coverage
    legend_type <- if (species_with_ages == 0) {
      "no_dates"  # No species have age data
    } else if (length(species_without_ages_scientific) == 0) {
      "all_dates"  # All species have age data
    } else {
      "mixed"  # Some species have age data, some don't
    }
    
    api_log_info(paste("[", request_id, "] Age coverage analysis complete:", sep=""))
    api_log_info(paste("[", request_id, "]   Final tree species count:", final_species_count))
    api_log_info(paste("[", request_id, "]   Species with ages:", species_with_ages, "/", final_species_count))
    api_log_info(paste("[", request_id, "]   Species without ages:", length(species_without_ages_scientific)))
    api_log_info(paste("[", request_id, "]   Coverage type:", coverage_type))
    api_log_info(paste("[", request_id, "]   Legend type:", legend_type))
    
    # Also report dropped species for transparency
    if (length(dropped_common_names) > 0) {
      api_log_info(paste("[", request_id, "]   Dropped during recovery:", paste(head(dropped_common_names, 3), collapse = ', '), if(length(dropped_common_names) > 3) '...' else '', sep=" "))
    }
    
    if (length(species_without_ages_common) > 0) {
      api_log_info(paste("[", request_id, "]   Species missing age data:", paste(head(species_without_ages_common, 3), collapse = ', '), if(length(species_without_ages_common) > 3) '...' else '', sep=" "))
    }
    
    api_log_info(paste("[", request_id, "] === HYBRID TREE GENERATION COMPLETED SUCCESSFULLY ===", sep=""))
    
    # Create base response with common fields
    response <- list(
      success = TRUE,
      species_count = nrow(valid_species),
      datelife_coverage = length(datelife_species),
      tree_type = "hybrid_rotl_datelife",
      data_source = "Open Tree of Life + DateLife",
      input_common_names = common_names,
      input_scientific_names = scientific_names,
      species_with_ages = datelife_species,
      species_without_ages = species_without_ages_scientific,
      # Add missing species fields for frontend compatibility (like /api/dated-tree)
      missing_common_names = species_without_ages_common,
      missing_scientific_names = species_without_ages_scientific,
      # Add dropped species fields for pruned OTT ID recovery
      dropped_common_names = dropped_common_names,
      dropped_scientific_names = dropped_scientific_names,
      coverage = coverage_type,
      legend_type = legend_type
    )

    # Add format-specific output field
    if (as_json) {
      response$tree_json <- tree_output
      response$output_format <- "json"
    } else {
      response$html <- tree_output
      response$output_format <- "html"
    }

    return(response)
    
  }, error = function(e) {
    # Safe logging without glue interpolation to avoid recursive errors
    api_log_error(paste("HYBRID TREE GENERATION FAILED for request", request_id, ":", conditionMessage(e)))
    return(list(
      success = FALSE,
      error = paste("Error generating hybrid tree:", conditionMessage(e)),
      input_common_names = if(exists("common_names")) common_names else c(),
      input_scientific_names = if(exists("scientific_names")) scientific_names else c(),
      missing_common_names = c(),  # Initialize empty for frontend compatibility
      missing_scientific_names = c(),  # Initialize empty for frontend compatibility
      legend_type = "mixed"  # Default to mixed for error cases
    ))
  })
}

#' Create mapping between phylogenetic tree tips and species data
#' @param phylo_tree ROTL phylo object
#' @param species_data Species data frame with user-provided names
#' @param request_id Request ID for logging
#' @return Named vector mapping tip numbers to species_data row indices
create_tip_to_species_mapping <- function(phylo_tree, species_data, request_id) {
  n_tips <- length(phylo_tree$tip.label)
  mapping <- integer(n_tips)

  api_log_info(paste("[", request_id, "] Creating tip-to-species mapping for", n_tips, "tips"))

  for (tip_num in 1:n_tips) {
    tip_label <- phylo_tree$tip.label[tip_num]

    # For regular species nodes (with OTT IDs), extract scientific name and match
    if (grepl("_ott\\d+$", tip_label)) {
      tip_clean <- gsub("_ott\\d+", "", tip_label)
      tip_clean <- gsub("_", " ", tip_clean)

      # Find matching species by scientific name
      match_idx <- which(species_data$scientific == tip_clean)
      if (length(match_idx) > 0) {
        mapping[tip_num] <- match_idx[1]
        api_log_info(paste("[", request_id, "] Tip", tip_num, "->", species_data$common[match_idx[1]], "(scientific name match)"))
        next
      }
    }

    # For mrcaott nodes or nodes that don't match by scientific name,
    # try to match by OTT ID from the tip label
    ott_matches <- regmatches(tip_label, gregexpr("ott\\d+", tip_label))[[1]]
    matched <- FALSE

    if (length(ott_matches) > 0) {
      for (ott_match in ott_matches) {
        ott_id <- as.numeric(gsub("ott", "", ott_match))
        match_idx <- which(species_data$ott == ott_id)
        if (length(match_idx) > 0) {
          mapping[tip_num] <- match_idx[1]
          api_log_info(paste("[", request_id, "] Tip", tip_num, "->", species_data$common[match_idx[1]], "(OTT ID", ott_id, "match)"))
          matched <- TRUE
          break
        }
      }
    }

    # If no OTT ID match, use process of elimination
    if (!matched) {
      # Find species that haven't been mapped yet
      used_indices <- mapping[mapping > 0]
      available_indices <- setdiff(1:nrow(species_data), used_indices)

      if (length(available_indices) > 0) {
        mapping[tip_num] <- available_indices[1]
        api_log_info(paste("[", request_id, "] Tip", tip_num, "->", species_data$common[available_indices[1]], "(elimination)"))
      } else {
        # Last resort: use position-based mapping
        if (tip_num <= nrow(species_data)) {
          mapping[tip_num] <- tip_num
          api_log_info(paste("[", request_id, "] Tip", tip_num, "->", species_data$common[tip_num], "(positional fallback)"))
        }
      }
    }
  }

  return(mapping)
}

#' Convert ROTL phylo tree to network format with hybrid age information
#' @param phylo_tree ROTL phylo object
#' @param species_data Species data frame with user-provided names
#' @param datelife_species Vector of species names that have DateLife age data
#' @param ancestor_ages Named list mapping descendant combinations to ancestor ages
#' @param request_id Optional request ID for logging correlation
#' @return Data frame with parent-child network structure and age information
convert_phylo_to_network_hybrid <- function(phylo_tree, species_data, datelife_species, ancestor_ages, age_assignment_method = "chronos", request_id = NULL) {

  if (is.null(request_id)) {
    request_id <- "network_conv"
  }

  n_tips <- length(phylo_tree$tip.label)
  n_nodes <- phylo_tree$Nnode

  api_log_info(paste("[", request_id, "] Processing hybrid tree with", n_tips, "tips and", n_nodes, "internal nodes"))
  api_log_info(paste("[", request_id, "] DateLife coverage:", length(datelife_species), "/", nrow(species_data), "species"))

  # Create a mapping between phylogenetic tree tips and species_data entries
  # This is crucial for correct labeling, especially for mrcaott nodes
  tip_to_species_mapping <- create_tip_to_species_mapping(phylo_tree, species_data, request_id)
  
  # Map pairwise ages to ROTL tree node numbers to prevent inappropriate cascading
  rotl_node_ages <- list()
  
  # Process pairwise age data (non-node keys from ancestor_ages)
  # CRITICAL: Filter out root extrapolations BEFORE mapping to prevent contamination
  n_species <- length(phylo_tree$tip.label)
  
  # Apply quality check to exclude unreliable root age extrapolations early
  filtered_ancestor_ages <- ancestor_ages
  quality_check <- attr(ancestor_ages, "root_quality_check")
  
  if (age_assignment_method == "chronos" && !is.null(quality_check) && !quality_check$sufficient_quality) {
    api_log_warn(paste("[", request_id, "] Applying early quality filter: Removing root extrapolated ages before node mapping"))
    api_log_warn(paste("[", request_id, "] Quality assessment:", quality_check$reason))
    
    # Remove root extrapolation keys before mapping
    for (ancestor_key in names(ancestor_ages)) {
      if (!startsWith(ancestor_key, "node_") && grepl("\\|", ancestor_key)) {
        species_in_key <- length(strsplit(ancestor_key, "\\|")[[1]])
        if (species_in_key >= n_species) {  # Root extrapolation (all species)
          api_log_info(paste("[", request_id, "] Removing root extrapolation source before mapping:", ancestor_key, "(", species_in_key, "species)"))
          filtered_ancestor_ages[[ancestor_key]] <- NULL
        }
      }
    }
    
    api_log_info(paste("[", request_id, "] Filtered ancestor_ages:", length(ancestor_ages), "→", length(filtered_ancestor_ages), "entries"))
  }
  
  # Process remaining (clean) pairwise age data
  for (key in names(filtered_ancestor_ages)) {
    if (!startsWith(key, "node_") && grepl("\\|", key)) {
      # This is a pairwise key like "Characidium_fasciatum|Crenimugil_crenilabis"
      species_pair <- strsplit(key, "\\|")[[1]]
      
      # Find these species in ROTL tree tips
      tip1 <- NULL
      tip2 <- NULL
      
      for (tip_label in phylo_tree$tip.label) {
        tip_clean <- gsub("_ott\\d+", "", tip_label)
        tip_clean <- gsub("_", " ", tip_clean)
        
        # Match with DateLife species names
        species1_clean <- gsub("_", " ", species_pair[1])
        species2_clean <- gsub("_", " ", species_pair[2])
        
        if (tip_clean == species1_clean || tip_clean == species_pair[1]) {
          tip1 <- which(phylo_tree$tip.label == tip_label)
        }
        if (tip_clean == species2_clean || tip_clean == species_pair[2]) {
          tip2 <- which(phylo_tree$tip.label == tip_label)
        }
      }
      
      # Find MRCA in ROTL tree
      if (!is.null(tip1) && !is.null(tip2)) {
        mrca_node <- getMRCA(phylo_tree, tip = c(tip1, tip2))
        if (!is.null(mrca_node) && !is.na(mrca_node)) {
          node_key <- paste0("rotl_node_", mrca_node)
          if (is.null(rotl_node_ages[[node_key]])) {
            rotl_node_ages[[node_key]] <- c()
          }
          rotl_node_ages[[node_key]] <- c(rotl_node_ages[[node_key]], filtered_ancestor_ages[[key]])
          api_log_info(paste("[", request_id, "] Mapped age:", key, "→ ROTL Node", mrca_node, "=", round(filtered_ancestor_ages[[key]], 1), "Mya"))
        }
      }
    }
  }
  
  # Consolidate multiple ages for same ROTL nodes using median
  for (node_key in names(rotl_node_ages)) {
    if (length(rotl_node_ages[[node_key]]) > 1) {
      median_age <- median(rotl_node_ages[[node_key]])
      api_log_info(paste("[", request_id, "] ROTL", gsub("rotl_node_", "Node ", node_key), "consolidated:", paste(round(rotl_node_ages[[node_key]], 1), collapse = ", "), "Mya → median:", round(median_age, 1), "Mya"))
      rotl_node_ages[[node_key]] <- median_age
    } else {
      rotl_node_ages[[node_key]] <- rotl_node_ages[[node_key]][1]
    }
  }
  
  # CRITICAL: Quality filtering moved to earlier stage - no post-processing needed
  # Quality check is now applied before mapping to prevent contamination
  
  # Create network data frame
  network_data <- data.frame(
    from = character(0),
    to = character(0),
    NodeType = character(0),
    AgeInfo = character(0),  # Age information or "age unavailable"
    HasAge = logical(0),     # TRUE if age data is available
    stringsAsFactors = FALSE
  )
  
  # Function to check if a species has DateLife age data
  species_has_datelife_data <- function(species_scientific) {
    # Clean names for comparison (DateLife uses underscores, input may have spaces)
    species_clean <- gsub("[ _]", "_", species_scientific)
    species_clean_spaces <- gsub("[ _]", " ", species_scientific)
    
    # Check direct matches and partial matches (DateLife species may have underscores)
    any(species_clean %in% datelife_species |
        species_clean_spaces %in% datelife_species |
        species_scientific %in% datelife_species |
        any(sapply(datelife_species, function(dl_sp) {
          dl_clean <- gsub("_", " ", dl_sp)
          dl_clean == species_clean_spaces || dl_sp == species_clean
        })))
  }
  
  # Function to assign ages using chronos results (trust chronos completely)
  assign_chronos_ages <- function(node_num, node_type) {
    if (node_num <= n_tips) {
      # Species node - species themselves don't have ages (they're at present time)
      tip_label <- phylo_tree$tip.label[node_num]
      tip_clean <- gsub("_ott\\d+", "", tip_label)
      tip_clean <- gsub("_", " ", tip_clean)
      
      # Find the species in our data
      match_idx <- which(species_data$scientific == tip_clean)
      if (length(match_idx) > 0) {
        species_scientific <- species_data$scientific[match_idx[1]]
        if (species_has_datelife_data(species_scientific)) {
          return(list(info = "present (0 Mya)", has_age = TRUE))
        }
      }
      return(list(info = "present", has_age = FALSE))
    } else {
      # Internal node - use original chronos logic (CRITICAL: total_descendants check)
      descendants <- extract.clade(phylo_tree, node_num)$tip.label
      datelife_descendants <- c()
      for (desc_tip in descendants) {
        tip_clean <- gsub("_ott\\d+", "", desc_tip)
        tip_clean <- gsub("_", " ", tip_clean)
        tip_datelife <- gsub(" ", "_", tip_clean)
        if (tip_datelife %in% datelife_species || tip_clean %in% datelife_species) {
          datelife_descendants <- c(datelife_descendants, tip_datelife)
        }
      }
      
      # Use ROTL node lookup (prevents inappropriate cascading)
      rotl_node_key <- paste0("rotl_node_", node_num)
      if (rotl_node_key %in% names(rotl_node_ages)) {
        ancestor_age_mya <- round(rotl_node_ages[[rotl_node_key]], 1)
        return(list(info = paste0(ancestor_age_mya, " Mya"), has_age = TRUE))
      }
      
      return(list(info = "age unavailable", has_age = FALSE))
    }
  }
  
  # Helper function to extract numeric age from age info string
  extract_numeric_age <- function(age_result) {
    if (is.null(age_result) || !age_result$has_age) {
      return(NA_real_)
    }
    age_match <- regmatches(age_result$info, regexpr("\\d+\\.?\\d*", age_result$info))
    if (length(age_match) > 0) {
      return(as.numeric(age_match[1]))
    }
    return(NA_real_)
  }

  # Final phylogenetic consistency validation - removes conflicting parent ages
  validate_phylogenetic_consistency_final <- function() {
    api_log_info(paste("[", request_id, "] Starting final phylogenetic consistency validation..."))
    conflicts_resolved <- 0
    
    # Check each parent-child edge in the tree
    for (i in 1:nrow(phylo_tree$edge)) {
      parent_num <- phylo_tree$edge[i, 1]
      child_num <- phylo_tree$edge[i, 2]
      
      parent_key <- as.character(parent_num)
      child_key <- as.character(child_num)
      
      # Extract ages from cache
      parent_age_result <- node_age_cache[[parent_key]]
      child_age_result <- node_age_cache[[child_key]]
      
      parent_age <- extract_numeric_age(parent_age_result)
      child_age <- extract_numeric_age(child_age_result)
      
      # Check for phylogenetic conflict: parent age <= child age
      if (!is.na(parent_age) && !is.na(child_age) && parent_age <= child_age) {
        api_log_info(paste("[", request_id, "] FINAL VALIDATION CONFLICT: Parent node", parent_num, "(", parent_age, "Mya) younger than child node", child_num, "(", child_age, "Mya) - removing parent age"))
        
        # Remove the conflicting parent age, preserve the child age
        node_age_cache[[parent_key]] <<- list(info = "age unavailable", has_age = FALSE)
        
        # Update the cached label to remove age information
        parent_type <- node_type_cache[[parent_key]]
        updated_label <- get_node_label_with_age(parent_num, parent_type)
        node_label_cache[[parent_key]] <<- updated_label
        
        api_log_info(paste("[", request_id, "] CONFLICT RESOLVED: Removed age from parent node", parent_num, "- child age", child_age, "Mya preserved"))
        conflicts_resolved <- conflicts_resolved + 1
      }
    }
    
    if (conflicts_resolved > 0) {
      api_log_info(paste("[", request_id, "] Final phylogenetic validation completed -", conflicts_resolved, "parent age conflicts resolved"))
    } else {
      api_log_info(paste("[", request_id, "] Final phylogenetic validation completed - no conflicts found"))
    }
  }

  # Helper function to get maximum age from child nodes for phylogenetic validation
  get_max_child_age <- function(parent_node_num) {
    # Find all direct children of this parent node
    child_node_nums <- phylo_tree$edge[phylo_tree$edge[, 1] == parent_node_num, 2]
    
    if (length(child_node_nums) == 0) {
      return(NA_real_)  # No children
    }
    
    max_age <- NA_real_
    
    for (child_num in child_node_nums) {
      # Check if this child already has age information in the cache
      child_key <- as.character(child_num)
      if (child_key %in% names(node_age_cache)) {
        child_age_result <- node_age_cache[[child_key]]
        if (child_age_result$has_age) {
          # Extract numeric age from age info string
          age_match <- regmatches(child_age_result$info, regexpr("\\d+\\.?\\d*", child_age_result$info))
          if (length(age_match) > 0) {
            child_age <- as.numeric(age_match[1])
            if (is.na(max_age) || child_age > max_age) {
              max_age <- child_age
            }
          }
        }
      }
    }
    
    return(max_age)
  }

  # Function to assign ages using fallback pairwise method (includes MRCA logic)
  assign_fallback_ages <- function(node_num, node_type) {
    if (node_num <= n_tips) {
      # Species node - same as chronos case
      tip_label <- phylo_tree$tip.label[node_num]
      tip_clean <- gsub("_ott\\d+", "", tip_label)
      tip_clean <- gsub("_", " ", tip_clean)
      
      match_idx <- which(species_data$scientific == tip_clean)
      if (length(match_idx) > 0) {
        species_scientific <- species_data$scientific[match_idx[1]]
        if (species_has_datelife_data(species_scientific)) {
          return(list(info = "present (0 Mya)", has_age = TRUE))
        }
      }
      return(list(info = "present", has_age = FALSE))
    } else {
      # Internal node - use MRCA logic for fallback pairwise ages
      descendants <- extract.clade(phylo_tree, node_num)$tip.label
      datelife_descendants <- c()
      for (desc_tip in descendants) {
        tip_clean <- gsub("_ott\\d+", "", desc_tip)
        tip_clean <- gsub("_", " ", tip_clean)
        tip_datelife <- gsub(" ", "_", tip_clean)
        if (tip_datelife %in% datelife_species || tip_clean %in% datelife_species) {
          datelife_descendants <- c(datelife_descendants, tip_datelife)
        }
      }
      
      # First check exact match
      desc_key <- paste(sort(datelife_descendants), collapse = "|")
      api_log_info(paste("DEBUG FALLBACK: node", node_num, "desc_key:", desc_key))
      api_log_info(paste("DEBUG FALLBACK: available keys:", paste(names(ancestor_ages), collapse = ", ")))
      
      if (desc_key %in% names(ancestor_ages)) {
        ancestor_age_mya <- round(ancestor_ages[[desc_key]], 1)
        api_log_info(paste("DEBUG FALLBACK: EXACT MATCH found for", desc_key, "age:", ancestor_age_mya))
        
        # PHYLOGENETIC VALIDATION: Check if this age conflicts with any child node ages
        max_child_age <- get_max_child_age(node_num)
        if (!is.na(max_child_age) && ancestor_age_mya <= max_child_age) {
          api_log_info(paste("DEBUG FALLBACK: PHYLOGENETIC CONFLICT - Node", node_num, "exact match age", ancestor_age_mya, "Mya would be younger than child age", max_child_age, "Mya - age assignment REJECTED"))
          return(list(info = "age unavailable", has_age = FALSE))
        }
        
        return(list(info = paste0(ancestor_age_mya, " Mya"), has_age = TRUE))
      }
      
      # NEW PROPER MRCA LOGIC: For each pairwise age, find the true MRCA and check if it's this node
      api_log_info(paste("DEBUG FALLBACK: Starting proper MRCA logic for node", node_num))
      
      # Collect all ages that belong to this node (handle multiple pairwise ages for same MRCA)
      node_ages <- c()
      
      for (age_key in names(ancestor_ages)) {
        age_descendants <- strsplit(age_key, "\\|")[[1]]
        api_log_info(paste("DEBUG FALLBACK: Processing pairwise age:", age_key, "with species:", paste(age_descendants, collapse = " + ")))
        
        # Only process pairwise relationships (exactly 2 species)
        if (length(age_descendants) == 2) {
          # Find the tip numbers for these two species in the phylogenetic tree
          tip1_num <- NULL
          tip2_num <- NULL
          
          for (i in 1:n_tips) {
            tip_label <- phylo_tree$tip.label[i]
            tip_clean <- gsub("_ott\\d+", "", tip_label)
            tip_clean <- gsub("_", " ", tip_clean)
            tip_datelife <- gsub(" ", "_", tip_clean)
            
            if (tip_datelife == age_descendants[1] || tip_clean == age_descendants[1]) {
              tip1_num <- i
            }
            if (tip_datelife == age_descendants[2] || tip_clean == age_descendants[2]) {
              tip2_num <- i
            }
          }
          
          api_log_info(paste("DEBUG FALLBACK: Found tip numbers:", age_descendants[1], "=", tip1_num, ",", age_descendants[2], "=", tip2_num))
          
          if (!is.null(tip1_num) && !is.null(tip2_num)) {
            # Find the MRCA of these two tips using ape::getMRCA
            mrca_node <- getMRCA(phylo_tree, c(tip1_num, tip2_num))
            api_log_info(paste("DEBUG FALLBACK: MRCA node for", age_descendants[1], "and", age_descendants[2], "is:", mrca_node))
            
            # If this current node IS the MRCA, collect the age
            if (mrca_node == node_num) {
              node_ages <- c(node_ages, ancestor_ages[[age_key]])
              api_log_info(paste("DEBUG FALLBACK: Node", node_num, "is MRCA for", age_key, "- collected age:", ancestor_ages[[age_key]], "Mya"))
            }
          }
        }
      }
      
      # If we found any ages for this node, validate against child ages before assignment
      if (length(node_ages) > 0) {
        median_age <- median(node_ages)
        ancestor_age_mya <- round(median_age, 1)
        
        # PHYLOGENETIC VALIDATION: Check if this age conflicts with any child node ages
        max_child_age <- get_max_child_age(node_num)
        if (!is.na(max_child_age) && ancestor_age_mya <= max_child_age) {
          api_log_info(paste("DEBUG FALLBACK: PHYLOGENETIC CONFLICT - Node", node_num, "age", ancestor_age_mya, "Mya would be younger than child age", max_child_age, "Mya - age assignment REJECTED"))
          return(list(info = "age unavailable", has_age = FALSE))
        }
        
        api_log_info(paste("DEBUG FALLBACK: SUCCESS! Node", node_num, "assigned median age:", ancestor_age_mya, "Mya from", length(node_ages), "pairwise relationships"))
        return(list(info = paste0(ancestor_age_mya, " Mya"), has_age = TRUE))
      }
      
      # RESTORE ORIGINAL FALLBACK LOGIC: Apply complete coverage restriction as fallback
      # Only apply this if we didn't find an exact match or MRCA match above
      total_descendants <- length(descendants)
      if (length(datelife_descendants) >= 2 && length(datelife_descendants) == total_descendants) {
        
        # If no exact match, check for subset matches within DateLife data
        for (age_key in names(ancestor_ages)) {
          age_descendants <- strsplit(age_key, "\\|")[[1]]
          # Check if our descendants are a subset of this DateLife age group
          if (all(datelife_descendants %in% age_descendants)) {
            ancestor_age_mya <- round(ancestor_ages[[age_key]], 1)
            return(list(info = paste0(ancestor_age_mya, " Mya"), has_age = TRUE))
          }
        }
        
        return(list(info = "age unavailable", has_age = FALSE))
      } else if (length(datelife_descendants) >= 1 && length(datelife_descendants) < total_descendants) {
        # Some descendants missing from DateLife - age unavailable
        return(list(info = "age unavailable", has_age = FALSE))
      } else if (length(datelife_descendants) == 1) {
        return(list(info = "age unavailable", has_age = FALSE))
      } else {
        return(list(info = "age unavailable", has_age = FALSE))
      }
    }
  }

  # Function to get age information for a species or node (dispatcher)
  get_age_info <- function(node_num, node_type) {
    # Debug logging to track method dispatch
    method_exists <- exists("age_assignment_method")
    current_method <- if (method_exists) age_assignment_method else "UNDEFINED"
    
    # Only log for internal nodes to avoid spam
    if (node_num > n_tips) {
      api_log_info(paste("DEBUG: get_age_info node", node_num, "- method exists:", method_exists, "method:", current_method))
    }
    
    # Dispatch to the appropriate method based on how ages were assigned
    if (exists("age_assignment_method") && age_assignment_method == "direct_pairwise_fallback") {
      if (node_num > n_tips) {
        api_log_info(paste("DEBUG: Using FALLBACK method for node", node_num))
      }
      return(assign_fallback_ages(node_num, node_type))
    } else {
      # Default to chronos method (or when no age method is set)
      if (node_num > n_tips) {
        api_log_info(paste("DEBUG: Using CHRONOS method for node", node_num))
      }
      return(assign_chronos_ages(node_num, node_type))
    }
  }
  
  # Function to get node label with age information
  get_node_label_with_age <- function(node_num, node_type) {
    if (node_num <= n_tips) {
      # Tip node - use the tip-to-species mapping to get the correct species
      species_idx <- tip_to_species_mapping[node_num]

      if (species_idx > 0 && species_idx <= nrow(species_data)) {
        common_name <- species_data$common[species_idx]
        # Species nodes don't show age labels - just the name
        return(common_name)
      } else {
        # Fallback if mapping failed
        tip_label <- phylo_tree$tip.label[node_num]
        tip_clean <- gsub("_ott\\d+", "", tip_label)
        tip_clean <- gsub("_", " ", tip_clean)
        readable_name <- convert_to_readable_name(tip_clean)
        return(readable_name)
      }
    } else {
      # Internal node
      internal_index <- node_num - n_tips
      age_result <- get_age_info(node_num, node_type)
      
      if (!is.null(phylo_tree$node.label) && 
          length(phylo_tree$node.label) >= internal_index && 
          !is.na(phylo_tree$node.label[internal_index]) && 
          nchar(trimws(phylo_tree$node.label[internal_index])) > 0 && 
          !grepl("^[Mm]rcaott\\d+ott\\d+", phylo_tree$node.label[internal_index])) {
        
        readable_name <- convert_to_readable_name(phylo_tree$node.label[internal_index])
        if (age_result$has_age) {
          return(paste0(readable_name, " (", age_result$info, ")"))
        } else {
          return(readable_name)
        }
      } else {
        ancestor_label <- paste("Ancestor", LETTERS[min(internal_index, 26)])
        if (age_result$has_age) {
          return(paste0(ancestor_label, " (", age_result$info, ")"))
        } else {
          return(ancestor_label)
        }
      }
    }
  }
  
  # First pass: collect all unique parent and child nodes with their age information
  # This ensures that parent nodes (including root) get their age data processed
  all_parent_nums <- unique(phylo_tree$edge[, 1])
  all_child_nums <- unique(phylo_tree$edge[, 2])
  all_node_nums <- unique(c(all_parent_nums, all_child_nums))
  
  # Pre-calculate age information for all nodes to avoid missing root nodes
  node_age_cache <- list()
  node_type_cache <- list()
  node_label_cache <- list()
  
  for (node_num in all_node_nums) {
    # Determine node type
    node_type <- if (node_num <= n_tips) {
      # All tip nodes (leaf nodes) should be treated as species regardless of taxonomic level
      # This ensures they get green coloring like other leaf nodes
      "species"
    } else {
      internal_index <- node_num - n_tips
      if (!is.null(phylo_tree$node.label) && 
          length(phylo_tree$node.label) >= internal_index && 
          !is.na(phylo_tree$node.label[internal_index]) && 
          !grepl("^[Mm]rcaott\\d+ott\\d+", phylo_tree$node.label[internal_index])) {
        "taxonomic"
      } else {
        "ancestor"
      }
    }
    
    # Get age information and label
    node_age_result <- get_age_info(node_num, node_type)
    node_label <- get_node_label_with_age(node_num, node_type)
    
    # Cache the results
    node_age_cache[[as.character(node_num)]] <- node_age_result
    node_type_cache[[as.character(node_num)]] <- node_type
    node_label_cache[[as.character(node_num)]] <- node_label
  }
  
  # FINAL PHYLOGENETIC VALIDATION: Remove conflicting parent ages (fallback method only)
  if (exists("age_assignment_method") && age_assignment_method == "direct_pairwise_fallback") {
    validate_phylogenetic_consistency_final()
  }
  
  # Process each edge in the ROTL tree using cached data
  for (i in 1:nrow(phylo_tree$edge)) {
    parent_num <- phylo_tree$edge[i, 1]
    child_num <- phylo_tree$edge[i, 2]
    
    # Get cached information
    parent_type <- node_type_cache[[as.character(parent_num)]]
    child_type <- node_type_cache[[as.character(child_num)]]
    parent_label <- node_label_cache[[as.character(parent_num)]]
    child_label <- node_label_cache[[as.character(child_num)]]
    child_age_result <- node_age_cache[[as.character(child_num)]]
    
    # Add edge to network
    network_data <- rbind(network_data, data.frame(
      from = parent_label,
      to = child_label,
      NodeType = child_type,
      AgeInfo = child_age_result$info,
      HasAge = child_age_result$has_age,
      stringsAsFactors = FALSE
    ))
  }
  
  # Add root handling - find orphaned parents and connect them to conceptual root
  if (nrow(network_data) > 0) {
    all_parents <- unique(network_data$from)
    all_children <- unique(network_data$to)
    orphaned_parents <- setdiff(all_parents, all_children)
    
    root_name <- "Common ancestor"
    
    # Pre-calculate root display name with age information for chronos method
    root_age_info <- "age unavailable"
    root_has_age <- FALSE
    root_display_name <- root_name
    
    # "Common ancestor" should always remain a clean static label - age goes on taxonomic root instead
    if (root_name == "Common ancestor") {
      # Keep "Common ancestor" as clean static conceptual label
      root_display_name <- root_name
      api_log_info(paste("[", request_id, "] Keeping 'Common ancestor' as clean static label - age information will appear on taxonomic root"))
    } else {
      # For actual taxonomic roots (like "Euteleostomi"), apply chronos age logic
      if (exists("age_assignment_method") && age_assignment_method == "chronos") {
        # Check calibration quality from stored assessment
        quality_check <- attr(ancestor_ages, "root_quality_check")
        
        if (!is.null(quality_check)) {
          if (quality_check$sufficient_quality) {
            # Quality check PASSED - safe to display chronos root age
            if (length(ancestor_ages) > 0) {
              # Find the key that represents all species (longest key with most species)
              all_species_keys <- names(ancestor_ages)[sapply(names(ancestor_ages), function(key) grepl("\\|", key))]
              if (length(all_species_keys) > 0) {
                # Use the key with the most species (should be the root)
                root_key <- all_species_keys[which.max(sapply(all_species_keys, function(key) length(strsplit(key, "\\|")[[1]])))]
                if (!is.null(ancestor_ages[[root_key]])) {
                  root_age_info <- paste0(round(ancestor_ages[[root_key]], 1), " Mya")
                  root_has_age <- TRUE
                  # Include age in root display name for CollapsibleTree visualization
                  root_display_name <- paste0(root_name, " (", root_age_info, ")")
                  api_log_info(paste("[", request_id, "] Including scientifically justified chronos root age:", root_age_info, "- Quality:", quality_check$reason))
                }
              }
            }
          } else {
            # Quality check FAILED - do not display root age, clean root name without extra messaging
            api_log_warn(paste("[", request_id, "] Excluding chronos root age - insufficient calibration quality:", quality_check$reason))
            root_display_name <- root_name  # Clean name - missing age data represented by absence
          }
        } else {
          api_log_warn(paste("[", request_id, "] No quality check available - excluding root age as precaution"))
          root_display_name <- root_name  # Clean name - missing age data represented by absence
        }
      } else {
        api_log_info(paste("[", request_id, "] Excluding root age from visualization (pairwise method - no integrated clock model)"))
      }
    }
    
    if (length(orphaned_parents) > 0) {
      for (orphaned_parent in orphaned_parents) {
        # Determine node type
        if (grepl("^Ancestor [A-Z]", orphaned_parent)) {
          node_type <- "ancestor"
        } else {
          node_type <- "taxonomic"
        }
        
        # Find the original node number for this orphaned parent to get its cached age data
        # We need to reverse-lookup from the cached labels to find the node number
        orphaned_parent_node_num <- NULL
        for (node_num_str in names(node_label_cache)) {
          if (node_label_cache[[node_num_str]] == orphaned_parent) {
            orphaned_parent_node_num <- as.numeric(node_num_str)
            break
          }
        }
        
        # Get age information from cache if available
        if (!is.null(orphaned_parent_node_num) && as.character(orphaned_parent_node_num) %in% names(node_age_cache)) {
          age_result <- node_age_cache[[as.character(orphaned_parent_node_num)]]
          age_info <- age_result$info
          has_age <- age_result$has_age
        } else {
          # Fallback to default values if cache lookup fails
          age_info <- "age unavailable"
          has_age <- FALSE
        }
        
        network_data <- rbind(data.frame(
          from = root_display_name,
          to = orphaned_parent,
          NodeType = node_type,
          AgeInfo = age_info,
          HasAge = has_age,
          stringsAsFactors = FALSE
        ), network_data)
      }
    }
    
    # Add root row (display name and age info already calculated above)
    network_data <- rbind(data.frame(
      from = NA,
      to = root_display_name,
      NodeType = "root",
      AgeInfo = root_age_info,
      HasAge = root_has_age,
      stringsAsFactors = FALSE
    ), network_data)
  }
  
  # Note: Phylogenetic age conflicts are now prevented during MRCA assignment phase
  # No post-processing conflict resolution needed
  
  return(network_data)
}


#' Transform hybrid network data to info panel format
#' @param network_data Hybrid network data with AgeInfo and HasAge fields
#' @return Network data compatible with info panel system
transform_hybrid_to_info_panel_format <- function(network_data) {
  # Transform hybrid data structure to info panel format
  # Convert from: from/to, AgeInfo, HasAge
  # Convert to: to/Child, Age, AgeValid, AgeSource, ValidationNotes
  
  info_panel_data <- data.frame(
    to = network_data$to,
    NodeType = network_data$NodeType,
    stringsAsFactors = FALSE
  )
  
  # Extract age from AgeInfo and convert to numeric
  info_panel_data$Age <- sapply(network_data$AgeInfo, function(age_info) {
    if (is.na(age_info) || age_info == "age unavailable") {
      return(NA_real_)
    }
    # Extract numeric value from strings like "65.2 Mya" or "~65.2 Mya"
    age_match <- regmatches(age_info, regexpr("\\d+\\.?\\d*", age_info))
    if (length(age_match) > 0) {
      return(as.numeric(age_match[1]))
    }
    return(NA_real_)
  })
  
  # Set validation fields based on whether we actually extracted a numeric age
  # This fixes the bug where nodes with age data in their labels were marked as invalid
  info_panel_data$AgeValid <- !is.na(info_panel_data$Age) & is.numeric(info_panel_data$Age)
  info_panel_data$AgeSource <- ifelse(!is.na(info_panel_data$Age) & is.numeric(info_panel_data$Age), 
                                      "DateLife chronogram database (hybrid tree)",
                                      "Age data unavailable")
  info_panel_data$ValidationNotes <- ifelse(!is.na(info_panel_data$Age) & is.numeric(info_panel_data$Age),
                                            NA_character_,
                                            NA_character_)
  
  return(info_panel_data)
}

#' Convert network data to nested JSON tree structure
#' @param network_data Network data frame with parent-child relationships
#' @param tree_data Enhanced data frame with info panel data
#' @param request_id Optional request ID for logging correlation
#' @return Nested list structure representing the tree
convert_network_to_nested_json <- function(network_data, tree_data, request_id = NULL, phylopic_data = NULL) {
  if (is.null(request_id)) {
    request_id <- "json_convert"
  }

  api_log_info(paste("[", request_id, "] Converting network data to nested JSON structure..."))

  # Create a mapping from child names to their data
  child_to_data <- list()
  for (i in 1:nrow(tree_data)) {
    child_name <- tree_data$Child[i]
    child_to_data[[child_name]] <- tree_data[i, ]
  }

  # Helper function to extract node metadata from info panel data
  extract_node_metadata <- function(node_name, node_data) {
    # Initialize metadata with defaults
    metadata <- list(
      node_label = node_name,
      node_type = if (!is.null(node_data$NodeType)) node_data$NodeType else "unknown",
      color = if (!is.null(node_data$Color)) node_data$Color else "#666666",
      has_age = if (!is.null(node_data$HasAge)) node_data$HasAge else FALSE,
      age_info = "age unavailable",
      age_numeric = NA_real_,
      node_shape = "circle",
      image_url = NA_character_,
      image_type = "none",
      image_attribution = NA_character_,
      wikipedia_text = NA_character_,
      wikipedia_url = NA_character_,
      wikipedia_title = NA_character_,
      geologic_age = NA_character_,
      phylopic_uuid = NA_character_,
      phylopic_url = NA_character_,
      phylopic_attribution = NA_character_
    )

    # Extract age information from network_data if available
    network_row <- network_data[network_data$to == node_name, ]
    if (nrow(network_row) > 0) {
      metadata$age_info <- if (!is.na(network_row$AgeInfo[1])) network_row$AgeInfo[1] else "age unavailable"
      metadata$has_age <- if (!is.na(network_row$HasAge[1])) network_row$HasAge[1] else FALSE

      # Extract numeric age from age_info
      if (metadata$has_age && metadata$age_info != "age unavailable") {
        age_match <- regmatches(metadata$age_info, regexpr("\\d+\\.?\\d*", metadata$age_info))
        if (length(age_match) > 0) {
          metadata$age_numeric <- as.numeric(age_match[1])
        }
      }
    }

    # Check for PhyloPic data from our dedicated collection first
    phylopic_found <- FALSE
    if (!is.null(phylopic_data)) {
      # Extract taxonomic name for PhyloPic lookup
      taxonomic_name <- gsub("\\s*\\([^)]*\\)\\s*", "", node_name)  # Remove age info
      taxonomic_name <- gsub("^(Ancestor|Node)\\s+", "", taxonomic_name)  # Remove prefixes
      taxonomic_name <- trimws(taxonomic_name)

      # Look for PhyloPic data for this taxonomic name
      if (taxonomic_name %in% names(phylopic_data)) {
        phylopic_info <- phylopic_data[[taxonomic_name]]
        if (!is.null(phylopic_info$uuid) && phylopic_info$uuid != "") {
          metadata$phylopic_uuid <- phylopic_info$uuid
          metadata$phylopic_url <- phylopic_info$phylopic_url
          metadata$phylopic_attribution <- phylopic_info$attribution
          metadata$node_shape <- phylopic_info$phylopic_url
          phylopic_found <- TRUE
        }
      }
    }

    # Enhanced info panel data extraction with robust pattern matching
    if (!is.null(node_data$InfoPanel) && node_data$InfoPanel != "") {
      info_html <- node_data$InfoPanel

      # Extract PhyloPic data from HTML only if not found in dedicated collection
      if (!phylopic_found) {
        # Extract PhyloPic data with multiple pattern attempts
        # Pattern 1: data-uuid attribute
        phylopic_patterns <- c(
          'data-uuid="([^"]+)"',
          'uuid-([a-f0-9\\-]{36})',
          'phylopic\\.org/images/([^/]+)/'
        )

        for (pattern in phylopic_patterns) {
          phylopic_match <- regmatches(info_html, gregexpr(pattern, info_html, ignore.case = TRUE))
          if (length(phylopic_match[[1]]) > 0) {
            # Extract UUID from the first match
            uuid_text <- phylopic_match[[1]][1]
            uuid_clean <- gsub('.*([a-f0-9\\-]{36}).*', '\\1', uuid_text, ignore.case = TRUE)
            if (nchar(uuid_clean) == 36 && grepl('[a-f0-9\\-]{36}', uuid_clean)) {
              metadata$phylopic_uuid <- uuid_clean
              metadata$phylopic_url <- paste0("https://images.phylopic.org/images/", uuid_clean, "/vector.svg")
              metadata$node_shape <- metadata$phylopic_url
              # Don't overwrite image_url - it should come from the actual info panel image
              break
            }
          }
        }
      }

      # Extract Wikipedia data with multiple patterns
      wiki_patterns <- c(
        'href="(https://en\\.wikipedia\\.org/wiki/[^"]+)"',
        'wikipedia\\.org/wiki/([^"\\s]+)',
        'en\\.wikipedia\\.org/wiki/([^"\\s>]+)'
      )

      for (pattern in wiki_patterns) {
        wiki_match <- regmatches(info_html, gregexpr(pattern, info_html, ignore.case = TRUE))
        if (length(wiki_match[[1]]) > 0) {
          url_text <- wiki_match[[1]][1]
          # Clean URL extraction
          if (grepl('href=', url_text)) {
            clean_url <- gsub('.*href="([^"]+)".*', '\\1', url_text)
          } else {
            clean_url <- paste0("https://en.wikipedia.org/wiki/", gsub('.*wikipedia\\.org/wiki/([^"\\s>]+).*', '\\1', url_text))
          }
          if (grepl('https://en\\.wikipedia\\.org/wiki/', clean_url)) {
            metadata$wikipedia_url <- clean_url
            break
          }
        }
      }

      # Extract Wikipedia text content specifically from wikipedia-summary div
      wiki_summary_pattern <- '<div class="wikipedia-summary">([^<]*(?:<[^>]*>[^<]*)*?)</div>'
      wiki_summary_match <- regmatches(info_html, regexpr(wiki_summary_pattern, info_html))

      if (length(wiki_summary_match) > 0) {
        # Extract content between the div tags
        wiki_content <- gsub('<div class="wikipedia-summary">(.*?)</div>', '\\1', wiki_summary_match, perl = TRUE)
        # Clean up any remaining HTML tags within the content
        wiki_content <- gsub('<[^>]+>', '', wiki_content)
        # Clean up whitespace
        wiki_content <- gsub('\\s+', ' ', wiki_content)
        wiki_content <- trimws(wiki_content)

        if (nchar(wiki_content) > 10) {
          # Limit to 500 characters for the JSON field
          metadata$wikipedia_text <- if (nchar(wiki_content) > 500) {
            paste0(substr(wiki_content, 1, 497), "...")
          } else {
            wiki_content
          }
        }
      }

      # Extract geological age information
      # Look for patterns like "During the [Period] Period" or "lived during the [Period]"
      geologic_patterns <- c(
        'During the ([^.]+(?:Period|Era|Epoch))',
        'during the ([^.]+(?:Period|Era|Epoch))',
        'lived.*during.*the ([^.]+(?:Period|Era|Epoch))',
        'from the ([^.]+(?:Period|Era|Epoch))',
        'in the ([^.]+(?:Period|Era|Epoch))'
      )

      for (pattern in geologic_patterns) {
        geologic_match <- regmatches(info_html, regexpr(pattern, info_html, ignore.case = TRUE))
        if (length(geologic_match) > 0) {
          # Extract the geological period from the match
          period_text <- gsub(pattern, '\\1', geologic_match, ignore.case = TRUE, perl = TRUE)
          period_text <- trimws(period_text)
          if (nchar(period_text) > 0 && nchar(period_text) < 100) {
            metadata$geologic_age <- period_text
            break
          }
        }
      }

      # For JSON output, fetch Wikimedia images directly instead of using override system
      # This provides clean URLs rather than base64 data
      api_log_info(paste("[JSON] Node:", node_name, "| Type:", metadata$node_type, "| Image URL:", metadata$image_url))
      if (metadata$node_type == "taxonomic" && (is.na(metadata$image_url) || metadata$image_url == "" || is.null(metadata$image_url))) {
        # Extract taxonomic name for image search
        taxonomic_name <- gsub("\\s*\\([^)]*\\)\\s*", "", node_name)  # Remove age info
        taxonomic_name <- gsub("^(Ancestor|Node)\\s+", "", taxonomic_name)  # Remove prefixes
        taxonomic_name <- trimws(taxonomic_name)

        api_log_info(paste("[JSON] Attempting Wikimedia image fetch for taxonomic node:", taxonomic_name))

        if (nchar(taxonomic_name) > 0) {
          tryCatch({
            source("functions/wikipedia_images.R", local = TRUE)  # Contains attribution helper functions
            source("functions/wikimedia_images.R", local = TRUE)
            api_log_info(paste("[JSON] Calling get_wikimedia_image for:", taxonomic_name))
            wikimedia_result <- get_wikimedia_image(taxonomic_name, target_width = 200)
            if (wikimedia_result$success) {
              api_log_info(paste("[JSON] Wikimedia image found:", wikimedia_result$image_url))
              metadata$image_url <- wikimedia_result$image_url
              metadata$image_type <- "wikimedia"
              metadata$image_attribution <- wikimedia_result$attribution
            } else {
              api_log_info(paste("[JSON] Wikimedia image not found for", taxonomic_name, ":", wikimedia_result$error))
            }
          }, error = function(e) {
            api_log_warn(paste("[JSON] Wikimedia image fetch failed for", taxonomic_name, ":", e$message))
          })
        }
      }

      # Extract image URLs from info panel (always run to get actual display image)
      if (is.na(metadata$image_url) || metadata$image_url == "" || is.null(metadata$image_url)) {
        img_patterns <- c(
          '<img[^>]+src="([^"]+)"',
          'src="([^"]+\\.(jpg|jpeg|png|gif|svg))"'
        )

        for (pattern in img_patterns) {
          img_match <- regmatches(info_html, gregexpr(pattern, info_html, ignore.case = TRUE))
          if (length(img_match[[1]]) > 0) {
            src_text <- img_match[[1]][1]
            url_clean <- gsub('.*src="([^"]+)".*', '\\1', src_text)
            if (grepl('^https?://', url_clean) || grepl('^image_overrides/', url_clean) || grepl('^data:image/', url_clean)) {
              metadata$image_url <- url_clean
              if (grepl("wikimedia|wikipedia", url_clean, ignore.case = TRUE)) {
                metadata$image_type <- "wikimedia"
              } else if (grepl("unsplash", url_clean, ignore.case = TRUE)) {
                metadata$image_type <- "unsplash"
              } else if (grepl("pixabay", url_clean, ignore.case = TRUE)) {
                metadata$image_type <- "pixabay"
              } else if (grepl("^image_overrides/", url_clean) || grepl("^data:image/", url_clean)) {
                metadata$image_type <- "override"
              } else {
                metadata$image_type <- "other"
              }
              break
            }
          }
        }
      }

      # Extract attribution information
      if (grepl("attribution|license|credit|©|&copy;", info_html, ignore.case = TRUE)) {
        # Simple attribution extraction - look for common attribution patterns
        attr_text <- gsub('<[^>]+>', ' ', info_html)
        attr_text <- gsub('\\s+', ' ', attr_text)

        # Extract text that looks like attribution
        attr_patterns <- c(
          '(Image by[^.]+)',
          '(©[^.]+)',
          '(Attribution[^.]+)',
          '(License[^.]+)',
          '(Credit[^.]+)'
        )

        for (pattern in attr_patterns) {
          attr_match <- regmatches(attr_text, regexpr(pattern, attr_text, ignore.case = TRUE))
          if (length(attr_match) > 0) {
            metadata$image_attribution <- trimws(attr_match[1])
            break
          }
        }
      }
    }

    # Group info panel related fields together
    metadata$info_panel <- list(
      image_url = metadata$image_url,
      image_type = metadata$image_type,
      image_attribution = metadata$image_attribution,
      wikipedia_text = metadata$wikipedia_text,
      wikipedia_url = metadata$wikipedia_url,
      wikipedia_title = metadata$wikipedia_title,
      geologic_age = metadata$geologic_age
    )

    # Remove individual fields from top level since they're now in info_panel
    metadata$image_url <- NULL
    metadata$image_type <- NULL
    metadata$image_attribution <- NULL
    metadata$wikipedia_text <- NULL
    metadata$wikipedia_url <- NULL
    metadata$wikipedia_title <- NULL
    metadata$geologic_age <- NULL

    return(metadata)
  }

  # Helper function to build tree recursively with cycle detection
  build_tree_recursive <- function(parent_name, visited = character(0)) {
    # Cycle detection - prevent infinite recursion
    if (parent_name %in% visited) {
      api_log_warn(paste("[", request_id, "] Cycle detected for node:", parent_name, "- stopping recursion"))
      node_data <- child_to_data[[parent_name]]
      return(extract_node_metadata(parent_name, node_data))
    }

    # Add current node to visited set
    visited <- c(visited, parent_name)

    # Get all direct children of this parent
    children_data <- network_data[network_data$from == parent_name & !is.na(network_data$from), ]

    if (nrow(children_data) == 0) {
      # Leaf node - return just the metadata
      node_data <- child_to_data[[parent_name]]
      return(extract_node_metadata(parent_name, node_data))
    }

    # Internal node - build with children
    node_data <- child_to_data[[parent_name]]
    node_metadata <- extract_node_metadata(parent_name, node_data)

    # Recursively build children
    children <- list()
    for (i in 1:nrow(children_data)) {
      child_name <- children_data$to[i]
      if (!is.na(child_name) && child_name != parent_name) {  # Prevent self-reference
        child_node <- build_tree_recursive(child_name, visited)
        children[[length(children) + 1]] <- child_node
      } else {
        api_log_warn(paste("[", request_id, "] Skipping invalid child:", child_name, "for parent:", parent_name))
      }
    }

    if (length(children) > 0) {
      node_metadata$children <- children
    }
    return(node_metadata)
  }

  # Find the root node - should be a node that has NA as parent (appears in 'to' with NA 'from')
  api_log_info(paste("[", request_id, "] Network data analysis:"))
  api_log_info(paste("[", request_id, "]   Total edges:", nrow(network_data)))

  # Show first few rows for debugging
  for (i in 1:min(5, nrow(network_data))) {
    api_log_info(paste("[", request_id, "]   Edge", i, ":",
                      network_data$from[i], "->", network_data$to[i]))
  }

  # Find the true root - node with NA parent
  root_rows <- network_data[is.na(network_data$from), ]
  api_log_info(paste("[", request_id, "]   Rows with NA parents:", nrow(root_rows)))

  if (nrow(root_rows) > 0) {
    root_name <- root_rows$to[1]
    api_log_info(paste("[", request_id, "]   Using NA parent root:", root_name))
  } else {
    # Fallback: find node that appears as parent but never as child
    all_children <- network_data$to[!is.na(network_data$to)]
    all_parents <- network_data$from[!is.na(network_data$from)]
    true_root_candidates <- setdiff(all_parents, all_children)

    api_log_info(paste("[", request_id, "]   True root candidates (parents never children):", paste(true_root_candidates, collapse = ", ")))

    if (length(true_root_candidates) > 0) {
      root_name <- true_root_candidates[1]
      api_log_info(paste("[", request_id, "]   Using true root candidate:", root_name))
    } else {
      # Last resort: use first node that appears
      root_name <- all_children[1]
      api_log_info(paste("[", request_id, "]   Using fallback first node:", root_name))
    }
  }

  api_log_info(paste("[", request_id, "] Building tree from root:", root_name))

  # Build the complete tree
  tree_json <- build_tree_recursive(root_name)

  api_log_info(paste("[", request_id, "] JSON tree structure created successfully"))
  return(tree_json)
}

#' Create hybrid tree JSON structure
#' @param network_data Network data frame with age information
#' @param request_id Optional request ID for logging correlation
#' @param progress_token Optional progress token for tracking external API calls
#' @return Nested JSON structure representing the tree
create_hybrid_tree_json <- function(network_data, request_id = NULL, progress_token = NULL) {
  if (is.null(request_id)) {
    request_id <- "json_create"
  }

  # Helper function to update progress if token provided
  update_progress_internal <- function(step_name, status = "completed", additional_data = NULL) {
    if (!is.null(progress_token) && progress_token != "") {
      update_progress(progress_token, step_name, status, additional_data)
    }
  }

  api_log_info(paste("[", request_id, "] Starting hybrid tree JSON creation..."))

  # Step 5.1: Prepare tree structure data (same as HTML version)
  update_progress_internal("preparing_json_structure", "in_progress",
                         list(step = "Preparing tree structure and color mapping"))
  tree_data <- data.frame(
    Parent = network_data$from,
    Child = network_data$to,
    NodeType = network_data$NodeType,
    HasAge = network_data$HasAge,
    stringsAsFactors = FALSE
  )

  # Add color mapping using new adaptive ancestral node coloring system
  tree_data$Color <- character(nrow(tree_data))

  # Get ancestral node colors using new system
  ancestral_colors <- get_ancestral_node_color(network_data)

  # Assign colors to each row
  ancestral_idx <- 1
  for (i in 1:nrow(tree_data)) {
    node_type <- tree_data$NodeType[i]

    if (node_type %in% c("taxonomic", "ancestor")) {
      # Use new ancestral coloring system
      tree_data$Color[i] <- ancestral_colors[ancestral_idx]
      ancestral_idx <- ancestral_idx + 1
    } else {
      # Use original system for root and species
      has_age <- tree_data$HasAge[i]
      tree_data$Color[i] <- get_node_color(node_type, has_age)
    }
  }
  update_progress_internal("preparing_json_structure", "completed")

  # Step 5.2: Transform network data for info panels (same as HTML version)
  update_progress_internal("transforming_network_data", "in_progress",
                         list(step = "Transforming network data to info panel format"))
  info_panel_network_data <- transform_hybrid_to_info_panel_format(network_data)
  update_progress_internal("transforming_network_data", "completed")

  # Step 5.3: Generate info panel data (Wikipedia & PhyloPic content)
  update_progress_internal("generating_info_panels", "in_progress",
                         list(step = "Generating info panels with Wikipedia and PhyloPic data"))
  tree_data <- add_info_panel_data(tree_data, info_panel_network_data, request_id, progress_token)
  update_progress_internal("generating_info_panels", "completed")

  # Step 5.3.5: Collect PhyloPic metadata for JSON output (regardless of override images)
  update_progress_internal("collecting_phylopic_data", "in_progress",
                         list(step = "Collecting PhyloPic metadata for JSON output"))
  phylopic_data <- NULL
  tryCatch({
    source("functions/phylopic_silhouettes.R", local = TRUE)
    phylopic_json_raw <- create_phylopic_node_replacement_data(network_data, request_id)
    if (!is.null(phylopic_json_raw) && phylopic_json_raw != "{}") {
      phylopic_data <- jsonlite::fromJSON(phylopic_json_raw)
      api_log_info(paste("[", request_id, "] PhyloPic metadata collected for", length(phylopic_data), "taxonomic nodes"))
    } else {
      api_log_info(paste("[", request_id, "] No PhyloPic data available for taxonomic nodes"))
    }
  }, error = function(e) {
    api_log_warn(paste("[", request_id, "] Failed to collect PhyloPic metadata:", e$message))
  })
  update_progress_internal("collecting_phylopic_data", "completed")

  # Step 5.4: Convert to nested JSON structure
  update_progress_internal("converting_to_json", "in_progress",
                         list(step = "Converting tree structure to nested JSON"))
  tree_json <- convert_network_to_nested_json(network_data, tree_data, request_id, phylopic_data)
  update_progress_internal("converting_to_json", "completed")

  api_log_info(paste("[", request_id, "] Hybrid tree JSON creation completed"))
  return(tree_json)
}

#' Create CollapsibleTree visualization for hybrid tree
#' @param network_data Network data frame with age information
#' @param request_id Optional request ID for logging correlation
#' @return HTML string for CollapsibleTree
create_hybrid_tree_visualization <- function(network_data, request_id = NULL, progress_token = NULL, expansion_speed = 750) {
  if (is.null(request_id)) {
    request_id <- "viz_create"
  }
  
  # Helper function to update progress if token provided
  update_progress_internal <- function(step_name, status = "completed", additional_data = NULL) {
    if (!is.null(progress_token) && progress_token != "") {
      update_progress(progress_token, step_name, status, additional_data)
    }
  }
  
  api_log_info(paste("[", request_id, "] Starting hybrid tree visualization creation...", sep=""))
  
  # Step 5.1: Calculate dynamic link lengths
  update_progress_internal("calculating_link_lengths", "in_progress", 
                         list(step = "Calculating dynamic link lengths for tree layout"))
  link_length <- calculate_dynamic_link_length_hybrid(network_data)
  update_progress_internal("calculating_link_lengths", "completed")
  
  # Step 5.2: Prepare tree structure data
  update_progress_internal("preparing_tree_structure", "in_progress", 
                         list(step = "Preparing tree structure and color mapping"))
  tree_data <- data.frame(
    Parent = network_data$from,
    Child = network_data$to,
    NodeType = network_data$NodeType,
    HasAge = network_data$HasAge,
    stringsAsFactors = FALSE
  )
  
  # Add color mapping using new adaptive ancestral node coloring system
  tree_data$Color <- character(nrow(tree_data))

  # Get ancestral node colors using new system
  ancestral_colors <- get_ancestral_node_color(network_data)

  # Assign colors to each row
  ancestral_idx <- 1
  for (i in 1:nrow(tree_data)) {
    node_type <- tree_data$NodeType[i]

    if (node_type %in% c("taxonomic", "ancestor")) {
      # Use new ancestral coloring system
      tree_data$Color[i] <- ancestral_colors[ancestral_idx]
      ancestral_idx <- ancestral_idx + 1
    } else {
      # Use original system for root and species
      has_age <- tree_data$HasAge[i]
      tree_data$Color[i] <- get_node_color(node_type, has_age)
    }
  }
  update_progress_internal("preparing_tree_structure", "completed")
  
  # Step 5.3: Create base tree widget
  update_progress_internal("creating_base_widget", "in_progress", 
                         list(step = "Creating collapsibleTree widget with interactions"))
  tree_widget <- collapsibleTreeNetwork(
    tree_data,
    attribute = "NodeType",
    fill = "Color", 
    fontSize = 12,
    linkLength = link_length,
    nodeSize = "leafCount",
    width = 1000,
    height = 800,
    zoomable = TRUE,
    collapsed = TRUE  # Start collapsed, will expand all with custom speed
  )
  update_progress_internal("creating_base_widget", "completed")
  
  # Step 5.4: Apply JavaScript enhancements for expansion behavior
  if (expansion_speed != 750) {
    update_progress_internal("applying_js_enhancements", "in_progress", 
                           list(step = "Applying JavaScript enhancements for tree expansion"))
    tree_widget <- tree_widget %>%
      htmlwidgets::onRender(paste0("
        function(el, x) {
          // Override D3 transition duration for all transitions
          var originalTransition = d3.selection.prototype.transition;
          d3.selection.prototype.transition = function() {
            var transition = originalTransition.apply(this, arguments);
            var originalDuration = transition.duration;
            transition.duration = function(d) {
              if (arguments.length === 0) return originalDuration.apply(this, arguments);
              return originalDuration.call(this, ", expansion_speed, ");
            };
            return transition;
          };
          
          // Sequential expansion function - each node waits for previous to complete
          function expandAll() {
            var nodes = d3.select(el).selectAll('g.node').filter(function(d) {
              return d._children;
            });
            
            if (nodes.size() > 0) {
              // Group nodes by their depth level
              var nodesByDepth = new Map();
              nodes.each(function(d) {
                if (!nodesByDepth.has(d.depth)) {
                  nodesByDepth.set(d.depth, []);
                }
                nodesByDepth.get(d.depth).push({ element: this, data: d });
              });
              
              // Sort nodes within each level by their vertical position (y coordinate)
              nodesByDepth.forEach(function(levelNodes) {
                levelNodes.sort(function(a, b) {
                  return a.data.y - b.data.y; // Top to bottom ordering
                });
              });
              
              // Get sorted depth levels and flatten into single array
              var sortedDepths = Array.from(nodesByDepth.keys()).sort(function(a, b) {
                return a - b;
              });
              
              var allNodes = [];
              sortedDepths.forEach(function(depth) {
                var levelNodes = nodesByDepth.get(depth);
                allNodes = allNodes.concat(levelNodes);
              });
              
              // Expand nodes one by one, waiting for each to complete
              function expandNext(index) {
                if (index >= allNodes.length) {
                  // All nodes in this batch are expanded, check for more
                  setTimeout(expandAll, 100);
                  return;
                }
                
                var nodeInfo = allNodes[index];
                var clickEvent = new MouseEvent('click', {
                  bubbles: true,
                  cancelable: true,
                  view: window
                });
                nodeInfo.element.dispatchEvent(clickEvent);
                
                // Wait for this node's animation to complete before expanding next
                setTimeout(function() {
                  expandNext(index + 1);
                }, ", expansion_speed, " + 50);
              }
              
              // Start the sequential expansion
              expandNext(0);
            } else {
              // All nodes expanded! Restore default 750ms speed for future user interactions
              d3.selection.prototype.transition = originalTransition;
            }
          }
          
          // Wait for tree to render, then expand all nodes
          setTimeout(function() {
            expandAll();
          }, 500);
        }
      "))
    update_progress_internal("applying_js_enhancements", "completed")
  }
  
  # Step 5.5: Transform network data for info panels
  update_progress_internal("transforming_network_data", "in_progress", 
                         list(step = "Transforming network data to info panel format"))
  info_panel_network_data <- transform_hybrid_to_info_panel_format(network_data)
  update_progress_internal("transforming_network_data", "completed")
  
  # Step 5.6: Generate info panel data (Wikipedia & PhyloPic content)
  update_progress_internal("generating_info_panels", "in_progress", 
                         list(step = "Generating info panels with Wikipedia and PhyloPic data"))
  tree_data <- add_info_panel_data(tree_data, info_panel_network_data, request_id, progress_token)
  update_progress_internal("generating_info_panels", "completed")
  
  # Step 5.7: Generate final enhanced HTML
  update_progress_internal("generating_final_html", "in_progress", 
                         list(step = "Generating final enhanced HTML with interactive features"))
  tree_html <- create_enhanced_tree_html(tree_data, info_panel_network_data, tree_widget, request_id, progress_token, expansion_speed)
  update_progress_internal("generating_final_html", "completed")
  
  api_log_info(paste("[", request_id, "] Hybrid tree visualization creation completed", sep=""))
  return(tree_html)
}