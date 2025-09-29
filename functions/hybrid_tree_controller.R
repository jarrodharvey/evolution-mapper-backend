# Required libraries
library(rotl)
library(ape)
library(datelife)
library(logger)

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

        # Modern age mapping functions are already sourced at startup

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

      tree_output <- create_hybrid_tree_json(network_data, request_id, progress_token, as_json)
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