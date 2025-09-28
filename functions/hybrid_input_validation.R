# Required libraries
library(rotl)
library(ape)
library(datelife)
library(memoise)

#' Clean scientific names by removing parenthetical addendums
#' @param scientific_names Vector of scientific names to clean
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