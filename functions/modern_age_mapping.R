# Modern Age Mapping Functions
# Uses ape::chronos instead of problematic summary_matrix_to_phylo()
# Implements direct mapping of DateLife ages to ROTL tree nodes

library(rotl)
library(ape)
library(datelife)
library(dplyr)

# Source shared logging configuration
source("functions/logging_config.R")

#' Extract pairwise ages from DateLife chronograms
#' @param datelife_results List of chronogram matrices from get_datelife_result()
#' @param request_id Optional request ID for logging
#' @return Named list of pairwise ages (key = "species1_species2", value = vector of ages)
extract_pairwise_ages_modern <- function(datelife_results, request_id = "extract_ages") {
  api_log_info(paste("[", request_id, "] Extracting pairwise ages from", length(datelife_results), "DateLife chronograms"))
  
  age_data <- list()
  
  for (i in seq_along(datelife_results)) {
    matrix <- datelife_results[[i]]
    taxa <- rownames(matrix)
    
    if (length(taxa) < 2) next
    
    api_log_info(paste("[", request_id, "] Processing chronogram", i, "with species:", paste(taxa, collapse = ", ")))
    
    # Get all unique pairs
    pairs <- combn(taxa, 2)
    
    for (j in 1:ncol(pairs)) {
      taxon1 <- pairs[1, j]
      taxon2 <- pairs[2, j]
      
      # Get the patristic distance (total evolutionary distance)
      distance <- matrix[taxon1, taxon2]
      
      # Convert distance to MRCA age (distance represents 2x the MRCA age)
      age <- distance / 2
      
      # Create a sorted key for the pair
      pair_key <- paste(sort(c(taxon1, taxon2)), collapse = "_")
      
      if (!is.null(age) && !is.na(age) && is.finite(age)) {
        if (is.null(age_data[[pair_key]])) {
          age_data[[pair_key]] <- c()
        }
        age_data[[pair_key]] <- c(age_data[[pair_key]], age)
        
        api_log_info(paste("[", request_id, "] Found age:", taxon1, "—", taxon2, "MRCA =", round(age, 1), "Mya"))
      }
    }
  }
  
  # Calculate summary statistics for each pair
  age_summary <- list()
  for (pair_key in names(age_data)) {
    ages <- age_data[[pair_key]]
    age_summary[[pair_key]] <- list(
      median = median(ages),
      mean = mean(ages),
      min = min(ages),
      max = max(ages),
      count = length(ages),
      all_values = ages
    )
  }
  
  api_log_info(paste("[", request_id, "] Extracted ages for", length(age_summary), "species pairs"))
  
  return(age_summary)
}

#' Create calibration points for ape::chronos from DateLife ages and ROTL tree
#' @param rotl_tree Phylo tree from ROTL
#' @param pairwise_ages Named list from extract_pairwise_ages_modern()
#' @param species_data Data frame mapping common names to scientific names
#' @param request_id Optional request ID for logging
#' @return Data frame with calibration points for chronos()
create_chronos_calibrations <- function(rotl_tree, pairwise_ages, species_data, request_id = "create_cal") {
  api_log_info(paste("[", request_id, "] Creating chronos calibration points from", length(pairwise_ages), "pairwise ages"))
  
  calibration_points <- data.frame(
    node = integer(),
    age.min = numeric(),
    age.max = numeric(),
    soft.bounds = logical(),
    pair_key = character(),
    species1 = character(),
    species2 = character(),
    stringsAsFactors = FALSE
  )
  
  # Clean tip labels to match DateLife format (remove OTT IDs)
  clean_tip_labels <- gsub("_ott\\d+", "", rotl_tree$tip.label)
  clean_tip_labels <- gsub("_", " ", clean_tip_labels)
  
  for (pair_key in names(pairwise_ages)) {
    # Parse the pair key correctly - it's in format "Species1_species1_Species2_species2"
    # Need to split by "_" but then recombine genus + species
    taxa <- strsplit(pair_key, "_")[[1]]
    
    # For binomial names, we expect 4 parts: Genus1, species1, Genus2, species2
    if (length(taxa) >= 4) {
      taxon1 <- paste(taxa[1], taxa[2])  # Genus species
      taxon2 <- paste(taxa[3], taxa[4])  # Genus species
    } else if (length(taxa) == 2) {
      # If only 2 parts, assume they are complete species names with underscores
      taxon1 <- gsub("_", " ", taxa[1])
      taxon2 <- gsub("_", " ", taxa[2])  
    } else {
      # Skip malformed keys
      api_log_warn(paste("[", request_id, "] Malformed pair key:", pair_key))
      next
    }
    
    api_log_info(paste("[", request_id, "] Processing pair:", taxon1, "—", taxon2))
    
    # Find corresponding tip labels in ROTL tree
    rotl_tip1 <- NULL
    rotl_tip2 <- NULL
    
    for (i in seq_along(clean_tip_labels)) {
      if (clean_tip_labels[i] == taxon1) {
        rotl_tip1 <- rotl_tree$tip.label[i]
      }
      if (clean_tip_labels[i] == taxon2) {
        rotl_tip2 <- rotl_tree$tip.label[i]
      }
    }
    
    if (!is.null(rotl_tip1) && !is.null(rotl_tip2)) {
      # Find the MRCA node in the ROTL tree
      mrca_node <- getMRCA(rotl_tree, tip = c(rotl_tip1, rotl_tip2))
      
      if (!is.null(mrca_node) && !is.na(mrca_node)) {
        age_info <- pairwise_ages[[pair_key]]
        
        # Use median with some uncertainty range
        median_age <- age_info$median
        min_age <- max(age_info$min, median_age * 0.8)  # At least 80% of median
        max_age <- min(age_info$max, median_age * 1.2)  # At most 120% of median
        
        calibration_points <- rbind(calibration_points, data.frame(
          node = mrca_node,
          age.min = min_age,
          age.max = max_age,
          soft.bounds = FALSE,
          pair_key = pair_key,
          species1 = taxon1,
          species2 = taxon2,
          stringsAsFactors = FALSE
        ))
        
        api_log_info(paste("[", request_id, "] Calibration: Node", mrca_node, "for", taxon1, "—", taxon2, "=", round(median_age, 1), "Mya (", round(min_age, 1), "-", round(max_age, 1), ")"))
      } else {
        api_log_warn(paste("[", request_id, "] Could not find MRCA for", taxon1, "—", taxon2, "in ROTL tree"))
      }
    } else {
      api_log_warn(paste("[", request_id, "] Could not find tips for", taxon1, "—", taxon2, "in ROTL tree"))
    }
  }
  
  # Remove duplicate nodes, keeping the one with the most data
  if (nrow(calibration_points) > 0) {
    # Sort by node, then by number of studies (prefer pairs with more data)
    calibration_points$study_count <- sapply(calibration_points$pair_key, function(pk) pairwise_ages[[pk]]$count)
    calibration_points <- calibration_points[order(calibration_points$node, -calibration_points$study_count), ]
    calibration_points <- calibration_points[!duplicated(calibration_points$node), ]
    calibration_points$study_count <- NULL
  }
  
  api_log_info(paste("[", request_id, "] Created", nrow(calibration_points), "calibration points for chronos"))
  
  return(calibration_points)
}

#' Create direct pairwise age mapping when chronos fails
#' @param rotl_tree Phylo tree from ROTL (topology only) 
#' @param pairwise_ages Named list of pairwise ages from extract_pairwise_ages_modern()
#' @param calibrations Calibration points data frame from create_chronos_calibrations()
#' @param species_data Data frame mapping common names to scientific names
#' @param request_id Optional request ID for logging
#' @return List with success status and node_ages or error
create_direct_pairwise_mapping <- function(rotl_tree, pairwise_ages, calibrations, species_data, request_id = "direct_mapping") {
  api_log_info(paste("[", request_id, "] Creating direct pairwise age mapping from", length(pairwise_ages), "pairwise ages"))
  
  tryCatch({
    node_ages <- list()
    
    # Clean tip labels to match with calibrations
    clean_tip_labels <- gsub("_ott\\d+", "", rotl_tree$tip.label)
    clean_tip_labels <- gsub("_", " ", clean_tip_labels)
    
    # Use the calibration points to map ages directly to ROTL tree nodes
    for (i in 1:nrow(calibrations)) {
      calib <- calibrations[i, ]
      species1 <- calib$species1
      species2 <- calib$species2
      
      # Get the median age for this pair from pairwise_ages
      pair_key <- calib$pair_key
      if (pair_key %in% names(pairwise_ages)) {
        age_info <- pairwise_ages[[pair_key]]
        median_age <- age_info$median
        
        api_log_info(paste("[", request_id, "] Direct mapping:", species1, "—", species2, "=", round(median_age, 1), "Mya"))
        
        # Create the descendant key in DateLife format (with underscores and pipe separator)
        species1_datelife <- gsub(" ", "_", species1)
        species2_datelife <- gsub(" ", "_", species2)
        desc_key <- paste(sort(c(species1_datelife, species2_datelife)), collapse = "|")
        
        # Store the age
        node_ages[[desc_key]] <- median_age
      }
    }
    
    api_log_info(paste("[", request_id, "] Direct pairwise mapping completed - created", length(node_ages), "node ages"))
    
    return(list(
      success = TRUE,
      node_ages = node_ages,
      calibrations_used = calibrations,
      pairwise_ages_found = length(pairwise_ages),
      method = "direct_pairwise_fallback"
    ))
    
  }, error = function(e) {
    api_log_error(paste("[", request_id, "] Direct pairwise mapping failed:", conditionMessage(e)))
    return(list(success = FALSE, error = conditionMessage(e), method = "direct_pairwise_fallback"))
  })
}

#' Generate dated tree using modern ape::chronos approach instead of summary_matrix_to_phylo
#' @param rotl_tree Phylo tree from ROTL (topology only)
#' @param datelife_results List of chronogram matrices from DateLife
#' @param species_data Data frame mapping common names to scientific names
#' @param request_id Optional request ID for logging
#' @return List with success status and dated phylo tree or error
generate_dated_tree_chronos <- function(rotl_tree, datelife_results, species_data, request_id = "chronos_dating") {
  api_log_info(paste("[", request_id, "] Starting modern chronos-based tree dating"))
  
  tryCatch({
    # Step 1: Extract pairwise ages from DateLife
    step_start <- Sys.time()
    pairwise_ages <- extract_pairwise_ages_modern(datelife_results, request_id)
    step_duration <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Pairwise age extraction completed - Duration:", round(step_duration, 3), "s"))
    
    if (length(pairwise_ages) == 0) {
      api_log_warn(paste("[", request_id, "] No pairwise ages found - cannot calibrate tree"))
      return(list(success = FALSE, error = "No pairwise ages available from DateLife", method = "chronos"))
    }
    
    # Step 2: Create calibration points for chronos
    step_start <- Sys.time()
    calibrations <- create_chronos_calibrations(rotl_tree, pairwise_ages, species_data, request_id)
    step_duration <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Calibration creation completed - Duration:", round(step_duration, 3), "s"))
    
    if (nrow(calibrations) == 0) {
      api_log_warn(paste("[", request_id, "] No calibration points created - cannot date tree"))
      return(list(success = FALSE, error = "No calibration points could be matched to ROTL tree", method = "chronos"))
    }
    
    # Step 3: Use chronos to date the tree
    step_start <- Sys.time()
    api_log_info(paste("[", request_id, "] Running ape::chronos with", nrow(calibrations), "calibration points"))
    
    # ROTL trees don't have branch lengths, so we need to add them first
    # Use compute.brlen to add unit branch lengths, then chronos will calibrate them
    if (is.null(rotl_tree$edge.length)) {
      api_log_info(paste("[", request_id, "] Adding unit branch lengths to ROTL tree"))
      rotl_tree <- compute.brlen(rotl_tree, method = "Grafen", power = 1)
    }
    
    # Use chronos with relaxed clock model
    dated_tree <- chronos(
      phy = rotl_tree,
      calibration = calibrations,
      model = "relaxed",  # Relaxed molecular clock
      control = chronos.control(nb.rate.cat = 1)  # Single rate category for simplicity
    )
    
    step_duration <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Chronos dating completed - Duration:", round(step_duration, 3), "s"))
    
    # Extract node ages from the dated tree using modern approach
    node_ages <- list()
    
    api_log_info(paste("[", request_id, "] Extracting node ages from dated tree using modern method"))
    
    # Use the modern approach: find MRCA nodes for each pairwise relationship we calibrated with
    # This ensures we get the exact same nodes that chronos used for calibration
    
    # Get the branching times (this gives us all internal node ages)
    branching_times_tree <- branching.times(dated_tree)
    root_age <- max(branching_times_tree)
    api_log_info(paste("[", request_id, "] Root age:", round(root_age, 1), "Mya"))
    
    # Clean tip labels to match DateLife format for comparison
    clean_tip_labels <- gsub("_ott\\d+", "", dated_tree$tip.label)
    clean_tip_labels <- gsub("_", " ", clean_tip_labels)
    
    # For each calibration point we created, find the corresponding node age in the dated tree
    if (exists("calibrations") && nrow(calibrations) > 0) {
      for (i in 1:nrow(calibrations)) {
        calib <- calibrations[i, ]
        species1 <- calib$species1
        species2 <- calib$species2
        
        api_log_info(paste("[", request_id, "] Processing calibrated pair:", species1, "—", species2))
        
        # Find the tip labels that correspond to these species
        tip1 <- NULL
        tip2 <- NULL
        
        for (j in seq_along(clean_tip_labels)) {
          if (clean_tip_labels[j] == species1) {
            tip1 <- dated_tree$tip.label[j]
          }
          if (clean_tip_labels[j] == species2) {
            tip2 <- dated_tree$tip.label[j]
          }
        }
        
        if (!is.null(tip1) && !is.null(tip2)) {
          # Find the MRCA node for this pair
          mrca_node <- getMRCA(dated_tree, tip = c(tip1, tip2))
          
          if (!is.null(mrca_node) && !is.na(mrca_node)) {
            # Get the age of this node using node.depth.edgelength approach
            # branching.times() uses text labels, but getMRCA() returns integers
            # So we need to calculate the age directly from the tree structure
            node_depths <- node.depth.edgelength(dated_tree)
            root_depth <- max(node_depths)
            node_age <- root_depth - node_depths[mrca_node]
            
            if (!is.na(node_age)) {
              # Create the descendant key for this pair (DateLife format)
              species1_datelife <- gsub(" ", "_", species1)
              species2_datelife <- gsub(" ", "_", species2)
              desc_key <- paste(sort(c(species1_datelife, species2_datelife)), collapse = "|")
              
              node_ages[[desc_key]] <- node_age
              api_log_info(paste("[", request_id, "] Node age: MRCA of", species1_datelife, "and", species2_datelife, "=", round(node_age, 1), "Mya"))
            }
          }
        }
      }
    }
    
    # Also try to extract ages for larger groups (e.g., all 3 species)
    # Find the root node age and assign it to all species combined
    if (length(clean_tip_labels) >= 3) {
      # Get all species in DateLife format
      all_species_datelife <- sort(gsub(" ", "_", clean_tip_labels))
      root_desc_key <- paste(all_species_datelife, collapse = "|")
      node_ages[[root_desc_key]] <- root_age
      api_log_info(paste("[", request_id, "] Root node age: MRCA of all species =", round(root_age, 1), "Mya"))
    }
    
    api_log_info(paste("[", request_id, "] Modern chronos approach completed successfully"))
    
    return(list(
      success = TRUE,
      dated_tree = dated_tree,
      node_ages = node_ages,
      calibrations_used = calibrations,
      pairwise_ages_found = length(pairwise_ages),
      method = "chronos"
    ))
    
  }, error = function(e) {
    api_log_error(paste("[", request_id, "] Modern chronos approach failed:", conditionMessage(e)))
    
    # Check if this is the "cannot find reasonable starting dates" error that we can work around
    error_msg <- conditionMessage(e)
    if (grepl("cannot find reasonable starting dates", error_msg) && exists("pairwise_ages") && exists("calibrations")) {
      api_log_info(paste("[", request_id, "] Attempting direct pairwise age mapping fallback..."))
      
      # Try the direct pairwise mapping approach
      fallback_result <- create_direct_pairwise_mapping(rotl_tree, pairwise_ages, calibrations, species_data, request_id)
      
      if (fallback_result$success) {
        return(fallback_result)
      } else {
        api_log_warn(paste("[", request_id, "] Fallback also failed:", fallback_result$error))
      }
    }
    
    return(list(success = FALSE, error = conditionMessage(e), method = "chronos"))
  })
}