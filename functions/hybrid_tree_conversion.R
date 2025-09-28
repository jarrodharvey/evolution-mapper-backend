# Required libraries
library(rotl)
library(ape)
library(datelife)

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
      species_list <- strsplit(key, "\\|")[[1]]

      # CRITICAL FIX: Distinguish between pairwise keys and multi-species root keys
      if (length(species_list) > 2) {
        # This is a multi-species root key - skip to prevent incorrect MRCA assignment
        api_log_info(paste("[", request_id, "] Skipping multi-species root key (", length(species_list), "species):", key, "- root ages should not contaminate internal nodes"))
        next
      }

      # This is a true pairwise key like "Characidium_fasciatum|Crenimugil_crenilabis"
      species_pair <- species_list

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
          return(list(info = "present", has_age = FALSE))
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

        # PHYLOGENETIC VALIDATION: Check if this age conflicts with any child node ages
        max_child_age <- get_max_child_age(node_num)
        if (!is.na(max_child_age) && ancestor_age_mya <= max_child_age) {
          api_log_info(paste("DEBUG CHRONOS: PHYLOGENETIC CONFLICT - Node", node_num, "chronos age", ancestor_age_mya, "Mya would be younger than child age", max_child_age, "Mya - age assignment REJECTED"))
          return(list(info = "age unavailable", has_age = FALSE))
        }

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
          return(list(info = "present", has_age = FALSE))
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


    # Dispatch to the appropriate method based on how ages were assigned
    if (exists("age_assignment_method") && age_assignment_method == "direct_pairwise_fallback") {
      return(assign_fallback_ages(node_num, node_type))
    } else {
      # Default to chronos method (or when no age method is set)
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

  # FINAL PHYLOGENETIC VALIDATION: Remove conflicting parent ages (ALL methods)
  # This is critical to prevent phantom ages from chronos extrapolations
  validate_phylogenetic_consistency_final()

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