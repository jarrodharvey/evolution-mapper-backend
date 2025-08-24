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

source("functions/rotl_tree_generation.R")
source("functions/color_config.R")

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
#' @return List with success status and HTML or error message
generate_hybrid_tree_html <- function(common_names, scientific_names) {
  tryCatch({
    cat("=== GENERATING HYBRID TREE (ROTL + DateLife) ===\n")
    
    # Step 1: Get complete topology from ROTL
    cat("Getting complete topology from ROTL...\n")
    species_data <- get_species_from_db_paired(common_names, scientific_names)
    valid_species <- species_data[!is.na(species_data$ott), ]
    
    if (nrow(valid_species) < 2) {
      # Find missing species for both OTT ID lookup and consistency with /api/dated-tree format
      missing_indices <- which(is.na(species_data$ott))
      missing_common <- if (length(missing_indices) > 0) species_data$common[missing_indices] else c()
      missing_scientific <- if (length(missing_indices) > 0) species_data$scientific[missing_indices] else c()
      
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
    
    rotl_tree <- tol_induced_subtree(ott_ids = valid_species$ott)
    
    if (is.null(rotl_tree)) {
      return(list(
        success = FALSE,
        error = "Failed to get tree from Open Tree of Life"
      ))
    }
    
    # Step 2: Try to get DateLife age data for available species
    cat("Getting age data from DateLife...\n")
    datelife_result <- tryCatch({
      # Set timeout (90 seconds for hybrid tree DateLife query)
      setTimeLimit(cpu = 90, elapsed = 90, transient = TRUE)
      
      # Call DateLife
      result <- get_datelife_result(input = scientific_names, get_spp_from_taxon = FALSE, reference_taxonomy = 'opentree')
      
      # Reset timeout
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
      
      result
    }, error = function(e) {
      # Reset timeout on error
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
      
      if (grepl("timeout|time limit", e$message)) {
        cat("DateLife query timed out after 90 seconds, falling back to topology-only tree\n")
        return(list())  # Return empty list to trigger topology-only fallback
      } else {
        cat("DateLife error:", e$message, "\n")
        return(list())  # Return empty list for other errors too
      }
    })
    
    # Step 3: Create age mapping from DateLife data
    datelife_phylo <- NULL
    datelife_species <- c()
    ancestor_ages <- list()
    
    if (length(datelife_result) > 0) {
      cat("DateLife found", length(datelife_result), "chronograms\n")
      
      # Extract species that have DateLife data
      for (i in seq_along(datelife_result)) {
        study_species <- rownames(datelife_result[[i]])
        datelife_species <- unique(c(datelife_species, study_species))
      }
      
      # Create consensus matrix and phylo tree to get ancestor ages
      tryCatch({
        consensus_matrix <- datelife_result_median_matrix(datelife_result)
        
        if (nrow(consensus_matrix) >= 2) {
          # Convert to phylo tree using proper DateLife function
          datelife_phylo <- summary_matrix_to_phylo(consensus_matrix)
          
          # Get node depths (ages) from DateLife tree
          node_depths <- node.depth.edgelength(datelife_phylo)
          root_age <- max(node_depths)
          node_ages <- root_age - node_depths
          
          # Extract ancestor ages for internal nodes
          n_tips_datelife <- length(datelife_phylo$tip.label)
          for (i in 1:datelife_phylo$Nnode) {
            node_idx <- n_tips_datelife + i
            ancestor_age <- node_ages[node_idx]
            
            # Get descendant species for this internal node
            subtree <- extract.clade(datelife_phylo, node_idx)
            descendants <- subtree$tip.label
            # Create a key based on sorted descendant species
            desc_key <- paste(sort(descendants), collapse = "|")
            ancestor_ages[[desc_key]] <- ancestor_age
          }
          
          cat("Extracted ancestor ages for", length(ancestor_ages), "internal nodes from DateLife\n")
        }
      }, error = function(e) {
        cat("Warning: Could not create DateLife phylo tree:", conditionMessage(e), "\n")
      })
    }
    
    # Step 4: Convert ROTL tree to network format with hybrid age information
    network_data <- convert_phylo_to_network_hybrid(rotl_tree, valid_species, datelife_species, ancestor_ages)
    
    if (is.null(network_data) || nrow(network_data) == 0) {
      return(list(
        success = FALSE,
        error = "Failed to convert tree to network format"
      ))
    }
    
    # Step 5: Create visualization
    tree_html <- create_hybrid_tree_visualization(network_data)
    
    # Determine which species are missing age data (similar to /api/dated-tree)
    # Need to normalize species names for comparison (DateLife uses underscores, input uses spaces)
    datelife_species_normalized <- gsub("_", " ", datelife_species)
    species_without_ages_scientific <- c()
    species_without_ages_common <- c()
    
    # Check each input species to see if it has age data
    for (i in seq_along(scientific_names)) {
      sci_name <- scientific_names[i]
      common_name <- common_names[i]
      
      # Check if this species is in DateLife (normalize both formats for comparison)
      has_datelife_data <- sci_name %in% datelife_species_normalized || 
                          gsub(" ", "_", sci_name) %in% datelife_species
      
      if (!has_datelife_data) {
        species_without_ages_scientific <- c(species_without_ages_scientific, sci_name)
        species_without_ages_common <- c(species_without_ages_common, common_name)
      }
    }
    
    return(list(
      success = TRUE,
      html = tree_html,
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
      coverage = if (length(species_without_ages_scientific) == 0) "complete" else "partial"
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating hybrid tree:", conditionMessage(e)),
      input_common_names = common_names,
      input_scientific_names = scientific_names,
      missing_common_names = c(),  # Initialize empty for frontend compatibility
      missing_scientific_names = c()  # Initialize empty for frontend compatibility
    ))
  })
}

#' Convert ROTL phylo tree to network format with hybrid age information
#' @param phylo_tree ROTL phylo object
#' @param species_data Species data frame with user-provided names
#' @param datelife_species Vector of species names that have DateLife age data
#' @param ancestor_ages Named list mapping descendant combinations to ancestor ages
#' @return Data frame with parent-child network structure and age information
convert_phylo_to_network_hybrid <- function(phylo_tree, species_data, datelife_species, ancestor_ages) {
  
  n_tips <- length(phylo_tree$tip.label)
  n_nodes <- phylo_tree$Nnode
  
  cat(sprintf("Processing hybrid tree with %d tips and %d internal nodes\n", n_tips, n_nodes))
  cat(sprintf("DateLife coverage: %d/%d species\n", length(datelife_species), nrow(species_data)))
  
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
  
  # Function to get age information for a species or node
  get_age_info <- function(node_num, node_type) {
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
      # Internal node - try to find matching ancestor age from DateLife
      # Get all descendant tips for this internal node
      subtree <- extract.clade(phylo_tree, node_num)
      descendants <- subtree$tip.label
      
      # Clean descendant names and match to DateLife format
      datelife_descendants <- c()
      for (desc_tip in descendants) {
        tip_clean <- gsub("_ott\\d+", "", desc_tip)
        tip_clean <- gsub("_", " ", tip_clean)
        
        # Convert to DateLife format (underscores)
        tip_datelife <- gsub(" ", "_", tip_clean)
        
        # Check if this species is in DateLife data
        if (tip_datelife %in% datelife_species || tip_clean %in% datelife_species) {
          datelife_descendants <- c(datelife_descendants, tip_datelife)
        }
      }
      
      # Check if this ROTL ancestor should have age data
      # Only ancestors whose descendants are ALL in DateLife should get ages
      total_descendants <- length(descendants)
      
      if (length(datelife_descendants) >= 2 && length(datelife_descendants) == total_descendants) {
        # All descendants of this ancestor are in DateLife - we can apply ages
        desc_key <- paste(sort(datelife_descendants), collapse = "|")
        
        # Check if we have an exact match for this ancestor
        if (desc_key %in% names(ancestor_ages)) {
          ancestor_age_mya <- round(ancestor_ages[[desc_key]], 1)
          return(list(info = paste0(ancestor_age_mya, " Mya"), has_age = TRUE))
        }
        
        # If no exact match, check for subset matches within DateLife data
        for (age_key in names(ancestor_ages)) {
          age_descendants <- strsplit(age_key, "\\|")[[1]]
          # Check if our descendants are a subset of this DateLife age group
          if (all(datelife_descendants %in% age_descendants)) {
            ancestor_age_mya <- round(ancestor_ages[[age_key]], 1)
            return(list(info = paste0("~", ancestor_age_mya, " Mya"), has_age = TRUE))
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
  
  # Function to get node label with age information
  get_node_label_with_age <- function(node_num, node_type) {
    if (node_num <= n_tips) {
      # Species node - use user-provided common name
      tip_label <- phylo_tree$tip.label[node_num]
      tip_clean <- gsub("_ott\\d+", "", tip_label)
      tip_clean <- gsub("_", " ", tip_clean)
      
      match_idx <- which(species_data$scientific == tip_clean)
      if (length(match_idx) > 0) {
        common_name <- species_data$common[match_idx[1]]
        return(common_name)  # Species never need age labels - just show the name
      }
      return(gsub("_", " ", tip_clean))
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
  
  # Process each edge in the ROTL tree
  for (i in 1:nrow(phylo_tree$edge)) {
    parent_num <- phylo_tree$edge[i, 1]
    child_num <- phylo_tree$edge[i, 2]
    
    # Determine node types
    parent_type <- if (parent_num <= n_tips) "species" else {
      internal_index <- parent_num - n_tips
      if (!is.null(phylo_tree$node.label) && 
          length(phylo_tree$node.label) >= internal_index && 
          !is.na(phylo_tree$node.label[internal_index]) && 
          !grepl("^[Mm]rcaott\\d+ott\\d+", phylo_tree$node.label[internal_index])) {
        "taxonomic"
      } else {
        "ancestor"
      }
    }
    
    child_type <- if (child_num <= n_tips) "species" else {
      internal_index <- child_num - n_tips
      if (!is.null(phylo_tree$node.label) && 
          length(phylo_tree$node.label) >= internal_index && 
          !is.na(phylo_tree$node.label[internal_index]) && 
          !grepl("^[Mm]rcaott\\d+ott\\d+", phylo_tree$node.label[internal_index])) {
        "taxonomic"
      } else {
        "ancestor"
      }
    }
    
    # Get labels with age information
    parent_label <- get_node_label_with_age(parent_num, parent_type)
    child_label <- get_node_label_with_age(child_num, child_type)
    
    # Get age information
    child_age_result <- get_age_info(child_num, child_type)
    
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
  
  # Add root handling (same as ROTL approach)
  if (nrow(network_data) > 0) {
    all_parents <- unique(network_data$from)
    all_children <- unique(network_data$to)
    orphaned_parents <- setdiff(all_parents, all_children)
    
    root_name <- "Common ancestor - click me!"
    
    if (length(orphaned_parents) > 0) {
      for (orphaned_parent in orphaned_parents) {
        if (grepl("^Ancestor [A-Z]", orphaned_parent)) {
          node_type <- "ancestor"
        } else {
          node_type <- "taxonomic"
        }
        
        network_data <- rbind(data.frame(
          from = root_name,
          to = orphaned_parent,
          NodeType = node_type,
          AgeInfo = "age unavailable",
          HasAge = FALSE,
          stringsAsFactors = FALSE
        ), network_data)
      }
    }
    
    # Add root row
    network_data <- rbind(data.frame(
      from = NA,
      to = root_name,
      NodeType = "root",
      AgeInfo = "age unavailable",
      HasAge = FALSE,
      stringsAsFactors = FALSE
    ), network_data)
  }
  
  return(network_data)
}

#' Create CollapsibleTree visualization for hybrid tree
#' @param network_data Network data frame with age information
#' @return HTML string for CollapsibleTree
create_hybrid_tree_visualization <- function(network_data) {
  
  # Calculate dynamic link length with extra space for age information
  link_length <- calculate_dynamic_link_length_hybrid(network_data)
  
  # Prepare data for collapsibleTreeNetwork
  tree_data <- data.frame(
    Parent = network_data$from,
    Child = network_data$to,
    NodeType = network_data$NodeType,
    HasAge = network_data$HasAge,
    stringsAsFactors = FALSE
  )
  
  # Add color mapping using centralized color configuration
  tree_data$Color <- sapply(1:nrow(tree_data), function(i) {
    node_type <- tree_data$NodeType[i]
    has_age <- tree_data$HasAge[i]
    
    get_node_color(node_type, has_age)
  })
  
  # Create collapsibleTree with color mapping
  tree_widget <- collapsibleTreeNetwork(
    tree_data,
    attribute = "NodeType",
    fill = "Color", 
    fontSize = 12,
    linkLength = link_length,
    nodeSize = "leafCount",
    width = 1000,
    height = 800,
    zoomable = TRUE
  )
  
  # Convert to HTML
  temp_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(tree_widget, temp_file, selfcontained = TRUE)
  tree_html <- paste(readLines(temp_file), collapse = "\n")
  unlink(temp_file)
  
  # Remove white bar
  custom_css <- "<style>body { margin: 0 !important; padding: 0 !important; overflow: hidden !important; } html { margin: 0 !important; padding: 0 !important; } #htmlwidget_container { margin: 0 !important; padding: 0 !important; }</style>"
  tree_html <- gsub("</head>", paste0(custom_css, "</head>"), tree_html)
  
  return(tree_html)
}