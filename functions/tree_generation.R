# Tree generation functions migrated from correct_tree_walking.R
# Handles phylogenetic tree creation and CollapsibleTree HTML generation

library(rotl)
library(ape)
library(collapsibleTree)
library(htmlwidgets)
library(RSQLite)
library(DBI)
library(dplyr)

# Function to get species from database
get_species_from_db <- function(common_names) {
  db_path <- "data/species.sqlite"
  species_db <- dbConnect(SQLite(), db_path)
  
  species_data <- lapply(common_names, function(common_name) {
    # Get all matches for this common name
    sql_string <- paste0("SELECT * FROM species WHERE common = '", common_name, "'")
    query_result <- dbSendQuery(species_db, sql_string)
    species_df <- dbFetch(query_result)
    dbClearResult(query_result)
    
    if (nrow(species_df) > 0) {
      # Pick one at random if multiple matches exist
      random_index <- sample(nrow(species_df), 1)
      return(species_df[random_index, ])
    } else {
      return(data.frame(ott = NA, common = common_name, scientific = NA))
    }
  }) %>%
    bind_rows()
  
  dbDisconnect(species_db)
  return(species_data)
}

# Function to convert scientific names to readable ancestor labels
convert_to_readable_name <- function(name) {
  # Handle common scientific name patterns
  readable_names <- list(
    "Homo sapiens" = "Human",
    "Canis lupus" = "Dog", 
    "Felis catus" = "Cat",
    "Boreoeutheria" = "Northern placental mammals",
    "Canidae" = "Dog family",
    "Felidae" = "Cat family",
    "Carnivora" = "Carnivores (meat-eaters)",
    "Mammalia" = "Mammals",
    "Chordata" = "Vertebrates",
    "Primates" = "Primates (apes and monkeys)",
    "Hominidae" = "Great apes"
  )
  
  # Return readable name if available, otherwise clean up scientific name
  if (name %in% names(readable_names)) {
    return(readable_names[[name]])
  } else {
    # Clean up scientific names - capitalize first letter, replace underscores
    clean_name <- gsub("_", " ", name)
    clean_name <- gsub("^([a-z])", "\\U\\1", clean_name, perl = TRUE)
    return(clean_name)
  }
}

# Function to trace path from tip to root
trace_path_to_root <- function(tip_number, edges, node_labels, tip_labels, species_common_name) {
  path <- c()
  current_node <- tip_number
  n_tips <- length(tip_labels)
  
  # Start with the species (tip) - use provided common name
  path <- c(species_common_name)
  
  # Walk up the tree following parent-child relationships
  while (TRUE) {
    # Find edge where current_node is the child
    parent_edge <- which(edges[, 2] == current_node)
    
    if (length(parent_edge) == 0) break # Reached root
    
    # Get parent node
    parent_node <- edges[parent_edge[1], 1]
    
    # Add parent to path if it's an internal node
    if (parent_node > n_tips && !is.null(node_labels)) {
      parent_idx <- parent_node - n_tips
      if (parent_idx <= length(node_labels)) {
        raw_name <- gsub(" ott[0-9]+$", "", node_labels[parent_idx])
        
        # Handle unnamed nodes
        if (grepl("^mrcaott[0-9]+ott[0-9]+$", raw_name) || raw_name == "") {
          readable_name <- paste("Ancestor", LETTERS[parent_idx])
        } else {
          readable_name <- convert_to_readable_name(raw_name)
        }
        
        path <- c(readable_name, path)
      }
    }
    
    current_node <- parent_node
  }
  
  return(path)
}

# Function to trim conflicting branches to prevent duplicate nodes
trim_conflicting_branches <- function(hierarchy_data) {
  hierarchy_cols <- names(hierarchy_data)[names(hierarchy_data) != "Species"]
  
  # Create a comprehensive map of all node appearances across levels
  node_level_map <- list()
  
  for (i in 1:nrow(hierarchy_data)) {
    for (col_idx in 1:length(hierarchy_cols)) {
      col <- hierarchy_cols[col_idx]
      if (!is.na(hierarchy_data[[col]][i])) {
        node_name <- hierarchy_data[[col]][i]
        
        if (is.null(node_level_map[[node_name]])) {
          node_level_map[[node_name]] <- list()
        }
        
        level_key <- paste("Level", col_idx, sep = "")
        if (is.null(node_level_map[[node_name]][[level_key]])) {
          node_level_map[[node_name]][[level_key]] <- c()
        }
        
        node_level_map[[node_name]][[level_key]] <- c(node_level_map[[node_name]][[level_key]], i)
      }
    }
  }
  
  # Identify cross-level conflicts (same node at different hierarchical levels)
  for (node_name in names(node_level_map)) {
    levels_with_node <- names(node_level_map[[node_name]])
    
    if (length(levels_with_node) > 1) {
      # Cross-level conflict detected
      cat("Cross-level conflict detected for '", node_name, "' at levels: ", paste(levels_with_node, collapse = ", "), "\n")
      
      # Strategy: Keep the node at its highest level (earliest level number)
      # Remove it from all lower levels
      level_numbers <- as.numeric(gsub("Level", "", levels_with_node))
      primary_level <- min(level_numbers)
      secondary_levels <- level_numbers[level_numbers > primary_level]
      
      cat("  Keeping '", node_name, "' at Level", primary_level, "\n")
      cat("  Removing from levels: ", paste(secondary_levels, collapse = ", "), "\n")
      
      for (secondary_level in secondary_levels) {
        col_name <- hierarchy_cols[secondary_level]
        affected_rows <- node_level_map[[node_name]][[paste("Level", secondary_level, sep = "")]]
        
        for (row in affected_rows) {
          cat("    Row", row, "(", hierarchy_data$Species[row], "): removing '", node_name, "' from", col_name, "\n")
          hierarchy_data[[col_name]][row] <- NA
        }
      }
    }
  }
  
  return(hierarchy_data)
}

# Function to resolve outdated OTT IDs using TNRS
resolve_outdated_otts <- function(species_data) {
  # Only use TNRS as fallback when tree generation fails
  # This conservative approach prevents unnecessary duplicates
  return(species_data)
}

# Fallback TNRS resolution for when initial tree generation fails
fallback_tnrs_resolution <- function(species_data) {
  cat("Attempting more aggressive TNRS resolution...\n")
  
  for (i in 1:nrow(species_data)) {
    scientific_name <- species_data$scientific[i]
    common_name <- species_data$common[i]
    
    # Skip if no scientific name
    if (is.na(scientific_name)) {
      species_data$ott[i] <- NA
      next
    }
    
    tryCatch({
      # Use TNRS to find current OTT ID (try without context for broader matching)
      tnrs_result <- rotl::tnrs_match_names(scientific_name)
      
      if (nrow(tnrs_result) > 0 && !is.na(tnrs_result$ott_id[1])) {
        old_ott <- species_data$ott[i]
        new_ott <- tnrs_result$ott_id[1]
        species_data$ott[i] <- new_ott
        
        if (!is.na(old_ott) && old_ott != new_ott) {
          cat("  Fallback updated", common_name, ":", old_ott, "->", new_ott, "\n")
        } else {
          cat("  Fallback resolved", common_name, "->", new_ott, "\n")
        }
      } else {
        cat("  Could not resolve", common_name, "(", scientific_name, ")\n")
        species_data$ott[i] <- NA
      }
    }, error = function(e) {
      cat("  Fallback TNRS failed for", common_name, "\n")
      species_data$ott[i] <- NA
    })
  }
  
  return(species_data)
}

# Function to convert phylo object directly to collapsibleTreeNetwork format
convert_phylo_to_network <- function(phylo_tree, species_data) {
  # Extract components from phylo object
  edges <- phylo_tree$edge
  tip_labels <- phylo_tree$tip.label  
  node_labels <- phylo_tree$node.label
  n_tips <- Ntip(phylo_tree)
  
  # Create node label lookup table
  node_lookup <- c()
  
  # Add tip labels (nodes 1 to n_tips)
  for (i in 1:n_tips) {
    tip_scientific <- tip_labels[i]
    # Extract clean scientific name (remove OTT suffix)
    clean_scientific <- gsub("_ott[0-9]+$", "", tip_scientific)
    clean_scientific <- gsub("_", " ", clean_scientific)
    
    # Find corresponding common name from species_data
    matching_species <- species_data[species_data$scientific == clean_scientific, ]
    if (nrow(matching_species) > 0) {
      node_lookup[i] <- matching_species$common[1]
    } else {
      # Fallback to cleaned scientific name
      node_lookup[i] <- clean_scientific
    }
  }
  
  # Add internal node labels (nodes n_tips+1 to n_tips+Nnode)
  if (!is.null(node_labels) && length(node_labels) > 0) {
    for (i in 1:length(node_labels)) {
      node_num <- n_tips + i
      raw_name <- node_labels[i]
      
      # Clean up node labels - make unnamed ancestors unique
      if (grepl("^mrcaott[0-9]+ott[0-9]+$", raw_name) || raw_name == "") {
        readable_name <- paste("Ancestor", LETTERS[i])  # Make unique with letters
      } else {
        # Remove OTT IDs and convert to readable name
        clean_name <- gsub(" ott[0-9]+$", "", raw_name)
        readable_name <- convert_to_readable_name(clean_name)
      }
      
      node_lookup[node_num] <- readable_name
    }
  }
  
  # Build network data frame directly from edge matrix
  network_data <- data.frame(
    from = character(0),
    to = character(0), 
    NodeType = character(0),
    Color = character(0),
    stringsAsFactors = FALSE
  )
  
  # Convert each edge to parent-child relationship
  for (i in 1:nrow(edges)) {
    parent_num <- edges[i, 1]
    child_num <- edges[i, 2]
    
    parent_label <- node_lookup[parent_num]
    child_label <- node_lookup[child_num]
    
    # Determine child node type
    if (child_num <= n_tips) {
      child_type <- "species"
      child_color <- "#27AE60"  # Green for species
    } else {
      if (grepl("^Ancestor [A-Z]$", child_label)) {
        child_type <- "ancestor"
        child_color <- "#3498DB"  # Blue for ancestors
      } else {
        child_type <- "taxonomic"  
        child_color <- "#F39C12"  # Orange for taxonomic groups
      }
    }
    
    network_data <- rbind(network_data, data.frame(
      from = parent_label,
      to = child_label,
      NodeType = child_type,
      Color = child_color,
      stringsAsFactors = FALSE
    ))
  }
  
  # Find root node (appears as parent but never as child)
  all_parents <- unique(network_data$from)
  all_children <- unique(network_data$to)
  root_nodes <- setdiff(all_parents, all_children)
  
  if (length(root_nodes) == 1) {
    root_name <- "Common ancestor - click me!"
    
    # Add root row with NA parent
    root_row <- data.frame(
      from = NA,
      to = root_name, 
      NodeType = "root",
      Color = "#E74C3C",  # Red for root
      stringsAsFactors = FALSE
    )
    
    # Replace the original root in all parent relationships
    network_data$from[network_data$from == root_nodes[1]] <- root_name
    
    # Prepend root row
    network_data <- rbind(root_row, network_data)
  }
  
  return(network_data)
}

# Main function to convert rotl tree to CollapsibleTree hierarchy  
convert_rotl_to_hierarchy <- function(common_names) {
  # Get species from database
  species_data <- get_species_from_db(common_names)
  
  # Try to resolve outdated OTT IDs using TNRS (Taxonomic Name Resolution Service)
  species_data <- resolve_outdated_otts(species_data)
  
  # Deduplicate species data by OTT ID to prevent duplicate entries in tree
  # Only deduplicate if we have valid OTT IDs, keep species with NA OTT IDs
  valid_species <- !is.na(species_data$ott)
  if (sum(valid_species) > 0) {
    # Keep first occurrence of each unique OTT ID among valid species
    unique_valid <- !duplicated(species_data$ott[valid_species])
    species_data_valid <- species_data[valid_species, ][unique_valid, ]
    # Keep all species with NA OTT IDs  
    species_data_na <- species_data[!valid_species, ]
    species_data <- rbind(species_data_valid, species_data_na)
  }
  
  # Get rotl tree
  valid_otts <- species_data$ott[!is.na(species_data$ott)]
  if (length(valid_otts) < 2) {
    stop("Need at least 2 valid species with OTT IDs")
  }
  
  # Try to get phylo tree, if it fails due to invalid OTT IDs, try TNRS resolution again
  phylo_tree <- tryCatch({
    tol_induced_subtree(ott_ids = valid_otts)
  }, error = function(e) {
    if (grepl("was not found", conditionMessage(e))) {
      cat("Some OTT IDs are invalid, attempting fallback TNRS resolution...\n")
      
      # Try more aggressive TNRS resolution
      species_data <<- fallback_tnrs_resolution(species_data)
      valid_otts <- species_data$ott[!is.na(species_data$ott)]
      
      if (length(valid_otts) < 2) {
        stop("After TNRS resolution, still need at least 2 valid species")
      }
      
      tol_induced_subtree(ott_ids = valid_otts)
    } else {
      stop(conditionMessage(e))
    }
  })
  
  # Extract tree components
  edges <- phylo_tree$edge
  tip_labels <- phylo_tree$tip.label
  node_labels <- phylo_tree$node.label
  n_tips <- Ntip(phylo_tree)
  
  # Trace path for each species
  hierarchy_rows <- list()
  
  for (i in 1:nrow(species_data)) {
    common_name <- species_data$common[i]
    scientific_name <- species_data$scientific[i]
    
    # Find matching tip in phylo tree
    if (!is.na(scientific_name)) {
      # Match scientific name to tip label
      scientific_pattern <- gsub(" ", "_", scientific_name)
      tip_match <- which(grepl(scientific_pattern, tip_labels, fixed = TRUE))
      
      if (length(tip_match) > 0) {
        tip_num <- tip_match[1]
        path <- trace_path_to_root(tip_num, edges, node_labels, tip_labels, common_name)
        
        # Better tree depth handling - keep meaningful levels
        if (length(path) >= 2) {
          # Extract meaningful levels from path (excluding the species itself)
          ancestors <- path[-length(path)]  # Remove species from end
          species_name <- path[length(path)]  # Species is at end
          
          # Create hierarchical structure based on actual path length
          if (length(ancestors) == 1) {
            # Direct parent only
            hierarchy_rows[[i]] <- data.frame(
              Level1 = ancestors[1],
              Species = species_name,
              stringsAsFactors = FALSE
            )
          } else if (length(ancestors) == 2) {
            # Two ancestor levels
            hierarchy_rows[[i]] <- data.frame(
              Level1 = ancestors[1],
              Level2 = ancestors[2],
              Species = species_name,
              stringsAsFactors = FALSE
            )
          } else if (length(ancestors) >= 3) {
            # Three or more ancestor levels - take root, middle, and direct parent
            middle_idx <- ceiling(length(ancestors) / 2)
            hierarchy_rows[[i]] <- data.frame(
              Level1 = ancestors[1],                    # Root
              Level2 = ancestors[middle_idx],           # Middle ancestor
              Level3 = ancestors[length(ancestors)],    # Direct parent
              Species = species_name,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
  
  # Combine all paths and standardize structure
  if (length(hierarchy_rows) > 0) {
    # Find maximum number of levels across all species
    max_cols <- max(sapply(hierarchy_rows, ncol))
    
    # Standardize all rows to have same column structure
    standardized_rows <- lapply(hierarchy_rows, function(row) {
      if (ncol(row) < max_cols) {
        # Add missing levels
        for (i in (ncol(row)):(max_cols-1)) {
          if (i == 1) row$Level1 <- if(!"Level1" %in% names(row)) "Life" else row$Level1
          if (i == 2) row$Level2 <- if(!"Level2" %in% names(row)) NA else row$Level2  
          if (i == 3) row$Level3 <- if(!"Level3" %in% names(row)) NA else row$Level3
        }
      }
      return(row)
    })
    
    hierarchy_df <- bind_rows(standardized_rows)
    
    # Clean up empty levels and ensure consistent column order
    if ("Level1" %in% names(hierarchy_df)) hierarchy_df$Level1[is.na(hierarchy_df$Level1)] <- "Life"
    
    return(hierarchy_df)
  } else {
    return(NULL)
  }
}

# Simplified function using direct phylo-to-network conversion
generate_tree_html_direct <- function(common_names) {
  tryCatch({
    # Get species from database
    species_data <- get_species_from_db(common_names)
    
    # Try to resolve outdated OTT IDs using TNRS (conservative approach)
    species_data <- resolve_outdated_otts(species_data)
    
    # Deduplicate species data by OTT ID to prevent duplicate entries in tree
    valid_species <- !is.na(species_data$ott)
    if (sum(valid_species) > 0) {
      # Keep first occurrence of each unique OTT ID among valid species
      unique_valid <- !duplicated(species_data$ott[valid_species])
      species_data_valid <- species_data[valid_species, ][unique_valid, ]
      # Keep all species with NA OTT IDs  
      species_data_na <- species_data[!valid_species, ]
      species_data <- rbind(species_data_valid, species_data_na)
    }
    
    # Get rotl tree
    valid_otts <- species_data$ott[!is.na(species_data$ott)]
    if (length(valid_otts) < 2) {
      stop("Need at least 2 valid species with OTT IDs")
    }
    
    # Get phylo tree from rotl
    phylo_tree <- tryCatch({
      tol_induced_subtree(ott_ids = valid_otts)
    }, error = function(e) {
      if (grepl("was not found", conditionMessage(e))) {
        cat("Some OTT IDs are invalid, attempting fallback TNRS resolution...\\n")
        
        # Try more aggressive TNRS resolution
        species_data <<- fallback_tnrs_resolution(species_data)
        valid_otts <- species_data$ott[!is.na(species_data$ott)]
        
        if (length(valid_otts) < 2) {
          stop("After TNRS resolution, still need at least 2 valid species")
        }
        
        tol_induced_subtree(ott_ids = valid_otts)
      } else {
        stop(conditionMessage(e))
      }
    })
    
    # Convert phylo tree directly to network format - NO hierarchical levels!
    network_data <- convert_phylo_to_network(phylo_tree, species_data)
    
    # Create collapsibleTree
    tree <- collapsibleTreeNetwork(
      network_data,
      attribute = "NodeType",
      fill = "Color", 
      width = 1200,
      height = 800,
      zoomable = TRUE,
      fontSize = 12,
      tooltip = TRUE,
      linkLength = 120,
      collapsed = TRUE,
      nodeSize = "leafCount"
    )
    
    # Save widget to HTML file
    temp_file <- tempfile(pattern = "tree_", fileext = ".html")
    saveWidget(tree, file = temp_file, selfcontained = TRUE)
    
    # Read HTML content
    html_content <- readChar(temp_file, file.info(temp_file)$size)
    unlink(temp_file)
    
    return(list(
      success = TRUE,
      html = html_content,
      species_count = length(common_names),
      network_edges = nrow(network_data)
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating tree:", conditionMessage(e)),
      html = NULL
    ))
  })
}

# Function to generate CollapsibleTree HTML string
generate_tree_html <- function(common_names) {
  tryCatch({
    hierarchy_data <- convert_rotl_to_hierarchy(common_names)
    
    if (is.null(hierarchy_data) || nrow(hierarchy_data) < 2) {
      return(list(
        success = FALSE,
        error = "Unable to generate hierarchy data or insufficient species",
html = NULL
      ))
    }
    
    # Create CollapsibleTree with dynamic hierarchy and color coding
    hierarchy_cols <- names(hierarchy_data)[names(hierarchy_data) != "Species"]
    hierarchy_cols <- c(hierarchy_cols, "Species")  # Species always last
    
    # Add a Color column to assign individual node colors
    hierarchy_data$Color <- "#27AE60"  # Default all species to green
    
    # Create a comprehensive color mapping for all unique nodes in the tree
    all_unique_nodes <- c("Common ancestor - click me!")  # Root
    
    # Collect all unique values from hierarchy levels
    for (col in hierarchy_cols[hierarchy_cols != "Species"]) {
      unique_vals <- unique(hierarchy_data[[col]][!is.na(hierarchy_data[[col]])])
      all_unique_nodes <- c(all_unique_nodes, unique_vals)
    }
    
    # Add all species
    all_unique_nodes <- c(all_unique_nodes, hierarchy_data$Species)
    
    # Create named color vector for all nodes
    node_colors <- sapply(all_unique_nodes, function(node) {
      if (node == "Common ancestor - click me!") {
        return("#E74C3C")  # Red for root
      } else if (grepl("^Ancestor [A-Z]$", node)) {
        return("#3498DB")  # Blue for ancestor nodes
      } else if (node %in% hierarchy_data$Species) {
        return("#27AE60")  # Green for species
      } else {
        return("#F39C12")  # Orange for taxonomic groups
      }
    })
    
    
    # Back to collapsibleTreeNetwork with better data handling
    # Convert hierarchy to parent-child relationships
    network_data <- data.frame(
      from = character(0),
      to = character(0),
      NodeType = character(0),
      stringsAsFactors = FALSE
    )
    
    # Add root
    root_name <- "Common ancestor - click me!"
    
    # Apply branch trimming to resolve conflicts before processing paths
    hierarchy_data <- trim_conflicting_branches(hierarchy_data)
    
    # Process each species path  
    valid_species <- c()  # Initialize tracking for valid species
    for (i in 1:nrow(hierarchy_data)) {
      path_levels <- c()
      
      # Build path from root through hierarchy levels to species
      for (col in hierarchy_cols) {
        if (col == "Species") {
          path_levels <- c(path_levels, hierarchy_data[[col]][i])
        } else if (!is.na(hierarchy_data[[col]][i])) {
          # Clean up long scientific names that cause issues
          level_name <- hierarchy_data[[col]][i]
          # Truncate very long names that might cause issues
          if (nchar(level_name) > 50) {
            level_name <- paste0(substr(level_name, 1, 47), "...")
          }
          path_levels <- c(path_levels, level_name)
        }
      }
      
      # Add root as first level if not already there
      if (length(path_levels) > 0 && path_levels[1] != root_name) {
        path_levels <- c(root_name, path_levels)
      }
      
      # Create parent-child relationships - skip if path is too short
      if (length(path_levels) < 2) next
      
      # Track if this species path is successfully added
      path_valid <- TRUE
      
      for (j in 1:(length(path_levels) - 1)) {
        parent <- path_levels[j]
        child <- path_levels[j + 1]
        
        # Skip if we already have this relationship
        if (any(network_data$from == parent & network_data$to == child)) next
        
        # Check if this child already has a different parent (prevents duplicate subtrees)
        existing_parents <- network_data$from[network_data$to == child]
        if (length(existing_parents) > 0 && !any(existing_parents == parent)) {
          # This child already exists under a different parent - invalidate entire path
          path_valid <- FALSE
          break  # Exit the inner loop, species will be excluded
        }
        
        # Determine child node type with robust ancestor detection
        if (child == root_name) {
          child_type <- "root"
        } else if (grepl("^Ancestor [A-Z]$", child)) {
          child_type <- "ancestor"  # Match new letter-based ancestor naming
        } else if (child %in% hierarchy_data$Species) {
          child_type <- "species"
        } else {
          child_type <- "taxonomic"
        }
        
        # Add to network data
        network_data <- rbind(network_data, data.frame(
          from = parent,
          to = child,
          NodeType = child_type,
          stringsAsFactors = FALSE
        ))
      }
      
      # Track valid species for final result
      if (path_valid) {
        valid_species <- c(valid_species, hierarchy_data$Species[i])
      }
    }
    
    # Remove any duplicate relationships that might have been created
    network_data <- network_data[!duplicated(network_data[c("from", "to")]), ]
    
    # Skip if we don't have enough data
    if (nrow(network_data) < 2) {
      return(list(
        success = FALSE,
        error = "Insufficient network data for tree generation",
html = NULL
      ))
    }
    
    # Add colors based on node type
    network_data$Color <- sapply(network_data$NodeType, function(type) {
      switch(type,
        "root" = "#E74C3C",      # Red
        "ancestor" = "#3498DB",   # Blue  
        "species" = "#27AE60",    # Green
        "taxonomic" = "#F39C12"   # Orange
      )
    })
    
    # Fix: Add required NA root row for collapsibleTreeNetwork
    # The function requires exactly one row with NA in 'from' column for the root
    root_row <- data.frame(
      from = NA,
      to = root_name,
      NodeType = "root",
      Color = "#E74C3C",
      stringsAsFactors = FALSE
    )
    
    # Prepend root row to network data
    network_data <- rbind(root_row, network_data)
    
    tree <- collapsibleTreeNetwork(
      network_data,
      attribute = "NodeType",
      fill = "Color",
      width = 1200,
      height = 800,
      zoomable = TRUE,
      fontSize = 12,
      tooltip = TRUE,
      linkLength = 120,
      collapsed = TRUE,
      nodeSize = "leafCount"
    )
    
    # Create temporary file to save HTML
    temp_file <- tempfile(pattern = "tree_", fileext = ".html")
    
    # Save widget to HTML file like correct_tree_walking.R does
    saveWidget(tree, file = temp_file, selfcontained = TRUE)
    
    # Read the HTML content from the file
    html_content <- readChar(temp_file, file.info(temp_file)$size)
    
    # Clean up temporary file
    unlink(temp_file)
    
    return(list(
      success = TRUE,
      html = html_content,
      species_count = length(common_names),
      hierarchy_levels = length(hierarchy_cols)
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating tree:", conditionMessage(e)),
      html = NULL
    ))
  })
}

# Function to get random species from database
get_random_species <- function(count = sample(3:7, 1)) {
  db_path <- "data/species.sqlite"
  species_db <- dbConnect(SQLite(), db_path)
  
  # Get random species that have both common names and OTT IDs
  query <- paste0(
    "SELECT common, scientific, ott FROM species ",
    "WHERE ott IS NOT NULL AND ott != '' AND common IS NOT NULL ",
    "ORDER BY RANDOM() LIMIT ", count
  )
  
  random_species <- dbGetQuery(species_db, query)
  dbDisconnect(species_db)
  
  return(random_species$common)
}

# Function to search species with optional search term and limit
search_species <- function(search_term = NULL, limit = 50) {
  db_path <- "data/species.sqlite"
  species_db <- dbConnect(SQLite(), db_path)
  
  if (!is.null(search_term) && search_term != "") {
    # Search for species matching the term (case insensitive)
    # Randomly select one scientific name per common name to avoid duplicates
    search_term <- gsub("'", "''", search_term)  # Escape single quotes for SQL safety
    query <- paste0(
      "SELECT common, scientific, ott FROM (",
      "SELECT common, scientific, ott, ",
      "ROW_NUMBER() OVER (PARTITION BY common ORDER BY RANDOM()) as rn ",
      "FROM species ",
      "WHERE common IS NOT NULL AND common != '' ",
      "AND LOWER(common) LIKE LOWER('%", search_term, "%') ",
      ") WHERE rn = 1 ",
      "ORDER BY LENGTH(common), common LIMIT ", limit
    )
  } else {
    # Return limited results without search
    # Randomly select one scientific name per common name to avoid duplicates
    query <- paste0(
      "SELECT common, scientific, ott FROM (",
      "SELECT common, scientific, ott, ",
      "ROW_NUMBER() OVER (PARTITION BY common ORDER BY RANDOM()) as rn ",
      "FROM species ",
      "WHERE common IS NOT NULL AND common != '' ",
      ") WHERE rn = 1 ",
      "ORDER BY LENGTH(common), common LIMIT ", limit
    )
  }
  
  species_data <- dbGetQuery(species_db, query)
  dbDisconnect(species_db)
  
  return(species_data)
}