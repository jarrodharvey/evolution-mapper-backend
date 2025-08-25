# DateLife Tree Generation Functions
# Functions for converting DateLife chronograms to CollapsibleTree HTML

library(datelife)
library(ape)
library(collapsibleTree)
library(RSQLite)
library(DBI)
library(dplyr)
library(colorspace)
source("functions/tree_html_enhancement.R")

source("functions/color_config.R")

#' Function to get common names from scientific names using database
#' @param scientific_names Vector of scientific names
#' @return Named vector with scientific names as keys and common names as values
get_common_names_from_scientific <- function(scientific_names) {
  db_path <- "data/species.sqlite"
  species_db <- dbConnect(SQLite(), db_path)
  
  common_names <- sapply(scientific_names, function(sci_name) {
    # Clean scientific name - remove underscores and extra whitespace
    clean_sci <- gsub("_", " ", sci_name)
    clean_sci <- trimws(clean_sci)
    
    # Query database for common name
    query <- sprintf("SELECT common FROM species WHERE scientific = '%s' LIMIT 1", clean_sci)
    result <- dbGetQuery(species_db, query)
    
    if (nrow(result) > 0 && !is.na(result$common[1])) {
      return(result$common[1])
    } else {
      # If no common name found, return cleaned scientific name
      return(clean_sci)
    }
  })
  
  dbDisconnect(species_db)
  names(common_names) <- scientific_names
  return(common_names)
}

#' Generate CollapsibleTree HTML from DateLife phylo object
#' @param phylo_tree A phylo object from DateLife
#' @param distance_matrix The distance matrix from DateLife
#' @param original_species The original species names requested
#' @return List with success status and HTML or error message
generate_dated_tree_html <- function(phylo_tree, distance_matrix, original_species) {
  tryCatch({
    
    # Convert phylo to network data structure preserving actual tree topology
    network_data <- convert_phylo_to_network_with_ages(phylo_tree, distance_matrix)
    
    if (is.null(network_data) || nrow(network_data) == 0) {
      return(list(
        success = FALSE,
        error = "Failed to convert phylo tree to network format"
      ))
    }
    
    # Create CollapsibleTree directly from network data
    tree_html <- create_collapsible_tree_from_network(network_data)
    
    return(list(
      success = TRUE,
      html = tree_html,
      species_count = length(original_species),
      tree_type = "dated_chronogram",
      data_source = "DateLife"
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating dated tree HTML:", conditionMessage(e))
    ))
  })
}

#' Convert phylo object to network format preserving actual tree structure
#' @param phylo_tree A phylo object from DateLife
#' @param distance_matrix Distance matrix with ages
#' @return Data frame with parent-child network structure and ages
convert_phylo_to_network_with_ages <- function(phylo_tree, distance_matrix) {
  
  # Get common names for species from database
  species_common_names <- get_common_names_from_scientific(phylo_tree$tip.label)
  
  # Get node ages from the phylo tree
  node_depths <- node.depth.edgelength(phylo_tree)
  root_age <- max(node_depths)
  node_ages <- root_age - node_depths
  
  n_tips <- length(phylo_tree$tip.label)
  
  # Create network data frame directly from phylo edges with info panel fields
  network_data <- data.frame(
    Parent = character(0),
    Child = character(0),
    Age = numeric(0),
    NodeType = character(0),
    AgeValid = logical(0),
    AgeSource = character(0),
    ValidationNotes = character(0),
    stringsAsFactors = FALSE
  )
  
  # Create consistent node label mapping
  node_labels_map <- list()
  ancestor_counter <- 1
  
  # Function to get consistent node label
  get_node_label <- function(node_num, age_val) {
    if (node_num <= n_tips) {
      # Species node - use common name
      return(species_common_names[phylo_tree$tip.label[node_num]])
    } else {
      # Internal node
      if (!exists(as.character(node_num), node_labels_map)) {
        # Create new ancestor label
        if (!is.null(phylo_tree$node.label)) {
          internal_index <- node_num - n_tips
          if (length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index])) {
            node_labels_map[[as.character(node_num)]] <<- paste0("Ancestor ", LETTERS[ancestor_counter], " (", round(age_val, 1), " Mya)")
          } else {
            node_labels_map[[as.character(node_num)]] <<- paste0("Ancestor ", LETTERS[ancestor_counter], " (", round(age_val, 1), " Mya)")
          }
        } else {
          node_labels_map[[as.character(node_num)]] <<- paste0("Ancestor ", LETTERS[ancestor_counter], " (", round(age_val, 1), " Mya)")
        }
        ancestor_counter <<- ancestor_counter + 1
      }
      return(node_labels_map[[as.character(node_num)]])
    }
  }
  
  # Function to get node type
  get_node_type <- function(node_num) {
    if (node_num <= n_tips) {
      return("species")
    } else {
      internal_index <- node_num - n_tips
      if (!is.null(phylo_tree$node.label) && length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index])) {
        node_label <- phylo_tree$node.label[internal_index]
        # Check if it's a generic node label (like "n1", "n2") vs a meaningful taxonomic name
        if (grepl("^n\\d+$", node_label)) {
          return("ancestor")  # Generic node labels should be treated as ancestors
        } else {
          return("taxonomic") # Meaningful taxonomic names
        }
      } else {
        return("ancestor")
      }
    }
  }
  
  # Process each edge in the phylo tree
  for (i in 1:nrow(phylo_tree$edge)) {
    parent_num <- phylo_tree$edge[i, 1]
    child_num <- phylo_tree$edge[i, 2]
    
    # Get consistent parent and child labels using mapping functions
    parent_age_val <- if (parent_num <= n_tips) 0 else node_ages[parent_num]
    child_age_val <- if (child_num <= n_tips) 0 else node_ages[child_num]
    
    parent_label <- get_node_label(parent_num, parent_age_val)
    child_label <- get_node_label(child_num, child_age_val)
    
    parent_type <- get_node_type(parent_num)
    child_type <- get_node_type(child_num)
    
    # Add edge to network with info panel fields
    network_data <- rbind(network_data, data.frame(
      Parent = parent_label,
      Child = child_label,
      Age = child_age_val,
      NodeType = child_type,
      AgeValid = !is.na(child_age_val) && child_age_val > 0,
      AgeSource = "DateLife chronogram database",
      ValidationNotes = if (!is.na(child_age_val) && child_age_val > 0) NA_character_ else "Age data not available from chronogram",
      stringsAsFactors = FALSE
    ))
  }
  
  # Add root node (parent = NA)
  # Find the root node (the parent that never appears as a child)
  all_parents <- unique(network_data$Parent)
  all_children <- unique(network_data$Child)
  root_nodes <- setdiff(all_parents, all_children)
  
  if (length(root_nodes) > 0) {
    root_label <- "Common ancestor - click me!"
    network_data <- rbind(data.frame(
      Parent = NA,
      Child = root_label,
      Age = root_age,
      NodeType = "root",
      AgeValid = !is.na(root_age) && root_age > 0,
      AgeSource = "DateLife chronogram database",
      ValidationNotes = if (!is.na(root_age) && root_age > 0) NA_character_ else "Root age data not available",
      stringsAsFactors = FALSE
    ), data.frame(
      Parent = root_label,
      Child = root_nodes[1],
      Age = root_age,
      NodeType = "ancestor",
      AgeValid = !is.na(root_age) && root_age > 0,
      AgeSource = "DateLife chronogram database",
      ValidationNotes = if (!is.na(root_age) && root_age > 0) NA_character_ else "Age data not available from chronogram",
      stringsAsFactors = FALSE
    ), network_data)
  }
  
  return(network_data)
}

#' Create CollapsibleTree HTML directly from network data with age-based colors
#' @param network_data Data frame with Parent, Child, Age, NodeType, AgeValid, AgeSource, ValidationNotes columns
#' @return HTML string for CollapsibleTree
create_collapsible_tree_from_network <- function(network_data) {
  
  # Create age-based color mapping using centralized configuration
  network_data$Color <- sapply(1:nrow(network_data), function(i) {
    node_type <- network_data$NodeType[i]
    node_age <- network_data$Age[i]
    
    if (node_type %in% c("root", "species")) {
      # Use base colors for root and species
      get_node_color(node_type)
    } else if (node_type %in% c("taxonomic", "ancestor")) {
      # Use gradient colors for taxonomic and ancestor nodes
      if (is.numeric(node_age) && node_age > 0) {
        # Get all non-zero ages for scaling
        all_ages <- network_data$Age[network_data$NodeType %in% c("taxonomic", "ancestor") & network_data$Age > 0]
        if (length(all_ages) > 1) {
          min_age <- min(all_ages, na.rm = TRUE)
          max_age <- max(all_ages, na.rm = TRUE)
          # Scale age to 0-1 range
          age_scale <- (node_age - min_age) / (max_age - min_age)
          # Use centralized gradient function
          get_gradient_color(node_type, age_scale)
        } else {
          # Fallback to base color
          get_node_color(node_type)
        }
      } else {
        # Fallback to base color for zero/invalid ages
        get_node_color(node_type)
      }
    } else {
      # Default fallback
      get_node_color("default")
    }
  })
  
  # Prepare data for collapsibleTreeNetwork (needs specific column names)
  tree_network_data <- data.frame(
    Parent = network_data$Parent,
    Child = network_data$Child,
    Color = network_data$Color,
    stringsAsFactors = FALSE
  )
  
  # Add info panel data to tree_network_data
  tree_network_data <- add_info_panel_data(tree_network_data, network_data)
  
  # Create the tree with color mapping (extra large dimensions to eliminate white space)
  tree <- collapsibleTreeNetwork(
    tree_network_data,
    fill = "Color",
    fontSize = 12,
    nodeSize = "leafCount",
    width = 1000,
    height = 800,
    zoomable = TRUE
  )
  
  # Convert to HTML and enhance with info panel system
  tree_html <- create_enhanced_tree_html(tree_network_data, network_data, tree)
  
  return(tree_html)
}

#' Generate CollapsibleTree HTML from DateLife phylo object with paired names
#' @param phylo_tree A phylo object from DateLife
#' @param distance_matrix The distance matrix from DateLife
#' @param common_names The original common names requested by user
#' @param scientific_names The original scientific names requested by user
#' @return List with success status and HTML or error message
generate_dated_tree_html_paired <- function(phylo_tree, distance_matrix, common_names, scientific_names) {
  tryCatch({
    
    # Convert phylo to network data structure preserving user-provided common names
    network_data <- convert_phylo_to_network_with_ages_paired(phylo_tree, distance_matrix, common_names, scientific_names)
    
    if (is.null(network_data) || nrow(network_data) == 0) {
      return(list(
        success = FALSE,
        error = "Failed to convert phylo tree to network format"
      ))
    }
    
    # Create CollapsibleTree directly from network data (same approach as DateLife)
    tree_html <- create_collapsible_tree_from_network(network_data)
    
    return(list(
      success = TRUE,
      html = tree_html,
      species_count = length(common_names),
      tree_type = "dated_chronogram",
      data_source = "DateLife",
      input_common_names = common_names,
      input_scientific_names = scientific_names
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating paired dated tree HTML:", conditionMessage(e))
    ))
  })
}

#' Convert phylo object to network format preserving user-provided common names
#' @param phylo_tree A phylo object from DateLife
#' @param distance_matrix Distance matrix with ages
#' @param common_names User-provided common names
#' @param scientific_names User-provided scientific names
#' @return Data frame with parent-child network structure and ages
convert_phylo_to_network_with_ages_paired <- function(phylo_tree, distance_matrix, common_names, scientific_names) {
  
  # Create mapping from scientific names in phylo tree to user-provided common names
  # DateLife scientific names may have underscores, user input may have spaces
  user_common_map <- list()
  for (i in seq_along(scientific_names)) {
    # Try both formats
    sci_underscore <- gsub(" ", "_", scientific_names[i])
    sci_space <- gsub("_", " ", scientific_names[i])
    user_common_map[[scientific_names[i]]] <- common_names[i]
    user_common_map[[sci_underscore]] <- common_names[i]
    user_common_map[[sci_space]] <- common_names[i]
  }
  
  # Function to get user-provided common name for a species
  get_user_common_name <- function(phylo_tip_label) {
    # Try exact match first
    if (phylo_tip_label %in% names(user_common_map)) {
      return(user_common_map[[phylo_tip_label]])
    }
    
    # Try with space/underscore conversion
    tip_space <- gsub("_", " ", phylo_tip_label)
    tip_underscore <- gsub(" ", "_", phylo_tip_label)
    
    if (tip_space %in% names(user_common_map)) {
      return(user_common_map[[tip_space]])
    }
    if (tip_underscore %in% names(user_common_map)) {
      return(user_common_map[[tip_underscore]])
    }
    
    # Fallback - return cleaned scientific name
    return(gsub("_", " ", phylo_tip_label))
  }
  
  # Get node ages from the phylo tree
  node_depths <- node.depth.edgelength(phylo_tree)
  root_age <- max(node_depths)
  node_ages <- root_age - node_depths
  
  n_tips <- length(phylo_tree$tip.label)
  
  # Create network data frame directly from phylo edges with info panel fields
  network_data <- data.frame(
    Parent = character(0),
    Child = character(0),
    Age = numeric(0),
    NodeType = character(0),
    AgeValid = logical(0),
    AgeSource = character(0),
    ValidationNotes = character(0),
    stringsAsFactors = FALSE
  )
  
  # Create consistent node label mapping
  node_labels_map <- list()
  ancestor_counter <- 1
  
  # Function to get consistent node label
  get_node_label <- function(node_num, age_val) {
    if (node_num <= n_tips) {
      # Species node - use user-provided common name
      return(get_user_common_name(phylo_tree$tip.label[node_num]))
    } else {
      # Internal node
      if (!exists(as.character(node_num), node_labels_map)) {
        # Create new ancestor label with age
        if (!is.null(phylo_tree$node.label)) {
          internal_index <- node_num - n_tips
          if (length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index])) {
            node_labels_map[[as.character(node_num)]] <<- paste0("Ancestor ", LETTERS[ancestor_counter], " (", round(age_val, 1), " Mya)")
          } else {
            node_labels_map[[as.character(node_num)]] <<- paste0("Ancestor ", LETTERS[ancestor_counter], " (", round(age_val, 1), " Mya)")
          }
        } else {
          node_labels_map[[as.character(node_num)]] <<- paste0("Ancestor ", LETTERS[ancestor_counter], " (", round(age_val, 1), " Mya)")
        }
        ancestor_counter <<- ancestor_counter + 1
      }
      return(node_labels_map[[as.character(node_num)]])
    }
  }
  
  # Function to get node type (same as original DateLife approach)
  get_node_type <- function(node_num) {
    if (node_num <= n_tips) {
      return("species")
    } else {
      internal_index <- node_num - n_tips
      if (!is.null(phylo_tree$node.label) && length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index])) {
        node_label <- phylo_tree$node.label[internal_index]
        # Check if it's a generic node label (like "n1", "n2") vs a meaningful taxonomic name
        if (grepl("^n\\d+$", node_label)) {
          return("ancestor")  # Generic node labels should be treated as ancestors
        } else {
          return("taxonomic") # Meaningful taxonomic names
        }
      } else {
        return("ancestor")
      }
    }
  }
  
  # Process each edge in the phylo tree (same logic as original DateLife)
  for (i in 1:nrow(phylo_tree$edge)) {
    parent_num <- phylo_tree$edge[i, 1]
    child_num <- phylo_tree$edge[i, 2]
    
    # Get consistent parent and child labels using mapping functions
    parent_age_val <- if (parent_num <= n_tips) 0 else node_ages[parent_num]
    child_age_val <- if (child_num <= n_tips) 0 else node_ages[child_num]
    
    parent_label <- get_node_label(parent_num, parent_age_val)
    child_label <- get_node_label(child_num, child_age_val)
    
    parent_type <- get_node_type(parent_num)
    child_type <- get_node_type(child_num)
    
    # Add edge to network with info panel fields
    network_data <- rbind(network_data, data.frame(
      Parent = parent_label,
      Child = child_label,
      Age = child_age_val,
      NodeType = child_type,
      AgeValid = !is.na(child_age_val) && child_age_val > 0,
      AgeSource = "DateLife chronogram database",
      ValidationNotes = if (!is.na(child_age_val) && child_age_val > 0) NA_character_ else "Age data not available from chronogram",
      stringsAsFactors = FALSE
    ))
  }
  
  # Add root node (parent = NA) - same logic as original DateLife
  # Find the root node (the parent that never appears as a child)
  all_parents <- unique(network_data$Parent)
  all_children <- unique(network_data$Child)
  root_nodes <- setdiff(all_parents, all_children)
  
  if (length(root_nodes) > 0) {
    root_label <- "Common ancestor - click me!"
    network_data <- rbind(data.frame(
      Parent = NA,
      Child = root_label,
      Age = root_age,
      NodeType = "root",
      AgeValid = !is.na(root_age) && root_age > 0,
      AgeSource = "DateLife chronogram database",
      ValidationNotes = if (!is.na(root_age) && root_age > 0) NA_character_ else "Root age data not available",
      stringsAsFactors = FALSE
    ), data.frame(
      Parent = root_label,
      Child = root_nodes[1],
      Age = root_age,
      NodeType = "ancestor",
      AgeValid = !is.na(root_age) && root_age > 0,
      AgeSource = "DateLife chronogram database",
      ValidationNotes = if (!is.na(root_age) && root_age > 0) NA_character_ else "Age data not available from chronogram",
      stringsAsFactors = FALSE
    ), network_data)
  }
  
  return(network_data)
}