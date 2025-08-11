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
    sql_string <- paste0("SELECT * FROM species WHERE common = '", common_name, "'")
    query_result <- dbSendQuery(species_db, sql_string)
    species_df <- dbFetch(query_result)
    dbClearResult(query_result)
    
    if (nrow(species_df) > 0) {
      return(species_df[1, ])
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
          readable_name <- "Ancestor"
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

# Main function to convert rotl tree to CollapsibleTree hierarchy
convert_rotl_to_hierarchy <- function(common_names) {
  # Get species from database
  species_data <- get_species_from_db(common_names)
  
  # Get rotl tree
  valid_otts <- species_data$ott[!is.na(species_data$ott)]
  if (length(valid_otts) < 2) {
    stop("Need at least 2 valid species with OTT IDs")
  }
  
  phylo_tree <- tol_induced_subtree(ott_ids = valid_otts)
  
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

# Function to generate CollapsibleTree HTML string
generate_tree_html <- function(common_names) {
  tryCatch({
    hierarchy_data <- convert_rotl_to_hierarchy(common_names)
    
    if (is.null(hierarchy_data) || nrow(hierarchy_data) < 2) {
      return(list(
        success = FALSE,
        error = "Unable to generate hierarchy data or insufficient species"
      ))
    }
    
    # Create CollapsibleTree with dynamic hierarchy and color coding
    hierarchy_cols <- names(hierarchy_data)[names(hierarchy_data) != "Species"]
    hierarchy_cols <- c(hierarchy_cols, "Species")  # Species always last
    
    # Calculate total unique nodes at each level for color vector
    all_nodes <- c("Common ancestor - click me!")  # Root
    
    for (col in hierarchy_cols) {
      if (col == "Species") {
        # Add all species
        all_nodes <- c(all_nodes, hierarchy_data$Species)
      } else {
        # Add unique values from this hierarchy level
        unique_vals <- unique(hierarchy_data[[col]][!is.na(hierarchy_data[[col]])])
        all_nodes <- c(all_nodes, unique_vals)
      }
    }
    
    # Create color vector based on node names
    colors_vector <- sapply(all_nodes, function(node) {
      if (node == "Common ancestor - click me!") {
        return("#E74C3C")  # Red for root
      } else if (node == "Ancestor") {
        return("#3498DB")  # Blue for ancestor nodes  
      } else if (node %in% hierarchy_data$Species) {
        return("#27AE60")  # Green for species
      } else {
        return("#F39C12")  # Orange for taxonomic groups
      }
    })
    
    tree <- collapsibleTree(
      hierarchy_data,
      hierarchy = hierarchy_cols,
      width = 1200,
      height = 800,
      zoomable = TRUE,
      fontSize = 12,
      tooltip = TRUE,
      linkLength = 120,
      collapsed = TRUE,   # Start collapsed for cleaner initial view
      fill = colors_vector,  # Use the color vector we created
      nodeSize = "leafCount",
      root = "Common ancestor - click me!"  # Custom root node name
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
      error = paste("Error generating tree:", conditionMessage(e))
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
    search_term <- gsub("'", "''", search_term)  # Escape single quotes for SQL safety
    query <- paste0(
      "SELECT DISTINCT common, scientific, ott FROM species ",
      "WHERE common IS NOT NULL AND common != '' ",
      "AND LOWER(common) LIKE LOWER('%", search_term, "%') ",
      "ORDER BY common LIMIT ", limit
    )
  } else {
    # Return limited results without search
    query <- paste0(
      "SELECT DISTINCT common, scientific, ott FROM species ",
      "WHERE common IS NOT NULL AND common != '' ",
      "ORDER BY common LIMIT ", limit
    )
  }
  
  species_data <- dbGetQuery(species_db, query)
  dbDisconnect(species_db)
  
  return(species_data)
}