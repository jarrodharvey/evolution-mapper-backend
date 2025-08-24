# ROTL Tree generation functions - topology only (no ages)
# Handles phylogenetic tree creation using Open Tree of Life
# This version provides tree topology only, without ancestor ages

library(rotl)
library(ape)
library(collapsibleTree)
library(htmlwidgets)
library(RSQLite)
library(DBI)
library(dplyr)

source("functions/color_config.R")

# Calculate dynamic link length based on label lengths
calculate_dynamic_link_length <- function(network_data, base_length = 80, char_multiplier = 3) {
  # Get all node names
  all_names <- c(network_data$from, network_data$to)
  
  # Find the longest label
  max_chars <- max(nchar(all_names), na.rm = TRUE)
  
  # Calculate dynamic length: base + (characters * multiplier)
  dynamic_length <- base_length + (max_chars * char_multiplier)
  
  # Set reasonable bounds (minimum 120, maximum 250)
  dynamic_length <- max(120, min(250, dynamic_length))
  
  return(dynamic_length)
}

# Function to get species from database using paired common and scientific names
get_species_from_db_paired <- function(common_names, scientific_names) {
  db_path <- "data/species.sqlite"
  species_db <- dbConnect(SQLite(), db_path)
  
  species_data <- data.frame(
    ott = integer(length(common_names)),
    common = character(length(common_names)),
    scientific = character(length(common_names)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(common_names)) {
    common_name <- common_names[i]
    scientific_name <- scientific_names[i]
    
    # Try to find exact match with both common and scientific names
    sql_string <- sprintf("SELECT * FROM species WHERE common = '%s' AND scientific = '%s'", 
                         gsub("'", "''", common_name), gsub("'", "''", scientific_name))
    query_result <- dbSendQuery(species_db, sql_string)
    species_df <- dbFetch(query_result)
    dbClearResult(query_result)
    
    if (nrow(species_df) > 0) {
      # Found exact match - use it
      species_data[i, ] <- species_df[1, ]
    } else {
      # No exact match - try scientific name only (more reliable for OTT lookup)
      sql_string <- sprintf("SELECT * FROM species WHERE scientific = '%s'", 
                           gsub("'", "''", scientific_name))
      query_result <- dbSendQuery(species_db, sql_string)
      species_df <- dbFetch(query_result)
      dbClearResult(query_result)
      
      if (nrow(species_df) > 0) {
        # Found by scientific name - use provided common name but database OTT and scientific
        species_data[i, ] <- data.frame(
          ott = species_df$ott[1],
          common = common_name,  # Use user-provided common name
          scientific = species_df$scientific[1]
        )
      } else {
        # No match found - create placeholder
        species_data[i, ] <- data.frame(
          ott = NA,
          common = common_name,
          scientific = scientific_name
        )
      }
    }
  }
  
  dbDisconnect(species_db)
  return(species_data)
}

# Legacy function for backward compatibility (deprecated)
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
    # Clean up scientific names - remove OTT IDs, content in parentheses and brackets, capitalize first letter, replace underscores
    clean_name <- name
    
    # Handle Mrcaott patterns - these should never reach this function but just in case
    if (grepl("^[Mm]rcaott\\d+ott\\d+", clean_name)) {
      return(paste0("Internal node (", substr(clean_name, 1, 15), "...)"))
    } 
    
    # Handle labels that are ONLY an OTT ID
    if (grepl("^ott\\d+$", clean_name)) {
      return(paste0("Taxonomic group (", clean_name, ")"))
    } 
    
    # Clean up normal taxonomic names by removing OTT IDs
    clean_name <- gsub("\\s*ott\\d+", "", clean_name)
    clean_name <- gsub("\\s*\\([^)]*\\)", "", clean_name)  # Remove content in parentheses
    clean_name <- gsub("\\s*\\[[^]]*\\]", "", clean_name)  # Remove content in brackets
    clean_name <- gsub("_", " ", clean_name)
    clean_name <- gsub("^([a-z])", "\\U\\1", clean_name, perl = TRUE)
    clean_name <- trimws(clean_name)  # Remove any extra whitespace
    
    # If cleaning removed everything, return a fallback with original name for uniqueness
    if (clean_name == "" || nchar(clean_name) == 0) {
      return(paste0("Taxonomic group (", substr(name, 1, 10), "...)"))
    }
    
    return(clean_name)
  }
}

# Function to trace path from tip to root (simplified - no age tracking)
trace_path_to_root <- function(tip_number, edges, node_labels, tip_labels, species_common_name) {
  path <- c()
  current_node <- tip_number
  n_tips <- length(tip_labels)
  
  # Start with the species (tip) - use provided common name
  path <- c(species_common_name)
  
  # Traverse up the tree until we reach the root
  while (TRUE) {
    # Find the edge where this node is the child (second column)
    edge_index <- which(edges[, 2] == current_node)
    
    if (length(edge_index) == 0) {
      # We've reached the root
      break
    }
    
    # Get the parent node
    parent_node <- edges[edge_index, 1]
    
    # Convert node label
    if (parent_node <= n_tips) {
      # This shouldn't happen in a proper tree, but handle it
      parent_label <- tip_labels[parent_node]
    } else {
      # Internal node
      internal_index <- parent_node - n_tips
      if (!is.null(node_labels) && length(node_labels) >= internal_index && !is.na(node_labels[internal_index])) {
        parent_label <- convert_to_readable_name(node_labels[internal_index])
      } else {
        # Generate a generic ancestor label
        ancestor_count <- sum(grepl("^Ancestor", path)) + 1
        parent_label <- paste("Ancestor", LETTERS[ancestor_count])
      }
    }
    
    # Add to path
    path <- c(parent_label, path)
    
    # Move to parent for next iteration
    current_node <- parent_node
  }
  
  return(path)
}

# Main function to generate tree HTML from phylo object (topology only)
convert_phylo_to_network <- function(phylo_tree, species_data) {
  # Basic tree information
  n_tips <- length(phylo_tree$tip.label)
  n_nodes <- phylo_tree$Nnode
  cat(sprintf("Processing tree with %d tips and %d internal nodes\n", n_tips, n_nodes))
  
  # Build network data frame directly from edge matrix
  network_data <- data.frame(
    from = character(0),
    to = character(0),
    NodeType = character(0),
    stringsAsFactors = FALSE
  )
  
  # Convert each edge to parent-child relationship
  for (i in 1:nrow(phylo_tree$edge)) {
    parent_num <- phylo_tree$edge[i, 1]
    child_num <- phylo_tree$edge[i, 2]
    
    # Determine parent label
    if (parent_num <= n_tips) {
      # Parent is a tip (shouldn't happen in proper trees)
      parent_label <- species_data$common[parent_num]
    } else {
      # Parent is internal node
      internal_index <- parent_num - n_tips
      if (!is.null(phylo_tree$node.label) && length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index]) && nchar(trimws(phylo_tree$node.label[internal_index])) > 0 && !grepl("^[Mm]rcaott\\d+ott\\d+", phylo_tree$node.label[internal_index])) {
        parent_label <- convert_to_readable_name(phylo_tree$node.label[internal_index])
      } else {
        parent_label <- paste("Ancestor", LETTERS[min(internal_index, 26)])
      }
    }
    
    # Determine child label and type
    if (child_num <= n_tips) {
      # Child is a tip (species)
      child_label <- species_data$common[child_num]
      child_type <- "species"
    } else {
      # Child is internal node
      internal_index <- child_num - n_tips
      if (!is.null(phylo_tree$node.label) && length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index]) && nchar(trimws(phylo_tree$node.label[internal_index])) > 0 && !grepl("^[Mm]rcaott\\d+ott\\d+", phylo_tree$node.label[internal_index])) {
        child_label <- convert_to_readable_name(phylo_tree$node.label[internal_index])
        child_type <- "taxonomic"
      } else {
        child_label <- paste("Ancestor", LETTERS[min(internal_index, 26)])
        child_type <- "ancestor"
      }
    }
    
    # Add edge to network
    network_data <- rbind(network_data, data.frame(
      from = parent_label,
      to = child_label,
      NodeType = child_type,
      stringsAsFactors = FALSE
    ))
  }
  
  # Add proper root handling for collapsibleTreeNetwork
  if (nrow(network_data) > 0) {
    # Find the phylogenetic root from the tree structure
    # The root is the internal node with the highest number (n_tips + n_nodes)
    root_node_num <- n_tips + phylo_tree$Nnode
    
    # Find what label this root node has in our network
    root_label <- NULL
    for (i in 1:nrow(phylo_tree$edge)) {
      parent_num <- phylo_tree$edge[i, 1]
      if (parent_num == root_node_num) {
        # This is an edge from the root
        internal_index <- parent_num - n_tips
        if (!is.null(phylo_tree$node.label) && length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index]) && nchar(trimws(phylo_tree$node.label[internal_index])) > 0) {
          root_label <- convert_to_readable_name(phylo_tree$node.label[internal_index])
        } else {
          root_label <- paste("Ancestor", LETTERS[internal_index])
        }
        break
      }
    }
    
    # Find orphaned parents (parents that are never children) from network data
    all_parents <- unique(network_data$from)
    all_children <- unique(network_data$to)
    orphaned_parents <- setdiff(all_parents, all_children)
    
    # Connect all orphaned parents to a single root
    root_name <- "Common ancestor - click me!"
    
    if (length(orphaned_parents) > 0) {
      # For each orphaned parent, connect it to the root with correct node type
      for (orphaned_parent in orphaned_parents) {
        # Determine node type based on the name
        if (grepl("^Ancestor [A-Z]$", orphaned_parent)) {
          node_type <- "ancestor"
        } else {
          node_type <- "taxonomic"
        }
        
        network_data <- rbind(data.frame(
          from = root_name,
          to = orphaned_parent,
          NodeType = node_type,
          stringsAsFactors = FALSE
        ), network_data)
      }
    }
    
    # Add root row with NA parent (required by collapsibleTreeNetwork)
    network_data <- rbind(data.frame(
      from = NA,
      to = root_name,
      NodeType = "root",
      stringsAsFactors = FALSE
    ), network_data)
  }
  
  return(network_data)
}

# Generate tree HTML from species list (topology only)
generate_tree_html <- function(species_names) {
  tryCatch({
    cat("=== GENERATING TOPOLOGY-ONLY TREE ===\n")
    
    # Get species data from database
    species_data <- get_species_from_db(species_names)
    
    # Check for missing OTT IDs
    valid_species <- species_data[!is.na(species_data$ott), ]
    
    if (nrow(valid_species) < 2) {
      missing_species <- species_data$common[is.na(species_data$ott)]
      return(list(
        success = FALSE,
        error = "Insufficient species with valid OTT IDs for tree generation",
        missing_species = missing_species,
        note = "At least 2 species with valid OTT IDs are required"
      ))
    }
    
    # Get tree from Open Tree of Life
    cat("Fetching tree from Open Tree of Life...\n")
    tree <- tol_induced_subtree(ott_ids = valid_species$ott)
    
    if (is.null(tree)) {
      return(list(
        success = FALSE,
        error = "Failed to get tree from Open Tree of Life"
      ))
    }
    
    # Convert phylo tree to network format
    network_data <- convert_phylo_to_network(tree, valid_species)
    
    if (is.null(network_data) || nrow(network_data) == 0) {
      return(list(
        success = FALSE,
        error = "Failed to convert tree to network format"
      ))
    }
    
    # Calculate dynamic link length
    link_length <- calculate_dynamic_link_length(network_data)
    
    # Prepare data frame for collapsibleTreeNetwork (Parent, Child format)
    tree_data <- data.frame(
      Parent = network_data$from,
      Child = network_data$to,
      NodeType = network_data$NodeType,
      stringsAsFactors = FALSE
    )
    
    # Add color mapping using centralized configuration
    tree_data$Color <- sapply(tree_data$NodeType, function(type) {
      get_node_color(type)
    })
    
    # Create collapsibleTree with proper color mapping
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
    
    # Convert to HTML using temporary file approach
    temp_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(tree_widget, temp_file, selfcontained = TRUE)
    tree_html <- paste(readLines(temp_file), collapse = "\n")
    unlink(temp_file)  # Clean up temp file
    
    # Remove white bar by adding comprehensive CSS to override all spacing
    custom_css <- "<style>body { margin: 0 !important; padding: 0 !important; overflow: hidden !important; } html { margin: 0 !important; padding: 0 !important; } #htmlwidget_container { margin: 0 !important; padding: 0 !important; }</style>"
    tree_html <- gsub("</head>", paste0(custom_css, "</head>"), tree_html)
    
    # Replace collapsibleTree tooltip with custom cross-browser implementation using event delegation
    firefox_fix_script <- '<script>
    // Custom tooltip implementation that works across browsers with event delegation
    setTimeout(function() {
      // Remove existing tooltips
      d3.selectAll(".tooltip").remove();
      
      // Create our own tooltip div
      var customTooltip = d3.select("body")
        .append("div")
        .attr("class", "custom-tooltip")
        .style("position", "absolute")
        .style("padding", "8px")
        .style("background", "rgba(0, 0, 0, 0.8)")
        .style("color", "white")
        .style("border-radius", "4px")
        .style("font-size", "14px")
        .style("pointer-events", "none")
        .style("opacity", 0)
        .style("z-index", 1000);
      
      // Use event delegation on the SVG container to handle dynamically created nodes
      d3.select(".collapsibleTree")
        .on("mouseover", function() {
          var target = d3.event.target;
          // Check if we are hovering over a node or its children
          var nodeElement = target.closest(".node") || (target.tagName === "g" && target.classList.contains("node") ? target : null);
          
          if (nodeElement) {
            // Get the D3 data bound to this node
            var nodeData = d3.select(nodeElement).datum();
            if (nodeData && nodeData.data) {
              var rect = nodeElement.getBoundingClientRect();
              var scrollLeft = window.pageXOffset || document.documentElement.scrollLeft;
              var scrollTop = window.pageYOffset || document.documentElement.scrollTop;
              
              customTooltip
                .style("opacity", 0.9)
                .html(nodeData.data.tooltip || nodeData.data.name)
                .style("left", (rect.right + scrollLeft + 10) + "px")
                .style("top", (rect.top + scrollTop) + "px");
            }
          }
        })
        .on("mouseout", function() {
          var target = d3.event.target;
          var nodeElement = target.closest(".node") || (target.tagName === "g" && target.classList.contains("node") ? target : null);
          
          if (nodeElement) {
            customTooltip.style("opacity", 0);
          }
        });
      
      // Also disable the original tooltip functionality
      d3.selectAll(".collapsibleTree .node").on("mouseover.tooltip", null).on("mouseout.tooltip", null);
    }, 1000);
    </script>'
    tree_html <- gsub("</body>", paste0(firefox_fix_script, "</body>"), tree_html)
    
    return(list(
      success = TRUE,
      html = tree_html,
      species_count = nrow(valid_species),
      tree_type = "topology_only",
      data_source = "Open Tree of Life"
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating tree:", conditionMessage(e))
    ))
  })
}

# Generate tree HTML from paired common and scientific names (ROTL topology only)
generate_tree_html_paired <- function(common_names, scientific_names) {
  tryCatch({
    cat("=== GENERATING TOPOLOGY-ONLY TREE WITH PAIRED NAMES ===\n")
    
    # Get species data from database using paired approach
    species_data <- get_species_from_db_paired(common_names, scientific_names)
    
    # Check for missing OTT IDs
    valid_species <- species_data[!is.na(species_data$ott), ]
    
    if (nrow(valid_species) < 2) {
      missing_indices <- which(is.na(species_data$ott))
      missing_common <- species_data$common[missing_indices]
      missing_scientific <- species_data$scientific[missing_indices]
      return(list(
        success = FALSE,
        error = "Insufficient species with valid OTT IDs for tree generation",
        missing_common_names = missing_common,
        missing_scientific_names = missing_scientific,
        note = "At least 2 species with valid OTT IDs are required"
      ))
    }
    
    # Get tree from Open Tree of Life using OTT IDs
    cat("Fetching tree from Open Tree of Life...\n")
    tree <- tol_induced_subtree(ott_ids = valid_species$ott)
    
    if (is.null(tree)) {
      return(list(
        success = FALSE,
        error = "Failed to get tree from Open Tree of Life"
      ))
    }
    
    # Convert phylo tree to network format - preserve user-provided common names
    network_data <- convert_phylo_to_network_paired(tree, valid_species)
    
    if (is.null(network_data) || nrow(network_data) == 0) {
      return(list(
        success = FALSE,
        error = "Failed to convert tree to network format"
      ))
    }
    
    # Calculate dynamic link length
    link_length <- calculate_dynamic_link_length(network_data)
    
    # Prepare data frame for collapsibleTreeNetwork (Parent, Child format)
    tree_data <- data.frame(
      Parent = network_data$from,
      Child = network_data$to,
      NodeType = network_data$NodeType,
      stringsAsFactors = FALSE
    )
    
    # Add color mapping based on node type (same as ROTL approach)
    tree_data$Color <- sapply(tree_data$NodeType, function(type) {
      switch(type,
        "root" = "#8E44AD",        # Deep purple for root
        "ancestor" = "#3498DB",     # Blue for ancestors  
        "taxonomic" = "#F39C12",    # Orange for taxonomic groups
        "species" = "#27AE60",      # Green for species
        "#999999"                   # Default gray
      )
    })
    
    # Create collapsibleTree with proper color mapping
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
    
    # Convert to HTML using temporary file approach
    temp_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(tree_widget, temp_file, selfcontained = TRUE)
    tree_html <- paste(readLines(temp_file), collapse = "\n")
    unlink(temp_file)  # Clean up temp file
    
    # Remove white bar by adding comprehensive CSS to override all spacing
    custom_css <- "<style>body { margin: 0 !important; padding: 0 !important; overflow: hidden !important; } html { margin: 0 !important; padding: 0 !important; } #htmlwidget_container { margin: 0 !important; padding: 0 !important; }</style>"
    tree_html <- gsub("</head>", paste0(custom_css, "</head>"), tree_html)
    
    # Replace collapsibleTree tooltip with custom cross-browser implementation using event delegation
    firefox_fix_script <- '<script>
    // Custom tooltip implementation that works across browsers with event delegation
    setTimeout(function() {
      // Remove existing tooltips
      d3.selectAll(".tooltip").remove();
      
      // Create our own tooltip div
      var customTooltip = d3.select("body")
        .append("div")
        .attr("class", "custom-tooltip")
        .style("position", "absolute")
        .style("padding", "8px")
        .style("background", "rgba(0, 0, 0, 0.8)")
        .style("color", "white")
        .style("border-radius", "4px")
        .style("font-size", "14px")
        .style("pointer-events", "none")
        .style("opacity", 0)
        .style("z-index", 1000);
      
      // Use event delegation on the SVG container to handle dynamically created nodes
      d3.select(".collapsibleTree")
        .on("mouseover", function() {
          var target = d3.event.target;
          // Check if we are hovering over a node or its children
          var nodeElement = target.closest(".node") || (target.tagName === "g" && target.classList.contains("node") ? target : null);
          
          if (nodeElement) {
            // Get the D3 data bound to this node
            var nodeData = d3.select(nodeElement).datum();
            if (nodeData && nodeData.data) {
              var rect = nodeElement.getBoundingClientRect();
              var scrollLeft = window.pageXOffset || document.documentElement.scrollLeft;
              var scrollTop = window.pageYOffset || document.documentElement.scrollTop;
              
              customTooltip
                .style("opacity", 0.9)
                .html(nodeData.data.tooltip || nodeData.data.name)
                .style("left", (rect.right + scrollLeft + 10) + "px")
                .style("top", (rect.top + scrollTop) + "px");
            }
          }
        })
        .on("mouseout", function() {
          var target = d3.event.target;
          var nodeElement = target.closest(".node") || (target.tagName === "g" && target.classList.contains("node") ? target : null);
          
          if (nodeElement) {
            customTooltip.style("opacity", 0);
          }
        });
      
      // Also disable the original tooltip functionality
      d3.selectAll(".collapsibleTree .node").on("mouseover.tooltip", null).on("mouseout.tooltip", null);
    }, 1000);
    </script>'
    tree_html <- gsub("</body>", paste0(firefox_fix_script, "</body>"), tree_html)
    
    return(list(
      success = TRUE,
      html = tree_html,
      species_count = nrow(valid_species),
      tree_type = "topology_only",
      data_source = "Open Tree of Life",
      input_common_names = common_names,
      input_scientific_names = scientific_names
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating paired tree:", conditionMessage(e))
    ))
  })
}

# Convert phylo tree to network format preserving user-provided common names
convert_phylo_to_network_paired <- function(phylo_tree, species_data) {
  # Basic tree information
  n_tips <- length(phylo_tree$tip.label)
  n_nodes <- phylo_tree$Nnode
  cat(sprintf("Processing paired tree with %d tips and %d internal nodes\n", n_tips, n_nodes))
  
  # Build network data frame directly from edge matrix
  network_data <- data.frame(
    from = character(0),
    to = character(0),
    NodeType = character(0),
    stringsAsFactors = FALSE
  )
  
  # Convert each edge to parent-child relationship
  for (i in 1:nrow(phylo_tree$edge)) {
    parent_num <- phylo_tree$edge[i, 1]
    child_num <- phylo_tree$edge[i, 2]
    
    # Determine parent label
    if (parent_num <= n_tips) {
      # Parent is a tip (shouldn't happen in proper trees)
      tip_label <- phylo_tree$tip.label[parent_num]
      tip_clean <- gsub("_ott\\d+", "", tip_label)
      tip_clean <- gsub("_", " ", tip_clean)
      match_idx <- which(species_data$scientific == tip_clean)
      if (length(match_idx) > 0) {
        parent_label <- species_data$common[match_idx[1]]
      } else {
        parent_label <- tip_clean
      }
    } else {
      # Parent is internal node
      internal_index <- parent_num - n_tips
      if (!is.null(phylo_tree$node.label) && length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index]) && nchar(trimws(phylo_tree$node.label[internal_index])) > 0 && !grepl("^[Mm]rcaott\\d+ott\\d+", phylo_tree$node.label[internal_index])) {
        parent_label <- convert_to_readable_name(phylo_tree$node.label[internal_index])
      } else {
        parent_label <- paste("Ancestor", LETTERS[min(internal_index, 26)])
      }
    }
    
    # Determine child label and type
    if (child_num <= n_tips) {
      # Child is a tip (species) - find correct mapping by scientific name
      tip_label <- phylo_tree$tip.label[child_num]
      tip_clean <- gsub("_ott\\d+", "", tip_label)
      tip_clean <- gsub("_", " ", tip_clean)
      
      # Find matching species in species_data by scientific name
      match_idx <- which(species_data$scientific == tip_clean)
      if (length(match_idx) > 0) {
        child_label <- species_data$common[match_idx[1]]
      } else {
        child_label <- tip_clean  # fallback
      }
      child_type <- "species"
    } else {
      # Child is internal node
      internal_index <- child_num - n_tips
      if (!is.null(phylo_tree$node.label) && length(phylo_tree$node.label) >= internal_index && !is.na(phylo_tree$node.label[internal_index]) && nchar(trimws(phylo_tree$node.label[internal_index])) > 0 && !grepl("^[Mm]rcaott\\d+ott\\d+", phylo_tree$node.label[internal_index])) {
        child_label <- convert_to_readable_name(phylo_tree$node.label[internal_index])
        child_type <- "taxonomic"
      } else {
        child_label <- paste("Ancestor", LETTERS[min(internal_index, 26)])
        child_type <- "ancestor"
      }
    }
    
    # Add edge to network
    network_data <- rbind(network_data, data.frame(
      from = parent_label,
      to = child_label,
      NodeType = child_type,
      stringsAsFactors = FALSE
    ))
  }
  
  # Add proper root handling for collapsibleTreeNetwork (same logic as original)
  if (nrow(network_data) > 0) {
    # Find orphaned parents (parents that are never children) from network data
    all_parents <- unique(network_data$from)
    all_children <- unique(network_data$to)
    orphaned_parents <- setdiff(all_parents, all_children)
    
    # Connect all orphaned parents to a single root
    root_name <- "Common ancestor - click me!"
    
    if (length(orphaned_parents) > 0) {
      # For each orphaned parent, connect it to the root with correct node type
      for (orphaned_parent in orphaned_parents) {
        # Determine node type based on the name
        if (grepl("^Ancestor [A-Z]$", orphaned_parent)) {
          node_type <- "ancestor"
        } else {
          node_type <- "taxonomic"
        }
        
        network_data <- rbind(data.frame(
          from = root_name,
          to = orphaned_parent,
          NodeType = node_type,
          stringsAsFactors = FALSE
        ), network_data)
      }
    }
    
    # Add root row with NA parent (required by collapsibleTreeNetwork)
    network_data <- rbind(data.frame(
      from = NA,
      to = root_name,
      NodeType = "root",
      stringsAsFactors = FALSE
    ), network_data)
  }
  
  return(network_data)
}