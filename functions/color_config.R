# Centralized Color Configuration for Tree Visualization
# This file defines all colors used across different tree generation methods
# to ensure consistency between visualizations and the legend

# Base node colors
TREE_COLORS <- list(
  root = list(
    hex = "#8E44AD",
    name = "Deep Purple",
    description = "Common ancestor of all species in the tree"
  ),
  species = list(
    hex = "#27AE60", 
    name = "Green",
    description = "Individual species (leaf nodes)"
  ),
  ancestor = list(
    hex = "#3498DB",
    name = "Blue", 
    description = "Unnamed evolutionary ancestors (ages shown where available in Mya)"
  ),
  ancestor_no_age = list(
    hex = "#2C3E50",
    name = "Dark Blue",
    description = "Unnamed evolutionary ancestors without age data"
  ),
  taxonomic = list(
    hex = "#F39C12",
    name = "Orange",
    description = "Named taxonomic groups (families, orders, etc.) - ages shown where available in Mya"
  ),
  taxonomic_no_age = list(
    hex = "#D35400", 
    name = "Dark Orange",
    description = "Named taxonomic groups without age data"
  ),
  default = list(
    hex = "#999999",
    name = "Gray",
    description = "Default fallback color"
  ),
  # New ancestral node coloring system
  viridis_old = list(
    hex = "#440154",
    name = "Deep Blue",
    description = "Oldest ancestral nodes in age gradient"
  ),
  viridis_young = list(
    hex = "#fde725",
    name = "Pale Greenish Yellow",
    description = "Youngest ancestral nodes in age gradient"
  ),
  no_age_mixed = list(
    hex = "#999999",
    name = "Medium Grey",
    description = "Ancestral nodes without age data in mixed scenarios"
  )
)

#' Get color hex value for a node type
#' @param node_type The type of node (root, species, ancestor, taxonomic)
#' @param has_age Whether the node has age data (for hybrid trees)
#' @return Hex color string
get_node_color <- function(node_type, has_age = TRUE) {
  switch(node_type,
    "root" = TREE_COLORS$root$hex,
    "species" = TREE_COLORS$species$hex,
    "ancestor" = {
      if (has_age) TREE_COLORS$ancestor$hex else TREE_COLORS$ancestor_no_age$hex
    },
    "taxonomic" = {
      if (has_age) TREE_COLORS$taxonomic$hex else TREE_COLORS$taxonomic_no_age$hex
    },
    TREE_COLORS$default$hex
  )
}

#' Get age-based gradient color for DateLife trees
#' @param node_type The type of node (taxonomic or ancestor)
#' @param age_scale Scaled age value (0-1)
#' @return Hex color string
get_gradient_color <- function(node_type, age_scale) {
  switch(node_type,
    "taxonomic" = {
      # Create gradient from light orange to dark orange
      rgb(1.0 - (age_scale * 0.3), 0.6 - (age_scale * 0.4), 0.07 - (age_scale * 0.05))
    },
    "ancestor" = {
      # Create gradient from light blue to dark blue  
      rgb(0.2 - (age_scale * 0.1), 0.6 - (age_scale * 0.3), 0.9 - (age_scale * 0.3))
    },
    get_node_color(node_type)  # Fallback to base colors
  )
}

#' Get legend data for all node types
#' @return List of legend entries with color information
get_legend_data <- function() {
  list(
    list(
      node_type = "root",
      label = "Root Ancestor",
      color = TREE_COLORS$root$hex,
      color_name = TREE_COLORS$root$name,
      description = TREE_COLORS$root$description
    ),
    list(
      node_type = "ancestor",
      label = "Evolutionary Ancestor",
      color = TREE_COLORS$ancestor$hex,
      color_name = TREE_COLORS$ancestor$name,
      description = TREE_COLORS$ancestor$description
    ),
    list(
      node_type = "taxonomic",
      label = "Taxonomic Group",
      color = TREE_COLORS$taxonomic$hex,
      color_name = TREE_COLORS$taxonomic$name,
      description = TREE_COLORS$taxonomic$description
    ),
    list(
      node_type = "species",
      label = "Species",
      color = TREE_COLORS$species$hex,
      color_name = TREE_COLORS$species$name,
      description = TREE_COLORS$species$description
    )
  )
}

#' Get ancestral node colors using new adaptive coloring system
#' @param network_data Data frame with NodeType, HasAge, and AgeInfo columns
#' @return Vector of hex color strings for ancestral nodes
get_ancestral_node_color <- function(network_data) {
  # Filter to ancestral nodes only (taxonomic + ancestor, exclude root/species)
  ancestral_nodes <- network_data[network_data$NodeType %in% c("taxonomic", "ancestor"), ]

  if (nrow(ancestral_nodes) == 0) {
    return(character(0))
  }

  # Count nodes with age data
  nodes_with_age <- sum(ancestral_nodes$HasAge, na.rm = TRUE)
  total_ancestral_nodes <- nrow(ancestral_nodes)

  if (nodes_with_age == 0) {
    # Scenario 1: No age data available - all ancestral nodes get orange
    return(rep(TREE_COLORS$taxonomic$hex, total_ancestral_nodes))

  } else if (nodes_with_age == total_ancestral_nodes) {
    # Scenario 2: All ancestral nodes have age data - use Viridis gradient
    return(get_viridis_gradient_colors(ancestral_nodes))

  } else {
    # Scenario 3: Mixed age data - gradient for aged nodes, grey for non-aged
    colors <- character(total_ancestral_nodes)

    # Get aged nodes for gradient calculation
    aged_nodes <- ancestral_nodes[ancestral_nodes$HasAge, ]
    aged_colors <- get_viridis_gradient_colors(aged_nodes)

    # Assign colors
    aged_idx <- 1
    for (i in 1:total_ancestral_nodes) {
      if (ancestral_nodes$HasAge[i]) {
        colors[i] <- aged_colors[aged_idx]
        aged_idx <- aged_idx + 1
      } else {
        colors[i] <- TREE_COLORS$no_age_mixed$hex
      }
    }
    return(colors)
  }
}

#' Generate Viridis-inspired gradient colors based on age data
#' @param nodes_with_age Data frame of nodes that have age data
#' @return Vector of hex color strings
get_viridis_gradient_colors <- function(nodes_with_age) {
  if (nrow(nodes_with_age) == 0) {
    return(character(0))
  }

  if (nrow(nodes_with_age) == 1) {
    # Single node - use middle of gradient
    return("#21908c")  # Viridis middle point
  }

  # Extract age values from AgeInfo strings
  ages <- numeric(nrow(nodes_with_age))
  for (i in seq_len(nrow(nodes_with_age))) {
    age_info <- nodes_with_age$AgeInfo[i]

    # Extract numeric age from strings like "42.5 Mya" or "42.5"
    age_match <- regexpr("\\d+\\.?\\d*", age_info)
    if (age_match > 0) {
      ages[i] <- as.numeric(regmatches(age_info, age_match))
    } else {
      ages[i] <- 0  # Fallback for unparseable ages
    }
  }

  # Create gradient from deep blue (oldest) to pale greenish yellow (youngest)
  if (length(unique(ages)) == 1) {
    # All same age - use middle color
    return(rep("#21908c", nrow(nodes_with_age)))
  }

  # Normalize ages to 0-1 scale (inverse: older = higher value for darker color)
  min_age <- min(ages, na.rm = TRUE)
  max_age <- max(ages, na.rm = TRUE)
  age_scales <- (ages - min_age) / (max_age - min_age)
  age_scales <- 1 - age_scales  # Invert so older = 1, younger = 0

  # Generate colors using linear interpolation
  colors <- character(length(age_scales))
  old_color <- col2rgb(TREE_COLORS$viridis_old$hex) / 255
  young_color <- col2rgb(TREE_COLORS$viridis_young$hex) / 255

  for (i in seq_along(age_scales)) {
    scale <- age_scales[i]
    r <- old_color[1] * scale + young_color[1] * (1 - scale)
    g <- old_color[2] * scale + young_color[2] * (1 - scale)
    b <- old_color[3] * scale + young_color[3] * (1 - scale)
    colors[i] <- rgb(r, g, b)
  }

  colors
}