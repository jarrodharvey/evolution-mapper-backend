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