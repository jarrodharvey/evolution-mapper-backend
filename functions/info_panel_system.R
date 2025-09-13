# Info panel system for phylogenetic trees
# Replaces tooltips with clickable info icons and expandable panels
# Mobile-friendly alternative to hover-based tooltips

# Source Wikipedia API functions
source("functions/wikipedia_api.R")

# Source Wikipedia image API functions
source("functions/wikipedia_images.R")

# Source Unsplash image API functions
source("functions/unsplash_images.R")

# Source PhyloPic silhouette functions
source("functions/phylopic_silhouettes.R")

# Source cached API functions for improved performance
source("functions/cached_api_functions.R")

# Load parallel processing library
library(parallel)

# Source shared logging configuration
source("functions/logging_config.R")



# Generate info panel HTML for ancestor nodes
generate_info_panel_html <- function(node_data) {
  # Handle both naming conventions: "to" (ROTL) and "Child" (DateLife)
  node_name <- if (is.list(node_data) && "to" %in% names(node_data)) {
    node_data$to
  } else if (is.list(node_data) && "Child" %in% names(node_data)) {
    node_data$Child
  } else if (is.character(node_data)) {
    node_data
  } else {
    "Unknown node"
  }
  
  # Only show info panels for ancestor nodes (not species)
  if (is.list(node_data) && "NodeType" %in% names(node_data) && node_data$NodeType == "species") {
    return("")  # No info panel for species
  }
  
  
  
  # Create info icon and panel structure
  info_panel_html <- paste0(
    '<div class="ancestor-info-container">',
    '<span class="info-icon" onclick="toggleInfoPanel(this)" title="Click for ancestor details">ⓘ</span>',
    '<div class="info-panel" style="display: none;">',
    '<div class="info-panel-content">',
    format_panel_content(node_data),
    '</div>',
    '<button class="close-panel" onclick="closeInfoPanel(this)">×</button>',
    '</div>',
    '</div>'
  )
  
  return(info_panel_html)
}

# Format the panel content with proper structure
format_panel_content <- function(node_data) {
  # Handle both naming conventions: "to" (ROTL) and "Child" (DateLife)
  node_name <- if (is.list(node_data) && "to" %in% names(node_data)) {
    node_data$to
  } else if (is.list(node_data) && "Child" %in% names(node_data)) {
    node_data$Child
  } else if (is.character(node_data)) {
    node_data
  } else {
    "Unknown node"
  }
  
  # Handle root node
  if (is.list(node_data) && "NodeType" %in% names(node_data) && node_data$NodeType == "root") {
    return(paste0(
      '<h4>', node_name, '</h4>',
      '<p class="ancestor-type">Conceptual root ancestor</p>',
      '<p class="ancestor-description">This represents the most recent common ancestor of all species in your tree.</p>'
    ))
  }
  
  # Handle nodes with age data
  if (is.list(node_data) && all(c("Age", "AgeValid", "AgeSource") %in% names(node_data))) {
    
    # Check if age is invalid or unavailable
    if (is.na(node_data$Age) || !node_data$AgeValid) {
      reason <- if ("ValidationNotes" %in% names(node_data) && !is.na(node_data$ValidationNotes)) {
        node_data$ValidationNotes
      } else {
        "No validated age data available"
      }
      
      content_html <- paste0(
        '<h4>', node_name, '</h4>',
        '<p class="ancestor-type">Evolutionary ancestor</p>',
        '<p class="age-unavailable">Age data unavailable</p>'
      )
      
      # Add silhouette and Wikipedia sections side by side for taxonomic nodes even without age data
      if (is.list(node_data) && should_add_taxonomic_content(node_data)) {
        content_html <- paste0(content_html, format_combined_taxonomic_section(node_data))
      }
      
      return(content_html)
    }
    
    # Valid age - create comprehensive panel
    age_text <- sprintf("%.1f million years ago", node_data$Age)
    confidence_stars <- get_confidence_stars(node_data$AgeSource)
    source_text <- format_age_source(node_data$AgeSource)
    
    content_html <- paste0(
      '<h4>', node_name, '</h4>',
      '<p class="ancestor-type">Evolutionary ancestor</p>',
      '<div class="age-info">',
      '<p class="age-main">Lived approximately <strong>', age_text, '</strong></p>'
    )
    
    # Add geological period if available
    geological_period <- get_geological_period(node_data$Age)
    if (!is.null(geological_period)) {
      content_html <- paste0(content_html, 
        '<p class="geological-period">During the <em>', geological_period, ' Period</em></p>'
      )
    }
    
    # Add source information (removed star rating for compactness)
    content_html <- paste0(content_html,
      '<div class="confidence-info">',
      '<p class="data-source">', source_text, '</p>',
      '</div>',
      '</div>'
    )
    
    # Add silhouette and Wikipedia sections side by side for nodes that have taxonomic information
    # This includes both pure taxonomic nodes AND hybrid nodes with both age and taxonomic data
    if (is.list(node_data) && should_add_taxonomic_content(node_data)) {
      content_html <- paste0(content_html, format_combined_taxonomic_section(node_data))
    }
    
    return(content_html)
  }
  
  # Fallback for unknown node types
  content_html <- paste0(
    '<h4>', node_name, '</h4>',
    '<p class="ancestor-type">Unknown ancestor</p>',
    '<p>Limited information available for this node.</p>'
  )
  
  # Add silhouette and Wikipedia sections side by side for nodes with taxonomic information even in fallback case
  if (is.list(node_data) && should_add_taxonomic_content(node_data)) {
    content_html <- paste0(content_html, format_combined_taxonomic_section(node_data))
  }
  
  return(content_html)
}

# Helper function to determine if a node should get taxonomic content (silhouettes/Wikipedia)
should_add_taxonomic_content <- function(node_data) {
  # Check if this node has taxonomic information
  # This includes both pure taxonomic nodes and hybrid nodes with taxonomic names
  if ("NodeType" %in% names(node_data)) {
    return(node_data$NodeType == "taxonomic" || 
           (node_data$NodeType == "ancestor" && has_extractable_taxonomic_name(node_data)))
  }
  return(FALSE)
}

# Helper function to check if an ancestor node has an extractable taxonomic name
has_extractable_taxonomic_name <- function(node_data) {
  # Check if the node has a name that contains a recognizable taxonomic group
  # For example: "Spermatophyta (352.2 Mya)" should extract "Spermatophyta"
  if ("Name" %in% names(node_data) && !is.null(node_data$Name) && !is.na(node_data$Name)) {
    node_name <- as.character(node_data$Name)
    
    # Check for empty or zero-length node_name
    if (length(node_name) == 0 || nchar(trimws(node_name)) == 0) {
      return(FALSE)
    }
    
    # Extract taxonomic name from hybrid format like "GroupName (age Mya)"
    # Handle both parentheses format "Boreoeutheria (99.3 Mya)" and dot format
    if (grepl("\\s*\\([0-9]+\\.?[0-9]*\\s+Mya\\)", node_name)) {
      # Extract taxonomic part from parentheses format
      taxonomic_name <- sub("\\s*\\([0-9]+\\.?[0-9]*\\s+Mya\\).*$", "", node_name)
    } else if (grepl("\\.*\\.[0-9]+\\.[0-9]+\\.*Mya\\.", node_name)) {
      # Extract taxonomic part from dot format (backward compatibility)
      taxonomic_name <- sub("\\.*\\.[0-9.]+\\.*Mya\\.$", "", node_name)
    } else {
      taxonomic_name <- node_name
    }
    taxonomic_name <- trimws(taxonomic_name)
    if (nchar(taxonomic_name) > 0) {
      # Check if it's not a generic ancestor name
      return(!grepl("^(Ancestor|Node)\\.+[A-Z]$", taxonomic_name) && 
             !grepl("^Common ancestor", taxonomic_name) &&
             nchar(trimws(taxonomic_name)) > 2)
    }
  }
  return(FALSE)
}

# Format combined image (Unsplash image prioritized, PhyloPic fallback) and Wikipedia text section
format_combined_taxonomic_section <- function(node_data) {
  # Check for Unsplash image (primary choice)
  has_unsplash_image <- !is.null(node_data$unsplash_image_html) &&
                        !is.na(node_data$unsplash_image_html) &&
                        nchar(as.character(node_data$unsplash_image_html)) > 0
  
  # Check for PhyloPic silhouette (fallback choice)
  has_silhouette <- !is.null(node_data$silhouette_html) && 
                    !is.na(node_data$silhouette_html) && 
                    nchar(as.character(node_data$silhouette_html)) > 0
  
  # Check for Wikipedia text content
  has_wikipedia <- !is.null(node_data$wikipedia_summary) && 
                   !is.na(node_data$wikipedia_summary) && 
                   nchar(as.character(node_data$wikipedia_summary)) > 0
  
  # Check for errors
  has_unsplash_image_error <- !is.null(node_data$unsplash_image_error) &&
                              !is.na(node_data$unsplash_image_error) &&
                              nchar(as.character(node_data$unsplash_image_error)) > 0
  
  has_silhouette_error <- !is.null(node_data$silhouette_error) && 
                         !is.na(node_data$silhouette_error) && 
                         nchar(as.character(node_data$silhouette_error)) > 0
  
  has_wikipedia_error <- !is.null(node_data$wikipedia_error) && 
                        !is.na(node_data$wikipedia_error) && 
                        nchar(as.character(node_data$wikipedia_error)) > 0
  
  # Show section if we have any data OR errors to display
  if (!has_unsplash_image && !has_silhouette && !has_wikipedia &&
      !has_unsplash_image_error && !has_silhouette_error && !has_wikipedia_error) {
    return("")
  }
  
  # Start the combined section
  combined_html <- '<div class="taxonomic-combined-section">'
  
  # Prioritize Unsplash image, fallback to PhyloPic silhouette
  image_content <- ""
  if (has_unsplash_image) {
    # Use Unsplash image (highest priority)
    image_content <- node_data$unsplash_image_html
  } else if (has_silhouette) {
    # Fallback to PhyloPic silhouette
    image_content <- node_data$silhouette_html
  } else if (has_unsplash_image_error && has_silhouette_error) {
    # Both image sources failed - show combined error
    image_content <- paste0(
      '<div class="image-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ No images available<br>',
      'Unsplash: ', node_data$unsplash_image_error, '<br>',
      'PhyloPic: ', node_data$silhouette_error,
      '</div>'
    )
  } else if (has_unsplash_image_error) {
    # Unsplash image failed, no PhyloPic attempted or available
    image_content <- paste0(
      '<div class="image-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ ', node_data$unsplash_image_error,
      '</div>'
    )
  } else if (has_silhouette_error) {
    # PhyloPic failed, no Unsplash image attempted or available
    image_content <- paste0(
      '<div class="image-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ ', node_data$silhouette_error,
      '</div>'
    )
  }
  
  # Prepare Wikipedia text content (data or error)  
  wikipedia_content <- ""
  if (has_wikipedia) {
    wikipedia_content <- format_wikipedia_content(node_data)
  } else if (has_wikipedia_error) {
    wikipedia_content <- paste0(
      '<div class="wikipedia-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ ', node_data$wikipedia_error,
      '</div>'
    )
  }
  
  # Combine content based on what we have
  has_any_image <- has_unsplash_image || has_silhouette || has_unsplash_image_error || has_silhouette_error
  has_any_text <- has_wikipedia || has_wikipedia_error
  
  if (has_any_image && has_any_text) {
    # Both image and text - image floated left with text wrapping
    combined_html <- paste0(combined_html,
      '<div class="taxonomic-wrapped-container">',
      '<div class="image-float">',
      image_content,
      '</div>',
      wikipedia_content,
      '</div>'
    )
  } else if (has_any_image) {
    # Only image (data or error)
    combined_html <- paste0(combined_html,
      '<div class="image-only">',
      image_content,
      '</div>'
    )
  } else if (has_any_text) {
    # Only Wikipedia text (data or error)
    combined_html <- paste0(combined_html,
      '<div class="wikipedia-only">',
      wikipedia_content,
      '</div>'
    )
  }
  
  combined_html <- paste0(combined_html, '</div>')
  return(combined_html)
}

# Format Wikipedia content without section wrapper
format_wikipedia_content <- function(node_data) {
  paste0(
    '<div class="wikipedia-summary">', node_data$wikipedia_summary, '</div>',
    '<a href="', node_data$wikipedia_url, '" target="_blank" rel="noopener noreferrer" class="wikipedia-link">Read more on Wikipedia →</a>'
  )
}

# Format silhouette section for taxonomic nodes (kept for backward compatibility)
format_silhouette_section <- function(node_data) {
  # Check if silhouette data is available
  has_silhouette <- !is.null(node_data$silhouette_html) && 
                    !is.na(node_data$silhouette_html) && 
                    nchar(as.character(node_data$silhouette_html)) > 0
  
  if (has_silhouette) {
    return(node_data$silhouette_html)
  } else {
    return("")  # Return empty string if no silhouette available
  }
}

# Format Wikipedia section for taxonomic nodes
format_wikipedia_section <- function(node_data) {
  # Check if Wikipedia data is available
  has_wikipedia <- !is.null(node_data$wikipedia_summary) && 
                   !is.na(node_data$wikipedia_summary) && 
                   nchar(as.character(node_data$wikipedia_summary)) > 0
  
  has_wikipedia_error <- !is.null(node_data$wikipedia_error) && 
                        !is.na(node_data$wikipedia_error) && 
                        nchar(as.character(node_data$wikipedia_error)) > 0
  
  if (has_wikipedia) {
    # Display Wikipedia content
    wikipedia_html <- paste0(
      '<div class="wikipedia-section">',
      '<div class="wikipedia-content">',
      '<div class="wikipedia-summary">', node_data$wikipedia_summary, '</div>',
      '<a href="', node_data$wikipedia_url, '" target="_blank" rel="noopener noreferrer" class="wikipedia-link">',
      'Read more on Wikipedia →',
      '</a>',
      '</div>',
      '</div>'
    )
  } else if (has_wikipedia_error) {
    # Show specific error message
    wikipedia_html <- paste0(
      '<div class="wikipedia-section">',
      '<div class="wikipedia-content">',
      '<div class="wikipedia-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ ', node_data$wikipedia_error,
      '</div>',
      '</div>',
      '</div>'
    )
  } else {
    # Show generic message that Wikipedia data is not available
    wikipedia_html <- paste0(
      '<div class="wikipedia-section">',
      '<div class="wikipedia-content">',
      '<p class="wikipedia-unavailable" style="color: #95a5a6; font-size: 13px; font-style: italic;">',
      'Wikipedia information not available for this taxonomic group',
      '</p>',
      '</div>',
      '</div>'
    )
  }
  
  return(wikipedia_html)
}

# Generate CSS styles for the info panel system
generate_info_panel_css <- function() {
  return('
<style>
/* Info Panel System Styles */
.ancestor-info-container {
  position: relative;
  display: inline-block;
  margin-left: 8px;
  vertical-align: middle;
}

.info-icon {
  display: inline-block;
  width: 18px;
  height: 18px;
  background-color: #3498db;
  color: white;
  border-radius: 50%;
  text-align: center;
  line-height: 18px;
  font-size: 12px;
  font-weight: bold;
  cursor: pointer;
  user-select: none;
  transition: all 0.2s ease;
  margin-left: 4px;
}

.info-icon:hover {
  background-color: #2980b9;
  transform: scale(1.1);
}

.info-icon:active {
  transform: scale(0.95);
}

.info-panel {
  position: fixed;
  width: 450px;
  background: white;
  border: 2px solid #3498db;
  border-radius: 8px;
  box-shadow: 0 4px 15px rgba(0,0,0,0.15);
  z-index: 1000;
  max-height: 400px;
  overflow-y: auto;
  /* Position and transform will be set dynamically by JavaScript to center on SVG */
  /* The transform: translate(-50%, -50%) is key for perfect centering */
}

/* Legacy positioning classes kept for fallback compatibility */
.info-panel.position-above {
  top: auto;
  bottom: 25px;
}

.info-panel.position-left {
  left: -470px; /* panel width (450px) + some padding */
}

.info-panel.position-right {
  left: 25px; /* position to the right of the icon */
}

.info-panel.position-center {
  left: -225px; /* center the panel (half of 450px width) */
}

.info-panel-content {
  padding: 16px;
  color: #2c3e50;
}

.info-panel-content h4 {
  margin: 0 0 8px 0;
  color: #2c3e50;
  font-size: 16px;
  border-bottom: 1px solid #ecf0f1;
  padding-bottom: 4px;
}

.ancestor-type {
  font-style: italic;
  color: #7f8c8d;
  margin: 4px 0 12px 0;
  font-size: 14px;
}

.age-info {
  background: #f8f9fa;
  padding: 12px;
  border-radius: 4px;
  margin: 8px 0;
}

.age-main {
  font-size: 15px;
  margin: 0 0 8px 0;
  color: #2c3e50;
}

.geological-period {
  color: #8e44ad;
  margin: 6px 0;
  font-size: 14px;
}

.confidence-info {
  border-top: 1px solid #e9ecef;
  padding-top: 8px;
  margin-top: 8px;
}

.confidence-stars {
  margin: 4px 0;
  font-size: 14px;
}

.data-source {
  margin: 4px 0 0 0;
  font-size: 12px;
  color: #6c757d;
}

.age-unavailable {
  color: #e74c3c;
  font-weight: bold;
  margin: 8px 0 4px 0;
}

.age-reason {
  color: #95a5a6;
  font-size: 13px;
  font-style: italic;
  margin: 4px 0;
}

.silhouette-section {
  border-top: 1px solid #e9ecef;
  margin-top: 12px;
  padding-top: 12px;
  text-align: center;
}

.silhouette-container {
  margin: 8px 0;
  padding: 12px;
  background: #f8f9fa;
  border-radius: 4px;
  display: flex;
  justify-content: center;
  align-items: center;
  height: 80px;
  width: 100%;
  overflow: hidden;
  box-sizing: border-box;
}

.ancestor-silhouette {
  max-width: 90px;
  max-height: 90px;
  width: auto;
  height: auto;
  object-fit: contain;
  filter: drop-shadow(0 2px 4px rgba(0,0,0,0.1));
  display: block;
  margin: 0 auto;
}

/* Silhouette attribution removed - cited on about page */

/* New combined taxonomic section styles */
.taxonomic-combined-section {
  border-top: 1px solid #e9ecef;
  margin-top: 12px;
  padding-top: 12px;
}

/* New wrapped layout for silhouette + Wikipedia */
.taxonomic-wrapped-container {
  overflow: hidden; /* Clear float */
}

.silhouette-float {
  float: left;
  width: 100px;
  margin-right: 12px;
  margin-bottom: 8px;
}

.taxonomic-wrapped-container .wikipedia-summary {
  text-align: left;
  word-wrap: break-word;
  overflow-wrap: break-word;
  margin: 0 0 8px 0;
  font-size: 13px;
  line-height: 1.3;
  color: #495057;
}

/* Legacy flex container for compatibility */
.taxonomic-flex-container {
  display: flex;
  gap: 12px;
  align-items: flex-start;
}

.silhouette-column {
  flex: 0 0 auto;
  width: 100px; /* Fixed width to minimize white space */
}

.wikipedia-column {
  flex: 1;
  min-width: 0;
  word-wrap: break-word;
  overflow-wrap: break-word;
}

.silhouette-only,
.wikipedia-only {
  width: 100%;
}

.silhouette-column .silhouette-section,
.silhouette-only .silhouette-section,
.silhouette-float .silhouette-section {
  border: none;
  margin: 0;
  padding: 0;
}

.silhouette-column .silhouette-container,
.silhouette-only .silhouette-container {
  margin: 0;
  height: 70px; /* Slightly smaller for side-by-side layout */
}

.wikipedia-column .wikipedia-summary,
.wikipedia-only .wikipedia-summary {
  margin: 0 0 8px 0;
  font-size: 13px; /* Slightly smaller for compactness */
  line-height: 1.3;
}

.wikipedia-section {
  border-top: 1px solid #e9ecef;
  margin-top: 12px;
  padding-top: 12px;
}

.wikipedia-loading {
  color: #6c757d;
  font-style: italic;
  font-size: 13px;
}

.wikipedia-content {
  margin-top: 8px;
}

.wikipedia-summary {
  font-size: 14px;
  line-height: 1.4;
  color: #2c3e50;
  margin: 8px 0;
}

.wikipedia-link {
  display: inline-block;
  margin-top: 8px;
  color: #3498db;
  text-decoration: none;
  font-size: 13px;
}

.wikipedia-link:hover {
  text-decoration: underline;
}

/* Unsplash image styles */
.unsplash-image-section {
  margin: 0;
  padding: 0;
}

.unsplash-image-container {
  margin: 8px 0;
  padding: 12px;
  background: #f8f9fa;
  border-radius: 4px;
  display: flex;
  justify-content: center;
  align-items: center;
  min-height: 60px;
  width: 100%;
  overflow: hidden;
  box-sizing: border-box;
}

.unsplash-taxonomic-image {
  max-width: 90px;
  max-height: 90px;
  width: auto;
  height: auto;
  object-fit: contain;
  border-radius: 3px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  display: block;
  margin: 0 auto;
}

/* Wikipedia image styles (legacy - kept for backwards compatibility) */
.wikipedia-image-section {
  margin: 0;
  padding: 0;
}

.wikipedia-image-container {
  margin: 8px 0;
  padding: 12px;
  background: #f8f9fa;
  border-radius: 4px;
  display: flex;
  justify-content: center;
  align-items: center;
  min-height: 60px;
  width: 100%;
  overflow: hidden;
  box-sizing: border-box;
}

.wikipedia-taxonomic-image {
  max-width: 90px;
  max-height: 90px;
  width: auto;
  height: auto;
  object-fit: contain;
  border-radius: 3px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  display: block;
  margin: 0 auto;
}

/* Floating images in combined layout */
.image-float {
  float: left;
  width: 100px;
  margin-right: 12px;
  margin-bottom: 8px;
}

.image-float .unsplash-image-container,
.image-float .wikipedia-image-container,
.image-float .silhouette-container {
  margin: 0;
  height: 70px;
}

.close-panel {
  position: absolute;
  top: 4px;
  right: 8px;
  background: none;
  border: none;
  font-size: 20px;
  cursor: pointer;
  color: #95a5a6;
  width: 24px;
  height: 24px;
  display: flex;
  align-items: center;
  justify-content: center;
  line-height: 1;
}

.close-panel:hover {
  color: #e74c3c;
  background: #f8f9fa;
  border-radius: 50%;
}

/* Mobile responsive styles */
@media (max-width: 768px) {
  .info-panel {
    left: -180px;
    width: 380px;
    max-height: 350px;
  }
  
  .taxonomic-flex-container {
    flex-direction: column;
    gap: 12px;
  }
  
  .silhouette-column {
    flex: none;
    width: 100%;
  }
}
  
  .info-icon {
    width: 20px;
    height: 20px;
    line-height: 20px;
    font-size: 13px;
  }
  
  .info-panel-content {
    padding: 12px;
  }
}

/* Very small screens */
@media (max-width: 480px) {
  .info-panel {
    left: -140px;
    width: 320px;
    max-height: 300px;
  }
  
  .info-panel-content h4 {
    font-size: 15px;
  }
  
  .age-main {
    font-size: 14px;
  }
}
</style>
  ')
}

# Generate JavaScript for info panel interactions
generate_info_panel_js <- function() {
  return('
<script>
// Info Panel System JavaScript

function toggleInfoPanel(iconElement) {
  // Close any other open panels first
  closeAllInfoPanels();

  // Find and toggle the panel for this icon
  const container = iconElement.parentElement;
  const panel = container.querySelector(".info-panel");

  if (panel) {
    panel.style.display = panel.style.display === "none" ? "block" : "none";

    // Add click outside listener if panel is now open
    if (panel.style.display === "block") {
      // Position the panel intelligently to avoid viewport cutoff
      positionPanelInViewport(panel, iconElement);

      // Store reference to current panel for scroll/resize handlers
      window.currentInfoPanel = {panel: panel, iconElement: iconElement};

      // Add scroll and resize listeners to keep panel centered
      window.addEventListener("scroll", repositionCurrentPanel);
      window.addEventListener("resize", repositionCurrentPanel);

      // Small delay to prevent immediate closing from this click
      setTimeout(() => {
        document.addEventListener("click", handleClickOutside);
      }, 10);

      // No need to fetch Wikipedia data - it is already embedded server-side
    } else {
      // Panel is being closed, remove scroll/resize listeners
      window.currentInfoPanel = null;
      window.removeEventListener("scroll", repositionCurrentPanel);
      window.removeEventListener("resize", repositionCurrentPanel);
    }
  }
}

function positionPanelInViewport(panel, iconElement) {
  console.log("=== POSITIONING PANEL DEBUG START ===");

  // Remove any existing positioning classes
  panel.classList.remove("position-above", "position-left", "position-right", "position-center");

  // Find the SVG container to center the panel on
  const svgElement = document.querySelector("svg");
  console.log("SVG element found:", !!svgElement);

  if (!svgElement) {
    // Fallback to old positioning if no SVG found
    console.log("No SVG found, using fallback positioning");
    panel.classList.add("position-center");
    return;
  }

  // Get SVG container dimensions and position
  const svgRect = svgElement.getBoundingClientRect();

  // Calculate center position of the SVG
  const svgCenterX = svgRect.left + (svgRect.width / 2);
  const svgCenterY = svgRect.top + (svgRect.height / 2);

  // Set position to fixed and center on SVG using transform
  // This is the key technique from the research: use transform for perfect centering
  console.log("Setting panel to fixed position at SVG center:", svgCenterX, svgCenterY);
  panel.style.position = "fixed";
  panel.style.left = svgCenterX + "px";
  panel.style.top = svgCenterY + "px";
  panel.style.transform = "translate(-50%, -50%)";
  panel.style.zIndex = "1000";
  console.log("Panel positioned with transform: translate(-50%, -50%)");

  // Ensure panel fits within viewport bounds
  const viewportWidth = window.innerWidth;
  const viewportHeight = window.innerHeight;
  const padding = 20;

  // Get panel dimensions after positioning
  const panelRect = panel.getBoundingClientRect();

  // Adjust if panel would go outside viewport bounds
  let adjustedX = svgCenterX;
  let adjustedY = svgCenterY;

  // Check horizontal bounds
  if (panelRect.left < padding) {
    adjustedX = svgCenterX + (padding - panelRect.left);
  } else if (panelRect.right > viewportWidth - padding) {
    adjustedX = svgCenterX - (panelRect.right - (viewportWidth - padding));
  }

  // Check vertical bounds
  if (panelRect.top < padding) {
    adjustedY = svgCenterY + (padding - panelRect.top);
  } else if (panelRect.bottom > viewportHeight - padding) {
    adjustedY = svgCenterY - (panelRect.bottom - (viewportHeight - padding));
  }

  // Apply final position
  panel.style.left = adjustedX + "px";
  panel.style.top = adjustedY + "px";
}

// Function to reposition current panel on scroll/resize
function repositionCurrentPanel() {
  if (window.currentInfoPanel && window.currentInfoPanel.panel.style.display === "block") {
    positionPanelInViewport(window.currentInfoPanel.panel, window.currentInfoPanel.iconElement);
  }
}

function closeInfoPanel(buttonElement) {
  const panel = buttonElement.closest(".info-panel");
  if (panel) {
    panel.style.display = "none";
  }
  // Clean up event listeners
  window.currentInfoPanel = null;
  window.removeEventListener("scroll", repositionCurrentPanel);
  window.removeEventListener("resize", repositionCurrentPanel);
  document.removeEventListener("click", handleClickOutside);
}

function closeAllInfoPanels() {
  const allPanels = document.querySelectorAll(".info-panel");
  allPanels.forEach(panel => {
    panel.style.display = "none";
  });
  // Clean up event listeners
  window.currentInfoPanel = null;
  window.removeEventListener("scroll", repositionCurrentPanel);
  window.removeEventListener("resize", repositionCurrentPanel);
  document.removeEventListener("click", handleClickOutside);
}

function handleClickOutside(event) {
  // Check if click is outside any info panel container
  const clickedContainer = event.target.closest(".ancestor-info-container");
  if (!clickedContainer) {
    closeAllInfoPanels();
  }
}

// Close panels on escape key
document.addEventListener("keydown", function(event) {
  if (event.key === "Escape") {
    closeAllInfoPanels();
  }
});

// Prevent panel content clicks from bubbling up
document.addEventListener("click", function(event) {
  if (event.target.closest(".info-panel-content")) {
    event.stopPropagation();
  }
});

console.log("Info panel system initialized with server-side Wikipedia integration");
</script>
  ')
}

# Create info panel data for network
# Sequential version with natural rate limiting for improved reliability
create_info_panel_data_sequential <- function(network_data, request_id = NULL, progress_token = NULL) {
  if (is.null(request_id)) {
    request_id <- "info_panel"
  }
  
  # Helper function to update progress if token provided
  update_progress_internal <- function(step_name, status = "completed", additional_data = NULL) {
    if (!is.null(progress_token) && progress_token != "") {
      update_progress(progress_token, step_name, status, additional_data)
    }
  }
  
  # Configure logging for future worker process - write to same log file as main process
  if (exists("log_appender", mode = "function")) {
    # Ensure logs directory exists in worker
    if (!dir.exists("logs")) {
      dir.create("logs", recursive = TRUE)
    }
    # Configure file appender to match main process logging
    log_appender(appender_file("logs/api.log", append = TRUE), namespace = "evolution.api")
    log_layout(layout_simple, namespace = "evolution.api")
    log_threshold(INFO, namespace = "evolution.api")
  }
  
  api_log_info(paste("[", request_id, "] Starting sequential info panel data creation...", sep=""))
  start_time <- Sys.time()
  
  # Handle both naming conventions: "to" (ROTL) and "Child" (DateLife)
  if (!("to" %in% names(network_data)) && !("Child" %in% names(network_data))) {
    api_log_warn(paste("[", request_id, "] No valid node naming convention found in network data", sep=""))
    return(rep("", nrow(network_data)))
  }
  
  info_panel_data <- character(nrow(network_data))
  total_nodes <- nrow(network_data)
  
  api_log_info(paste("[", request_id, "] Processing", total_nodes, "nodes for info panel creation..."))
  
  # Identify nodes that need taxonomic content (Wikipedia + PhyloPic data)
  taxonomic_indices <- c()
  taxonomic_node_info <- list()
  species_count <- 0
  
  for (i in 1:nrow(network_data)) {
    node_info <- as.list(network_data[i, ])
    
    # Only show info panels for ancestor nodes (not species)
    if ("NodeType" %in% names(node_info) && node_info$NodeType == "species") {
      info_panel_data[i] <- ""  # No info panel for species
      species_count <- species_count + 1
    } else {
      # Check if this node needs taxonomic content
      if (should_add_taxonomic_content(node_info)) {
        taxonomic_indices <- c(taxonomic_indices, i)
        taxonomic_node_info[[length(taxonomic_node_info) + 1]] <- list(index = i, node_info = node_info)
      } else {
        # Generate panel content without taxonomic data
        info_panel_data[i] <- format_panel_content(node_info)
      }
    }
  }
  
  ancestor_nodes <- total_nodes - species_count
  taxonomic_nodes <- length(taxonomic_indices)
  non_taxonomic_ancestors <- ancestor_nodes - taxonomic_nodes
  
  api_log_info(paste("[", request_id, "] Node classification:", species_count, "species,", ancestor_nodes, "ancestors (", taxonomic_nodes, "taxonomic,", non_taxonomic_ancestors, "non-taxonomic)"))
  
  # Process taxonomic nodes sequentially with natural rate limiting
  if (length(taxonomic_indices) > 0) {
    api_log_info(paste("[", request_id, "] Processing", length(taxonomic_indices), "taxonomic nodes sequentially (PhyloPic → Wikipedia → PhyloPic → Wikipedia...)"))
    
    wikipedia_successes <- 0
    unsplash_image_successes <- 0
    phylopic_successes <- 0
    unsplash_image_errors <- c()
    phylopic_errors <- c()
    processed_taxonomic_names <- c()
    
    sequential_start <- Sys.time()
    
    # Sequential processing: Unsplash → PhyloPic → Wikipedia → Unsplash → PhyloPic → Wikipedia
    for (i in seq_along(taxonomic_node_info)) {
      item <- taxonomic_node_info[[i]]
      node_info <- item$node_info
      
      # Extract taxonomic name
      taxonomic_name <- extract_taxonomic_name(node_info)
      
      # Skip if it's a generic ancestor name
      # Add comprehensive null and length checks to prevent "argument is of length zero" error
      if (is.null(taxonomic_name) || 
          length(taxonomic_name) == 0 || 
          is.na(taxonomic_name) ||
          nchar(trimws(as.character(taxonomic_name))) <= 2) {
        info_panel_data[item$index] <- format_panel_content(node_info)
        next
      }
      
      # Additional safety checks before using grepl
      taxonomic_name_str <- as.character(taxonomic_name)
      if (length(taxonomic_name_str) == 0 || is.na(taxonomic_name_str) || nchar(trimws(taxonomic_name_str)) == 0) {
        info_panel_data[item$index] <- format_panel_content(node_info)
        next
      }
      
      # Now safe to use grepl with proper string
      if (grepl("^(Ancestor|Node)\\.+[A-Z]$", taxonomic_name_str) || 
          grepl("^Common ancestor", taxonomic_name_str)) {
        info_panel_data[item$index] <- format_panel_content(node_info)
        next
      }
      
      api_log_info(paste("[", request_id, "] Processing taxonomic node", i, "of", length(taxonomic_node_info), ":", taxonomic_name))
      processed_taxonomic_names <- c(processed_taxonomic_names, taxonomic_name)
      
      wikipedia_success <- FALSE
      unsplash_image_success <- FALSE
      phylopic_success <- FALSE
      node_start_time <- Sys.time()
      
      # 1. Unsplash Image API call first (highest priority)
      api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching Unsplash image for:", taxonomic_name))
      unsplash_image_start <- Sys.time()
      tryCatch({
        if (exists("cached_get_unsplash_random_image")) {
          unsplash_image_result <- cached_get_unsplash_random_image(taxonomic_name, target_width = 200)
          if (unsplash_image_result$success) {
            unsplash_image_html <- format_unsplash_image_html(unsplash_image_result)
            if (!is.null(unsplash_image_html) && nchar(unsplash_image_html) > 0) {
              node_info$unsplash_image_html <- unsplash_image_html
              node_info$unsplash_image_url <- unsplash_image_result$image_url
              node_info$unsplash_image_attribution <- unsplash_image_result$attribution
              unsplash_image_success <- TRUE
              unsplash_image_successes <- unsplash_image_successes + 1
              api_log_info(paste("[", request_id, "] Unsplash image success for:", taxonomic_name, "(", round(as.numeric(difftime(Sys.time(), unsplash_image_start, units = "secs")), 3), "s)"))
            } else {
              node_info$unsplash_image_error <- "Empty Unsplash image HTML generated"
              unsplash_image_errors <- c(unsplash_image_errors, paste(taxonomic_name, ": Empty HTML"))
              api_log_info(paste("[", request_id, "] Unsplash image empty result for:", taxonomic_name))
            }
          } else {
            node_info$unsplash_image_error <- paste("Unsplash image failed:", unsplash_image_result$error)
            unsplash_image_errors <- c(unsplash_image_errors, paste(taxonomic_name, ":", unsplash_image_result$error))
            api_log_info(paste("[", request_id, "] Unsplash image failed for:", taxonomic_name, "-", unsplash_image_result$error))
          }
        }
      }, error = function(e) {
        node_info$unsplash_image_error <- paste("Unsplash image error:", e$message)
        unsplash_image_errors <- c(unsplash_image_errors, paste(taxonomic_name, ":", e$message))
        api_log_error(paste("[", request_id, "] Unsplash image error for:", taxonomic_name, "-", e$message))
      })
      
      # 2. PhyloPic API call as fallback (only if Unsplash image failed)
      if (!unsplash_image_success) {
        api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching PhyloPic data for:", taxonomic_name, "(Unsplash image unavailable)"))
      } else {
        api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping PhyloPic - Unsplash image available for:", taxonomic_name))
      }

      phylopic_start <- Sys.time()
      if (!unsplash_image_success) {
        tryCatch({
          if (exists("cached_get_silhouette_data")) {
            silhouette_result <- cached_get_silhouette_data(taxonomic_name)
            if (silhouette_result$success) {
              silhouette_html <- format_silhouette_html(silhouette_result)
              if (!is.null(silhouette_html) && nchar(silhouette_html) > 0) {
                node_info$silhouette_html <- silhouette_html
                node_info$silhouette_uuid <- silhouette_result$uuid
                node_info$silhouette_attribution <- silhouette_result$attribution
                phylopic_success <- TRUE
                phylopic_successes <- phylopic_successes + 1
                api_log_info(paste("[", request_id, "] PhyloPic success for:", taxonomic_name, "(", round(as.numeric(difftime(Sys.time(), phylopic_start, units = "secs")), 3), "s)"))
              } else {
                node_info$silhouette_error <- "Empty silhouette HTML generated"
                phylopic_errors <- c(phylopic_errors, paste(taxonomic_name, ": Empty HTML"))
                api_log_info(paste("[", request_id, "] PhyloPic empty result for:", taxonomic_name))
              }
            } else {
              node_info$silhouette_error <- paste("Silhouette data failed:", silhouette_result$error)
              phylopic_errors <- c(phylopic_errors, paste(taxonomic_name, ":", silhouette_result$error))
              api_log_info(paste("[", request_id, "] PhyloPic failed for:", taxonomic_name, "-", silhouette_result$error))
            }
          }
        }, error = function(e) {
          node_info$silhouette_error <- paste("PhyloPic error:", e$message)
          phylopic_errors <- c(phylopic_errors, paste(taxonomic_name, ":", e$message))
          api_log_error(paste("[", request_id, "] PhyloPic error for:", taxonomic_name, "-", e$message))
        })
      }
      
      # 3. Wikipedia text API call (natural rate limiting)
      api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching Wikipedia data for:", taxonomic_name))
      wikipedia_start <- Sys.time()
      tryCatch({
        if (exists("cached_get_wikipedia_intro")) {
          wikipedia_result <- cached_get_wikipedia_intro(taxonomic_name, truncate_length = 250)
          if (wikipedia_result$success) {
            node_info$wikipedia_summary <- wikipedia_result$introduction
            node_info$wikipedia_url <- wikipedia_result$url
            node_info$wikipedia_title <- wikipedia_result$wikipedia_title
            wikipedia_success <- TRUE
            wikipedia_successes <- wikipedia_successes + 1
            api_log_info(paste("[", request_id, "] Wikipedia success for:", taxonomic_name, "(", round(as.numeric(difftime(Sys.time(), wikipedia_start, units = "secs")), 3), "s)"))
          } else {
            api_log_info(paste("[", request_id, "] Wikipedia failed for:", taxonomic_name, "-", wikipedia_result$error))
          }
        }
      }, error = function(e) {
        api_log_error(paste("[", request_id, "] Wikipedia error for:", taxonomic_name, "-", e$message))
      })
      
      # Generate final panel content for this node
      info_panel_data[item$index] <- format_panel_content(node_info)
      
      node_duration <- as.numeric(difftime(Sys.time(), node_start_time, units = "secs"))
      api_log_info(paste("[", request_id, "] Completed taxonomic node", i, ":", taxonomic_name, "- Duration:", round(node_duration, 3), "s"))
      
      # Update progress after each taxonomic node is completed
      update_progress_internal("taxonomic_data_fetching", "in_progress",
                             list(completed_nodes = i,
                                  total_nodes = length(taxonomic_node_info),
                                  current_node = taxonomic_name,
                                  wikipedia_successes = wikipedia_successes,
                                  unsplash_image_successes = unsplash_image_successes,
                                  phylopic_successes = phylopic_successes))
    }
    
    sequential_duration <- as.numeric(difftime(Sys.time(), sequential_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Sequential processing completed - Duration:", round(sequential_duration, 3), "s"))
    
    api_log_info(paste("[", request_id, "] Sequential processing results:", sep=""))
    api_log_info(paste("[", request_id, "]   Taxonomic names processed:", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   Wikipedia text successes:", wikipedia_successes, "/", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   Unsplash image successes:", unsplash_image_successes, "/", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   PhyloPic successes:", phylopic_successes, "/", length(processed_taxonomic_names)))
    if (length(unsplash_image_errors) > 0) {
      api_log_info(paste("[", request_id, "]   Unsplash image errors:", paste(head(unsplash_image_errors, 3), collapse = "; "), if(length(unsplash_image_errors) > 3) '...' else ''))
    }
    if (length(phylopic_errors) > 0) {
      api_log_info(paste("[", request_id, "]   PhyloPic errors:", paste(head(phylopic_errors, 3), collapse = "; "), if(length(phylopic_errors) > 3) '...' else ''))
    }
    
    if (length(processed_taxonomic_names) > 0) {
      api_log_info(paste("[", request_id, "]   Processed taxonomic groups:", paste(head(processed_taxonomic_names, 3), collapse = ', '), if(length(processed_taxonomic_names) > 3) '...' else '', sep=" "))
    }
    
    # Mark taxonomic data fetching as completed
    update_progress_internal("taxonomic_data_fetching", "completed",
                           list(total_processed = length(processed_taxonomic_names),
                                wikipedia_successes = wikipedia_successes,
                                unsplash_image_successes = unsplash_image_successes,
                                phylopic_successes = phylopic_successes,
                                duration_seconds = round(sequential_duration, 3)))
  } else {
    api_log_info(paste("[", request_id, "] No taxonomic nodes requiring external data - skipping external API calls", sep=""))
  }
  
  total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  api_log_info(paste("[", request_id, "] Sequential info panel creation completed - Total Duration:", round(total_duration, 3), "s"))
  
  return(info_panel_data)
}

# Keep the old parallel function for backwards compatibility but mark as deprecated
create_info_panel_data_parallel <- function(network_data, request_id = NULL) {
  api_log_warn(paste("[", request_id, "] create_info_panel_data_parallel is deprecated - using sequential version for stability"))
  return(create_info_panel_data_sequential(network_data, request_id))
}

# Helper function to extract taxonomic name from node info
extract_taxonomic_name <- function(node_info) {
  if ("to" %in% names(node_info)) {
    raw_name <- node_info$to
  } else if ("Child" %in% names(node_info)) {
    raw_name <- node_info$Child
  } else if ("Name" %in% names(node_info)) {
    raw_name <- node_info$Name
  } else {
    return(NULL)
  }
  
  # Check if raw_name is NULL, NA, or empty
  if (is.null(raw_name) || length(raw_name) == 0 || is.na(raw_name) || nchar(trimws(as.character(raw_name))) == 0) {
    return(NULL)
  }
  
  # Convert to character to ensure we have a proper string
  raw_name <- as.character(raw_name)
  
  # Extract taxonomic name from hybrid nodes like "Spermatophyta (352.2 Mya)" or "Boreoeutheria (99.3 Mya)" or "Clupeocephala (~532.4 Mya)"
  if (grepl("\\s*\\(~?[0-9]+\\.?[0-9]*\\s+Mya\\)", raw_name)) {
    # Extract just the taxonomic part before the age in parentheses (handles optional tilde)
    taxonomic_name <- sub("\\s*\\(~?[0-9]+\\.?[0-9]*\\s+Mya\\).*$", "", raw_name)
    taxonomic_name <- trimws(taxonomic_name)
  } else if (grepl("\\.[0-9]+\\.[0-9]+\\.*Mya\\.", raw_name)) {
    # Handle the old dot format for backward compatibility
    taxonomic_name <- sub("\\.*\\.[0-9]+\\.[0-9]+\\.*Mya\\.$", "", raw_name)
    taxonomic_name <- trimws(taxonomic_name)
  } else {
    taxonomic_name <- raw_name
  }
  
  # Final check to ensure we return NULL instead of empty string
  if (is.null(taxonomic_name) || length(taxonomic_name) == 0 || is.na(taxonomic_name) || nchar(trimws(taxonomic_name)) == 0) {
    return(NULL)
  }
  
  return(taxonomic_name)
}

create_info_panel_data <- function(network_data) {
  # Handle both naming conventions: "to" (ROTL) and "Child" (DateLife)
  if (!("to" %in% names(network_data)) && !("Child" %in% names(network_data))) {
    return(rep("", nrow(network_data)))
  }
  
  info_panel_data <- character(nrow(network_data))
  
  for (i in 1:nrow(network_data)) {
    node_info <- as.list(network_data[i, ])
    
    # Only show info panels for ancestor nodes (not species)
    if ("NodeType" %in% names(node_info) && node_info$NodeType == "species") {
      info_panel_data[i] <- ""  # No info panel for species
    } else {
      # Add Wikipedia and silhouette data for nodes with taxonomic content
      if (should_add_taxonomic_content(node_info)) {
        node_info <- add_wikipedia_data(node_info)
      }
      
      # Return just the clean panel content, not the full widget HTML
      info_panel_data[i] <- format_panel_content(node_info)
    }
  }
  
  return(info_panel_data)
}

# Add Wikipedia and silhouette data to taxonomic nodes
add_wikipedia_data <- function(node_info) {
  # Get the taxonomic group name, handling both pure taxonomic and hybrid nodes
  taxonomic_name <- NULL
  
  if ("to" %in% names(node_info)) {
    raw_name <- node_info$to
  } else if ("Child" %in% names(node_info)) {
    raw_name <- node_info$Child
  } else if ("Name" %in% names(node_info)) {
    raw_name <- node_info$Name
  } else {
    return(node_info)  # Return unchanged if no name found
  }
  
  # Check if raw_name is NULL, NA, or empty
  if (is.null(raw_name) || length(raw_name) == 0 || is.na(raw_name) || nchar(trimws(as.character(raw_name))) == 0) {
    return(node_info)  # Return unchanged if name is empty
  }
  
  # Convert to character to ensure we have a proper string
  raw_name <- as.character(raw_name)
  
  # Extract taxonomic name from hybrid nodes like "Spermatophyta (352.2 Mya)" or "Boreoeutheria (99.3 Mya)" or "Clupeocephala (~532.4 Mya)"
  if (grepl("\\s*\\(~?[0-9]+\\.?[0-9]*\\s+Mya\\)", raw_name)) {
    # Extract just the taxonomic part before the age in parentheses (handles optional tilde)
    taxonomic_name <- sub("\\s*\\(~?[0-9]+\\.?[0-9]*\\s+Mya\\).*$", "", raw_name)
    taxonomic_name <- trimws(taxonomic_name)
  } else if (grepl("\\.[0-9]+\\.[0-9]+\\.*Mya\\.", raw_name)) {
    # Handle the old dot format for backward compatibility
    taxonomic_name <- sub("\\.*\\.[0-9]+\\.[0-9]+\\.*Mya\\.$", "", raw_name)
    taxonomic_name <- trimws(taxonomic_name)
  } else {
    taxonomic_name <- raw_name
  }
  
  # Skip if it's a generic ancestor name
  # Add comprehensive null and length checks to prevent "argument is of length zero" error
  if (is.null(taxonomic_name) || 
      length(taxonomic_name) == 0 || 
      is.na(taxonomic_name) ||
      nchar(trimws(as.character(taxonomic_name))) <= 2) {
    return(node_info)
  }
  
  # Additional safety checks before using grepl
  taxonomic_name_str <- as.character(taxonomic_name)
  if (length(taxonomic_name_str) == 0 || is.na(taxonomic_name_str) || nchar(trimws(taxonomic_name_str)) == 0) {
    return(node_info)
  }
  
  # Now safe to use grepl with proper string
  if (grepl("^(Ancestor|Node)\\.+[A-Z]$", taxonomic_name_str) || 
      grepl("^Common ancestor", taxonomic_name_str)) {
    return(node_info)
  }
  
  # Try to fetch Unsplash image first (highest priority)
  tryCatch({
    # Check if cached unsplash image API function exists
    if (exists("cached_get_unsplash_random_image")) {
      unsplash_image_result <- cached_get_unsplash_random_image(taxonomic_name, target_width = 200)

      if (unsplash_image_result$success) {
        # Add Unsplash image data to node_info
        unsplash_image_html <- format_unsplash_image_html(unsplash_image_result)
        node_info$unsplash_image_html <- unsplash_image_html
        node_info$unsplash_image_url <- unsplash_image_result$image_url
        node_info$unsplash_image_attribution <- unsplash_image_result$attribution
        node_info$unsplash_image_error <- NULL
      } else {
        # Add failure information to show in info panel
        node_info$unsplash_image_html <- NULL
        node_info$unsplash_image_url <- NULL
        node_info$unsplash_image_attribution <- NULL
        node_info$unsplash_image_error <- paste("Failed to fetch Unsplash image:", unsplash_image_result$error)
      }
    }
  }, error = function(e) {
    # If there's an error, add error information
    api_log_warn(paste("Could not fetch Unsplash image for", taxonomic_name, ":", e$message))
    node_info$unsplash_image_html <- NULL
    node_info$unsplash_image_url <- NULL
    node_info$unsplash_image_attribution <- NULL
    node_info$unsplash_image_error <- paste("Failed to connect to Unsplash Image API:", e$message)
  })

  # Try to fetch Wikipedia text data using the cached Wikipedia API function
  tryCatch({
    # Check if cached wikipedia API function exists
    if (exists("cached_get_wikipedia_intro")) {
      wikipedia_result <- cached_get_wikipedia_intro(taxonomic_name, truncate_length = 250)
      
      if (wikipedia_result$success) {
        # Add Wikipedia data to node_info
        node_info$wikipedia_summary <- wikipedia_result$introduction
        node_info$wikipedia_url <- wikipedia_result$url
        node_info$wikipedia_title <- wikipedia_result$wikipedia_title
        node_info$wikipedia_error <- NULL
      } else {
        # Add failure information to show in info panel
        node_info$wikipedia_summary <- NULL
        node_info$wikipedia_url <- NULL
        node_info$wikipedia_title <- NULL
        node_info$wikipedia_error <- paste("Failed to fetch Wikipedia data:", wikipedia_result$error)
      }
    }
  }, error = function(e) {
    # If there's an error, add error information
    api_log_warn(paste("Could not fetch Wikipedia data for", taxonomic_name, ":", e$message))
    node_info$wikipedia_summary <- NULL
    node_info$wikipedia_url <- NULL
    node_info$wikipedia_title <- NULL
    node_info$wikipedia_error <- paste("Failed to connect to Wikipedia API:", e$message)
  })
  
  # Try to fetch silhouette data using cached PhyloPic function (only if Unsplash image failed)
  if (is.null(node_info$unsplash_image_html) || is.na(node_info$unsplash_image_html) || nchar(as.character(node_info$unsplash_image_html)) == 0) {
    tryCatch({
      # Check if cached phylopic function exists
      if (exists("cached_get_silhouette_data")) {
        silhouette_result <- cached_get_silhouette_data(taxonomic_name)
        
        if (silhouette_result$success) {
          # Format the silhouette HTML and add to node_info
          silhouette_html <- format_silhouette_html(silhouette_result)
          node_info$silhouette_html <- silhouette_html
          node_info$silhouette_uuid <- silhouette_result$uuid
          node_info$silhouette_attribution <- silhouette_result$attribution
          node_info$silhouette_error <- NULL
        } else {
          # Add failure information to show in info panel
          node_info$silhouette_html <- NULL
          node_info$silhouette_uuid <- NULL
          node_info$silhouette_attribution <- NULL
          node_info$silhouette_error <- paste("Failed to fetch silhouette:", silhouette_result$error)
        }
      }
    }, error = function(e) {
      # If there's an error, add error information
      api_log_warn(paste("Could not fetch silhouette data for", taxonomic_name, ":", e$message))
      node_info$silhouette_html <- NULL
      node_info$silhouette_uuid <- NULL
      node_info$silhouette_attribution <- NULL
      node_info$silhouette_error <- paste("Failed to connect to PhyloPic API:", e$message)
    })
  }
  
  return(node_info)
}

# Helper functions for age information formatting

#' Get confidence stars based on age source
#' @param age_source The source of age data
#' @return String with star ratings
get_confidence_stars <- function(age_source) {
  if (is.null(age_source) || is.na(age_source)) {
    return("★☆☆☆ (No source)")
  }
  
  # Different confidence levels based on source
  if (grepl("DateLife chronogram database", age_source, ignore.case = TRUE)) {
    return("★★★☆ (Chronogram database)")
  } else if (grepl("molecular clock", age_source, ignore.case = TRUE)) {
    return("★★★★ (Molecular clock)")
  } else {
    return("★★☆☆ (Other source)")
  }
}

#' Format age source text for display
#' @param age_source The source of age data
#' @return Formatted source text
format_age_source <- function(age_source) {
  # Sources are now cited on the frontend about page, no need to show here
  return("")
}

#' Get geological period for a given age
#' @param age_mya Age in millions of years ago
#' @return Geological period name or NULL
get_geological_period <- function(age_mya) {
  if (is.null(age_mya) || is.na(age_mya) || age_mya <= 0) {
    return(NULL)
  }
  
  # Major geological periods (simplified)
  if (age_mya <= 2.6) return("Quaternary")
  if (age_mya <= 23) return("Neogene")
  if (age_mya <= 66) return("Paleogene")
  if (age_mya <= 145) return("Cretaceous")
  if (age_mya <= 201) return("Jurassic")
  if (age_mya <= 252) return("Triassic")
  if (age_mya <= 299) return("Permian")
  if (age_mya <= 359) return("Carboniferous")
  if (age_mya <= 419) return("Devonian")
  if (age_mya <= 444) return("Silurian")
  if (age_mya <= 485) return("Ordovician")
  if (age_mya <= 541) return("Cambrian")
  return("Precambrian")
}

cat("Info panel system functions loaded successfully\n")