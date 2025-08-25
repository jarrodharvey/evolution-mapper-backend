# Info panel system for phylogenetic trees
# Replaces tooltips with clickable info icons and expandable panels
# Mobile-friendly alternative to hover-based tooltips

source("functions/age_tooltips.R")

# Generate info panel HTML for ancestor nodes
generate_info_panel_html <- function(node_data) {
  node_name <- if (is.list(node_data) && "to" %in% names(node_data)) {
    node_data$to
  } else if (is.character(node_data)) {
    node_data
  } else {
    "Unknown node"
  }
  
  # Only show info panels for ancestor nodes (not species)
  if (is.list(node_data) && "NodeType" %in% names(node_data) && node_data$NodeType == "species") {
    return("")  # No info panel for species
  }
  
  # Get the tooltip content but without HTML formatting
  tooltip_content <- generate_age_tooltip(node_data)
  
  # Convert HTML tooltip to plain text content for the panel
  panel_content <- gsub("<br>", "\n", tooltip_content)
  panel_content <- gsub("<strong>([^<]*)</strong>", "\\1", panel_content)
  panel_content <- gsub("<em>([^<]*)</em>", "\\1", panel_content)
  panel_content <- gsub("<small>([^<]*)</small>", "\\1", panel_content)
  panel_content <- gsub("<[^>]*>", "", panel_content)  # Remove any remaining HTML tags
  
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
  node_name <- if (is.list(node_data) && "to" %in% names(node_data)) {
    node_data$to
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
      
      return(paste0(
        '<h4>', node_name, '</h4>',
        '<p class="ancestor-type">Evolutionary ancestor</p>',
        '<p class="age-unavailable">Age data unavailable</p>',
        '<p class="age-reason">', reason, '</p>'
      ))
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
    
    # Add confidence and source information
    content_html <- paste0(content_html,
      '<div class="confidence-info">',
      '<p class="confidence-stars">', confidence_stars, '</p>',
      '<p class="data-source">', source_text, '</p>',
      '</div>',
      '</div>'
    )
    
    return(content_html)
  }
  
  # Fallback for unknown node types
  return(paste0(
    '<h4>', node_name, '</h4>',
    '<p class="ancestor-type">Unknown ancestor</p>',
    '<p>Limited information available for this node.</p>'
  ))
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
  position: absolute;
  top: 25px;
  left: -150px;
  width: 320px;
  background: white;
  border: 2px solid #3498db;
  border-radius: 8px;
  box-shadow: 0 4px 15px rgba(0,0,0,0.15);
  z-index: 1000;
  max-height: 400px;
  overflow-y: auto;
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
    left: -120px;
    width: 280px;
    max-height: 300px;
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
    left: -100px;
    width: 240px;
    max-height: 250px;
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
      // Small delay to prevent immediate closing from this click
      setTimeout(() => {
        document.addEventListener("click", handleClickOutside);
      }, 10);
    }
  }
}

function closeInfoPanel(buttonElement) {
  const panel = buttonElement.closest(".info-panel");
  if (panel) {
    panel.style.display = "none";
  }
  document.removeEventListener("click", handleClickOutside);
}

function closeAllInfoPanels() {
  const allPanels = document.querySelectorAll(".info-panel");
  allPanels.forEach(panel => {
    panel.style.display = "none";
  });
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

console.log("Info panel system initialized");
</script>
  ')
}

# Create info panel data for network
create_info_panel_data <- function(network_data) {
  if (!"to" %in% names(network_data)) {
    return(rep("", nrow(network_data)))
  }
  
  info_panel_data <- character(nrow(network_data))
  
  for (i in 1:nrow(network_data)) {
    node_info <- as.list(network_data[i, ])
    
    # Only show info panels for ancestor nodes (not species)
    if ("NodeType" %in% names(node_info) && node_info$NodeType == "species") {
      info_panel_data[i] <- ""  # No info panel for species
    } else {
      # Return just the clean panel content, not the full widget HTML
      info_panel_data[i] <- format_panel_content(node_info)
    }
  }
  
  return(info_panel_data)
}

cat("Info panel system functions loaded successfully\n")