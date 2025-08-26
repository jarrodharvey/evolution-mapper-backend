# Info panel system for phylogenetic trees
# Replaces tooltips with clickable info icons and expandable panels
# Mobile-friendly alternative to hover-based tooltips

# Source Wikipedia API functions
source("functions/wikipedia_api.R")



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
      
      # Add Wikipedia section for taxonomic nodes even without age data
      if (is.list(node_data) && "NodeType" %in% names(node_data) && node_data$NodeType == "taxonomic") {
        content_html <- paste0(content_html, format_wikipedia_section(node_data))
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
    
    # Add confidence and source information
    content_html <- paste0(content_html,
      '<div class="confidence-info">',
      '<p class="confidence-stars">', confidence_stars, '</p>',
      '<p class="data-source">', source_text, '</p>',
      '</div>',
      '</div>'
    )
    
    # Add Wikipedia section for taxonomic nodes
    if (is.list(node_data) && "NodeType" %in% names(node_data) && node_data$NodeType == "taxonomic") {
      content_html <- paste0(content_html, format_wikipedia_section(node_data))
    }
    
    return(content_html)
  }
  
  # Fallback for unknown node types
  content_html <- paste0(
    '<h4>', node_name, '</h4>',
    '<p class="ancestor-type">Unknown ancestor</p>',
    '<p>Limited information available for this node.</p>'
  )
  
  # Add Wikipedia section for taxonomic nodes even in fallback case
  if (is.list(node_data) && "NodeType" %in% names(node_data) && node_data$NodeType == "taxonomic") {
    content_html <- paste0(content_html, format_wikipedia_section(node_data))
  }
  
  return(content_html)
}

# Format Wikipedia section for taxonomic nodes
format_wikipedia_section <- function(node_data) {
  # Check if Wikipedia data is available
  has_wikipedia <- !is.null(node_data$wikipedia_summary) && 
                   !is.na(node_data$wikipedia_summary) && 
                   nchar(as.character(node_data$wikipedia_summary)) > 0
  
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
  } else {
    # Show message that Wikipedia data is not available
    wikipedia_html <- paste0(
      '<div class="wikipedia-section">',
      '<div class="wikipedia-content">',
      '<p class="wikipedia-error" style="color: #95a5a6; font-size: 13px; font-style: italic;">',
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

.info-panel.position-above {
  top: auto;
  bottom: 25px;
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
      // Position the panel intelligently to avoid viewport cutoff
      positionPanelInViewport(panel, iconElement);
      
      // Small delay to prevent immediate closing from this click
      setTimeout(() => {
        document.addEventListener("click", handleClickOutside);
      }, 10);
      
      // No need to fetch Wikipedia data - it is already embedded server-side
    }
  }
}

function positionPanelInViewport(panel, iconElement) {
  // Remove any existing positioning classes
  panel.classList.remove("position-above");
  
  // Get panel and viewport dimensions
  const panelHeight = 400; // max-height from CSS
  const iconRect = iconElement.getBoundingClientRect();
  const viewportHeight = window.innerHeight;
  
  // Calculate space below and above the icon
  const spaceBelow = viewportHeight - iconRect.bottom;
  const spaceAbove = iconRect.top;
  
  // If there is not enough space below (with some padding) and more space above, position above
  if (spaceBelow < panelHeight + 50 && spaceAbove > spaceBelow) {
    panel.classList.add("position-above");
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

console.log("Info panel system initialized with server-side Wikipedia integration");
</script>
  ')
}

# Create info panel data for network
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
      # Add Wikipedia data for taxonomic nodes
      if ("NodeType" %in% names(node_info) && node_info$NodeType == "taxonomic") {
        node_info <- add_wikipedia_data(node_info)
      }
      
      # Return just the clean panel content, not the full widget HTML
      info_panel_data[i] <- format_panel_content(node_info)
    }
  }
  
  return(info_panel_data)
}

# Add Wikipedia data to taxonomic nodes
add_wikipedia_data <- function(node_info) {
  # Get the taxonomic group name
  taxonomic_name <- if ("to" %in% names(node_info)) {
    node_info$to
  } else if ("Child" %in% names(node_info)) {
    node_info$Child
  } else {
    return(node_info)  # Return unchanged if no name found
  }
  
  # Try to fetch Wikipedia data using the existing Wikipedia API function
  tryCatch({
    # Check if wikipedia API function exists
    if (exists("get_wikipedia_intro")) {
      wikipedia_result <- get_wikipedia_intro(taxonomic_name, truncate_length = 250)
      
      if (wikipedia_result$success) {
        # Add Wikipedia data to node_info
        node_info$wikipedia_summary <- wikipedia_result$introduction
        node_info$wikipedia_url <- wikipedia_result$url
        node_info$wikipedia_title <- wikipedia_result$wikipedia_title
      } else {
        # Add empty Wikipedia data to indicate we tried but failed
        node_info$wikipedia_summary <- NULL
        node_info$wikipedia_url <- NULL
        node_info$wikipedia_title <- NULL
      }
    }
  }, error = function(e) {
    # If there's an error, just don't add Wikipedia data
    cat("Warning: Could not fetch Wikipedia data for", taxonomic_name, ":", e$message, "\n")
  })
  
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
  if (is.null(age_source) || is.na(age_source)) {
    return("Source: Unknown")
  }
  return(paste("Source:", age_source))
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