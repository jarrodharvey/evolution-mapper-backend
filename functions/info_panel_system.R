# Info panel system for phylogenetic trees
# Replaces tooltips with clickable info icons and expandable panels
# Mobile-friendly alternative to hover-based tooltips

# Function to lookup scientific name from common name in species database
lookup_scientific_name <- function(common_name) {
  tryCatch({
    # Connect to the species database
    db_path <- "data/species.sqlite"
    if (!file.exists(db_path)) {
      return(NULL)
    }

    library(RSQLite)
    library(DBI)
    conn <- dbConnect(SQLite(), db_path)

    # Try exact match first
    query <- "SELECT scientific FROM species WHERE LOWER(common) = LOWER(?) LIMIT 1"
    result <- dbGetQuery(conn, query, params = list(common_name))

    if (nrow(result) > 0) {
      dbDisconnect(conn)
      return(result$scientific[1])
    }

    # Try singular/plural variations
    # Remove trailing 's' for plural to singular conversion
    singular_name <- gsub("s$", "", common_name, ignore.case = TRUE)
    if (singular_name != common_name) {
      query <- "SELECT scientific FROM species WHERE LOWER(common) = LOWER(?) LIMIT 1"
      result <- dbGetQuery(conn, query, params = list(singular_name))

      if (nrow(result) > 0) {
        dbDisconnect(conn)
        return(result$scientific[1])
      }
    }

    # Try partial match with the root word (simplified - no hardcoded patterns)
    query <- "SELECT scientific FROM species WHERE LOWER(common) LIKE LOWER(?) ORDER BY ott DESC LIMIT 1"
    result <- dbGetQuery(conn, query, params = list(paste0("%", singular_name, "%")))

    if (nrow(result) > 0) {
      dbDisconnect(conn)
      return(result$scientific[1])
    }

    # Try broader partial match with original name
    query <- "SELECT scientific FROM species WHERE LOWER(common) LIKE LOWER(?) ORDER BY ott DESC LIMIT 1"
    result <- dbGetQuery(conn, query, params = list(paste0("%", common_name, "%")))

    if (nrow(result) > 0) {
      dbDisconnect(conn)
      return(result$scientific[1])
    }

    dbDisconnect(conn)
    return(NULL)

  }, error = function(e) {
    if (exists("conn") && dbIsValid(conn)) {
      dbDisconnect(conn)
    }
    return(NULL)
  })
}

# Source Wikipedia API functions
source("functions/wikipedia_api.R")

# Source Wikipedia image API functions
source("functions/wikipedia_images.R")


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

# Check for local image overrides in the image_overrides directory
check_image_override <- function(taxonomic_name) {
  if (is.null(taxonomic_name) || is.na(taxonomic_name) || nchar(trimws(taxonomic_name)) == 0) {
    return(list(success = FALSE, error = "Invalid taxonomic name"))
  }

  # Clean taxonomic name for filename matching
  clean_name <- trimws(as.character(taxonomic_name))

  # Common image extensions to check
  extensions <- c("jpg", "jpeg", "png", "webp", "gif")
  override_dir <- "image_overrides"

  # Check if override directory exists
  if (!dir.exists(override_dir)) {
    return(list(success = FALSE, error = "Override directory does not exist"))
  }

  # Check each possible extension
  for (ext in extensions) {
    file_path <- file.path(override_dir, paste0(clean_name, ".", ext))
    if (file.exists(file_path)) {
      # Return success with file information
      file_info <- file.info(file_path)
      return(list(
        success = TRUE,
        file_path = file_path,
        taxonomic_name = clean_name,
        extension = ext,
        size_bytes = file_info$size,
        modified = file_info$mtime
      ))
    }
  }

  # No override found
  return(list(success = FALSE, error = "No override image found"))
}

# Format override image HTML for display in info panels
format_override_image_html <- function(override_result) {
  if (!override_result$success) {
    return("")
  }

  # Read the image file and encode as base64 data URI
  tryCatch({
    # Read image file as binary
    image_binary <- readBin(override_result$file_path, "raw", n = file.info(override_result$file_path)$size)

    # Convert to base64
    image_base64 <- base64enc::base64encode(image_binary)

    # Determine MIME type based on file extension
    mime_types <- list(
      "jpg" = "image/jpeg",
      "jpeg" = "image/jpeg",
      "png" = "image/png",
      "webp" = "image/webp",
      "gif" = "image/gif"
    )

    mime_type <- mime_types[[override_result$extension]] %||% "image/jpeg"

    # Create data URI
    data_uri <- paste0("data:", mime_type, ";base64,", image_base64)

    # Generate HTML with embedded image and responsive styling (no attribution text)
    image_html <- paste0(
      '<div class="override-image-section">',
      '<div class="override-image-container" style="width: 100%; max-width: 300px; margin: 0 auto;">',
      '<img src="', data_uri, '" ',
      'alt="', override_result$taxonomic_name, ' - Local Override Image" ',
      'class="override-taxonomic-image" ',
      'style="width: 100%; height: auto; max-height: 250px; object-fit: contain; border-radius: 4px; display: block;">',
      '</div>',
      '</div>'
    )

    return(image_html)
  }, error = function(e) {
    api_log_warn(paste("Failed to encode override image as base64:", e$message))
    return("")
  })
}

# Format combined image (Override → Wikimedia → Unsplash → Pixabay → PhyloPic priority) and Wikipedia text section
format_combined_taxonomic_section <- function(node_data) {
  # Check for override image (highest priority)
  has_override_image <- !is.null(node_data$override_image_html) &&
                        !is.na(node_data$override_image_html) &&
                        nchar(as.character(node_data$override_image_html)) > 0

  # Check for Wikimedia image (secondary priority)
  has_wikimedia_image <- !is.null(node_data$wikimedia_image_html) &&
                         !is.na(node_data$wikimedia_image_html) &&
                         nchar(as.character(node_data$wikimedia_image_html)) > 0

  # Check for Unsplash image (tertiary choice)
  has_unsplash_image <- !is.null(node_data$unsplash_image_html) &&
                        !is.na(node_data$unsplash_image_html) &&
                        nchar(as.character(node_data$unsplash_image_html)) > 0

  # Check for Pixabay image (quaternary choice)
  has_pixabay_image <- !is.null(node_data$pixabay_image_html) &&
                       !is.na(node_data$pixabay_image_html) &&
                       nchar(as.character(node_data$pixabay_image_html)) > 0

  # Check for PhyloPic silhouette (final fallback choice)
  has_silhouette <- !is.null(node_data$silhouette_html) &&
                    !is.na(node_data$silhouette_html) &&
                    nchar(as.character(node_data$silhouette_html)) > 0
  
  # Check for Wikipedia text content
  has_wikipedia <- !is.null(node_data$wikipedia_summary) && 
                   !is.na(node_data$wikipedia_summary) && 
                   nchar(as.character(node_data$wikipedia_summary)) > 0
  
  # Check for errors
  has_wikimedia_image_error <- !is.null(node_data$wikimedia_image_error) &&
                               !is.na(node_data$wikimedia_image_error) &&
                               nchar(as.character(node_data$wikimedia_image_error)) > 0

  has_unsplash_image_error <- !is.null(node_data$unsplash_image_error) &&
                              !is.na(node_data$unsplash_image_error) &&
                              nchar(as.character(node_data$unsplash_image_error)) > 0

  has_pixabay_image_error <- !is.null(node_data$pixabay_image_error) &&
                             !is.na(node_data$pixabay_image_error) &&
                             nchar(as.character(node_data$pixabay_image_error)) > 0

  has_silhouette_error <- !is.null(node_data$silhouette_error) &&
                         !is.na(node_data$silhouette_error) &&
                         nchar(as.character(node_data$silhouette_error)) > 0
  
  has_wikipedia_error <- !is.null(node_data$wikipedia_error) && 
                        !is.na(node_data$wikipedia_error) && 
                        nchar(as.character(node_data$wikipedia_error)) > 0
  
  # Show section if we have any data OR errors to display
  if (!has_override_image && !has_wikimedia_image && !has_unsplash_image && !has_pixabay_image && !has_silhouette && !has_wikipedia &&
      !has_wikimedia_image_error && !has_unsplash_image_error && !has_pixabay_image_error && !has_silhouette_error && !has_wikipedia_error) {
    return("")
  }

  # Start the combined section
  combined_html <- '<div class="taxonomic-combined-section">'

  # Priority order: Override → Wikimedia → Unsplash → Pixabay → PhyloPic
  image_content <- ""
  if (has_override_image) {
    # Use override image (highest priority)
    image_content <- node_data$override_image_html
  } else if (has_wikimedia_image) {
    # Use Wikimedia image (secondary priority)
    image_content <- node_data$wikimedia_image_html
  } else if (has_unsplash_image) {
    # Use Unsplash image (tertiary priority)
    image_content <- node_data$unsplash_image_html
  } else if (has_pixabay_image) {
    # Use Pixabay image (quaternary priority)
    image_content <- node_data$pixabay_image_html
  } else if (has_silhouette) {
    # Fallback to PhyloPic silhouette (final fallback)
    image_content <- node_data$silhouette_html
  } else if (has_wikimedia_image_error && has_unsplash_image_error && has_pixabay_image_error && has_silhouette_error) {
    # All four image sources failed - show combined error
    image_content <- paste0(
      '<div class="image-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ No images available<br>',
      'Wikimedia: ', node_data$wikimedia_image_error, '<br>',
      'Unsplash: ', node_data$unsplash_image_error, '<br>',
      'Pixabay: ', node_data$pixabay_image_error, '<br>',
      'PhyloPic: ', node_data$silhouette_error,
      '</div>'
    )
  } else if (has_wikimedia_image_error && has_unsplash_image_error && has_pixabay_image_error) {
    # Three main photo sources failed, PhyloPic may not have been attempted
    image_content <- paste0(
      '<div class="image-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ Photo sources failed<br>',
      'Wikimedia: ', node_data$wikimedia_image_error, '<br>',
      'Unsplash: ', node_data$unsplash_image_error, '<br>',
      'Pixabay: ', node_data$pixabay_image_error,
      '</div>'
    )
  } else if (has_wikimedia_image_error) {
    # Wikimedia image failed, others may not have been attempted
    image_content <- paste0(
      '<div class="image-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ ', node_data$wikimedia_image_error,
      '</div>'
    )
  } else if (has_unsplash_image_error) {
    # Unsplash image failed, others may not have been attempted
    image_content <- paste0(
      '<div class="image-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ ', node_data$unsplash_image_error,
      '</div>'
    )
  } else if (has_pixabay_image_error) {
    # Pixabay image failed, PhyloPic may not have been attempted
    image_content <- paste0(
      '<div class="image-error" style="color: #e74c3c; font-size: 12px; font-style: italic; padding: 8px; border: 1px solid #e74c3c; border-radius: 4px; background-color: #fdf2f2;">',
      '⚠️ ', node_data$pixabay_image_error,
      '</div>'
    )
  } else if (has_silhouette_error) {
    # PhyloPic failed, photo sources may not have been attempted
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
  has_any_image <- has_override_image || has_wikimedia_image || has_unsplash_image || has_pixabay_image || has_silhouette || has_wikimedia_image_error || has_unsplash_image_error || has_pixabay_image_error || has_silhouette_error
  has_any_text <- has_wikipedia || has_wikipedia_error
  
  if (has_any_image && has_any_text) {
    # Both image and text - flexbox layout with proper spacing
    combined_html <- paste0(combined_html,
      '<div class="taxonomic-wrapped-container">',
      '<div class="image-float">',
      image_content,
      '</div>',
      '<div class="wikipedia-text-column">',
      wikipedia_content,
      '</div>',
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


# Generate CSS styles for the info panel system
generate_info_panel_css <- function(panel_width = 800, text_column_width = 350) {
  return(paste0('
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
  position: fixed !important;
  background: white !important;
  border: 2px solid #3498db !important;
  border-radius: 8px !important;
  box-shadow: 0 4px 15px rgba(0,0,0,0.15) !important;
  z-index: 1000 !important;
  width: ', panel_width, 'px !important;
  max-height: 400px !important;
  overflow-y: auto !important;
  box-sizing: content-box !important;
  /* Position and transform will be set dynamically by JavaScript to center on SVG */
  /* The transform: translate(-50%, -50%) is key for perfect centering */
}

/* Legacy positioning classes kept for fallback compatibility */
.info-panel.position-above {
  top: auto;
  bottom: 25px;
}

.info-panel.position-left {
  left: -540px; /* panel width (520px) + some padding */
}

.info-panel.position-right {
  left: 25px; /* position to the right of the icon */
}

.info-panel.position-center {
  left: -260px; /* center the panel (half of 520px width) */
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

/* New wrapped layout for images + Wikipedia - CSS Grid with auto-width optimization */
.taxonomic-wrapped-container {
  display: grid !important;
  grid-template-columns: 200px ', text_column_width, 'px !important;
  gap: 12px !important;
  align-items: start !important;
  box-sizing: border-box !important; /* Include padding in width calculations */
  overflow: hidden !important; /* Prevent content overflow */
}

.silhouette-float {
  grid-column: 1;
}

.silhouette-float img {
  width: 200px;
  height: 200px;
  object-fit: cover;
  border-radius: 4px;
  display: block;
}

.wikipedia-summary {
  text-align: left !important;
  word-wrap: break-word !important;
  overflow-wrap: break-word !important;
  margin: 0 !important;
  font-size: 13px !important;
  line-height: 1.4 !important;
  color: #495057 !important;
  flex: 1 !important;
  overflow: hidden !important;
  padding-right: 8px !important;
  box-sizing: content-box !important;
  white-space: normal !important;
}

.wikipedia-link {
  color: #3498db;
  text-decoration: none;
  font-size: 12px;
  margin-top: 8px;
  flex-shrink: 0;
}

.wikipedia-link:hover {
  text-decoration: underline;
}

.wikipedia-text-column {
  height: 200px !important;
  display: flex !important;
  flex-direction: column !important;
  width: ', text_column_width, 'px !important; /* Pre-calculated width based on content */
  box-sizing: border-box !important; /* Include padding in width calculations */
  min-width: unset !important;
  max-width: unset !important;
  overflow: hidden !important; /* Prevent content from overflowing */
  word-wrap: break-word !important;
  overflow-wrap: break-word !important;
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
  width: 100%;
  box-sizing: border-box;
}

.unsplash-taxonomic-image {
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

/* Images in combined layout */
.image-float {
  flex: 0 0 auto;
  width: auto;
  max-width: 200px;
  margin-right: 0;
  margin-bottom: 0;
}

.image-float .unsplash-image-container,
.image-float .wikipedia-image-container,
.image-float .silhouette-container {
  margin: 0;
  padding: 8px;
  background: #f8f9fa;
  border-radius: 4px;
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
    min-width: 400px;
    max-width: 600px;
    max-height: 350px;
  }

  .taxonomic-wrapped-container .wikipedia-summary {
    max-height: 180px;
    font-size: 12px;
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
    min-width: 350px;
    max-width: 450px;
    max-height: 300px;
  }

  .taxonomic-wrapped-container {
    flex-direction: column;
    min-height: auto;
  }

  .taxonomic-wrapped-container .wikipedia-summary {
    max-height: 150px;
    font-size: 12px;
  }
  
  .info-panel-content h4 {
    font-size: 15px;
  }
  
  .age-main {
    font-size: 14px;
  }
}
</style>
  '))
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
    api_log_info(paste("[", request_id, "] Processing", length(taxonomic_indices), "taxonomic nodes sequentially (Override → Wikimedia → Unsplash → Pixabay → PhyloPic → Wikipedia...)"))

    wikipedia_successes <- 0
    override_successes <- 0
    wikimedia_successes <- 0
    unsplash_image_successes <- 0
    pixabay_successes <- 0
    phylopic_successes <- 0
    unsplash_image_errors <- c()
    pixabay_errors <- c()
    wikimedia_errors <- c()
    phylopic_errors <- c()
    processed_taxonomic_names <- c()

    sequential_start <- Sys.time()

    # Sequential processing: Override → Wikimedia → Unsplash → Pixabay → PhyloPic → Wikipedia
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
      override_success <- FALSE
      node_start_time <- Sys.time()

      # 0. Check for local image override first (highest priority)
      api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Checking for image override:", taxonomic_name))
      override_start <- Sys.time()
      tryCatch({
        override_result <- check_image_override(taxonomic_name)
        if (override_result$success) {
          override_image_html <- format_override_image_html(override_result)
          if (!is.null(override_image_html) && nchar(override_image_html) > 0) {
            node_info$override_image_html <- override_image_html
            node_info$override_image_path <- override_result$file_path
            node_info$override_image_attribution <- paste("Local Override •", taxonomic_name)
            override_success <- TRUE
            override_successes <- override_successes + 1
            api_log_info(paste("[", request_id, "] Override image found for:", taxonomic_name, "-", override_result$file_path, "(", round(as.numeric(difftime(Sys.time(), override_start, units = "secs")), 3), "s)"))
          } else {
            api_log_warn(paste("[", request_id, "] Override image empty result for:", taxonomic_name))
          }
        } else {
          api_log_info(paste("[", request_id, "] No override image for:", taxonomic_name, "-", override_result$error))
        }
      }, error = function(e) {
        api_log_warn(paste("[", request_id, "] Override image check error for:", taxonomic_name, "-", e$message))
      })

      # 1. Get species name from ChatGPT (only if no override found)
      search_species_name <- taxonomic_name  # Default fallback
      chatgpt_success <- FALSE
      if (!override_success) {
        api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Getting species name for:", taxonomic_name, "(no override found)"))
        chatgpt_start <- Sys.time()
        tryCatch({
          if (exists("cached_get_chatgpt_common_name")) {
            chatgpt_result <- cached_get_chatgpt_common_name(taxonomic_name)
            if (chatgpt_result$success && !is.null(chatgpt_result$common_name)) {
              search_species_name <- chatgpt_result$common_name
              chatgpt_success <- TRUE

              # Sanitize the species name for safe logging using centralized function
              safe_species_name <- sanitize_log_message(search_species_name)

              api_log_info(paste("[", request_id, "] ChatGPT species name success for:", taxonomic_name, "->", safe_species_name, "(", round(as.numeric(difftime(Sys.time(), chatgpt_start, units = "secs")), 3), "s)"))
            } else {
              api_log_warn(paste("[", request_id, "] ChatGPT species name failed for:", taxonomic_name, "-", chatgpt_result$error))
            }
          } else {
            api_log_warn(paste("[", request_id, "] ChatGPT function not available for:", taxonomic_name))
          }
        }, error = function(e) {
          api_log_error(paste("[", request_id, "] ChatGPT species name error for:", taxonomic_name, "-", e$message))
        })
      }

      # 2. Wikipedia/Wikimedia Image API call (using species name from ChatGPT)
      wikimedia_success <- FALSE
      if (!override_success) {
        # For Wikimedia searches, try to use scientific name from database lookup
        # If no scientific name found, skip Wikimedia and go to Unsplash
        wikimedia_search_term <- NULL
        skip_wikimedia <- FALSE

        if (chatgpt_success) {
          # Look up scientific name from the species database
          scientific_name <- lookup_scientific_name(search_species_name)
          if (!is.null(scientific_name)) {
            wikimedia_search_term <- scientific_name
            api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching Wikimedia image for:", sanitize_log_message(scientific_name), "(using scientific name from database lookup of", sanitize_log_message(search_species_name), ")"))
          } else {
            skip_wikimedia <- TRUE
            api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping Wikimedia - no scientific name found in database for:", sanitize_log_message(search_species_name), "(will try Unsplash with common name)"))
          }
        } else {
          # For taxonomic names, use them directly for Wikimedia
          wikimedia_search_term <- search_species_name
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching Wikimedia image for:", sanitize_log_message(search_species_name), "(using taxonomic name)"))
        }

        if (!skip_wikimedia) {
          wikimedia_start <- Sys.time()
          tryCatch({
            if (exists("cached_get_wikimedia_image_enhanced")) {
              wikimedia_result <- cached_get_wikimedia_image_enhanced(wikimedia_search_term, target_width = 200)
            if (wikimedia_result$success) {
              wikimedia_image_html <- format_wikimedia_image_html(wikimedia_result)
              if (!is.null(wikimedia_image_html) && nchar(wikimedia_image_html) > 0) {
                node_info$wikimedia_image_html <- wikimedia_image_html
                node_info$wikimedia_image_url <- wikimedia_result$image_url
                node_info$wikimedia_image_attribution <- wikimedia_result$attribution
                node_info$wikimedia_entity_id <- wikimedia_result$entity_id
                node_info$wikimedia_entity_label <- wikimedia_result$entity_label
                wikimedia_success <- TRUE
                wikimedia_successes <- wikimedia_successes + 1
                api_log_info(paste("[", request_id, "] Wikimedia image success for:", taxonomic_name, "(", round(as.numeric(difftime(Sys.time(), wikimedia_start, units = "secs")), 3), "s)"))
              } else {
                node_info$wikimedia_image_error <- "Empty Wikimedia image HTML generated"
                wikimedia_errors <- c(wikimedia_errors, paste(taxonomic_name, ": Empty HTML"))
                api_log_info(paste("[", request_id, "] Wikimedia image empty result for:", taxonomic_name))
              }
            } else {
              node_info$wikimedia_image_error <- paste("Wikimedia image failed:", wikimedia_result$error)
              wikimedia_errors <- c(wikimedia_errors, paste(taxonomic_name, ":", wikimedia_result$error))
              api_log_info(paste("[", request_id, "] Wikimedia image failed for:", taxonomic_name, "-", wikimedia_result$error))
            }
          }
        }, error = function(e) {
          node_info$wikimedia_image_error <- paste("Wikimedia image error:", e$message)
          wikimedia_errors <- c(wikimedia_errors, paste(taxonomic_name, ":", e$message))
          api_log_error(paste("[", request_id, "] Wikimedia image error for:", taxonomic_name, "-", e$message))
        })
        } # End of if (!skip_wikimedia)
      } else {
        api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping Wikimedia - override image available for:", taxonomic_name))
      }

      # 3. Unsplash Image API call (only if both override and Wikimedia failed OR Wikimedia was skipped)
      if (!override_success && (!wikimedia_success || skip_wikimedia)) {
        if (skip_wikimedia) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching Unsplash image for:", sanitize_log_message(search_species_name), "(override not found, Wikimedia skipped due to database lookup failure)"))
        } else {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching Unsplash image for:", sanitize_log_message(search_species_name), "(override and Wikimedia unavailable)"))
        }
        unsplash_image_start <- Sys.time()
        tryCatch({
        if (exists("cached_get_unsplash_random_image")) {
          # Skip ChatGPT conversion since search_species_name is already a ChatGPT-generated common name
          unsplash_image_result <- cached_get_unsplash_random_image(search_species_name, target_width = 200, skip_chatgpt_conversion = TRUE)
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
      } else {
        if (override_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping Unsplash - override image available for:", taxonomic_name))
        } else if (wikimedia_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping Unsplash - Wikimedia image available for:", taxonomic_name))
        }
      }

      # 4. Pixabay API call as tertiary fallback (only if override, Wikimedia and Unsplash all failed)
      pixabay_success <- FALSE
      if (!override_success && !wikimedia_success && !unsplash_image_success) {
        api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching Pixabay image for:", sanitize_log_message(search_species_name), "(override, Wikimedia and Unsplash unavailable)"))
        pixabay_start <- Sys.time()
        tryCatch({
          if (exists("cached_get_pixabay_random_image")) {
            # Skip ChatGPT conversion since search_species_name is already a ChatGPT-generated common name
            pixabay_result <- cached_get_pixabay_random_image(search_species_name, target_width = 200, skip_chatgpt_conversion = TRUE)
            if (pixabay_result$success) {
              pixabay_image_html <- format_pixabay_image_html(pixabay_result)
              if (!is.null(pixabay_image_html) && nchar(pixabay_image_html) > 0) {
                node_info$pixabay_image_html <- pixabay_image_html
                node_info$pixabay_image_url <- pixabay_result$image_url
                node_info$pixabay_image_attribution <- pixabay_result$attribution
                pixabay_success <- TRUE
                pixabay_successes <- pixabay_successes + 1
                api_log_info(paste("[", request_id, "] Pixabay image success for:", taxonomic_name, "(", round(as.numeric(difftime(Sys.time(), pixabay_start, units = "secs")), 3), "s)"))
              } else {
                node_info$pixabay_image_error <- "Empty Pixabay image HTML generated"
                pixabay_errors <- c(pixabay_errors, paste(taxonomic_name, ": Empty HTML"))
                api_log_info(paste("[", request_id, "] Pixabay image empty result for:", taxonomic_name))
              }
            } else {
              node_info$pixabay_image_error <- paste("Pixabay image failed:", pixabay_result$error)
              pixabay_errors <- c(pixabay_errors, paste(taxonomic_name, ":", pixabay_result$error))
              api_log_info(paste("[", request_id, "] Pixabay image failed for:", taxonomic_name, "-", pixabay_result$error))
            }
          }
        }, error = function(e) {
          node_info$pixabay_image_error <- paste("Pixabay image error:", e$message)
          pixabay_errors <- c(pixabay_errors, paste(taxonomic_name, ":", e$message))
          api_log_error(paste("[", request_id, "] Pixabay image error for:", taxonomic_name, "-", e$message))
        })
      } else {
        if (override_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping Pixabay - override image available for:", taxonomic_name))
        } else if (wikimedia_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping Pixabay - Wikimedia image available for:", taxonomic_name))
        } else if (unsplash_image_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping Pixabay - Unsplash image available for:", taxonomic_name))
        }
      }

      # 4. PhyloPic API call as final fallback (only if override, Wikimedia, Unsplash and Pixabay all failed)
      if (!override_success && !wikimedia_success && !unsplash_image_success && !pixabay_success) {
        api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Fetching PhyloPic data for:", taxonomic_name, "(override, Wikimedia, Unsplash and Pixabay unavailable)"))
      } else {
        if (override_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping PhyloPic - override image available for:", taxonomic_name))
        } else if (wikimedia_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping PhyloPic - Wikimedia image available for:", taxonomic_name))
        } else if (unsplash_image_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping PhyloPic - Unsplash image available for:", taxonomic_name))
        } else if (pixabay_success) {
          api_log_info(paste("[", request_id, "] [", i, "/", length(taxonomic_node_info), "] Skipping PhyloPic - Pixabay image available for:", taxonomic_name))
        }
      }

      phylopic_start <- Sys.time()
      if (!override_success && !wikimedia_success && !unsplash_image_success && !pixabay_success) {
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
          wikipedia_result <- cached_get_wikipedia_intro(taxonomic_name, truncate_length = 800)
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
                                  override_successes = override_successes,
                                  wikimedia_successes = wikimedia_successes,
                                  unsplash_image_successes = unsplash_image_successes,
                                  pixabay_successes = pixabay_successes,
                                  phylopic_successes = phylopic_successes))
    }
    
    sequential_duration <- as.numeric(difftime(Sys.time(), sequential_start, units = "secs"))
    api_log_info(paste("[", request_id, "] Sequential processing completed - Duration:", round(sequential_duration, 3), "s"))
    
    api_log_info(paste("[", request_id, "] Sequential processing results:", sep=""))
    api_log_info(paste("[", request_id, "]   Taxonomic names processed:", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   Override image successes:", override_successes, "/", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   Wikimedia image successes:", wikimedia_successes, "/", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   Wikipedia text successes:", wikipedia_successes, "/", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   Unsplash image successes:", unsplash_image_successes, "/", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   Pixabay image successes:", pixabay_successes, "/", length(processed_taxonomic_names)))
    api_log_info(paste("[", request_id, "]   PhyloPic successes:", phylopic_successes, "/", length(processed_taxonomic_names)))
    if (length(wikimedia_errors) > 0) {
      api_log_info(paste("[", request_id, "]   Wikimedia image errors:", paste(head(wikimedia_errors, 3), collapse = "; "), if(length(wikimedia_errors) > 3) '...' else ''))
    }
    if (length(unsplash_image_errors) > 0) {
      api_log_info(paste("[", request_id, "]   Unsplash image errors:", paste(head(unsplash_image_errors, 3), collapse = "; "), if(length(unsplash_image_errors) > 3) '...' else ''))
    }
    if (length(pixabay_errors) > 0) {
      api_log_info(paste("[", request_id, "]   Pixabay image errors:", paste(head(pixabay_errors, 3), collapse = "; "), if(length(pixabay_errors) > 3) '...' else ''))
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
                                override_successes = override_successes,
                                wikimedia_successes = wikimedia_successes,
                                unsplash_image_successes = unsplash_image_successes,
                                pixabay_successes = pixabay_successes,
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
  
  # Try to fetch Wikimedia image first (highest priority)
  tryCatch({
    # Check if cached wikimedia image API function exists
    if (exists("cached_get_wikimedia_image_enhanced")) {
      wikimedia_image_result <- cached_get_wikimedia_image_enhanced(taxonomic_name, target_width = 200)

      if (wikimedia_image_result$success) {
        # Add Wikimedia image data to node_info
        wikimedia_image_html <- format_wikimedia_image_html(wikimedia_image_result)
        node_info$wikimedia_image_html <- wikimedia_image_html
        node_info$wikimedia_image_url <- wikimedia_image_result$image_url
        node_info$wikimedia_image_attribution <- wikimedia_image_result$attribution
        node_info$wikimedia_image_error <- NULL
      } else {
        # Add failure information to show in info panel
        node_info$wikimedia_image_html <- NULL
        node_info$wikimedia_image_url <- NULL
        node_info$wikimedia_image_attribution <- NULL
        node_info$wikimedia_image_error <- paste("Failed to fetch Wikimedia image:", wikimedia_image_result$error)
      }
    }
  }, error = function(e) {
    # If there's an error, add error information
    api_log_warn(paste("Could not fetch Wikimedia image for", taxonomic_name, ":", e$message))
    node_info$wikimedia_image_html <- NULL
    node_info$wikimedia_image_url <- NULL
    node_info$wikimedia_image_attribution <- NULL
    node_info$wikimedia_image_error <- paste("Failed to connect to Wikimedia Image API:", e$message)
  })

  # Try to fetch Wikipedia text data using the cached Wikipedia API function
  tryCatch({
    # Check if cached wikipedia API function exists
    if (exists("cached_get_wikipedia_intro")) {
      wikipedia_result <- cached_get_wikipedia_intro(taxonomic_name, truncate_length = 800)
      
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
  
  # Try to fetch silhouette data using cached PhyloPic function (only if Wikimedia image failed)
  if (is.null(node_info$wikimedia_image_html) || is.na(node_info$wikimedia_image_html) || nchar(as.character(node_info$wikimedia_image_html)) == 0) {
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

# Pre-calculated dimension functions for info panels
# Eliminates race conditions by calculating dimensions server-side

# Calculate optimal text column width based on character count
# Uses formula from font metrics research: Arial 13px, line-height 1.4
calculate_text_column_width <- function(character_count) {
  if (is.null(character_count) || is.na(character_count) || character_count <= 0) {
    return(300)  # Default width for empty or invalid input
  }

  # Formula: Total Characters * 0.897
  # Based on: 9 lines max, 7.02px average character width, 15% safety margin
  calculated_width <- character_count * 0.897

  # Apply min/max constraints for reasonable layout
  min_width <- 250
  max_width <- 600

  final_width <- max(min_width, min(max_width, calculated_width))

  # Round to nearest 10px for cleaner CSS
  return(round(final_width / 10) * 10)
}

# Calculate total info panel width from components
calculate_total_panel_width <- function(text_column_width) {
  image_width <- 200  # Fixed image width
  gap_width <- 12     # Gap between image and text
  padding <- 40       # Panel padding (20px each side)
  margin <- 24        # Additional margins and borders

  total_width <- image_width + text_column_width + gap_width + padding + margin

  # Apply reasonable max width constraint
  max_panel_width <- 1200

  return(min(total_width, max_panel_width))
}

# Extract dimensions from API data for pre-calculation
extract_content_dimensions <- function(wikipedia_intro, image_data = NULL) {
  # Calculate text width from Wikipedia character count
  text_length <- if (!is.null(wikipedia_intro) && is.character(wikipedia_intro)) {
    nchar(wikipedia_intro)
  } else {
    300  # Default character count assumption
  }

  text_width <- calculate_text_column_width(text_length)

  # Image dimensions (Unsplash already provides these, but we fix at 200px for layout)
  image_width <- 200
  image_height <- 200

  # Calculate total panel dimensions
  panel_width <- calculate_total_panel_width(text_width)

  return(list(
    text_column_width = text_width,
    image_width = image_width,
    image_height = image_height,
    panel_width = panel_width,
    character_count = text_length
  ))
}

cat("Info panel system functions loaded successfully\n")