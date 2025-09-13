# Unsplash Image API Functions
# Functions for fetching random images from Unsplash for taxonomic groups
# Used as primary image source with PhyloPic silhouettes as fallback

library(httr2)
library(base64enc)

# Source shared logging configuration
source("functions/logging_config.R")

# Main function to fetch random Unsplash image for a taxonomic group
get_unsplash_random_image <- function(taxonomic_group, target_width = 800) {
  if (is.null(taxonomic_group) || is.na(taxonomic_group) || taxonomic_group == "") {
    return(list(success = FALSE, error = "No taxonomic group provided"))
  }

  tryCatch({
    # Get Unsplash access key from environment
    access_key <- Sys.getenv("UNSPLASH_ACCESS_KEY")
    if (access_key == "") {
      return(list(
        success = FALSE,
        error = "Unsplash access key not configured",
        taxonomic_group = taxonomic_group
      ))
    }

    # Clean and format the taxonomic group name for Unsplash search
    search_query <- clean_taxonomic_name_for_unsplash(taxonomic_group)

    # Make API request to Unsplash
    api_url <- "https://api.unsplash.com/photos/random"

    response <- request(api_url) |>
      req_url_query(
        query = search_query,
        orientation = "landscape",  # Prefer landscape for better panel display
        content_filter = "high"     # High quality content filter
      ) |>
      req_headers(
        Authorization = paste("Client-ID", access_key),
        `Accept-Version` = "v1"
      ) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_timeout(15) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    if (resp_status(response) != 200) {
      error_msg <- if (resp_status(response) == 403) {
        "Rate limit exceeded or access denied"
      } else if (resp_status(response) == 404) {
        paste("No images found for:", search_query)
      } else {
        paste("Unsplash API error:", resp_status(response))
      }

      return(list(
        success = FALSE,
        error = error_msg,
        taxonomic_group = taxonomic_group,
        search_query = search_query,
        status_code = resp_status(response)
      ))
    }

    image_data <- resp_body_json(response)

    # Extract image information
    image_url <- get_sized_image_url(image_data$urls, target_width)

    # Create attribution as required by Unsplash
    attribution <- format_unsplash_attribution(image_data)

    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group,
      search_query = search_query,
      image_url = image_url,
      image_width = image_data$width,
      image_height = image_data$height,
      photographer_name = image_data$user$name,
      photographer_username = image_data$user$username,
      photographer_url = image_data$user$links$html,
      unsplash_url = image_data$links$html,
      attribution = attribution,
      image_id = image_data$id,
      alt_description = image_data$alt_description %||% paste("Image of", taxonomic_group)
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error fetching Unsplash image:", conditionMessage(e)),
      taxonomic_group = taxonomic_group
    ))
  })
}

# Clean taxonomic name for Unsplash search
clean_taxonomic_name_for_unsplash <- function(name) {
  # Remove common prefixes and suffixes that might confuse search
  cleaned <- gsub("^(Order|Family|Class|Genus|Species)\\s+", "", name, ignore.case = TRUE)
  cleaned <- gsub("\\s+(group|clade)$", "", cleaned, ignore.case = TRUE)
  cleaned <- trimws(cleaned)

  # For better Unsplash results, add context for broad taxonomic groups
  if (cleaned %in% c("Mammalia", "Mammals")) {
    return("mammals wildlife")
  } else if (cleaned %in% c("Aves", "Birds")) {
    return("birds wildlife")
  } else if (cleaned %in% c("Reptilia", "Reptiles")) {
    return("reptiles wildlife")
  } else if (cleaned %in% c("Amphibia", "Amphibians")) {
    return("amphibians wildlife")
  } else if (cleaned %in% c("Actinopterygii", "Fish")) {
    return("fish underwater")
  } else if (cleaned %in% c("Insecta", "Insects")) {
    return("insects nature")
  } else if (cleaned %in% c("Plantae", "Plants")) {
    return("plants nature")
  } else if (cleaned %in% c("Fungi")) {
    return("fungi mushrooms nature")
  } else {
    # For more specific taxonomic groups, combine with nature/wildlife
    return(paste(cleaned, "wildlife nature"))
  }
}

# Get appropriately sized image URL from Unsplash URLs object
get_sized_image_url <- function(urls, target_width = 800) {
  # Unsplash provides multiple image sizes
  # Choose the most appropriate size based on target width

  if (target_width <= 200 && !is.null(urls$thumb)) {
    return(urls$thumb)  # 200x200
  } else if (target_width <= 400 && !is.null(urls$small)) {
    return(urls$small)  # 400x varies
  } else if (target_width <= 1080 && !is.null(urls$regular)) {
    return(urls$regular)  # 1080x varies
  } else {
    # For larger sizes or fallback, use the custom width parameter
    base_url <- urls$raw %||% urls$full
    if (!is.null(base_url)) {
      return(paste0(base_url, "&w=", target_width, "&fit=max"))
    } else {
      return(urls$regular %||% urls$small %||% urls$thumb)
    }
  }
}

# Format attribution as required by Unsplash terms
format_unsplash_attribution <- function(image_data) {
  photographer_name <- image_data$user$name %||% "Unknown photographer"
  photographer_username <- image_data$user$username %||% ""

  if (photographer_username != "") {
    return(paste0("Photo by ", photographer_name, " (@", photographer_username, ") on Unsplash"))
  } else {
    return(paste0("Photo by ", photographer_name, " on Unsplash"))
  }
}

# Generate HTML for Unsplash image display in info panels
format_unsplash_image_html <- function(image_data, max_display_width = 100) {
  if (!image_data$success) {
    return("")  # Return empty string if no image available
  }

  # Calculate display dimensions while maintaining aspect ratio
  display_width <- min(max_display_width, 100)  # Cap at 100px for panel display

  # Create HTML for image display with attribution
  image_html <- paste0(
    '<div class="unsplash-image-section">',
    '<div class="unsplash-image-container">',
    '<img src="', image_data$image_url,
    '" alt="', image_data$alt_description, '"',
    ' class="unsplash-taxonomic-image"',
    ' style="max-width:', display_width, 'px;"',
    ' loading="lazy" />',
    '</div>',
    '</div>'
  )

  return(image_html)
}

# Test function to verify Unsplash API connectivity
test_unsplash_connection <- function() {
  api_log_info("Testing Unsplash API connection...")

  # Test with well-known taxonomic groups that should have good image results
  test_groups <- c("Mammalia", "Primates", "Canidae", "Felidae", "Aves")

  for (group in test_groups) {
    api_log_info(paste("Testing Unsplash image for", group, "..."))
    result <- get_unsplash_random_image(group)
    if (result$success) {
      api_log_info(paste("  SUCCESS: Found image for", group, "-", result$image_url))
      api_log_info(paste("  Photographer:", result$photographer_name))
      api_log_info(paste("  Attribution:", result$attribution))
    } else {
      api_log_info(paste("  FAILED:", group, "-", result$error))
    }

    # Small delay to respect rate limits
    Sys.sleep(0.1)
  }

  api_log_info("Unsplash API connection test completed")
}

# Helper function for null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

api_log_info("Unsplash image API functions loaded successfully")