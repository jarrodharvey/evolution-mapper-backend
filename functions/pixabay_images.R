# Pixabay Image API Functions
# Functions for fetching random images from Pixabay for taxonomic groups
# Used as secondary fallback between Unsplash (primary) and PhyloPic (last resort)

library(httr2)
library(jsonlite)

# Source shared logging configuration
source("functions/logging_config.R")

# Main function to fetch appropriate Pixabay image using creative common name search
get_pixabay_random_image <- function(taxonomic_group, target_width = 200, skip_chatgpt_conversion = FALSE) {
  if (is.null(taxonomic_group) || is.na(taxonomic_group) || taxonomic_group == "") {
    return(list(success = FALSE, error = "No taxonomic group provided"))
  }

  tryCatch({
    # Get Pixabay API key from environment
    api_key <- Sys.getenv("PIXABAY_KEY")
    if (api_key == "") {
      return(list(
        success = FALSE,
        error = "Pixabay API key not configured",
        taxonomic_group = taxonomic_group
      ))
    }

    # Step 1: Check if Wikipedia has a good image source already (skip Pixabay if so)
    wikipedia_result <- NULL
    if (exists("cached_get_wikipedia_intro")) {
      api_log_info(paste("Checking Wikipedia for existing image source:", taxonomic_group))
      wikipedia_result <- cached_get_wikipedia_intro(taxonomic_group, truncate_length = 500)
    }

    if (!is.null(wikipedia_result) && wikipedia_result$success) {
      api_log_info(paste("Wikipedia image available for", taxonomic_group, "- skipping Pixabay fallback"))
      # Note: This would return Wikipedia image if we had that functionality implemented
      # For now, continue with Pixabay since we're just using Wikipedia for context
    }

    # Continue with Pixabay search (either Wikipedia failed, or we're using it for context only)

    # Step 2: Get common name for better search results (unless we're skipping conversion)
    search_query <- taxonomic_group  # Default fallback
    if (!skip_chatgpt_conversion && exists("cached_get_chatgpt_common_name")) {
      api_log_info(paste("Getting common name for Pixabay search:", taxonomic_group))
      common_name_result <- cached_get_chatgpt_common_name(taxonomic_group)

      if (common_name_result$success && !is.null(common_name_result$common_name)) {
        search_query <- common_name_result$common_name
        api_log_info(paste("Using common name for Pixabay search:", search_query))
      } else {
        api_log_info(paste("Common name conversion failed, using original:", taxonomic_group))
      }
    } else {
      if (skip_chatgpt_conversion) {
        api_log_info(paste("Using provided common name for Pixabay search:", taxonomic_group))
        search_query <- taxonomic_group
      }
    }

    # Step 3: Search for photos from Pixabay using exact phrase matching
    api_log_info(paste("Searching for exact phrase photos from Pixabay for", search_query))

    api_url <- "https://pixabay.com/api/"

    # Quote the search query for exact phrase matching (biological accuracy)
    quoted_query <- paste0('"', search_query, '"')

    response <- request(api_url) |>
      req_url_query(
        key = api_key,
        q = quoted_query,
        category = "nature",             # Filter for nature-related content
        image_type = "photo",            # Only photos, not illustrations
        min_width = 100,                 # Minimum quality threshold
        min_height = 100,
        per_page = 20,                   # Get multiple results for random selection
        safesearch = "true"              # Exclude inappropriate content
      ) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_timeout(15) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    if (resp_status(response) != 200) {
      error_msg <- if (resp_status(response) == 429) {
        "Pixabay API rate limit exceeded"
      } else if (resp_status(response) == 400) {
        "Bad request to Pixabay API"
      } else if (resp_status(response) == 403) {
        "Pixabay API access denied or invalid key"
      } else {
        paste("Pixabay API error:", resp_status(response))
      }

      api_log_warn(paste("Pixabay API failed:", error_msg, "- falling back to PhyloPic"))
      return(fallback_to_phylopic_final(taxonomic_group))
    }

    response_data <- resp_body_json(response)

    # Extract photos from search results
    photos <- response_data$hits
    total_hits <- response_data$totalHits %||% 0

    if (!is.list(photos) || length(photos) == 0) {
      api_log_warn(paste("No search results returned for", search_query, "- falling back to PhyloPic"))
      return(fallback_to_phylopic_final(taxonomic_group))
    }

    api_log_info(paste("Found", length(photos), "Pixabay results for", search_query, "(", total_hits, "total available)"))

    # Step 4: Select one of the results at random
    selected_photo <- photos[[sample(length(photos), 1)]]

    api_log_info(paste("Randomly selected Pixabay photo for", taxonomic_group, "(search query:", search_query, ")"))

    # Extract image information from selected photo
    image_url <- selected_photo$previewURL  # Use previewURL (150px images)
    attribution <- format_pixabay_attribution(selected_photo)

    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group,
      selection_method = "pixabay_random",
      search_query = search_query,
      common_name_used = search_query != taxonomic_group,
      total_results = length(photos),
      total_available = total_hits,
      image_url = image_url,
      image_width = selected_photo$previewWidth %||% 150,
      image_height = selected_photo$previewHeight %||% 150,
      photographer_name = selected_photo$user,
      pixabay_id = selected_photo$id,
      attribution = attribution,
      alt_description = selected_photo$tags %||% paste("Image of", search_query),
      tags = selected_photo$tags
    ))

  }, error = function(e) {
    api_log_error(paste("Error in Pixabay random selection:", conditionMessage(e), "- falling back to PhyloPic"))
    return(fallback_to_phylopic_final(taxonomic_group))
  })
}

# Helper function to fallback to PhyloPic when Pixabay fails (final fallback)
fallback_to_phylopic_final <- function(taxonomic_group) {
  if (exists("cached_get_silhouette_data")) {
    api_log_info(paste("Using PhyloPic final fallback for", taxonomic_group))
    silhouette_result <- cached_get_silhouette_data(taxonomic_group)

    if (silhouette_result$success) {
      # Format the silhouette HTML for proper display in info panels
      silhouette_html <- NULL
      if (exists("format_silhouette_html")) {
        silhouette_html <- format_silhouette_html(silhouette_result)
      }

      return(list(
        success = TRUE,
        taxonomic_group = taxonomic_group,
        selection_method = "phylopic_final_fallback",
        phylopic_uuid = silhouette_result$uuid,
        phylopic_url = silhouette_result$phylopic_url,
        attribution = silhouette_result$attribution,
        alt_description = paste("Silhouette of", taxonomic_group),
        silhouette_html = silhouette_html  # Add this for info panel compatibility
      ))
    }
  }

  # Complete failure - no images available
  return(list(
    success = FALSE,
    error = "All image sources failed (Unsplash, Pixabay, PhyloPic)",
    taxonomic_group = taxonomic_group
  ))
}

# Format attribution as required by Pixabay terms
format_pixabay_attribution <- function(image_data) {
  photographer_name <- image_data$user %||% "Unknown photographer"
  return(paste0("Image by ", photographer_name, " from Pixabay"))
}

# Generate HTML for Pixabay image display in info panels
format_pixabay_image_html <- function(image_data) {
  if (!image_data$success) {
    return("")  # Return empty string if no image available
  }

  # Handle PhyloPic final fallback case
  if (!is.null(image_data$selection_method) && image_data$selection_method == "phylopic_final_fallback") {
    # Return the pre-formatted silhouette HTML
    if (!is.null(image_data$silhouette_html) && nchar(image_data$silhouette_html) > 0) {
      return(image_data$silhouette_html)
    } else {
      return("")  # Return empty string if silhouette HTML is not available
    }
  }

  # Handle regular Pixabay images
  if (!is.null(image_data$image_url)) {
    # Create HTML for image display - use previewURL (150px images)
    image_html <- paste0(
      '<div class="pixabay-image-section">',
      '<div class="pixabay-image-container">',
      '<img src="', image_data$image_url,
      '" alt="', image_data$alt_description, '"',
      ' class="pixabay-taxonomic-image"',
      ' loading="lazy" />',
      '</div>',
      '</div>'
    )

    return(image_html)
  }

  # Fallback - no valid image data
  return("")
}

# Test function to verify Pixabay API connectivity
test_pixabay_connection <- function() {
  api_log_info("Testing Pixabay API connection...")

  # Test with well-known taxonomic groups that should have good image results
  test_groups <- c("Mammalia", "Primates", "Aves", "Reptilia", "Amphibia")

  for (group in test_groups) {
    api_log_info(paste("Testing Pixabay image for", group, "..."))
    result <- get_pixabay_random_image(group)
    if (result$success && result$selection_method == "pixabay_random") {
      api_log_info(paste("  SUCCESS: Found Pixabay image for", group, "-", result$image_url))
      api_log_info(paste("  Photographer:", result$photographer_name))
      api_log_info(paste("  Attribution:", result$attribution))
    } else if (result$success && result$selection_method == "phylopic_final_fallback") {
      api_log_info(paste("  FALLBACK: PhyloPic used for", group))
    } else {
      api_log_info(paste("  FAILED:", group, "-", result$error))
    }

    # Small delay to respect rate limits
    Sys.sleep(0.1)
  }

  api_log_info("Pixabay API connection test completed")
}

# Helper function for null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

api_log_info("Pixabay image API functions loaded successfully")