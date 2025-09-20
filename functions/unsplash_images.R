# Unsplash Image API Functions
# Functions for fetching random images from Unsplash for taxonomic groups
# Used as primary image source with PhyloPic silhouettes as fallback

library(httr2)
library(base64enc)

# Source shared logging configuration
source("functions/logging_config.R")

# Load ChatGPT API functions for taxonomic name conversion
# This will be loaded via cached_api_functions.R to avoid circular dependency
chatgpt_available <- FALSE
tryCatch({
  if (exists("cached_get_chatgpt_summary")) {
    chatgpt_available <- TRUE
  }
}, error = function(e) {
  # ChatGPT functions not yet loaded
})

# Main function to fetch appropriate Unsplash image using creative common name search
get_unsplash_random_image <- function(taxonomic_group, target_width = 800, skip_chatgpt_conversion = FALSE) {
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

    # Step 1: Get common name from ChatGPT first (unless we're skipping conversion)
    search_query <- taxonomic_group  # Default fallback
    if (!skip_chatgpt_conversion && exists("cached_get_chatgpt_common_name")) {
      api_log_info(paste("Getting common name for", taxonomic_group))
      common_name_result <- cached_get_chatgpt_common_name(taxonomic_group)

      if (common_name_result$success && !is.null(common_name_result$common_name)) {
        search_query <- common_name_result$common_name
        api_log_info(paste("Using common name for search:", search_query))
      } else {
        api_log_info(paste("Common name conversion failed, using original:", taxonomic_group))
      }
    } else {
      if (skip_chatgpt_conversion) {
        api_log_info(paste("Using provided common name for search:", taxonomic_group))
        search_query <- taxonomic_group
      } else {
        api_log_warn(paste("ChatGPT common name function not available, using taxonomic name:", taxonomic_group))
      }
    }

    # Step 3: Search for photos from Unsplash using common name - get multiple results for filtering
    api_log_info(paste("Searching for photos from Unsplash for", search_query))

    api_url <- "https://api.unsplash.com/search/photos"

    response <- request(api_url) |>
      req_url_query(
        query = search_query,
        per_page = 30,             # Get multiple results for topic filtering
        orientation = "squarish",   # Prefer squarish for better thumbnail display
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
        paste("No search results found for:", search_query)
      } else {
        paste("Unsplash API error:", resp_status(response))
      }

      api_log_warn(paste("Unsplash API failed:", error_msg, "- falling back to Pixabay"))
      return(fallback_to_pixabay(taxonomic_group))
    }

    response_data <- resp_body_json(response)

    # Extract photos from search results
    photos <- response_data$results

    if (!is.list(photos) || length(photos) == 0) {
      api_log_warn(paste("No search results returned for", search_query, "- falling back to Pixabay"))
      return(fallback_to_pixabay(taxonomic_group))
    }

    # Step 4: Filter by acceptable topic submissions
    acceptable_topics <- c("animals", "nature", "wildlife", "birds", "marine-life", "insects",
                          "plants", "forest", "ocean", "freshwater", "mountains", "savanna",
                          "macro", "zoology", "botany", "ecology", "aquatic-life", "wild-animals")

    # Log all topics found in the search results for assessment
    all_topics <- c()
    for (photo in photos) {
      if (!is.null(photo$topic_submissions) && length(photo$topic_submissions) > 0) {
        photo_topics <- names(photo$topic_submissions)
        all_topics <- c(all_topics, photo_topics)
      }
    }
    unique_topics <- unique(all_topics)
    if (length(unique_topics) > 0) {
      api_log_info(paste("Unsplash topics found for", search_query, ":", paste(sort(unique_topics), collapse = ", ")))
    } else {
      api_log_info(paste("No topic_submissions found in Unsplash results for", search_query))
    }

    filtered_photos <- Filter(function(photo) {
      if (is.null(photo$topic_submissions) || length(photo$topic_submissions) == 0) {
        return(FALSE)
      }

      # Check if any of the photo's topics match our acceptable topics
      photo_topics <- names(photo$topic_submissions)
      return(any(photo_topics %in% acceptable_topics))
    }, photos)

    api_log_info(paste("Found", length(photos), "total results,", length(filtered_photos), "match topic filters"))

    # If no photos pass the topic filter, fallback to Pixabay
    # CRITICAL: Do NOT use unfiltered photos as fallback - this would return completely
    # inappropriate images (e.g., sushi for "salmon", food for "turkey", etc.)
    # Topic filtering is essential for biological accuracy in a scientific application
    if (length(filtered_photos) == 0) {
      api_log_warn(paste("No photos match topic filters for", search_query, "- falling back to Pixabay"))
      return(fallback_to_pixabay(taxonomic_group))
    }

    # Step 5: Select one of the filtered results at random
    selected_photo <- filtered_photos[[sample(length(filtered_photos), 1)]]

    # Log which topics this photo matched
    photo_topics <- names(selected_photo$topic_submissions)
    matched_topics <- intersect(photo_topics, acceptable_topics)
    api_log_info(paste("Randomly selected photo for", taxonomic_group, "(search query:", search_query, ") with topics:", paste(matched_topics, collapse = ", ")))

    # Extract image information from selected photo
    image_url <- get_sized_image_url(selected_photo$urls, target_width)
    attribution <- format_unsplash_attribution(selected_photo)

    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group,
      selection_method = "topic_filtered_random",
      search_query = search_query,
      common_name_used = search_query != taxonomic_group,
      total_results = length(photos),
      filtered_results = length(filtered_photos),
      matched_topics = matched_topics,
      image_url = image_url,
      image_width = selected_photo$width,
      image_height = selected_photo$height,
      photographer_name = selected_photo$user$name,
      photographer_username = selected_photo$user$username,
      photographer_url = selected_photo$user$links$html,
      unsplash_url = selected_photo$links$html,
      attribution = attribution,
      image_id = selected_photo$id,
      alt_description = selected_photo$alt_description %||% paste("Image of", search_query)
    ))

  }, error = function(e) {
    api_log_error(paste("Error in Unsplash random selection:", conditionMessage(e), "- falling back to Pixabay"))
    return(fallback_to_pixabay(taxonomic_group))
  })
}

# Helper function to fallback to Pixabay when Unsplash fails
fallback_to_pixabay <- function(taxonomic_group) {
  if (exists("cached_get_pixabay_random_image")) {
    api_log_info(paste("Using Pixabay fallback for", taxonomic_group))
    pixabay_result <- cached_get_pixabay_random_image(taxonomic_group, target_width = 200)

    if (pixabay_result$success) {
      return(pixabay_result)  # Success - use Pixabay image
    }
  }

  # If Pixabay fails, fall back to PhyloPic (final fallback)
  return(fallback_to_phylopic_final(taxonomic_group))
}

# Helper function for final PhyloPic fallback when both Unsplash and Pixabay fail
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

  # Complete failure - no images available from any source
  return(list(
    success = FALSE,
    error = "All image sources failed (Unsplash, Pixabay, PhyloPic)",
    taxonomic_group = taxonomic_group
  ))
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
format_unsplash_image_html <- function(image_data) {
  if (!image_data$success) {
    return("")  # Return empty string if no image available
  }

  # Handle PhyloPic fallback case
  if (!is.null(image_data$selection_method) && image_data$selection_method == "phylopic_fallback") {
    # Return the pre-formatted silhouette HTML
    if (!is.null(image_data$silhouette_html) && nchar(image_data$silhouette_html) > 0) {
      return(image_data$silhouette_html)
    } else {
      return("")  # Return empty string if silhouette HTML is not available
    }
  }

  # Handle regular Unsplash images (search first result)
  if (!is.null(image_data$image_url)) {
    # Create HTML for image display - let CSS and natural image size determine dimensions
    image_html <- paste0(
      '<div class="unsplash-image-section">',
      '<div class="unsplash-image-container">',
      '<img src="', image_data$image_url,
      '" alt="', image_data$alt_description, '"',
      ' class="unsplash-taxonomic-image"',
      ' loading="lazy" />',
      '</div>',
      '</div>'
    )

    return(image_html)
  }

  # Fallback - no valid image data
  return("")
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