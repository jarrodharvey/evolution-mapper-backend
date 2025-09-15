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

# Main function to fetch appropriate Unsplash image using ChatGPT selection from random photos
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

    # Step 1: Get Wikipedia summary for ChatGPT context
    wikipedia_result <- NULL
    if (exists("cached_get_wikipedia_intro")) {
      api_log_info(paste("Getting Wikipedia summary for", taxonomic_group))
      wikipedia_result <- cached_get_wikipedia_intro(taxonomic_group, truncate_length = 500)
    }

    if (is.null(wikipedia_result) || !wikipedia_result$success) {
      api_log_warn(paste("Wikipedia lookup failed for", taxonomic_group, "- falling back to PhyloPic"))
      return(fallback_to_phylopic(taxonomic_group))
    }

    # Step 2: Fetch 5 random photos from Unsplash using taxonomic group as query
    api_log_info(paste("Fetching 5 random photos from Unsplash for", taxonomic_group))

    api_url <- "https://api.unsplash.com/photos/random"

    response <- request(api_url) |>
      req_url_query(
        query = taxonomic_group,
        count = 5,                  # Get 5 random photos
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
        paste("No random photos found for:", taxonomic_group)
      } else {
        paste("Unsplash API error:", resp_status(response))
      }

      api_log_warn(paste("Unsplash API failed:", error_msg, "- falling back to PhyloPic"))
      return(fallback_to_phylopic(taxonomic_group))
    }

    random_photos <- resp_body_json(response)

    # Ensure we have an array of photos
    if (!is.list(random_photos) || length(random_photos) == 0) {
      api_log_warn(paste("No random photos returned for", taxonomic_group, "- falling back to PhyloPic"))
      return(fallback_to_phylopic(taxonomic_group))
    }

    # Step 3: Extract image descriptions for ChatGPT
    image_descriptions <- sapply(random_photos, function(photo) {
      photo$alt_description %||% photo$description %||% "No description available"
    })

    api_log_info(paste("Got", length(image_descriptions), "photo descriptions for ChatGPT selection"))

    # Step 4: Use ChatGPT to select the most appropriate image
    if (exists("cached_get_chatgpt_image_selection")) {
      selection_result <- cached_get_chatgpt_image_selection(
        taxonomic_group,
        wikipedia_result$introduction,
        image_descriptions
      )

      if (selection_result$success && is.numeric(selection_result$selection)) {
        # ChatGPT selected a specific image
        selected_index <- selection_result$selection
        selected_photo <- random_photos[[selected_index]]

        api_log_info(paste("ChatGPT selected image", selected_index, "for", taxonomic_group))

        # Extract image information from selected photo
        image_url <- get_sized_image_url(selected_photo$urls, target_width)
        attribution <- format_unsplash_attribution(selected_photo)

        return(list(
          success = TRUE,
          taxonomic_group = taxonomic_group,
          selection_method = "chatgpt_random",
          selected_index = selected_index,
          chatgpt_response = selection_result$raw_response,
          image_url = image_url,
          image_width = selected_photo$width,
          image_height = selected_photo$height,
          photographer_name = selected_photo$user$name,
          photographer_username = selected_photo$user$username,
          photographer_url = selected_photo$user$links$html,
          unsplash_url = selected_photo$links$html,
          attribution = attribution,
          image_id = selected_photo$id,
          alt_description = selected_photo$alt_description %||% paste("Image of", taxonomic_group)
        ))

      } else if (selection_result$success && selection_result$selection == "no_match") {
        # ChatGPT said no images are appropriate
        api_log_info(paste("ChatGPT found no suitable matches for", taxonomic_group, "- falling back to PhyloPic"))
        return(fallback_to_phylopic(taxonomic_group))
      } else {
        # ChatGPT selection failed
        api_log_warn(paste("ChatGPT selection failed:", selection_result$error, "- falling back to PhyloPic"))
        return(fallback_to_phylopic(taxonomic_group))
      }
    } else {
      # ChatGPT selection function not available, fallback to PhyloPic
      api_log_warn(paste("ChatGPT selection function not available - falling back to PhyloPic"))
      return(fallback_to_phylopic(taxonomic_group))
    }

  }, error = function(e) {
    api_log_error(paste("Error in Unsplash random selection:", conditionMessage(e), "- falling back to PhyloPic"))
    return(fallback_to_phylopic(taxonomic_group))
  })
}

# Helper function to fallback to PhyloPic when Unsplash fails
fallback_to_phylopic <- function(taxonomic_group) {
  if (exists("cached_get_silhouette_data")) {
    api_log_info(paste("Using PhyloPic fallback for", taxonomic_group))
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
        selection_method = "phylopic_fallback",
        phylopic_uuid = silhouette_result$uuid,
        phylopic_url = silhouette_result$phylopic_url,
        attribution = silhouette_result$attribution,
        alt_description = paste("Silhouette of", taxonomic_group),
        silhouette_html = silhouette_html  # Add this for info panel compatibility
      ))
    }
  }

  # Final fallback - complete failure
  return(list(
    success = FALSE,
    error = "Both Unsplash and PhyloPic methods failed",
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

  # Handle regular Unsplash images (ChatGPT random selection)
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