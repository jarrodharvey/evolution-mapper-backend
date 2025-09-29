# Wikimedia/Wikipedia Images API Integration
# Functions for fetching high-quality taxonomic images from Wikimedia Commons
# Uses tidywikidatar package for Wikidata entity search and image retrieval

library(tidywikidatar)
library(httr2)
library(dplyr)

# All required functions are sourced at startup in plumber.R

# Main function to get Wikipedia/Wikimedia image for a taxonomic group
get_wikimedia_image <- function(taxonomic_name, target_width = 200, max_images = 5) {
  if (is.null(taxonomic_name) || is.na(taxonomic_name) || taxonomic_name == "") {
    return(list(
      success = FALSE,
      error = "No taxonomic name provided",
      taxonomic_name = taxonomic_name
    ))
  }

  # Clean taxonomic name for search
  clean_name <- trimws(as.character(taxonomic_name))

  api_log_info(paste("[WIKIMEDIA] Searching for entity:", clean_name))

  tryCatch({
    # Search for Wikidata entity
    search_start <- Sys.time()
    search_results <- tw_search(clean_name)

    if (nrow(search_results) == 0) {
      api_log_info(paste("[WIKIMEDIA] No Wikidata entity found for:", clean_name))
      return(list(
        success = FALSE,
        error = "No Wikidata entity found",
        taxonomic_name = clean_name,
        search_duration = as.numeric(difftime(Sys.time(), search_start, units = "secs"))
      ))
    }

    # Log search results
    api_log_info(paste("[WIKIMEDIA] Found", nrow(search_results), "entities for:", clean_name))
    if (nrow(search_results) > 0) {
      top_result <- search_results[1, ]
      api_log_info(paste("[WIKIMEDIA] Top result:", top_result$id, "-", top_result$label, "-", top_result$description))
    }

    # Prioritize the most relevant search result first
    # Look for exact matches, biological entities, or species first
    best_entity_index <- 1  # Default to first result

    for (i in 1:min(nrow(search_results), 5)) {
      entity <- search_results[i, ]
      description <- tolower(entity$description %||% "")
      label <- tolower(entity$label %||% "")

      # Prioritize biological/species entities over people, places, etc.
      if (grepl("species|fish|bird|mammal|reptile|amphibian|insect|plant|animal|organism", description) ||
          grepl("family.*fish|order.*bird|genus", description)) {
        best_entity_index <- i
        api_log_info(paste("[WIKIMEDIA] Found biological entity at position", i, ":", entity$id, "-", entity$label))
        break
      }
    }

    # Try the best entity first, then fallback to others if needed
    entity_indices <- c(best_entity_index, setdiff(1:min(nrow(search_results), 3), best_entity_index))

    for (idx in entity_indices) {
      entity <- search_results[idx, ]
      entity_id <- entity$id
      entity_label <- entity$label
      entity_description <- entity$description

      api_log_info(paste("[WIKIMEDIA] Checking images for entity", which(entity_indices == idx), ":", entity_id, "-", entity_label))

      # Get images for this entity
      image_start <- Sys.time()
      images_result <- tryCatch({
        tw_get_image(entity_id, format = "embed", width = target_width)
      }, error = function(e) {
        api_log_warn(paste("[WIKIMEDIA] Error getting images for entity", entity_id, ":", e$message))
        return(data.frame())  # Return empty data.frame on error
      })

      # Check if we got valid results
      if (is.null(images_result) || !is.data.frame(images_result) || nrow(images_result) == 0) {
        api_log_info(paste("[WIKIMEDIA] No images found for entity:", entity_id, "-", entity_label))
        next  # Try next entity
      }

      # Select a random image from available options
      selected_image_index <- sample(1:min(nrow(images_result), max_images), 1)
      selected_image <- images_result[selected_image_index, ]
      image_url <- selected_image$image

      # Validate image URL
      if (is.null(image_url) || is.na(image_url) || nchar(image_url) == 0) {
        api_log_warn(paste("[WIKIMEDIA] Invalid image URL for entity:", entity_id))
        next  # Try next entity
      }

      # Test image accessibility (more permissive - allow images even if HTTP test fails)
      image_test_result <- test_image_accessibility(image_url)
      if (!image_test_result$accessible) {
        api_log_warn(paste("[WIKIMEDIA] Image accessibility check failed:", image_url, "-", image_test_result$error, "- proceeding anyway"))
        # Don't skip - Wikimedia images are usually accessible even if HEAD request fails
      }

      total_duration <- as.numeric(difftime(Sys.time(), search_start, units = "secs"))
      image_duration <- as.numeric(difftime(Sys.time(), image_start, units = "secs"))

      api_log_info(paste("[WIKIMEDIA] Successfully found image for:", clean_name))
      api_log_info(paste("[WIKIMEDIA] Entity:", entity_id, "-", entity_label))
      api_log_info(paste("[WIKIMEDIA] Image URL:", image_url))
      api_log_info(paste("[WIKIMEDIA] Duration: search", round(as.numeric(difftime(image_start, search_start, units = "secs")), 3), "s, image", round(image_duration, 3), "s, total", round(total_duration, 3), "s"))

      # Get detailed image licensing/attribution information
      filename <- extract_filename_from_url(image_url)
      detailed_license_info <- get_image_license_info(filename)

      # Create detailed attribution text using actual metadata
      detailed_attribution <- format_image_attribution(detailed_license_info)

      # Create basic attribution text (fallback)
      basic_attribution <- paste0("Wikimedia Commons • ", entity_label)
      if (!is.null(entity_description) && !is.na(entity_description) && nchar(entity_description) > 0) {
        basic_attribution <- paste0(basic_attribution, " (", entity_description, ")")
      }

      # Use detailed attribution if available, otherwise fallback to basic
      attribution <- if (!is.null(detailed_license_info) && detailed_license_info$artist != "Unknown") {
        detailed_attribution
      } else {
        basic_attribution
      }

      api_log_info(paste("[WIKIMEDIA] Attribution:", attribution))

      return(list(
        success = TRUE,
        taxonomic_name = clean_name,
        entity_id = entity_id,
        entity_label = entity_label,
        entity_description = entity_description,
        image_url = image_url,
        image_width = target_width,
        attribution = attribution,
        detailed_license_info = detailed_license_info,
        basic_attribution = basic_attribution,
        total_images_available = nrow(images_result),
        selected_index = selected_image_index,
        search_duration = round(as.numeric(difftime(image_start, search_start, units = "secs")), 3),
        image_duration = round(image_duration, 3),
        total_duration = round(total_duration, 3)
      ))
    }

    # No entities had usable images
    total_duration <- as.numeric(difftime(Sys.time(), search_start, units = "secs"))
    api_log_info(paste("[WIKIMEDIA] No usable images found for:", clean_name, "after checking", min(nrow(search_results), 3), "entities"))
    return(list(
      success = FALSE,
      error = "No usable images found in available entities",
      taxonomic_name = clean_name,
      entities_checked = min(nrow(search_results), 3),
      total_entities = nrow(search_results),
      duration = round(total_duration, 3)
    ))

  }, error = function(e) {
    api_log_error(paste("[WIKIMEDIA] Error searching for:", clean_name, "-", e$message))
    return(list(
      success = FALSE,
      error = paste("Wikimedia search error:", e$message),
      taxonomic_name = clean_name
    ))
  })
}

# Test if an image URL is accessible
test_image_accessibility <- function(image_url, timeout = 10) {
  tryCatch({
    response <- request(image_url) |>
      req_method("HEAD") |>
      req_timeout(timeout) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    status <- resp_status(response)
    if (status == 200) {
      # Check content type to ensure it's an image
      content_type <- resp_header(response, "content-type")
      if (!is.null(content_type) && grepl("^image/", content_type)) {
        return(list(accessible = TRUE, status = status, content_type = content_type))
      } else {
        return(list(accessible = FALSE, error = paste("Invalid content type:", content_type), status = status))
      }
    } else {
      return(list(accessible = FALSE, error = paste("HTTP status:", status), status = status))
    }
  }, error = function(e) {
    return(list(accessible = FALSE, error = e$message))
  })
}

# Format Wikimedia image result as HTML for info panels
format_wikimedia_image_html <- function(wikimedia_result, container_class = "wikimedia-image-section") {
  if (!wikimedia_result$success) {
    return("")
  }

  # Create HTML structure matching the existing image format patterns
  image_html <- paste0(
    '<div class="', container_class, '">',
    '<div class="wikimedia-image-container" style="width: 100%; max-width: 300px; margin: 0 auto;">',
    '<img src="', wikimedia_result$image_url, '" ',
    'alt="', wikimedia_result$entity_label, ' - ', wikimedia_result$entity_description, '" ',
    'class="wikimedia-taxonomic-image" ',
    'style="width: 100%; height: auto; max-height: 250px; object-fit: contain; border-radius: 4px; display: block;">',
    '</div>',
    '</div>'
  )

  return(image_html)
}

# Enhanced search function that tries multiple search strategies
get_wikimedia_image_enhanced <- function(taxonomic_name, target_width = 200) {
  # Try direct search first
  result <- get_wikimedia_image(taxonomic_name, target_width)

  if (result$success) {
    return(result)
  }

  # If direct search failed, try alternative search terms
  alternative_searches <- generate_alternative_search_terms(taxonomic_name)

  for (alt_term in alternative_searches) {
    api_log_info(paste("[WIKIMEDIA] Trying alternative search term:", alt_term))
    result <- get_wikimedia_image(alt_term, target_width)

    if (result$success) {
      # Update the result to note it was found via alternative search
      result$original_search_term <- taxonomic_name
      result$successful_search_term <- alt_term
      api_log_info(paste("[WIKIMEDIA] Found image via alternative search:", taxonomic_name, "->", alt_term))
      return(result)
    }
  }

  # All searches failed
  api_log_info(paste("[WIKIMEDIA] All search strategies failed for:", taxonomic_name))
  return(list(
    success = FALSE,
    error = "No images found with direct or alternative search terms",
    taxonomic_name = taxonomic_name,
    alternative_terms_tried = alternative_searches
  ))
}

# Generate alternative search terms for better entity matching
generate_alternative_search_terms <- function(taxonomic_name) {
  alternatives <- c()

  # Add singular/plural variations
  if (endsWith(taxonomic_name, "s")) {
    alternatives <- c(alternatives, substr(taxonomic_name, 1, nchar(taxonomic_name) - 1))
  } else {
    alternatives <- c(alternatives, paste0(taxonomic_name, "s"))
  }

  # Add common taxonomic suffixes
  common_suffixes <- c("idae", "inae", "ini", "oidea", "formes")
  for (suffix in common_suffixes) {
    if (endsWith(taxonomic_name, suffix)) {
      base_name <- substr(taxonomic_name, 1, nchar(taxonomic_name) - nchar(suffix))
      alternatives <- c(alternatives, base_name)
    }
  }

  # Add "taxon" qualifier for scientific groups
  if (!grepl("\\s", taxonomic_name)) {  # Single word taxonomic names
    alternatives <- c(alternatives, paste(taxonomic_name, "taxon"))
  }

  # Remove duplicates and return
  return(unique(alternatives[alternatives != taxonomic_name]))
}

# Test function to verify Wikimedia integration
test_wikimedia_integration <- function() {
  api_log_info("Testing Wikimedia image integration...")

  # Test with various taxonomic groups
  test_groups <- c(
    "Mammalia",
    "Primates",
    "Lepidoptera",
    "Boreoeutheria",
    "Teleostei",
    "Aves"
  )

  success_count <- 0

  for (group in test_groups) {
    api_log_info(paste("Testing Wikimedia image for:", group))
    result <- get_wikimedia_image_enhanced(group, target_width = 200)

    if (result$success) {
      success_count <- success_count + 1
      api_log_info(paste("  SUCCESS:", group, "->", result$entity_label, "-", result$image_url))
      api_log_info(paste("  Duration:", result$total_duration, "seconds"))
    } else {
      api_log_info(paste("  FAILED:", group, "-", result$error))
    }

    # Small delay to be respectful to Wikimedia APIs
    Sys.sleep(0.5)
  }

  api_log_info(paste("Wikimedia integration test completed:", success_count, "/", length(test_groups), "successful"))
  return(list(
    success_rate = success_count / length(test_groups),
    successful = success_count,
    total = length(test_groups),
    test_groups = test_groups
  ))
}

api_log_info("Wikimedia image functions loaded successfully")
api_log_info("Available functions:")
api_log_info("  get_wikimedia_image() - Main Wikimedia image search function")
api_log_info("  get_wikimedia_image_enhanced() - Enhanced search with alternatives")
api_log_info("  format_wikimedia_image_html() - Format image results as HTML")
api_log_info("  test_wikimedia_integration() - Test function for debugging")