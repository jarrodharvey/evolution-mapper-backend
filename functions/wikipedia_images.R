# Wikipedia Image API Functions
# Functions for fetching the main/first image from Wikipedia articles for taxonomic groups
# Used as primary image source with PhyloPic silhouettes as fallback

library(httr2)
library(base64enc)

# Source shared logging configuration
source("functions/logging_config.R")

# Main function to fetch Wikipedia main image for a taxonomic group
get_wikipedia_main_image <- function(taxonomic_group, target_width = 800) {
  if (is.null(taxonomic_group) || is.na(taxonomic_group) || taxonomic_group == "") {
    return(list(success = FALSE, error = "No taxonomic group provided"))
  }
  
  tryCatch({
    # Clean and format the taxonomic group name for Wikipedia search
    search_term <- clean_taxonomic_name_for_image_search(taxonomic_group)
    
    # Step 1: Find the exact page title using existing Wikipedia search
    page_title <- find_wikipedia_page_title(search_term)
    
    if (is.null(page_title)) {
      return(list(
        success = FALSE,
        error = paste("No Wikipedia article found for:", taxonomic_group),
        taxonomic_group = taxonomic_group,
        search_term = search_term
      ))
    }
    
    # Step 2: Get the main image for that page title
    image_result <- get_page_main_image(page_title, target_width)
    
    if (!image_result$success) {
      return(list(
        success = FALSE,
        error = paste("No main image found for Wikipedia article:", page_title),
        taxonomic_group = taxonomic_group,
        wikipedia_title = page_title
      ))
    }
    
    # Step 3: Get image licensing information for attribution
    license_info <- get_image_license_info(image_result$filename)
    
    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group,
      wikipedia_title = page_title,
      image_url = image_result$image_url,
      image_width = image_result$width,
      image_height = image_result$height,
      filename = image_result$filename,
      attribution = format_image_attribution(license_info),
      license_info = license_info,
      wikipedia_url = paste0("https://en.wikipedia.org/wiki/", URLencode(gsub(" ", "_", page_title)))
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error fetching Wikipedia image:", conditionMessage(e)),
      taxonomic_group = taxonomic_group
    ))
  })
}

# Clean taxonomic name for Wikipedia image search (more specific than text search)
clean_taxonomic_name_for_image_search <- function(name) {
  # Remove common prefixes and suffixes that might confuse Wikipedia search
  cleaned <- gsub("^(Order|Family|Class|Genus|Species)\\s+", "", name, ignore.case = TRUE)
  cleaned <- gsub("\\s+(group|clade)$", "", cleaned, ignore.case = TRUE)
  cleaned <- trimws(cleaned)
  
  # For image searches, scientific names tend to work better than common names
  # Keep the name as-is for most accurate Wikipedia article matching
  return(cleaned)
}

# Find Wikipedia page title using existing search functionality
find_wikipedia_page_title <- function(search_term) {
  tryCatch({
    # Use existing Wikipedia search function if available
    if (exists("search_wikipedia_article", mode = "function")) {
      search_result <- search_wikipedia_article(search_term)
      if (!is.null(search_result) && length(search_result$pages) > 0) {
        return(search_result$pages[[1]]$title)
      }
    }
    
    # Fallback to direct MediaWiki API search
    api_url <- "https://en.wikipedia.org/w/api.php"
    
    response <- request(api_url) |>
      req_url_query(
        action = "query",
        list = "search",
        srsearch = search_term,
        srlimit = 1,
        format = "json"
      ) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_timeout(10) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()
    
    if (resp_status(response) != 200) {
      return(NULL)
    }
    
    search_data <- resp_body_json(response)
    
    if (length(search_data$query$search) == 0) {
      return(NULL)
    }
    
    return(search_data$query$search[[1]]$title)
    
  }, error = function(e) {
    api_log_warn(paste("Error in find_wikipedia_page_title:", conditionMessage(e)))
    return(NULL)
  })
}

# Get main image from a Wikipedia page using pageimages API
get_page_main_image <- function(page_title, target_width = 800) {
  tryCatch({
    api_url <- "https://en.wikipedia.org/w/api.php"
    
    response <- request(api_url) |>
      req_url_query(
        action = "query",
        titles = page_title,
        prop = "pageimages",
        pithumbsize = target_width,
        format = "json"
      ) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_timeout(10) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()
    
    if (resp_status(response) != 200) {
      return(list(success = FALSE, error = "Wikipedia API request failed"))
    }
    
    image_data <- resp_body_json(response)
    
    # Extract page data (API returns pages as object with page IDs as keys)
    pages <- image_data$query$pages
    if (length(pages) == 0) {
      return(list(success = FALSE, error = "Page not found"))
    }
    
    page <- pages[[1]]  # Get first (and usually only) page
    
    if (is.null(page$thumbnail)) {
      return(list(success = FALSE, error = "No main image found for this page"))
    }
    
    # Extract filename from the source URL for licensing lookup
    image_url <- page$thumbnail$source
    filename <- extract_filename_from_url(image_url)
    
    return(list(
      success = TRUE,
      image_url = image_url,
      width = page$thumbnail$width,
      height = page$thumbnail$height,
      filename = filename
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error fetching page image:", conditionMessage(e))
    ))
  })
}

# Extract filename from Wikipedia/Wikimedia image URL
extract_filename_from_url <- function(image_url) {
  tryCatch({
    # Handle Special:Redirect URLs from WikidataR
    # Example: https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/Panthera Diversity.jpg&width=200
    if (grepl("Special:Redirect/file/", image_url)) {
      # Extract filename from the Special:Redirect URL
      filename_match <- regexpr("Special:Redirect/file/([^&]+)", image_url)
      if (filename_match[1] > 0) {
        # Extract the filename part after "Special:Redirect/file/"
        matched_text <- substr(image_url, filename_match[1], filename_match[1] + attr(filename_match, "match.length") - 1)
        filename <- gsub("Special:Redirect/file/", "", matched_text)
        # URL decode the filename (spaces are encoded as %20)
        filename <- URLdecode(filename)
        return(paste0("File:", filename))
      }
    }

    # Handle standard Wikimedia URLs
    # Example: https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Lion_waiting_in_Namibia.jpg/800px-Lion_waiting_in_Namibia.jpg
    filename_match <- regexpr("/([^/]+\\.(?:jpg|jpeg|png|gif|svg|webp))(?:/\\d+px-[^/]+)?$", image_url, ignore.case = TRUE)

    if (filename_match[1] > 0) {
      # Extract the matched filename part
      matched_text <- substr(image_url, filename_match[1], filename_match[1] + attr(filename_match, "match.length") - 1)
      # Remove the leading slash and thumbnail prefix if present
      filename <- gsub("^/", "", matched_text)
      filename <- gsub("/\\d+px-", "", filename)
      return(paste0("File:", filename))
    }

    # Fallback: try to extract any filename-like pattern
    url_parts <- strsplit(image_url, "/")[[1]]
    for (part in rev(url_parts)) {
      if (grepl("\\.(jpg|jpeg|png|gif|svg|webp)$", part, ignore.case = TRUE)) {
        # Remove thumbnail size prefix if present
        clean_part <- gsub("^\\d+px-", "", part)
        return(paste0("File:", clean_part))
      }
    }

    return("File:Unknown")
  }, error = function(e) {
    api_log_warn(paste("Error extracting filename from URL:", image_url, "-", e$message))
    return("File:Unknown")
  })
}

# Get licensing information for an image file
get_image_license_info <- function(filename) {
  if (is.null(filename) || filename == "File:Unknown") {
    return(list(
      license = "Unknown",
      artist = "Unknown",
      credit = "Wikipedia",
      attribution_required = TRUE
    ))
  }
  
  tryCatch({
    api_url <- "https://en.wikipedia.org/w/api.php"
    
    response <- request(api_url) |>
      req_url_query(
        action = "query",
        titles = filename,
        prop = "imageinfo",
        iiprop = "extmetadata",
        format = "json"
      ) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_timeout(10) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()
    
    if (resp_status(response) != 200) {
      return(list(license = "Unknown", artist = "Unknown", credit = "Wikipedia", attribution_required = TRUE))
    }
    
    data <- resp_body_json(response)
    pages <- data$query$pages
    
    if (length(pages) == 0) {
      return(list(license = "Unknown", artist = "Unknown", credit = "Wikipedia", attribution_required = TRUE))
    }
    
    page <- pages[[1]]
    
    if (is.null(page$imageinfo) || length(page$imageinfo) == 0) {
      return(list(license = "Unknown", artist = "Unknown", credit = "Wikipedia", attribution_required = TRUE))
    }
    
    metadata <- page$imageinfo[[1]]$extmetadata
    
    if (is.null(metadata)) {
      return(list(license = "Unknown", artist = "Unknown", credit = "Wikipedia", attribution_required = TRUE))
    }
    
    # Extract key metadata fields
    license <- if (!is.null(metadata$LicenseShortName)) metadata$LicenseShortName$value else "Unknown"
    artist <- if (!is.null(metadata$Artist)) metadata$Artist$value else "Unknown"
    credit <- if (!is.null(metadata$Credit)) metadata$Credit$value else "Wikipedia"
    
    # Determine if attribution is required (most licenses require it except Public Domain)
    attribution_required <- !grepl("public domain|pd-", license, ignore.case = TRUE)
    
    return(list(
      license = license,
      artist = clean_html_tags(artist),
      credit = clean_html_tags(credit),
      attribution_required = attribution_required
    ))
    
  }, error = function(e) {
    api_log_warn(paste("Error fetching image license for", filename, ":", conditionMessage(e)))
    return(list(license = "Unknown", artist = "Unknown", credit = "Wikipedia", attribution_required = TRUE))
  })
}

# Clean HTML tags from attribution text
clean_html_tags <- function(text) {
  if (is.null(text) || is.na(text)) {
    return("Unknown")
  }
  # Simple HTML tag removal
  cleaned <- gsub("<[^>]*>", "", text)
  cleaned <- gsub("&nbsp;", " ", cleaned)
  cleaned <- gsub("&amp;", "&", cleaned)
  cleaned <- gsub("&lt;", "<", cleaned)
  cleaned <- gsub("&gt;", ">", cleaned)
  cleaned <- trimws(cleaned)
  
  if (nchar(cleaned) == 0) {
    return("Unknown")
  }
  
  return(cleaned)
}

# Format image attribution for display
format_image_attribution <- function(license_info) {
  if (!license_info$attribution_required) {
    return("Image via Wikipedia (Public Domain)")
  }
  
  artist <- if (license_info$artist != "Unknown" && nchar(license_info$artist) > 0) {
    license_info$artist
  } else {
    "Unknown photographer"
  }
  
  license <- if (license_info$license != "Unknown" && nchar(license_info$license) > 0) {
    license_info$license
  } else {
    "Wikipedia license"
  }
  
  return(paste("Image by", artist, "via Wikipedia (", license, ")"))
}

# Generate HTML for Wikipedia image display in info panels  
format_wikipedia_image_html <- function(image_data, max_display_width = 100) {
  if (!image_data$success) {
    return("")  # Return empty string if no image available
  }
  
  # Calculate display dimensions while maintaining aspect ratio
  display_width <- min(image_data$image_width, max_display_width)
  aspect_ratio <- image_data$image_height / image_data$image_width
  display_height <- round(display_width * aspect_ratio)
  
  # Create HTML for image display with attribution
  image_html <- paste0(
    '<div class="wikipedia-image-section">',
    '<div class="wikipedia-image-container">',
    '<img src="', image_data$image_url, 
    '" alt="', image_data$taxonomic_group, ' image from Wikipedia"',
    ' class="wikipedia-taxonomic-image"',
    ' style="max-width:', display_width, 'px; max-height:', display_height, 'px;"',
    ' loading="lazy" />',
    '</div>',
    '</div>'
  )
  
  return(image_html)
}

# Test function to verify Wikipedia image API functionality
test_wikipedia_image_connection <- function() {
  api_log_info("Testing Wikipedia image API connection...")
  
  # Test with well-known taxonomic groups that should have images
  test_groups <- c("Mammalia", "Primates", "Canidae", "Felidae", "Aves")
  
  for (group in test_groups) {
    api_log_info(paste("Testing Wikipedia image for", group, "..."))
    result <- get_wikipedia_main_image(group)
    if (result$success) {
      api_log_info(paste("  SUCCESS: Found image for", group, "-", result$image_url))
      api_log_info(paste("  Attribution:", result$attribution))
    } else {
      api_log_info(paste("  FAILED:", group, "-", result$error))
    }
  }
  
  api_log_info("Wikipedia image API connection test completed")
}

api_log_info("Wikipedia image API functions loaded successfully")