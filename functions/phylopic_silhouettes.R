# PhyloPic silhouette integration for taxonomic nodes
# Provides visual representations of ancestors using rphylopic package

library(rphylopic)
library(jsonlite)

# All required functions are sourced at startup in plumber.R

# Get a random silhouette UUID for a taxonomic group
get_random_silhouette_uuid <- function(taxonomic_name) {
  if (is.null(taxonomic_name) || is.na(taxonomic_name) || taxonomic_name == "") {
    return(NULL)
  }
  
  tryCatch({
    # Search for UUIDs matching the taxonomic name
    uuids <- get_uuid(name = taxonomic_name, n = 10)  # Get up to 10 options
    
    if (length(uuids) > 0) {
      # Pick a random UUID from the results
      selected_uuid <- sample(uuids, 1)
      return(selected_uuid)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    cat("Warning: Could not retrieve silhouette UUID for", taxonomic_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Get silhouette data including image and attribution
get_silhouette_data <- function(taxonomic_name) {
  if (is.null(taxonomic_name) || is.na(taxonomic_name) || taxonomic_name == "") {
    return(list(success = FALSE, error = "No taxonomic name provided"))
  }
  
  # Get a random UUID for this taxonomic group
  uuid <- get_random_silhouette_uuid(taxonomic_name)
  
  if (is.null(uuid)) {
    return(list(success = FALSE, error = "No silhouettes found"))
  }
  
  tryCatch({
    # Get the silhouette image as raster (PNG) - easier to handle than vector
    silhouette_raster <- get_phylopic(uuid = uuid, format = "raster")
    
    # Get attribution information
    attribution <- get_attribution(uuid = uuid)
    
    # Format attribution text
    attribution_text <- if (is.data.frame(attribution) && nrow(attribution) > 0) {
      contributor <- if (!is.na(attribution$contributor[1])) attribution$contributor[1] else "Unknown"
      paste("Silhouette by", contributor, "via PhyloPic")
    } else {
      "Silhouette via PhyloPic"
    }
    
    return(list(
      success = TRUE,
      uuid = uuid,
      raster_content = silhouette_raster,
      attribution = attribution_text,
      phylopic_url = paste0("https://www.phylopic.org/images/", uuid)
    ))
    
  }, error = function(e) {
    cat("Warning: Could not retrieve silhouette data for", taxonomic_name, "UUID:", uuid, ":", e$message, "\n")
    return(list(success = FALSE, error = paste("Silhouette retrieval failed:", e$message)))
  })
}

# Resize raster array using simple array operations
resize_raster_array <- function(raster_array, target_width = 80, target_height = 80) {
  if (!is.array(raster_array) || length(dim(raster_array)) < 2) {
    return(raster_array)  # Return unchanged if not a proper array
  }
  
  dims <- dim(raster_array)
  original_height <- dims[1]
  original_width <- dims[2]
  channels <- if (length(dims) > 2) dims[3] else 1
  
  # Calculate scaling factors
  scale_x <- original_width / target_width
  scale_y <- original_height / target_height
  
  # Create new array
  if (channels > 1) {
    new_array <- array(0, dim = c(target_height, target_width, channels))
    
    for (y in 1:target_height) {
      for (x in 1:target_width) {
        # Map to original coordinates using nearest neighbor
        orig_y <- min(round(y * scale_y), original_height)
        orig_x <- min(round(x * scale_x), original_width)
        if (orig_y < 1) orig_y <- 1
        if (orig_x < 1) orig_x <- 1
        
        # Copy all channels
        for (c in 1:channels) {
          new_array[y, x, c] <- raster_array[orig_y, orig_x, c]
        }
      }
    }
  } else {
    new_array <- array(0, dim = c(target_height, target_width))
    
    for (y in 1:target_height) {
      for (x in 1:target_width) {
        orig_y <- min(round(y * scale_y), original_height)
        orig_x <- min(round(x * scale_x), original_width)
        if (orig_y < 1) orig_y <- 1
        if (orig_x < 1) orig_x <- 1
        
        new_array[y, x] <- raster_array[orig_y, orig_x]
      }
    }
  }
  
  return(new_array)
}

# Convert raster image to base64 data URL for embedding in HTML
raster_to_data_url <- function(raster_content, target_width = 60, target_height = 60) {
  if (is.null(raster_content) || length(raster_content) == 0) {
    return(NULL)
  }
  
  tryCatch({
    library(png)
    
    # Create temporary PNG file
    temp_file <- tempfile(fileext = ".png")
    
    # Handle phylopic array format - resize before saving
    if (inherits(raster_content, "phylopic") && inherits(raster_content, "array")) {
      # Resize the raster array first
      resized_raster <- resize_raster_array(raster_content, target_width, target_height)
      # Write the resized array to PNG
      writePNG(resized_raster, temp_file)
    } else if (is.array(raster_content)) {
      # Handle generic arrays - resize first
      resized_raster <- resize_raster_array(raster_content, target_width, target_height)
      writePNG(resized_raster, temp_file)
    } else {
      # Fallback to plot method with smaller size
      png(temp_file, width = target_width, height = target_height, bg = "transparent")
      plot(raster_content)
      dev.off()
    }
    
    # Read PNG file and convert to base64
    png_raw <- readBin(temp_file, "raw", file.info(temp_file)$size)
    png_base64 <- base64enc::base64encode(png_raw)
    
    # Clean up temp file
    unlink(temp_file)
    
    # Return data URL
    data_url <- paste0("data:image/png;base64,", png_base64)
    return(data_url)
    
  }, error = function(e) {
    cat("Warning: Could not convert raster to data URL:", e$message, "\n")
    return(NULL)
  })
}

# Generate HTML for silhouette display in info panels
format_silhouette_html <- function(silhouette_data) {
  if (!silhouette_data$success) {
    return("")  # Return empty string if no silhouette available
  }
  
  # Convert raster to data URL for embedding
  data_url <- raster_to_data_url(silhouette_data$raster_content)
  
  if (is.null(data_url)) {
    return("")  # Return empty string if conversion failed
  }
  
  # Create HTML for silhouette display (attribution removed, cited on about page)
  silhouette_html <- paste0(
    '<div class="silhouette-section">',
    '<div class="silhouette-container">',
    '<img src="', data_url, '" alt="Ancestor silhouette" class="ancestor-silhouette" />',
    '</div>',
    '</div>'
  )
  
  return(silhouette_html)
}

# Test function to verify rphylopic functionality
test_phylopic_connection <- function() {
  cat("Testing rphylopic connection...\n")
  
  # Test with a well-known taxonomic group
  test_groups <- c("Mammalia", "Primates", "Canidae")
  
  for (group in test_groups) {
    cat("Testing", group, "...\n")
    result <- get_silhouette_data(group)
    if (result$success) {
      cat("  SUCCESS: Found silhouette for", group, "- UUID:", result$uuid, "\n")
    } else {
      cat("  FAILED:", group, "-", result$error, "\n")
    }
  }
}


# Get PhyloPic silhouette data formatted for inline SVG replacement
get_silhouette_for_node_replacement <- function(taxonomic_name, target_size = 35, use_cache = TRUE, inherited_color = NULL) {
  if (is.null(taxonomic_name) || is.na(taxonomic_name) || taxonomic_name == "") {
    return(list(success = FALSE, error = "No taxonomic name provided"))
  }

  # Get a random UUID for this taxonomic group using cached function if available
  if (use_cache && exists("cached_get_random_silhouette_uuid", mode = "function")) {
    uuid <- cached_get_random_silhouette_uuid(taxonomic_name)
  } else {
    uuid <- get_random_silhouette_uuid(taxonomic_name)
  }

  if (is.null(uuid)) {
    return(list(success = FALSE, error = "No silhouettes found"))
  }

  tryCatch({
    # Get the silhouette image as raster for consistent handling
    silhouette_raster <- get_phylopic(uuid = uuid, format = "raster")

    # Color handling: Use inherited color if available, otherwise skip coloring (default black)
    if (!is.null(inherited_color) && nchar(trimws(inherited_color)) > 0) {
      # Apply inherited color to the silhouette using recolor_phylopic
      colored_silhouette <- recolor_phylopic(silhouette_raster, fill = inherited_color, remove_background = TRUE)
      applied_color <- inherited_color
    } else {
      # No color inheritance - use default black silhouette (visually obvious failure)
      colored_silhouette <- silhouette_raster
      applied_color <- "#000000"  # Default black
    }

    # Convert silhouette to data URL with optimal sizing for node replacement
    data_url <- raster_to_data_url(colored_silhouette, target_width = target_size, target_height = target_size)

    if (is.null(data_url)) {
      return(list(success = FALSE, error = "Failed to convert silhouette to data URL"))
    }

    # Get attribution information
    attribution <- get_attribution(uuid = uuid)
    attribution_text <- if (is.data.frame(attribution) && nrow(attribution) > 0) {
      contributor <- if (!is.na(attribution$contributor[1])) attribution$contributor[1] else "Unknown"
      paste("Silhouette by", contributor, "via PhyloPic")
    } else {
      "Silhouette via PhyloPic"
    }

    return(list(
      success = TRUE,
      uuid = uuid,
      data_url = data_url,
      attribution = attribution_text,
      phylopic_url = paste0("https://www.phylopic.org/images/", uuid),
      target_size = target_size,
      applied_color = applied_color,
      inherited_color = inherited_color,
      color_inheritance_used = !is.null(inherited_color)
    ))

  }, error = function(e) {
    cat("Warning: Could not retrieve silhouette for node replacement:", taxonomic_name, "UUID:", uuid, ":", e$message, "\n")
    return(list(success = FALSE, error = paste("Silhouette retrieval failed:", e$message)))
  })
}

# Create JavaScript object mapping taxonomic names to PhyloPic data for node replacement
create_phylopic_node_replacement_data <- function(network_data, request_id = NULL) {
  if (is.null(request_id)) {
    request_id <- "phylopic_nodes"
  }
  
  # Cached API functions are already sourced at startup
  tryCatch({
    # Check if cached functions are available
    api_log_info(paste("[", request_id, "] Using cached PhyloPic functions for improved performance"))
    use_cache <- TRUE
  }, error = function(e) {
    api_log_warn(paste("[", request_id, "] Could not load cached functions, using direct API calls:", e$message))
    use_cache <- FALSE
  })
  
  api_log_info(paste("[", request_id, "] Creating PhyloPic node replacement data...", sep=""))
  
  # Identify taxonomic nodes that need PhyloPic replacement
  taxonomic_nodes <- list()
  
  for (i in 1:nrow(network_data)) {
    node_info <- as.list(network_data[i, ])
    
    # Check if this is a taxonomic node (not species, not generic ancestor)
    if ("NodeType" %in% names(node_info) && node_info$NodeType == "taxonomic") {
      # Get the scientific name for PhyloPic lookup (original taxonomic name)
      scientific_name <- extract_taxonomic_name(node_info)

      # Get the display name for JavaScript key (common name from NCBI or fallback)
      # This is what appears in the tree and what JavaScript will use to look up the data
      display_name <- if ("to" %in% names(node_info)) {
        node_info$to
      } else if ("Child" %in% names(node_info)) {
        node_info$Child
      } else {
        scientific_name  # Fallback to scientific name if no display name field
      }

      # Strip age info from display name to match JavaScript extraction
      display_name <- sub("\\s*\\(~?[0-9]+\\.?[0-9]*\\s+Mya\\).*$", "", display_name)
      display_name <- trimws(display_name)

      if (!is.null(scientific_name) && nchar(trimws(scientific_name)) > 2) {
        # Skip generic ancestor names
        if (!grepl("^(Ancestor|Node)\\.+[A-Z]$", scientific_name) &&
            !grepl("^Common ancestor", scientific_name)) {

          api_log_info(paste("[", request_id, "] Fetching PhyloPic for taxonomic node:", display_name, "(scientific:", scientific_name, ")"))
          
          # Extract age information for color gradient
          age_value <- NULL
          has_age_data <- FALSE
          
          # Check for age data in various possible fields
          if ("Age" %in% names(node_info) && !is.null(node_info$Age) && !is.na(node_info$Age)) {
            age_value <- as.numeric(node_info$Age)
            has_age_data <- TRUE
          } else if ("AgeValue" %in% names(node_info) && !is.null(node_info$AgeValue) && !is.na(node_info$AgeValue)) {
            age_value <- as.numeric(node_info$AgeValue)  
            has_age_data <- TRUE
          }
          
          # Validate age data
          if (has_age_data && (is.na(age_value) || age_value <= 0)) {
            has_age_data <- FALSE
            age_value <- NULL
          }
          
          # Get PhyloPic data using scientific name for lookup (better chance of finding silhouettes)
          phylopic_result <- get_silhouette_for_node_replacement(
            scientific_name,  # Use scientific name for PhyloPic API lookup
            target_size = 35,
            use_cache = use_cache,
            inherited_color = NULL  # Color inheritance happens on frontend
          )

          if (phylopic_result$success) {
            # Key by display name (what JavaScript will use for lookup)
            taxonomic_nodes[[display_name]] <- list(
              data_url = phylopic_result$data_url,
              uuid = phylopic_result$uuid,
              attribution = phylopic_result$attribution,
              phylopic_url = phylopic_result$phylopic_url,
              target_size = phylopic_result$target_size,
              applied_color = phylopic_result$applied_color,
              color_inheritance_used = phylopic_result$color_inheritance_used,
              age_value = age_value,
              has_age_data = has_age_data
            )
            api_log_info(paste("[", request_id, "] PhyloPic success for:", display_name, "(looked up as:", scientific_name, ")"))
          } else {
            api_log_info(paste("[", request_id, "] PhyloPic failed for:", scientific_name, "-", phylopic_result$error))
            # Don't add failed ones to the mapping - they'll fall back to orange circles
          }
        }
      }
    }
  }
  
  api_log_info(paste("[", request_id, "] PhyloPic node replacement data created for", length(taxonomic_nodes), "taxonomic groups"))
  
  # Convert to JSON for JavaScript consumption  
  if (length(taxonomic_nodes) > 0) {
    json_data <- jsonlite::toJSON(taxonomic_nodes, auto_unbox = TRUE)
    return(json_data)
  } else {
    return("{}")  # Empty object if no PhyloPic data available
  }
}

cat("PhyloPic silhouette functions loaded successfully\n")