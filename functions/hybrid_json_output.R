# Required libraries
library(jsonlite)

convert_network_to_nested_json <- function(network_data, tree_data, request_id = NULL, phylopic_data = NULL, as_json = FALSE, progress_token = NULL) {
  if (is.null(request_id)) {
    request_id <- "json_convert"
  }

  # Helper function to update progress if token provided
  update_progress_internal <- function(step_name, status = "completed", additional_data = NULL) {
    if (!is.null(progress_token) && progress_token != "") {
      update_progress(progress_token, step_name, status, additional_data)
    }
  }

  api_log_info(paste("[", request_id, "] Converting network data to nested JSON structure..."))

  # Step 1: Processing tree nodes and metadata
  update_progress_internal("processing_tree_nodes", "in_progress",
                         list(step = "Processing tree nodes and metadata"))

  # Create a mapping from child names to their data
  child_to_data <- list()
  for (i in 1:nrow(tree_data)) {
    child_name <- tree_data$Child[i]
    child_to_data[[child_name]] <- tree_data[i, ]
  }

  # Helper function to extract node metadata from info panel data
  extract_node_metadata <- function(node_name, node_data) {
    # Initialize metadata with defaults
    metadata <- list(
      node_label = node_name,
      node_type = if (!is.null(node_data$NodeType)) node_data$NodeType else "unknown",
      color = if (!is.null(node_data$Color)) node_data$Color else "#666666",
      has_age = if (!is.null(node_data$HasAge)) node_data$HasAge else FALSE,
      age_info = "age unavailable",
      age_numeric = NA_real_,
      node_shape = "circle",
      image_url = NA_character_,
      image_type = "none",
      image_attribution = NA_character_,
      wikipedia_text = NA_character_,
      wikipedia_url = NA_character_,
      wikipedia_title = NA_character_,
      geologic_age = NA_character_,
      phylopic_uuid = NA_character_,
      phylopic_url = NA_character_,
      phylopic_attribution = NA_character_
    )

    # Extract age information from network_data if available
    network_row <- network_data[network_data$to == node_name, ]
    if (nrow(network_row) > 0) {
      metadata$age_info <- if (!is.na(network_row$AgeInfo[1])) network_row$AgeInfo[1] else "age unavailable"
      metadata$has_age <- if (!is.na(network_row$HasAge[1])) network_row$HasAge[1] else FALSE

      # Extract numeric age from age_info
      if (metadata$has_age && metadata$age_info != "age unavailable") {
        age_match <- regmatches(metadata$age_info, regexpr("\\d+\\.?\\d*", metadata$age_info))
        if (length(age_match) > 0) {
          metadata$age_numeric <- as.numeric(age_match[1])
        }
      }
    }

    # Check for PhyloPic data from our dedicated collection first
    phylopic_found <- FALSE
    if (!is.null(phylopic_data)) {
      # Use original taxonomic name from node_data for PhyloPic lookups (before NCBI transformation)
      taxonomic_name <- if (!is.null(node_data$TaxonomicName) && !is.na(node_data$TaxonomicName) && nchar(node_data$TaxonomicName) > 0) {
        node_data$TaxonomicName
      } else {
        # Fallback: Extract from node name if TaxonomicName not available
        temp_name <- gsub("\\s*\\([^)]*\\)\\s*", "", node_name)  # Remove age info
        temp_name <- gsub("^(Ancestor|Node)\\s+", "", temp_name)  # Remove prefixes
        trimws(temp_name)
      }

      # Look for PhyloPic data for this taxonomic name
      if (taxonomic_name %in% names(phylopic_data)) {
        phylopic_info <- phylopic_data[[taxonomic_name]]
        if (!is.null(phylopic_info$uuid) && phylopic_info$uuid != "") {
          metadata$phylopic_uuid <- phylopic_info$uuid
          metadata$phylopic_url <- phylopic_info$phylopic_url
          metadata$phylopic_attribution <- phylopic_info$attribution
          # For JSON output, use UUID; for HTML output, use full URL
          metadata$node_shape <- if (as_json) phylopic_info$uuid else phylopic_info$phylopic_url
          phylopic_found <- TRUE
        }
      }
    }

    # Enhanced info panel data extraction with robust pattern matching
    if (!is.null(node_data$InfoPanel) && node_data$InfoPanel != "") {
      info_html <- node_data$InfoPanel

      # Extract PhyloPic data from HTML only if not found in dedicated collection
      if (!phylopic_found) {
        # Extract PhyloPic data with multiple pattern attempts
        # Pattern 1: data-uuid attribute
        phylopic_patterns <- c(
          'data-uuid="([^"]+)"',
          'uuid-([a-f0-9\\-]{36})',
          'phylopic\\.org/images/([^/]+)/'
        )

        for (pattern in phylopic_patterns) {
          phylopic_match <- regmatches(info_html, gregexpr(pattern, info_html, ignore.case = TRUE))
          if (length(phylopic_match[[1]]) > 0) {
            # Extract UUID from the first match
            uuid_text <- phylopic_match[[1]][1]
            uuid_clean <- gsub('.*([a-f0-9\\-]{36}).*', '\\1', uuid_text, ignore.case = TRUE)
            if (nchar(uuid_clean) == 36 && grepl('[a-f0-9\\-]{36}', uuid_clean)) {
              metadata$phylopic_uuid <- uuid_clean
              metadata$phylopic_url <- paste0("https://images.phylopic.org/images/", uuid_clean, "/vector.svg")
              # For JSON output, use UUID; for HTML output, use full URL
              metadata$node_shape <- if (as_json) uuid_clean else metadata$phylopic_url
              # Don't overwrite image_url - it should come from the actual info panel image
              break
            }
          }
        }
      }

      # Extract Wikipedia data with multiple patterns
      wiki_patterns <- c(
        'href="(https://en\\.wikipedia\\.org/wiki/[^"]+)"',
        'wikipedia\\.org/wiki/([^"\\s]+)',
        'en\\.wikipedia\\.org/wiki/([^"\\s>]+)'
      )

      for (pattern in wiki_patterns) {
        wiki_match <- regmatches(info_html, gregexpr(pattern, info_html, ignore.case = TRUE))
        if (length(wiki_match[[1]]) > 0) {
          url_text <- wiki_match[[1]][1]
          # Clean URL extraction
          if (grepl('href=', url_text)) {
            clean_url <- gsub('.*href="([^"]+)".*', '\\1', url_text)
          } else {
            clean_url <- paste0("https://en.wikipedia.org/wiki/", gsub('.*wikipedia\\.org/wiki/([^"\\s>]+).*', '\\1', url_text))
          }
          if (grepl('https://en\\.wikipedia\\.org/wiki/', clean_url)) {
            metadata$wikipedia_url <- clean_url
            break
          }
        }
      }

      # Extract Wikipedia text content specifically from wikipedia-summary div
      wiki_summary_pattern <- '<div class="wikipedia-summary">([^<]*(?:<[^>]*>[^<]*)*?)</div>'
      wiki_summary_match <- regmatches(info_html, regexpr(wiki_summary_pattern, info_html))

      if (length(wiki_summary_match) > 0) {
        # Extract content between the div tags
        wiki_content <- gsub('<div class="wikipedia-summary">(.*?)</div>', '\\1', wiki_summary_match, perl = TRUE)
        # Clean up any remaining HTML tags within the content
        wiki_content <- gsub('<[^>]+>', '', wiki_content)
        # Clean up whitespace
        wiki_content <- gsub('\\s+', ' ', wiki_content)
        wiki_content <- trimws(wiki_content)

        if (nchar(wiki_content) > 10) {
          # Limit to 500 characters for the JSON field
          metadata$wikipedia_text <- if (nchar(wiki_content) > 500) {
            paste0(substr(wiki_content, 1, 497), "...")
          } else {
            wiki_content
          }
        }
      }

      # Extract geological age information
      # Look for patterns like "During the [Period] Period" or "lived during the [Period]"
      geologic_patterns <- c(
        'During the ([^.]+(?:Period|Era|Epoch))',
        'during the ([^.]+(?:Period|Era|Epoch))',
        'lived.*during.*the ([^.]+(?:Period|Era|Epoch))',
        'from the ([^.]+(?:Period|Era|Epoch))',
        'in the ([^.]+(?:Period|Era|Epoch))'
      )

      for (pattern in geologic_patterns) {
        geologic_match <- regmatches(info_html, regexpr(pattern, info_html, ignore.case = TRUE))
        if (length(geologic_match) > 0) {
          # Extract the geological period from the match
          period_text <- gsub(pattern, '\\1', geologic_match, ignore.case = TRUE, perl = TRUE)
          # Clean HTML tags from the extracted text
          period_text <- gsub("<[^>]*>", "", period_text)
          period_text <- trimws(period_text)
          if (nchar(period_text) > 0 && nchar(period_text) < 100) {
            metadata$geologic_age <- period_text
            break
          }
        }
      }

      # For JSON output, fetch Wikimedia images directly instead of using override system
      # This provides clean URLs rather than base64 data
      api_log_info(paste("[JSON] Node:", node_name, "| Type:", metadata$node_type, "| Image URL:", metadata$image_url))
      if (metadata$node_type == "taxonomic" && (is.na(metadata$image_url) || metadata$image_url == "" || is.null(metadata$image_url))) {
        # Use original taxonomic name from node_data for Wikipedia lookups (before NCBI transformation)
        taxonomic_name <- if (!is.null(node_data$TaxonomicName) && !is.na(node_data$TaxonomicName) && nchar(node_data$TaxonomicName) > 0) {
          node_data$TaxonomicName
        } else {
          # Fallback: Extract from node name if TaxonomicName not available
          temp_name <- gsub("\\s*\\([^)]*\\)\\s*", "", node_name)  # Remove age info
          temp_name <- gsub("^(Ancestor|Node)\\s+", "", temp_name)  # Remove prefixes
          trimws(temp_name)
        }

        api_log_info(paste("[JSON] Attempting Wikimedia image fetch for taxonomic node:", taxonomic_name))

        if (nchar(taxonomic_name) > 0) {
          tryCatch({
            # Cached functions are already sourced at startup
            api_log_info(paste("[JSON] Calling cached_get_wikimedia_image_enhanced for:", taxonomic_name))
            wikimedia_result <- cached_get_wikimedia_image_enhanced(taxonomic_name, target_width = 200)
            if (wikimedia_result$success) {
              api_log_info(paste("[JSON] Wikimedia image found:", wikimedia_result$image_url))
              metadata$image_url <- wikimedia_result$image_url
              metadata$image_type <- "wikimedia"
              metadata$image_attribution <- wikimedia_result$attribution
            } else {
              api_log_info(paste("[JSON] Wikimedia image not found for", taxonomic_name, ":", wikimedia_result$error))
            }
          }, error = function(e) {
            api_log_warn(paste("[JSON] Wikimedia image fetch failed for", taxonomic_name, ":", e$message))
          })
        }
      }

      # Extract image URLs from info panel (always run to get actual display image)
      if (is.na(metadata$image_url) || metadata$image_url == "" || is.null(metadata$image_url)) {
        img_patterns <- c(
          '<img[^>]+src="([^"]+)"',
          'src="([^"]+\\.(jpg|jpeg|png|gif|svg))"'
        )

        for (pattern in img_patterns) {
          img_match <- regmatches(info_html, gregexpr(pattern, info_html, ignore.case = TRUE))
          if (length(img_match[[1]]) > 0) {
            src_text <- img_match[[1]][1]
            url_clean <- gsub('.*src="([^"]+)".*', '\\1', src_text)
            if (grepl('^https?://', url_clean) || grepl('^image_overrides/', url_clean) || grepl('^data:image/', url_clean)) {
              metadata$image_url <- url_clean
              if (grepl("wikimedia|wikipedia", url_clean, ignore.case = TRUE)) {
                metadata$image_type <- "wikimedia"
              } else if (grepl("unsplash", url_clean, ignore.case = TRUE)) {
                metadata$image_type <- "unsplash"
              } else if (grepl("pixabay", url_clean, ignore.case = TRUE)) {
                metadata$image_type <- "pixabay"
              } else if (grepl("^image_overrides/", url_clean) || grepl("^data:image/", url_clean)) {
                metadata$image_type <- "override"
              } else {
                metadata$image_type <- "other"
              }
              break
            }
          }
        }
      }

      # Extract attribution information
      if (grepl("attribution|license|credit|©|&copy;", info_html, ignore.case = TRUE)) {
        # Simple attribution extraction - look for common attribution patterns
        attr_text <- gsub('<[^>]+>', ' ', info_html)
        attr_text <- gsub('\\s+', ' ', attr_text)

        # Extract text that looks like attribution
        attr_patterns <- c(
          '(Image by[^.]+)',
          '(©[^.]+)',
          '(Attribution[^.]+)',
          '(License[^.]+)',
          '(Credit[^.]+)'
        )

        for (pattern in attr_patterns) {
          attr_match <- regmatches(attr_text, regexpr(pattern, attr_text, ignore.case = TRUE))
          if (length(attr_match) > 0) {
            metadata$image_attribution <- trimws(attr_match[1])
            break
          }
        }
      }
    }

    # Group info panel related fields together
    metadata$info_panel <- list(
      image_url = metadata$image_url,
      image_type = metadata$image_type,
      image_attribution = metadata$image_attribution,
      wikipedia_text = metadata$wikipedia_text,
      wikipedia_url = metadata$wikipedia_url,
      wikipedia_title = metadata$wikipedia_title,
      geologic_age = metadata$geologic_age
    )

    # Remove individual fields from top level since they're now in info_panel
    metadata$image_url <- NULL
    metadata$image_type <- NULL
    metadata$image_attribution <- NULL
    metadata$wikipedia_text <- NULL
    metadata$wikipedia_url <- NULL
    metadata$wikipedia_title <- NULL
    metadata$geologic_age <- NULL

    return(metadata)
  }

  # Helper function to count total descendants (leaf nodes) recursively
  count_total_descendants <- function(node) {
    if (is.null(node$children) || length(node$children) == 0) {
      # Leaf node has 0 descendants
      return(0)
    }

    # Internal node: count all descendants recursively
    total_count <- length(node$children)  # Direct children count
    for (child in node$children) {
      total_count <- total_count + count_total_descendants(child)
    }
    return(total_count)
  }

  # Helper function to sort children by total descendant count (fewest first)
  sort_children_by_descendant_count <- function(children) {
    if (is.null(children) || length(children) <= 1) {
      return(children)
    }

    # Calculate descendant counts for each child
    children_with_counts <- list()
    for (i in 1:length(children)) {
      child <- children[[i]]
      descendant_count <- count_total_descendants(child)
      children_with_counts[[i]] <- list(
        node = child,
        count = descendant_count
      )
    }

    # Sort by descendant count (ascending - leaf nodes first)
    sorted_indices <- order(sapply(children_with_counts, function(x) x$count))
    sorted_children <- list()
    for (i in sorted_indices) {
      sorted_children[[length(sorted_children) + 1]] <- children_with_counts[[i]]$node
    }

    return(sorted_children)
  }

  # Helper function to build tree recursively with cycle detection
  build_tree_recursive <- function(parent_name, visited = character(0)) {
    # Cycle detection - prevent infinite recursion
    if (parent_name %in% visited) {
      api_log_warn(paste("[", request_id, "] Cycle detected for node:", parent_name, "- stopping recursion"))
      node_data <- child_to_data[[parent_name]]
      return(extract_node_metadata(parent_name, node_data))
    }

    # Add current node to visited set
    visited <- c(visited, parent_name)

    # Get all direct children of this parent
    children_data <- network_data[network_data$from == parent_name & !is.na(network_data$from), ]

    if (nrow(children_data) == 0) {
      # Leaf node - return just the metadata
      node_data <- child_to_data[[parent_name]]
      return(extract_node_metadata(parent_name, node_data))
    }

    # Internal node - build with children
    node_data <- child_to_data[[parent_name]]
    node_metadata <- extract_node_metadata(parent_name, node_data)

    # Recursively build children
    children <- list()
    for (i in 1:nrow(children_data)) {
      child_name <- children_data$to[i]
      if (!is.na(child_name) && child_name != parent_name) {  # Prevent self-reference
        child_node <- build_tree_recursive(child_name, visited)
        children[[length(children) + 1]] <- child_node
      } else {
        api_log_warn(paste("[", request_id, "] Skipping invalid child:", child_name, "for parent:", parent_name))
      }
    }

    if (length(children) > 0) {
      # Sort children by total descendant count (leaf nodes first)
      sorted_children <- sort_children_by_descendant_count(children)
      node_metadata$children <- sorted_children
    }
    return(node_metadata)
  }

  # Find the root node - should be a node that has NA as parent (appears in 'to' with NA 'from')
  api_log_info(paste("[", request_id, "] Network data analysis:"))
  api_log_info(paste("[", request_id, "]   Total edges:", nrow(network_data)))

  # Show first few rows for debugging
  for (i in 1:min(5, nrow(network_data))) {
    api_log_info(paste("[", request_id, "]   Edge", i, ":",
                      network_data$from[i], "->", network_data$to[i]))
  }

  # Find the true root - node with NA parent
  root_rows <- network_data[is.na(network_data$from), ]
  api_log_info(paste("[", request_id, "]   Rows with NA parents:", nrow(root_rows)))

  if (nrow(root_rows) > 0) {
    root_name <- root_rows$to[1]
    api_log_info(paste("[", request_id, "]   Using NA parent root:", root_name))
  } else {
    # Fallback: find node that appears as parent but never as child
    all_children <- network_data$to[!is.na(network_data$to)]
    all_parents <- network_data$from[!is.na(network_data$from)]
    true_root_candidates <- setdiff(all_parents, all_children)

    api_log_info(paste("[", request_id, "]   True root candidates (parents never children):", paste(true_root_candidates, collapse = ", ")))

    if (length(true_root_candidates) > 0) {
      root_name <- true_root_candidates[1]
      api_log_info(paste("[", request_id, "]   Using true root candidate:", root_name))
    } else {
      # Last resort: use first node that appears
      root_name <- all_children[1]
      api_log_info(paste("[", request_id, "]   Using fallback first node:", root_name))
    }
  }

  api_log_info(paste("[", request_id, "] Building tree from root:", root_name))

  # Build the complete tree
  tree_json <- build_tree_recursive(root_name)

  # If as_json is TRUE and the root has children, return the children directly (skip root)
  if (as_json && !is.null(tree_json$children) && length(tree_json$children) > 0) {
    api_log_info(paste("[", request_id, "] Removing common ancestor root from JSON output"))

    # If there's only one child, return it directly
    if (length(tree_json$children) == 1) {
      tree_json <- tree_json$children[[1]]
    } else {
      # If there are multiple children, we need to return an array
      # This is unusual but we'll keep all children
      tree_json <- tree_json$children
    }
  }

  api_log_info(paste("[", request_id, "] JSON tree structure created successfully"))
  return(tree_json)
}

#' Create hybrid tree JSON structure
#' @param network_data Network data frame with age information
#' @param request_id Optional request ID for logging correlation
#' @param progress_token Optional progress token for tracking external API calls
#' @return Nested JSON structure representing the tree
create_hybrid_tree_json <- function(network_data, request_id = NULL, progress_token = NULL, as_json = FALSE) {
  if (is.null(request_id)) {
    request_id <- "json_create"
  }

  # Helper function to update progress if token provided
  update_progress_internal <- function(step_name, status = "completed", additional_data = NULL) {
    if (!is.null(progress_token) && progress_token != "") {
      update_progress(progress_token, step_name, status, additional_data)
    }
  }

  api_log_info(paste("[", request_id, "] Starting hybrid tree JSON creation..."))

  # Step 5.1: Prepare tree structure data (same as HTML version)
  update_progress_internal("preparing_json_structure", "in_progress",
                         list(step = "Preparing tree structure and color mapping"))
  tree_data <- data.frame(
    Parent = network_data$from,
    Child = network_data$to,
    NodeType = network_data$NodeType,
    HasAge = network_data$HasAge,
    stringsAsFactors = FALSE
  )

  # Add color mapping using new adaptive ancestral node coloring system
  tree_data$Color <- character(nrow(tree_data))

  # Get ancestral node colors using new system
  ancestral_colors <- get_ancestral_node_color(network_data)

  # Assign colors to each row
  ancestral_idx <- 1
  for (i in 1:nrow(tree_data)) {
    node_type <- tree_data$NodeType[i]

    if (node_type %in% c("taxonomic", "ancestor")) {
      # Use new ancestral coloring system
      tree_data$Color[i] <- ancestral_colors[ancestral_idx]
      ancestral_idx <- ancestral_idx + 1
    } else {
      # Use original system for root and species
      has_age <- tree_data$HasAge[i]
      tree_data$Color[i] <- get_node_color(node_type, has_age)
    }
  }
  update_progress_internal("preparing_json_structure", "completed")

  # Step 5.2: Transform network data for info panels (same as HTML version)
  update_progress_internal("transforming_network_data", "in_progress",
                         list(step = "Transforming network data to info panel format"))
  info_panel_network_data <- transform_hybrid_to_info_panel_format(network_data)
  update_progress_internal("transforming_network_data", "completed")

  # Step 5.3: Generate info panel data (Wikipedia & PhyloPic content)
  update_progress_internal("generating_info_panels", "in_progress",
                         list(step = "Generating info panels with Wikipedia and PhyloPic data"))
  tree_data <- add_info_panel_data(tree_data, info_panel_network_data, request_id, progress_token)
  update_progress_internal("generating_info_panels", "completed")

  # Step 5.3.5: Collect PhyloPic metadata for JSON output (regardless of override images)
  update_progress_internal("collecting_phylopic_data", "in_progress",
                         list(step = "Collecting PhyloPic metadata for JSON output"))
  phylopic_data <- NULL
  tryCatch({
    # PhyloPic functions are already sourced at startup
    phylopic_json_raw <- create_phylopic_node_replacement_data(network_data, request_id)
    if (!is.null(phylopic_json_raw) && phylopic_json_raw != "{}") {
      phylopic_data <- jsonlite::fromJSON(phylopic_json_raw)
      api_log_info(paste("[", request_id, "] PhyloPic metadata collected for", length(phylopic_data), "taxonomic nodes"))
    } else {
      api_log_info(paste("[", request_id, "] No PhyloPic data available for taxonomic nodes"))
    }
  }, error = function(e) {
    api_log_warn(paste("[", request_id, "] Failed to collect PhyloPic metadata:", e$message))
  })
  update_progress_internal("collecting_phylopic_data", "completed")

  # Step 5.4: Convert to nested JSON structure (with granular progress tracking)
  tree_json <- convert_network_to_nested_json(network_data, tree_data, request_id, phylopic_data, as_json, progress_token)

  api_log_info(paste("[", request_id, "] Hybrid tree JSON creation completed"))
  return(tree_json)
}