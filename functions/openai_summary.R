# OpenAI ChatGPT Integration for Taxonomic Summary Generation
# Functions for converting scientific/taxonomic names into plain language summaries
# Used to improve Unsplash image search relevance

library(httr2)
library(jsonlite)

# Source shared logging configuration
source("functions/logging_config.R")

# ChatGPT logging is now configured in logging_config.R before parallel workers start

# Wrapper function with automatic retry for garbled responses
get_chatgpt_common_name_with_retry <- function(taxonomic_group_name, max_retries = 2) {
  temperatures <- c(2.0, 1.5, 1.0)  # Start high, reduce on retry

  for (attempt in 1:min(max_retries + 1, length(temperatures))) {
    temp <- temperatures[attempt]

    if (attempt > 1) {
      chatgpt_log_info(paste("[CHATGPT-RETRY] Attempting retry", attempt - 1, "for", taxonomic_group_name, "with temperature", temp))
    }

    # Temporarily modify the temperature in the original function
    # We'll do this by making a copy and modifying it
    result <- tryCatch({
      get_chatgpt_common_name_internal(taxonomic_group_name, temperature = temp)
    }, error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    })

    # If successful and no retry recommended, return result
    if (result$success || !isTRUE(result$retry_recommended)) {
      if (result$success && attempt > 1) {
        chatgpt_log_info(paste("[CHATGPT-RETRY] Successful retry for", taxonomic_group_name, "after", attempt - 1, "attempts"))
      }
      return(result)
    }

    # Log the retry attempt
    if (attempt <= max_retries) {
      chatgpt_log_warn(paste("[CHATGPT-RETRY] Attempt", attempt, "failed for", taxonomic_group_name, "- retrying with lower temperature"))
    }
  }

  # All retries exhausted
  chatgpt_log_error(paste("[CHATGPT-RETRY] All retry attempts exhausted for", taxonomic_group_name))
  return(list(
    success = FALSE,
    error = "All retry attempts failed to generate valid response",
    taxonomic_group = taxonomic_group_name,
    retry_attempts = max_retries
  ))
}

# Main function to get ChatGPT summary of taxonomic group
get_chatgpt_summary <- function(taxonomic_group_name) {
  # Ensure logging is configured in parallel workers - force reload every time
  source("functions/logging_config.R", local = TRUE)

  if (is.null(taxonomic_group_name) || is.na(taxonomic_group_name) || taxonomic_group_name == "") {
    return(list(success = FALSE, error = "No taxonomic group name provided"))
  }

  tryCatch({
    # Get OpenAI API key from environment
    api_key <- Sys.getenv("OPENAI_KEY")
    if (api_key == "") {
      return(list(
        success = FALSE,
        error = "OpenAI API key not configured",
        taxonomic_group = taxonomic_group_name
      ))
    }

    # Construct the prompt
    prompt <- paste0("Using plain, everyday language, summarize the taxonomic group ",
                     taxonomic_group_name,
                     " in at most three words (one if possible):")

    # Log the prompt
    chatgpt_log_info(paste("[CHATGPT-SUMMARY] Prompt for", taxonomic_group_name, ":", prompt))

    # Prepare the API request payload
    request_body <- list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      ),
      max_tokens = 20,
      temperature = 0.3,
      n = 1
    )

    # Log the full request
    chatgpt_log_info(paste("[CHATGPT-SUMMARY] Request for", taxonomic_group_name, "- model:", request_body$model, "temp:", request_body$temperature, "max_tokens:", request_body$max_tokens))

    # Make API request to OpenAI
    api_url <- "https://api.openai.com/v1/chat/completions"

    response <- request(api_url) |>
      req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_timeout(30) |>
      req_body_json(request_body) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    if (resp_status(response) != 200) {
      error_msg <- if (resp_status(response) == 401) {
        "Invalid OpenAI API key"
      } else if (resp_status(response) == 429) {
        "OpenAI API rate limit exceeded"
      } else if (resp_status(response) == 400) {
        "Bad request to OpenAI API"
      } else {
        paste("OpenAI API error:", resp_status(response))
      }

      return(list(
        success = FALSE,
        error = error_msg,
        taxonomic_group = taxonomic_group_name,
        status_code = resp_status(response)
      ))
    }

    response_data <- resp_body_json(response)

    # Check if we have a valid response
    if (is.null(response_data$choices) || length(response_data$choices) == 0) {
      return(list(
        success = FALSE,
        error = "No response from OpenAI API",
        taxonomic_group = taxonomic_group_name
      ))
    }

    # Extract the summary text
    summary <- response_data$choices[[1]]$message$content
    summary <- trimws(summary)

    # Log the raw response
    chatgpt_log_info(paste("[CHATGPT-SUMMARY] Raw response for", taxonomic_group_name, ":", summary))

    # Clean up the common name - remove quotes and extra punctuation
    common_name <- gsub('^["\']|["\']$', '', summary)  # Remove leading/trailing quotes
    common_name <- gsub('[.!?]$', '', common_name)     # Remove trailing punctuation
    common_name <- trimws(common_name)

    # Advanced validation for gibberish/garbled responses
    is_valid_response <- function(text) {
      # Check if empty or too long
      if (text == "" || nchar(text) > 50) return(FALSE)

      # Check for non-ASCII characters (like "ständ" in the Teleostei response)
      if (any(utf8ToInt(text) > 127)) return(FALSE)

      # Check for obvious gibberish patterns
      if (grepl("[^a-zA-Z0-9 '.,-]", text)) return(FALSE)  # Invalid characters
      if (grepl("\\s{3,}", text)) return(FALSE)  # Multiple consecutive spaces
      if (grepl("[a-z][A-Z]{3,}", text)) return(FALSE)  # Mixed case patterns like "reifeaned"

      # Check word length distribution (no words longer than 15 chars)
      words <- strsplit(text, "\\s+")[[1]]
      if (any(nchar(words) > 15)) return(FALSE)

      # Must have at least one recognizable word pattern
      if (!grepl("[a-zA-Z]{2,}", text)) return(FALSE)

      return(TRUE)
    }

    # Validate the response
    if (!is_valid_response(common_name)) {
      chatgpt_log_warn(paste("[CHATGPT-COMMON_NAME] Invalid response detected for", taxonomic_group_name, ":", common_name))
      return(list(
        success = FALSE,
        error = "Invalid or garbled response from ChatGPT",
        taxonomic_group = taxonomic_group_name,
        raw_response = common_name,
        retry_recommended = TRUE
      ))
    }

    # Log the final result
    chatgpt_log_info(paste("[CHATGPT-SUMMARY] Final result for", taxonomic_group_name, ":", summary, "(tokens:", response_data$usage$total_tokens %||% "unknown", ")"))

    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group_name,
      summary = summary,
      original_prompt = prompt,
      tokens_used = response_data$usage$total_tokens %||% NA
    ))

  }, error = function(e) {
    # Log the error
    chatgpt_log_error(paste("[CHATGPT-SUMMARY] Error for", taxonomic_group_name, ":", conditionMessage(e)))

    return(list(
      success = FALSE,
      error = paste("Error getting ChatGPT summary:", conditionMessage(e)),
      taxonomic_group = taxonomic_group_name
    ))
  })
}

# Test function to verify OpenAI API connectivity
test_openai_connection <- function() {
  chatgpt_log_info("Testing OpenAI API connection...")

  # Test with well-known taxonomic groups
  test_groups <- c("Mammalia", "Boreoeutheria", "Primates", "Lepidoptera", "Cephalopoda")

  for (group in test_groups) {
    chatgpt_log_info(paste("Testing ChatGPT summary for", group, "..."))
    result <- get_chatgpt_summary(group)
    if (result$success) {
      chatgpt_log_info(paste("  SUCCESS:", group, "->", result$summary))
      if (!is.na(result$tokens_used)) {
        chatgpt_log_info(paste("  Tokens used:", result$tokens_used))
      }
    } else {
      chatgpt_log_info(paste("  FAILED:", group, "-", result$error))
    }

    # Small delay to respect rate limits
    Sys.sleep(0.5)
  }

  chatgpt_log_info("OpenAI API connection test completed")
}


# ChatGPT image selection function - chooses best image from multiple options
get_chatgpt_image_selection <- function(taxonomic_group_name, wikipedia_summary, image_descriptions) {
  if (is.null(taxonomic_group_name) || is.na(taxonomic_group_name) || taxonomic_group_name == "") {
    return(list(success = FALSE, error = "No taxonomic group name provided"))
  }

  if (is.null(wikipedia_summary) || is.na(wikipedia_summary) || wikipedia_summary == "") {
    return(list(success = FALSE, error = "No Wikipedia summary provided"))
  }

  if (is.null(image_descriptions) || length(image_descriptions) == 0) {
    return(list(success = FALSE, error = "No image descriptions provided"))
  }

  tryCatch({
    # Get OpenAI API key from environment
    api_key <- Sys.getenv("OPENAI_KEY")
    if (api_key == "") {
      return(list(
        success = FALSE,
        error = "OpenAI API key not configured",
        taxonomic_group = taxonomic_group_name
      ))
    }

    # Format image descriptions with numbers for easy reference
    formatted_descriptions <- paste(
      sapply(1:length(image_descriptions), function(i) {
        desc <- if (is.na(image_descriptions[i]) || image_descriptions[i] == "") {
          "No description available"
        } else {
          image_descriptions[i]
        }
        paste0(i, ". ", desc)
      }),
      collapse = "\n"
    )

    # Construct the selection prompt
    prompt <- paste0(
      "Based on this taxonomic group: ", taxonomic_group_name,
      " and this Wikipedia summary: ", wikipedia_summary,
      "\n\nWhich of these images is most appropriate? Image descriptions:\n",
      formatted_descriptions,
      "\n\nReply with just the image number (1-", length(image_descriptions),
      ") or 'no match' if none are suitable for this taxonomic group."
    )

    # Log the prompt
    chatgpt_log_info(paste("[CHATGPT-SELECTION] Prompt for", taxonomic_group_name, ":", prompt))

    # Prepare the API request payload
    request_body <- list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      ),
      max_tokens = 10,  # Very short response expected
      temperature = 0.1,  # Low temperature for consistent selection
      n = 1
    )

    # Log the full request
    chatgpt_log_info(paste("[CHATGPT-SELECTION] Request for", taxonomic_group_name, "- model:", request_body$model, "temp:", request_body$temperature, "max_tokens:", request_body$max_tokens, "images:", length(image_descriptions)))

    # Make API request to OpenAI
    api_url <- "https://api.openai.com/v1/chat/completions"

    response <- request(api_url) |>
      req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_timeout(30) |>
      req_body_json(request_body) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    if (resp_status(response) != 200) {
      error_msg <- if (resp_status(response) == 401) {
        "Invalid OpenAI API key"
      } else if (resp_status(response) == 429) {
        "OpenAI API rate limit exceeded"
      } else if (resp_status(response) == 400) {
        "Bad request to OpenAI API"
      } else {
        paste("OpenAI API error:", resp_status(response))
      }

      return(list(
        success = FALSE,
        error = error_msg,
        taxonomic_group = taxonomic_group_name,
        status_code = resp_status(response)
      ))
    }

    response_data <- resp_body_json(response)

    # Check if we have a valid response
    if (is.null(response_data$choices) || length(response_data$choices) == 0) {
      return(list(
        success = FALSE,
        error = "No response from OpenAI API",
        taxonomic_group = taxonomic_group_name
      ))
    }

    # Extract the selection result
    selection <- response_data$choices[[1]]$message$content
    selection <- trimws(tolower(selection))

    # Log the raw response
    chatgpt_log_info(paste("[CHATGPT-SELECTION] Raw response for", taxonomic_group_name, ":", selection))

    # Parse the response
    if (grepl("no match", selection)) {
      chatgpt_log_info(paste("[CHATGPT-SELECTION] No match decision for", taxonomic_group_name, "(tokens:", response_data$usage$total_tokens %||% "unknown", ")"))
      return(list(
        success = TRUE,
        taxonomic_group = taxonomic_group_name,
        selection = "no_match",
        raw_response = selection,
        tokens_used = response_data$usage$total_tokens %||% NA
      ))
    }

    # Try to extract a number from the response
    number_match <- regmatches(selection, regexpr("\\d+", selection))
    if (length(number_match) > 0) {
      selected_index <- as.numeric(number_match[1])
      if (selected_index >= 1 && selected_index <= length(image_descriptions)) {
        chatgpt_log_info(paste("[CHATGPT-SELECTION] Selected image", selected_index, "for", taxonomic_group_name, "(tokens:", response_data$usage$total_tokens %||% "unknown", ")"))
        return(list(
          success = TRUE,
          taxonomic_group = taxonomic_group_name,
          selection = selected_index,
          raw_response = selection,
          tokens_used = response_data$usage$total_tokens %||% NA
        ))
      }
    }

    # If we couldn't parse a valid selection, treat as no match
    chatgpt_log_info(paste("[CHATGPT-SELECTION] Parse error for", taxonomic_group_name, "- treating as no match (tokens:", response_data$usage$total_tokens %||% "unknown", ")"))
    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group_name,
      selection = "no_match",
      raw_response = selection,
      parse_error = "Could not parse valid selection from response",
      tokens_used = response_data$usage$total_tokens %||% NA
    ))

  }, error = function(e) {
    # Log the error
    chatgpt_log_error(paste("[CHATGPT-SELECTION] Error for", taxonomic_group_name, ":", conditionMessage(e)))

    return(list(
      success = FALSE,
      error = paste("Error getting ChatGPT image selection:", conditionMessage(e)),
      taxonomic_group = taxonomic_group_name
    ))
  })
}

# Internal function - use get_chatgpt_common_name_with_retry for public API
get_chatgpt_common_name_internal <- function(taxonomic_group_name, temperature = 2.0) {
  # Ensure logging is configured in parallel workers - force reload every time
  source("functions/logging_config.R", local = TRUE)

  if (is.null(taxonomic_group_name) || is.na(taxonomic_group_name) || taxonomic_group_name == "") {
    return(list(success = FALSE, error = "No taxonomic group name provided"))
  }

  tryCatch({
    # Get OpenAI API key from environment
    api_key <- Sys.getenv("OPENAI_KEY")
    if (api_key == "") {
      return(list(
        success = FALSE,
        error = "OpenAI API key not configured",
        taxonomic_group = taxonomic_group_name
      ))
    }

    # Construct the prompt to get a common species name
    prompt <- paste0("Name a species belonging to the taxonomic group '",
                     taxonomic_group_name,
                     "'. Reply with a common name only. The name must be at least two words to ensure specificity.")

    # Log the prompt
    chatgpt_log_info(paste("[CHATGPT-COMMON_NAME] Prompt for", taxonomic_group_name, ":", prompt))

    # Prepare the API request payload
    request_body <- list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      ),
      max_tokens = 10,
      temperature = temperature,  # Use passed temperature parameter
      n = 1
    )

    # Log the full request
    chatgpt_log_info(paste("[CHATGPT-COMMON_NAME] Request for", taxonomic_group_name, "- model:", request_body$model, "temp:", request_body$temperature, "max_tokens:", request_body$max_tokens))

    # Make API request to OpenAI
    api_url <- "https://api.openai.com/v1/chat/completions"

    response <- request(api_url) |>
      req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      req_user_agent("Evolution-Mapper-API/1.0") |>
      req_timeout(30) |>
      req_body_json(request_body) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    if (resp_status(response) != 200) {
      error_msg <- if (resp_status(response) == 401) {
        "Invalid OpenAI API key"
      } else if (resp_status(response) == 429) {
        "OpenAI API rate limit exceeded"
      } else if (resp_status(response) == 400) {
        "Bad request to OpenAI API"
      } else {
        paste("OpenAI API error:", resp_status(response))
      }

      return(list(
        success = FALSE,
        error = error_msg,
        taxonomic_group = taxonomic_group_name,
        status_code = resp_status(response)
      ))
    }

    response_data <- resp_body_json(response)

    # Check if we have a valid response
    if (is.null(response_data$choices) || length(response_data$choices) == 0) {
      return(list(
        success = FALSE,
        error = "No response from OpenAI API",
        taxonomic_group = taxonomic_group_name
      ))
    }

    # Extract the common name
    common_name <- response_data$choices[[1]]$message$content
    common_name <- trimws(common_name)

    # Log the raw response with safe fallback
    tryCatch({
      chatgpt_log_info(paste("[CHATGPT-COMMON_NAME] Raw response for", taxonomic_group_name, ":", common_name))
    }, error = function(log_err) {
      # If logging fails, use a sanitized version and log the logging failure
      safe_response <- gsub("[^[:print:]]", "?", common_name)  # Replace non-printable chars
      chatgpt_log_warn(paste("[CHATGPT-COMMON_NAME] Raw response for", taxonomic_group_name, "(sanitized):", safe_response))
      chatgpt_log_warn(paste("[CHATGPT-COMMON_NAME] Logging error occurred for", taxonomic_group_name, "- original response contained special characters"))
    })

    # Clean up the response - remove quotes and extra punctuation
    common_name <- gsub('^["\']|["\']$', '', common_name)
    common_name <- gsub('[.!?]$', '', common_name)
    common_name <- trimws(common_name)

    # Advanced validation for gibberish/garbled responses
    is_valid_response <- function(text) {
      # Check if empty or too long
      if (text == "" || nchar(text) > 30) return(FALSE)

      # Check for non-ASCII characters (like "ständ" in the Teleostei response)
      if (any(utf8ToInt(text) > 127)) return(FALSE)

      # Check for obvious gibberish patterns
      if (grepl("[^a-zA-Z0-9 '.,-]", text)) return(FALSE)  # Invalid characters
      if (grepl("\\s{3,}", text)) return(FALSE)  # Multiple consecutive spaces
      if (grepl("[a-z][A-Z]{3,}", text)) return(FALSE)  # Mixed case patterns like "reifeaned"

      # Check word length distribution (no words longer than 15 chars)
      words <- strsplit(text, "\\s+")[[1]]
      if (any(nchar(words) > 15)) return(FALSE)

      # Must have at least one recognizable word pattern
      if (!grepl("[a-zA-Z]{2,}", text)) return(FALSE)

      return(TRUE)
    }

    # Validate the response
    if (!is_valid_response(common_name)) {
      chatgpt_log_warn(paste("[CHATGPT-COMMON_NAME] Invalid response detected for", taxonomic_group_name, ":", common_name))
      return(list(
        success = FALSE,
        error = "Invalid or garbled response from ChatGPT",
        taxonomic_group = taxonomic_group_name,
        raw_response = common_name,
        retry_recommended = TRUE
      ))
    }

    # Log the final result
    chatgpt_log_info(paste("[CHATGPT-COMMON_NAME] Final result for", taxonomic_group_name, ":", common_name, "(tokens:", response_data$usage$total_tokens %||% "unknown", ")"))

    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group_name,
      common_name = common_name,
      original_prompt = prompt,
      raw_response = common_name,
      tokens_used = response_data$usage$total_tokens %||% NA
    ))

  }, error = function(e) {
    # Log the error with safe fallback to prevent recursive logging issues
    error_msg <- conditionMessage(e)
    tryCatch({
      chatgpt_log_error(paste("[CHATGPT-COMMON_NAME] Error for", taxonomic_group_name, ":", error_msg))
    }, error = function(log_err) {
      # If logging the error also fails, log a simplified version
      chatgpt_log_error(paste("[CHATGPT-COMMON_NAME] Error for", taxonomic_group_name, "- (error message contained special characters)"))
    })

    return(list(
      success = FALSE,
      error = paste("Error getting ChatGPT common name:", error_msg),
      taxonomic_group = taxonomic_group_name
    ))
  })
}

# Public API function with retry logic
get_chatgpt_common_name <- function(taxonomic_group_name) {
  return(get_chatgpt_common_name_with_retry(taxonomic_group_name))
}

# Helper function for null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

chatgpt_log_info("OpenAI ChatGPT summary functions loaded successfully")
chatgpt_log_info("ChatGPT logging configured - all prompts and responses will be logged to logs/chatgpt.log")