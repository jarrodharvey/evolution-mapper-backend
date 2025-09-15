# OpenAI ChatGPT Integration for Taxonomic Summary Generation
# Functions for converting scientific/taxonomic names into plain language summaries
# Used to improve Unsplash image search relevance

library(httr2)
library(jsonlite)

# Source shared logging configuration
source("functions/logging_config.R")

# Main function to get ChatGPT summary of taxonomic group
get_chatgpt_summary <- function(taxonomic_group_name) {
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

    # Clean up the summary - remove quotes and extra punctuation
    summary <- gsub('^["\']|["\']$', '', summary)  # Remove leading/trailing quotes
    summary <- gsub('[.!?]$', '', summary)         # Remove trailing punctuation
    summary <- trimws(summary)

    # Validate that we got a reasonable summary (not empty, not too long)
    if (summary == "" || nchar(summary) > 50) {
      return(list(
        success = FALSE,
        error = "Invalid summary returned from OpenAI",
        taxonomic_group = taxonomic_group_name,
        raw_summary = summary
      ))
    }

    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group_name,
      summary = summary,
      original_prompt = prompt,
      tokens_used = response_data$usage$total_tokens %||% NA
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error getting ChatGPT summary:", conditionMessage(e)),
      taxonomic_group = taxonomic_group_name
    ))
  })
}

# Test function to verify OpenAI API connectivity
test_openai_connection <- function() {
  api_log_info("Testing OpenAI API connection...")

  # Test with well-known taxonomic groups
  test_groups <- c("Mammalia", "Boreoeutheria", "Primates", "Lepidoptera", "Cephalopoda")

  for (group in test_groups) {
    api_log_info(paste("Testing ChatGPT summary for", group, "..."))
    result <- get_chatgpt_summary(group)
    if (result$success) {
      api_log_info(paste("  SUCCESS:", group, "->", result$summary))
      if (!is.na(result$tokens_used)) {
        api_log_info(paste("  Tokens used:", result$tokens_used))
      }
    } else {
      api_log_info(paste("  FAILED:", group, "-", result$error))
    }

    # Small delay to respect rate limits
    Sys.sleep(0.5)
  }

  api_log_info("OpenAI API connection test completed")
}

# Enhanced function to get ChatGPT image search query with Wikipedia context
get_chatgpt_image_query <- function(taxonomic_group_name, wikipedia_summary) {
  if (is.null(taxonomic_group_name) || is.na(taxonomic_group_name) || taxonomic_group_name == "") {
    return(list(success = FALSE, error = "No taxonomic group name provided"))
  }

  if (is.null(wikipedia_summary) || is.na(wikipedia_summary) || wikipedia_summary == "") {
    return(list(success = FALSE, error = "No Wikipedia summary provided"))
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

    # Construct the enhanced prompt with Wikipedia context
    prompt <- paste0("Generate an appropriate short (1-3 word) query for an image search website based on this taxonomic group: ",
                     taxonomic_group_name,
                     ". This is a Wikipedia summary that describes the taxonomic group: ",
                     wikipedia_summary,
                     ". Focus on visual characteristics, habitat, or distinctive features that would help find relevant nature photographs.")

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

    # Extract the image query text
    image_query <- response_data$choices[[1]]$message$content
    image_query <- trimws(image_query)

    # Clean up the query - remove quotes and extra punctuation
    image_query <- gsub('^["\']|["\']$', '', image_query)  # Remove leading/trailing quotes
    image_query <- gsub('[.!?]$', '', image_query)         # Remove trailing punctuation
    image_query <- trimws(image_query)

    # Validate that we got a reasonable query (not empty, not too long)
    if (image_query == "" || nchar(image_query) > 50) {
      return(list(
        success = FALSE,
        error = "Invalid image query returned from OpenAI",
        taxonomic_group = taxonomic_group_name,
        raw_query = image_query
      ))
    }

    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group_name,
      wikipedia_summary = wikipedia_summary,
      image_query = image_query,
      original_prompt = prompt,
      tokens_used = response_data$usage$total_tokens %||% NA
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error getting ChatGPT image query:", conditionMessage(e)),
      taxonomic_group = taxonomic_group_name
    ))
  })
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

    # Parse the response
    if (grepl("no match", selection)) {
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
    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group_name,
      selection = "no_match",
      raw_response = selection,
      parse_error = "Could not parse valid selection from response",
      tokens_used = response_data$usage$total_tokens %||% NA
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error getting ChatGPT image selection:", conditionMessage(e)),
      taxonomic_group = taxonomic_group_name
    ))
  })
}

# Helper function for null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

api_log_info("OpenAI ChatGPT summary functions loaded successfully")