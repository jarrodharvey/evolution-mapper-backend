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

# Helper function for null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

api_log_info("OpenAI ChatGPT summary functions loaded successfully")