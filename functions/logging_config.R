# Shared logging configuration for Evolution Mapper API
# Ensures consistent logging across all functions and endpoints

library(logger)

# Define the namespace for all Evolution Mapper logging
EVOLUTION_API_NAMESPACE <- "evolution.api"

# Configure the Evolution API logger
configure_evolution_logging <- function() {
  # Set up file appender
  log_appender(appender_file("logs/api.log", append = TRUE), 
               namespace = EVOLUTION_API_NAMESPACE)
  
  # Set up console appender (for development)
  log_appender(appender_console, 
               namespace = EVOLUTION_API_NAMESPACE, 
               index = 2)
  
  # Configure layout with simple formatting (no glue to avoid variable scoping issues)
  log_layout(layout_simple, namespace = EVOLUTION_API_NAMESPACE)
  
  # Set threshold to INFO level
  log_threshold(INFO, namespace = EVOLUTION_API_NAMESPACE)
  

  # Ensure logs directory exists
  if (!dir.exists("logs")) {
    dir.create("logs", recursive = TRUE)
  }
}

# Safe message sanitization for logging
sanitize_log_message <- function(message) {
  if (is.null(message) || is.na(message)) {
    return("<NULL_OR_NA>")
  }

  # Convert to character if not already
  message <- as.character(message)

  # Replace problematic characters that can break glue formatter
  message <- gsub("\r", "\\\\r", message, fixed = TRUE)  # Carriage returns
  message <- gsub("\n", "\\\\n", message, fixed = TRUE)  # Line feeds
  message <- gsub("\t", "\\\\t", message, fixed = TRUE)  # Tabs
  message <- gsub("\\{", "\\\\{", message, fixed = TRUE) # Opening braces
  message <- gsub("\\}", "\\\\}", message, fixed = TRUE) # Closing braces
  message <- gsub("`", "'", message, fixed = TRUE)       # Backticks
  message <- gsub("\"", "'", message, fixed = TRUE)      # Double quotes

  # Truncate extremely long messages to prevent log bloat
  if (nchar(message) > 500) {
    message <- paste0(substr(message, 1, 497), "...")
  }

  return(message)
}

# Safe convenience functions for API logging with sanitization
api_log_info <- function(message, ...) {
  safe_message <- sanitize_log_message(message)
  log_info(safe_message, namespace = EVOLUTION_API_NAMESPACE, ...)
}

api_log_warn <- function(message, ...) {
  safe_message <- sanitize_log_message(message)
  log_warn(safe_message, namespace = EVOLUTION_API_NAMESPACE, ...)
}

api_log_error <- function(message, ...) {
  safe_message <- sanitize_log_message(message)
  log_error(safe_message, namespace = EVOLUTION_API_NAMESPACE, ...)
}


# Initialize logging when this file is sourced
configure_evolution_logging()

cat("Evolution Mapper logging configured with namespace:", EVOLUTION_API_NAMESPACE, "\n")