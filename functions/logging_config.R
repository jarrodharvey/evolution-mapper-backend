# Shared logging configuration for Evolution Mapper API
# Ensures consistent logging across all functions and endpoints

library(logger)

# Define the namespace for all Evolution Mapper logging
EVOLUTION_API_NAMESPACE <- "evolution.api"
CHATGPT_NAMESPACE <- "chatgpt"

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
  
  # Configure ChatGPT logger
  log_appender(appender_file("logs/chatgpt.log", append = TRUE),
               namespace = CHATGPT_NAMESPACE)
  log_layout(layout_simple, namespace = CHATGPT_NAMESPACE)
  log_threshold(INFO, namespace = CHATGPT_NAMESPACE)

  # Ensure logs directory exists
  if (!dir.exists("logs")) {
    dir.create("logs", recursive = TRUE)
  }
}

# Convenience function for API logging
api_log_info <- function(message, ...) {
  log_info(message, namespace = EVOLUTION_API_NAMESPACE, ...)
}

api_log_warn <- function(message, ...) {
  log_warn(message, namespace = EVOLUTION_API_NAMESPACE, ...)
}

api_log_error <- function(message, ...) {
  log_error(message, namespace = EVOLUTION_API_NAMESPACE, ...)
}

# Convenience functions for ChatGPT logging
chatgpt_log_info <- function(message, ...) {
  log_info(message, namespace = CHATGPT_NAMESPACE, ...)
}

chatgpt_log_warn <- function(message, ...) {
  log_warn(message, namespace = CHATGPT_NAMESPACE, ...)
}

chatgpt_log_error <- function(message, ...) {
  log_error(message, namespace = CHATGPT_NAMESPACE, ...)
}

# Initialize logging when this file is sourced
configure_evolution_logging()

cat("Evolution Mapper logging configured with namespace:", EVOLUTION_API_NAMESPACE, "\n")