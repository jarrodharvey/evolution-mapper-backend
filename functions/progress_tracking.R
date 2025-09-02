# Progress Tracking Functions
# Shared functions for tracking API request progress

library(jsonlite)

# Progress tracking helper function
update_progress <- function(progress_token, step_name, status = "completed", additional_data = NULL) {
  if (is.null(progress_token) || progress_token == "") {
    return()  # Skip if no progress token provided
  }
  
  tryCatch({
    progress_file <- paste0("progress/", progress_token, ".json")
    
    # Read existing progress or create new structure
    if (file.exists(progress_file)) {
      existing_data <- jsonlite::fromJSON(progress_file, simplifyVector = FALSE)
    } else {
      existing_data <- list(
        token = progress_token,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        status = "initialized",
        steps = list()
      )
    }
    
    # Add new step with timestamp
    step_data <- list(
      step = step_name,
      status = status,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    # Add any additional data
    if (!is.null(additional_data)) {
      step_data <- c(step_data, additional_data)
    }
    
    # Add step to steps array
    existing_data$steps <- c(existing_data$steps, list(step_data))
    
    # Update overall status
    existing_data$last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    if (status == "error") {
      existing_data$status = "error"
    } else {
      existing_data$status = "in_progress"
    }
    
    # Write updated data back to file with proper scalar handling
    writeLines(jsonlite::toJSON(existing_data, pretty = TRUE, auto_unbox = TRUE, na = "null"), progress_file)
    
  }, error = function(e) {
    # Log error but don't fail the main operation
    if (exists("api_log_warn")) {
      api_log_warn(paste("Failed to update progress:", conditionMessage(e)))
    }
  })
}