# Wikipedia API Functions
# Functions for fetching Wikipedia article introductions for taxonomic groups

# Main function to fetch Wikipedia introduction for a taxonomic group
get_wikipedia_intro <- function(taxonomic_group, truncate_length = 300) {
  tryCatch({
    # Clean and format the taxonomic group name for Wikipedia search
    search_term <- clean_taxonomic_name(taxonomic_group)
    
    # First, search for the article to get the exact title
    search_result <- search_wikipedia_article(search_term)
    
    if (is.null(search_result) || length(search_result$pages) == 0) {
      return(list(
        success = FALSE,
        error = paste("No Wikipedia article found for:", taxonomic_group),
        taxonomic_group = taxonomic_group,
        search_term = search_term
      ))
    }
    
    # Get the first search result
    page_info <- search_result$pages[[1]]
    page_title <- page_info$title
    page_id <- page_info$pageid
    
    # Fetch the article extract
    extract_result <- get_wikipedia_extract(page_title, truncate_length)
    
    if (is.null(extract_result)) {
      return(list(
        success = FALSE,
        error = paste("Could not fetch Wikipedia extract for:", page_title),
        taxonomic_group = taxonomic_group,
        wikipedia_title = page_title
      ))
    }
    
    # Construct Wikipedia URL
    wikipedia_url <- paste0("https://en.wikipedia.org/wiki/", URLencode(gsub(" ", "_", page_title)))
    
    return(list(
      success = TRUE,
      taxonomic_group = taxonomic_group,
      wikipedia_title = page_title,
      introduction = extract_result$extract,
      url = wikipedia_url,
      page_id = page_id,
      truncated = nchar(extract_result$extract) >= truncate_length
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error fetching Wikipedia data:", conditionMessage(e)),
      taxonomic_group = taxonomic_group
    ))
  })
}

# Clean taxonomic name for Wikipedia search
clean_taxonomic_name <- function(name) {
  # Remove common prefixes and suffixes that might confuse Wikipedia search
  cleaned <- gsub("^(Order|Family|Class|Genus|Species)\\s+", "", name, ignore.case = TRUE)
  cleaned <- gsub("\\s+(group|clade)$", "", cleaned, ignore.case = TRUE)
  cleaned <- trimws(cleaned)
  
  # If it looks like a scientific name, try both scientific and common forms
  if (grepl("^[A-Z][a-z]+\\s+[a-z]+$", cleaned)) {
    # It's likely a binomial scientific name, use as-is
    return(cleaned)
  }
  
  return(cleaned)
}

# Search Wikipedia for articles matching the taxonomic group
search_wikipedia_article <- function(search_term) {
  tryCatch({
    # Wikipedia API search endpoint
    base_url <- "https://en.wikipedia.org/api/rest_v1/page/summary/"
    
    # Try direct page lookup first (more reliable)
    direct_url <- paste0(base_url, URLencode(gsub(" ", "_", search_term)))
    
    # Make HTTP request with User-Agent header (Wikipedia requires this)
    response <- httr::GET(direct_url, httr::user_agent("Evolution-Mapper-API/1.0"))
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      page_data <- jsonlite::fromJSON(content)
      
      # Return in consistent format
      return(list(
        pages = list(list(
          title = page_data$title,
          pageid = page_data$pageid %||% 0,
          extract = page_data$extract
        ))
      ))
    }
    
    # If direct lookup fails, try search API
    search_url <- "https://en.wikipedia.org/w/api.php"
    
    response <- httr::GET(search_url, query = list(
      action = "query",
      format = "json",
      list = "search",
      srsearch = search_term,
      srlimit = 1,
      srprop = "snippet"
    ), httr::user_agent("Evolution-Mapper-API/1.0"))
    
    if (httr::status_code(response) != 200) {
      return(NULL)
    }
    
    content <- httr::content(response, "text", encoding = "UTF-8")
    search_data <- jsonlite::fromJSON(content)
    
    if (length(search_data$query$search) == 0) {
      return(NULL)
    }
    
    # Return first search result
    first_result <- search_data$query$search[1, ]
    return(list(
      pages = list(list(
        title = first_result$title,
        pageid = first_result$pageid,
        extract = NULL  # Will be fetched separately
      ))
    ))
    
  }, error = function(e) {
    cat("Error in search_wikipedia_article:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Get Wikipedia extract for a specific page title
get_wikipedia_extract <- function(page_title, max_chars = 300) {
  tryCatch({
    # Use Wikipedia REST API for extracts
    base_url <- "https://en.wikipedia.org/api/rest_v1/page/summary/"
    url <- paste0(base_url, URLencode(gsub(" ", "_", page_title)))
    
    response <- httr::GET(url, httr::user_agent("Evolution-Mapper-API/1.0"))
    
    if (httr::status_code(response) != 200) {
      return(NULL)
    }
    
    content <- httr::content(response, "text", encoding = "UTF-8")
    page_data <- jsonlite::fromJSON(content)
    
    if (is.null(page_data$extract) || page_data$extract == "") {
      return(NULL)
    }
    
    # Truncate if necessary
    extract <- page_data$extract
    if (nchar(extract) > max_chars) {
      # Find the last complete sentence within the limit
      truncated <- substr(extract, 1, max_chars)
      last_period <- max(gregexpr("\\.", truncated)[[1]])
      
      if (last_period > max_chars * 0.7) {  # If we find a period in the last 30% of text
        extract <- substr(extract, 1, last_period)
      } else {
        # Otherwise, truncate at word boundary and add ellipsis
        last_space <- max(gregexpr(" ", truncated)[[1]])
        if (last_space > 0) {
          extract <- paste0(substr(extract, 1, last_space - 1), "...")
        } else {
          extract <- paste0(truncated, "...")
        }
      }
    }
    
    return(list(
      extract = extract,
      original_length = nchar(page_data$extract)
    ))
    
  }, error = function(e) {
    cat("Error in get_wikipedia_extract:", conditionMessage(e), "\n")
    return(NULL)
  })
}