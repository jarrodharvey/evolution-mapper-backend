# NCBI Taxonomy API Integration
# Provides common name lookups for taxonomic nodes using the NCBI Taxonomy database
# Uses the rentrez package to query NCBI's taxonomy database

library(rentrez)

# All required functions are sourced at startup in plumber.R

#' Get common name for a taxonomic group from NCBI Taxonomy database
#'
#' @param taxonomic_name Scientific name of the taxonomic group (e.g., "Bifurcata", "Carnivora")
#' @return Common name as string, or NULL if not found or on error
#' @examples
#' get_ncbi_common_name("Bifurcata")  # Returns "split-tongued squamates"
#' get_ncbi_common_name("Carnivora")  # Returns "carnivores"
get_ncbi_common_name <- function(taxonomic_name) {
  # Input validation
  if (is.null(taxonomic_name) || is.na(taxonomic_name) || nchar(trimws(taxonomic_name)) == 0) {
    return(NULL)
  }

  tryCatch({
    api_log_info(paste("Querying NCBI Taxonomy for:", taxonomic_name))

    # Search NCBI Taxonomy database
    search_result <- entrez_search(db = "taxonomy", term = taxonomic_name)

    # Check if any results were found
    if (is.null(search_result$ids) || length(search_result$ids) == 0) {
      api_log_info(paste("No NCBI Taxonomy results for:", taxonomic_name))
      return(NULL)
    }

    # If multiple results, fetch all and select the best match
    # The best match is typically the first one, but we validate it has a common name
    selected_id <- NULL
    selected_common_name <- NULL

    # Try the first result first (most relevant)
    if (length(search_result$ids) >= 1) {
      tax_record <- entrez_summary(db = "taxonomy", id = search_result$ids[1])
      common_name <- tax_record$commonname

      if (!is.null(common_name) && nchar(trimws(common_name)) > 0) {
        selected_id <- search_result$ids[1]
        selected_common_name <- common_name
        api_log_info(paste("NCBI found common name in first result for", taxonomic_name, ":", common_name))
      } else {
        # First result has no common name, try second result if available
        if (length(search_result$ids) >= 2) {
          api_log_info(paste("First NCBI result has no common name, trying second result for:", taxonomic_name))
          tax_record <- entrez_summary(db = "taxonomy", id = search_result$ids[2])
          common_name <- tax_record$commonname

          if (!is.null(common_name) && nchar(trimws(common_name)) > 0) {
            selected_id <- search_result$ids[2]
            selected_common_name <- common_name
            api_log_info(paste("NCBI found common name in second result for", taxonomic_name, ":", common_name))
          }
        }
      }
    }

    # Check if we found a valid common name
    if (is.null(selected_common_name)) {
      api_log_info(paste("NCBI Taxonomy found no common name for:", taxonomic_name))
      return(NULL)
    }

    # Convert to proper case (capitalize first letter)
    common_name <- paste0(toupper(substring(selected_common_name, 1, 1)), substring(selected_common_name, 2))

    return(common_name)

  }, error = function(e) {
    # Log the error but return NULL to allow graceful fallback
    api_log_warn(paste("NCBI Taxonomy API error for", taxonomic_name, ":", e$message))
    return(NULL)
  })
}

api_log_info("NCBI Taxonomy API functions loaded successfully")
