# Attribution data extraction from cache files
# Extracts all phylopic and wikipedia/wikimedia attributions from cached RDS files

source("functions/logging_config.R")

# Extract all attribution data from cache files
get_all_attributions <- function() {
  tryCatch({
    api_log_info("Extracting attribution data from cache files...")

    result <- list(
      phylopic = list(),
      wikipedia = list(),
      summary = list(
        phylopic_count = 0,
        wikipedia_count = 0,
        total_taxonomic_groups = 0
      )
    )

    # Extract PhyloPic attributions
    phylopic_cache_dir <- "cache/phylopic"
    if (dir.exists(phylopic_cache_dir)) {
      phylopic_files <- list.files(phylopic_cache_dir, pattern = "\\.rds$", full.names = TRUE)

      for (file in phylopic_files) {
        tryCatch({
          data <- readRDS(file)
          if (is.list(data) && !is.null(data$value)) {
            # This is just a UUID, we need to get the actual silhouette data
            uuid <- data$value
            if (is.character(uuid) && nchar(uuid) > 0) {
              # Try to get attribution for this UUID
              attribution_info <- tryCatch({
                library(rphylopic)
                attr_data <- get_attribution(uuid = uuid)
                if (is.data.frame(attr_data) && nrow(attr_data) > 0) {
                  contributor <- if (!is.na(attr_data$contributor[1])) attr_data$contributor[1] else "Unknown"
                  list(
                    uuid = uuid,
                    contributor = contributor,
                    attribution = paste("Silhouette by", contributor, "via PhyloPic"),
                    phylopic_url = paste0("https://www.phylopic.org/images/", uuid)
                  )
                } else {
                  list(
                    uuid = uuid,
                    contributor = "Unknown",
                    attribution = "Silhouette via PhyloPic",
                    phylopic_url = paste0("https://www.phylopic.org/images/", uuid)
                  )
                }
              }, error = function(e) {
                list(
                  uuid = uuid,
                  contributor = "Unknown",
                  attribution = "Silhouette via PhyloPic",
                  phylopic_url = paste0("https://www.phylopic.org/images/", uuid),
                  note = "Attribution lookup failed"
                )
              })

              # Use filename as key (represents the taxonomic group hash)
              filename_key <- basename(tools::file_path_sans_ext(file))
              result$phylopic[[filename_key]] <- attribution_info
            }
          }
        }, error = function(e) {
          api_log_warn(paste("Error reading PhyloPic cache file", file, ":", e$message))
        })
      }
    }

    # Extract Wikimedia image attributions
    wikimedia_cache_dir <- "cache/wikimedia_images"
    if (dir.exists(wikimedia_cache_dir)) {
      wikimedia_files <- list.files(wikimedia_cache_dir, pattern = "\\.rds$", full.names = TRUE)

      for (file in wikimedia_files) {
        tryCatch({
          data <- readRDS(file)
          if (is.list(data) && !is.null(data$value) && is.list(data$value)) {
            image_data <- data$value
            if (image_data$success && !is.null(image_data$image_url)) {
              attribution_info <- list(
                taxonomic_name = image_data$taxonomic_name,
                entity_id = image_data$entity_id,
                entity_label = image_data$entity_label,
                entity_description = image_data$entity_description,
                image_url = image_data$image_url,
                attribution = image_data$attribution,
                total_images_available = image_data$total_images_available,
                source = "Wikimedia Commons"
              )

              # Use filename as key
              filename_key <- basename(tools::file_path_sans_ext(file))
              result$wikipedia[[filename_key]] <- attribution_info
            }
          }
        }, error = function(e) {
          api_log_warn(paste("Error reading Wikimedia cache file", file, ":", e$message))
        })
      }
    }

    # Update summary counts
    result$summary$phylopic_count <- length(result$phylopic)
    result$summary$wikipedia_count <- length(result$wikipedia)
    result$summary$total_taxonomic_groups <- length(unique(c(
      sapply(result$wikipedia, function(x) x$taxonomic_name),
      names(result$phylopic)
    )))

    api_log_info(paste("Attribution extraction complete:",
                      result$summary$phylopic_count, "PhyloPic attributions,",
                      result$summary$wikipedia_count, "Wikipedia/Wikimedia attributions"))

    return(result)

  }, error = function(e) {
    api_log_error(paste("Error extracting attribution data:", e$message))
    return(list(
      error = paste("Attribution extraction failed:", e$message),
      phylopic = list(),
      wikipedia = list(),
      summary = list(phylopic_count = 0, wikipedia_count = 0, total_taxonomic_groups = 0)
    ))
  })
}

api_log_info("Attribution extractor functions loaded successfully")