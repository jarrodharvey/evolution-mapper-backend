# Caching configuration for info panel system
# Uses memoise with disk caching to speed up API calls

library(memoise)
library(cachem)

# Source shared logging configuration
source("functions/logging_config.R")

# Create disk cache in cache/ directory
# Max size: 500MB, Max age: 24 hours (86400 seconds)
info_panel_cache <- cache_disk(
  dir = "cache/info_panels/",
  max_size = 500 * 1024^2,  # 500MB
  max_age = 24 * 60 * 60    # 24 hours
)

# Wikipedia API cache - separate cache for Wikipedia calls
wikipedia_cache <- cache_disk(
  dir = "cache/wikipedia/",
  max_size = 200 * 1024^2,  # 200MB
  max_age = 7 * 24 * 60 * 60  # 7 days (Wikipedia content changes less frequently)
)

# PhyloPic silhouette cache - separate cache for PhyloPic calls
phylopic_cache <- cache_disk(
  dir = "cache/phylopic/",
  max_size = 300 * 1024^2,  # 300MB  
  max_age = 30 * 24 * 60 * 60  # 30 days (silhouettes rarely change)
)

# Wikipedia image cache - separate cache for Wikipedia image calls
wikipedia_images_cache <- cache_disk(
  dir = "cache/wikipedia_images/",
  max_size = 400 * 1024^2,  # 400MB (images are larger than text content)
  max_age = 30 * 24 * 60 * 60  # 30 days (images change rarely)
)

# Unsplash image cache - separate cache for Unsplash API calls
unsplash_images_cache <- cache_disk(
  dir = "cache/unsplash_images/",
  max_size = 400 * 1024^2,  # 400MB (images are larger than text content)
  max_age = 30 * 24 * 60 * 60  # 30 days (images change rarely)
)

api_log_info("Caching configuration loaded successfully")
api_log_info(paste("Cache directories:"))
api_log_info(paste("  Info panels:", file.path(getwd(), "cache/info_panels/")))
api_log_info(paste("  Wikipedia:", file.path(getwd(), "cache/wikipedia/")))
api_log_info(paste("  PhyloPic:", file.path(getwd(), "cache/phylopic/")))
api_log_info(paste("  Wikipedia Images:", file.path(getwd(), "cache/wikipedia_images/")))
api_log_info(paste("  Unsplash Images:", file.path(getwd(), "cache/unsplash_images/")))