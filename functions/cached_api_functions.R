# Cached API Functions for Info Panel System
# Implements memoised versions of Wikipedia and PhyloPic functions for improved performance

# Load required libraries and functions
source("functions/caching_config.R")
source("functions/wikipedia_api.R")
source("functions/phylopic_silhouettes.R")

# Memoised Wikipedia functions
#
# Cache the main get_wikipedia_intro function that combines search and extract
# This is the main entry point used by info panel system
cached_get_wikipedia_intro <- memoise(get_wikipedia_intro, cache = wikipedia_cache)

# Cache the Wikipedia search function separately in case it's called directly
cached_search_wikipedia_article <- memoise(search_wikipedia_article, cache = wikipedia_cache)

# Cache the Wikipedia extract function separately in case it's called directly  
cached_get_wikipedia_extract <- memoise(get_wikipedia_extract, cache = wikipedia_cache)

# Memoised PhyloPic functions
#
# Cache the main get_silhouette_data function that gets image and attribution
# This is the main entry point used by info panel system
cached_get_silhouette_data <- memoise(get_silhouette_data, cache = phylopic_cache)

# Cache the UUID lookup function separately
cached_get_random_silhouette_uuid <- memoise(get_random_silhouette_uuid, cache = phylopic_cache)

# Note: Info panel generation functions are cached within info_panel_system.R
# to avoid circular dependencies. The core API functions above provide the 
# main caching benefits.

# Utility functions for cache management
#
# Clear all caches
clear_all_caches <- function() {
  api_log_info("Clearing all info panel caches...")
  
  # Clear each cache
  info_panel_cache$reset()
  wikipedia_cache$reset()
  phylopic_cache$reset()
  
  api_log_info("All caches cleared successfully")
}

# Get cache statistics
get_cache_stats <- function() {
  list(
    info_panel = list(
      size = info_panel_cache$size(),
      keys = length(info_panel_cache$keys())
    ),
    wikipedia = list(
      size = wikipedia_cache$size(),
      keys = length(wikipedia_cache$keys())
    ),
    phylopic = list(
      size = phylopic_cache$size(),
      keys = length(phylopic_cache$keys())
    )
  )
}

# Prune expired cache entries
prune_caches <- function() {
  api_log_info("Pruning expired cache entries...")
  
  info_panel_cache$prune()
  wikipedia_cache$prune() 
  phylopic_cache$prune()
  
  api_log_info("Cache pruning completed")
}

api_log_info("Cached API functions loaded successfully")
api_log_info("Available cached functions:")
api_log_info("  cached_get_wikipedia_intro() - Main Wikipedia function")
api_log_info("  cached_get_silhouette_data() - Main PhyloPic function") 
api_log_info("  clear_all_caches() - Clear all cached data")
api_log_info("  get_cache_stats() - View cache statistics")
api_log_info("  prune_caches() - Remove expired entries")