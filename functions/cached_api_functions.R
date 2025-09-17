# Cached API Functions for Info Panel System
# Implements memoised versions of Wikipedia and PhyloPic functions for improved performance

# Load required libraries and functions
source("functions/caching_config.R")
source("functions/wikipedia_api.R")
source("functions/phylopic_silhouettes.R")
source("functions/wikipedia_images.R")
source("functions/unsplash_images.R")
source("functions/pixabay_images.R")
source("functions/wikimedia_images.R")
source("functions/openai_summary.R")

# Create memoised functions without logging first
memoised_get_wikipedia_intro <- memoise(get_wikipedia_intro, cache = wikipedia_cache)
memoised_search_wikipedia_article <- memoise(search_wikipedia_article, cache = wikipedia_cache)
memoised_get_wikipedia_extract <- memoise(get_wikipedia_extract, cache = wikipedia_cache)
memoised_get_silhouette_data <- memoise(get_silhouette_data, cache = phylopic_cache)
memoised_get_random_silhouette_uuid <- memoise(get_random_silhouette_uuid, cache = phylopic_cache)
memoised_get_wikipedia_main_image <- memoise(get_wikipedia_main_image, cache = wikipedia_images_cache)
memoised_get_unsplash_random_image <- memoise(get_unsplash_random_image, cache = unsplash_images_cache)
memoised_get_pixabay_random_image <- memoise(get_pixabay_random_image, cache = pixabay_images_cache)
memoised_get_wikimedia_image_enhanced <- memoise(get_wikimedia_image_enhanced, cache = wikimedia_images_cache)
memoised_get_chatgpt_summary <- memoise(get_chatgpt_summary, cache = chatgpt_summary_cache)
memoised_get_chatgpt_image_selection <- memoise(get_chatgpt_image_selection, cache = chatgpt_summary_cache)
memoised_get_chatgpt_common_name <- memoise(get_chatgpt_common_name, cache = chatgpt_summary_cache)

# Wikipedia functions with cache hit/miss logging
#
# Main Wikipedia function with cache logging
cached_get_wikipedia_intro <- function(taxonomic_group, truncate_length = 300) {
  # Check if data is already cached by trying to get cache stats before and after
  cache_stats_before <- wikipedia_cache$size()
  
  api_log_info(paste("Wikipedia Cache lookup for", taxonomic_group))
  
  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_wikipedia_intro(taxonomic_group, truncate_length)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised Wikipedia function failed, calling original:", e$message))
    # Re-source the wikipedia functions to ensure they're available
    source("functions/wikipedia_api.R", local = TRUE)
    result <- get_wikipedia_intro(taxonomic_group, truncate_length)
    return(result)
  })
  
  cache_stats_after <- wikipedia_cache$size()
  
  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("Wikipedia Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("Wikipedia Cache HIT - using cached data for", taxonomic_group))
  }
  
  return(result)
}

# Wikipedia search function with cache logging
cached_search_wikipedia_article <- function(taxonomic_group) {
  cache_stats_before <- wikipedia_cache$size()
  
  result <- memoised_search_wikipedia_article(taxonomic_group)
  
  cache_stats_after <- wikipedia_cache$size()
  
  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("Wikipedia Search Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("Wikipedia Search Cache HIT - using cached data for", taxonomic_group))
  }
  
  return(result)
}

# Wikipedia extract function with cache logging
cached_get_wikipedia_extract <- function(page_id, truncate_length = 300) {
  cache_stats_before <- wikipedia_cache$size()
  
  result <- memoised_get_wikipedia_extract(page_id, truncate_length)
  
  cache_stats_after <- wikipedia_cache$size()
  
  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("Wikipedia Extract Cache MISS - fetched from API for page", page_id))
  } else {
    api_log_info(paste("Wikipedia Extract Cache HIT - using cached data for page", page_id))
  }
  
  return(result)
}

# PhyloPic functions with cache hit/miss logging
#
# Main PhyloPic function with cache logging
cached_get_silhouette_data <- function(taxonomic_name) {
  cache_stats_before <- phylopic_cache$size()
  
  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_silhouette_data(taxonomic_name)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised PhyloPic function failed, calling original:", e$message))
    # Re-source the phylopic functions to ensure they're available
    source("functions/phylopic_silhouettes.R", local = TRUE)
    result <- get_silhouette_data(taxonomic_name)
    return(result)
  })
  
  cache_stats_after <- phylopic_cache$size()
  
  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("PhyloPic Cache MISS - fetched from API for", taxonomic_name))
  } else {
    api_log_info(paste("PhyloPic Cache HIT - using cached silhouette for", taxonomic_name))
  }
  
  return(result)
}

# PhyloPic UUID function with cache logging
cached_get_random_silhouette_uuid <- function(taxonomic_name) {
  cache_stats_before <- phylopic_cache$size()
  
  result <- memoised_get_random_silhouette_uuid(taxonomic_name)
  
  cache_stats_after <- phylopic_cache$size()
  
  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("PhyloPic UUID Cache MISS - fetched from API for", taxonomic_name))
  } else {
    api_log_info(paste("PhyloPic UUID Cache HIT - using cached UUID for", taxonomic_name))
  }
  
  return(result)
}

# Wikipedia Image functions with cache hit/miss logging
#
# Main Wikipedia image function with cache logging
cached_get_wikipedia_main_image <- function(taxonomic_group, target_width = 800) {
  cache_stats_before <- wikipedia_images_cache$size()
  
  api_log_info(paste("Wikipedia Image Cache lookup for", taxonomic_group))
  
  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_wikipedia_main_image(taxonomic_group, target_width)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised Wikipedia image function failed, calling original:", e$message))
    # Re-source the wikipedia image functions to ensure they're available
    source("functions/wikipedia_images.R", local = TRUE)
    result <- get_wikipedia_main_image(taxonomic_group, target_width)
    return(result)
  })
  
  cache_stats_after <- wikipedia_images_cache$size()
  
  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("Wikipedia Image Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("Wikipedia Image Cache HIT - using cached image for", taxonomic_group))
  }
  
  return(result)
}

# Unsplash Image functions with cache hit/miss logging
#
# Main Unsplash image function with cache logging
cached_get_unsplash_random_image <- function(taxonomic_group, target_width = 800, skip_chatgpt_conversion = FALSE) {
  cache_stats_before <- unsplash_images_cache$size()

  api_log_info(paste("Unsplash Image Cache lookup for", taxonomic_group))

  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_unsplash_random_image(taxonomic_group, target_width, skip_chatgpt_conversion)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised Unsplash image function failed, calling original:", e$message))
    # Re-source the unsplash image functions to ensure they're available
    source("functions/unsplash_images.R", local = TRUE)
    result <- get_unsplash_random_image(taxonomic_group, target_width, skip_chatgpt_conversion)
    return(result)
  })

  cache_stats_after <- unsplash_images_cache$size()

  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("Unsplash Image Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("Unsplash Image Cache HIT - using cached image for", taxonomic_group))
  }

  return(result)
}

# Pixabay Image functions with cache hit/miss logging
#
# Main Pixabay image function with cache logging
cached_get_pixabay_random_image <- function(taxonomic_group, target_width = 200, skip_chatgpt_conversion = FALSE) {
  cache_stats_before <- pixabay_images_cache$size()

  api_log_info(paste("Pixabay Image Cache lookup for", taxonomic_group))

  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_pixabay_random_image(taxonomic_group, target_width, skip_chatgpt_conversion)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised Pixabay image function failed, calling original:", e$message))
    # Re-source the pixabay image functions to ensure they're available
    source("functions/pixabay_images.R", local = TRUE)
    result <- get_pixabay_random_image(taxonomic_group, target_width, skip_chatgpt_conversion)
    return(result)
  })

  cache_stats_after <- pixabay_images_cache$size()

  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("Pixabay Image Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("Pixabay Image Cache HIT - using cached image for", taxonomic_group))
  }

  return(result)
}

# Wikimedia Image functions with cache hit/miss logging
#
# Main Wikimedia image function with cache logging
cached_get_wikimedia_image_enhanced <- function(taxonomic_group, target_width = 200) {
  cache_stats_before <- wikimedia_images_cache$size()

  api_log_info(paste("Wikimedia Image Cache lookup for", taxonomic_group))

  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_wikimedia_image_enhanced(taxonomic_group, target_width)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised Wikimedia image function failed, calling original:", e$message))
    # Re-source the wikimedia image functions to ensure they're available
    source("functions/wikimedia_images.R", local = TRUE)
    result <- get_wikimedia_image_enhanced(taxonomic_group, target_width)
    return(result)
  })

  cache_stats_after <- wikimedia_images_cache$size()

  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("Wikimedia Image Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("Wikimedia Image Cache HIT - using cached image for", taxonomic_group))
  }

  return(result)
}

# ChatGPT Summary functions with cache hit/miss logging
#
# Main ChatGPT summary function with cache logging
cached_get_chatgpt_summary <- function(taxonomic_group) {
  cache_stats_before <- chatgpt_summary_cache$size()

  api_log_info(paste("ChatGPT Summary Cache lookup for", taxonomic_group))

  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_chatgpt_summary(taxonomic_group)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised ChatGPT function failed, calling original:", e$message))
    # Re-source the openai functions to ensure they're available
    source("functions/openai_summary.R", local = TRUE)
    result <- get_chatgpt_summary(taxonomic_group)
    return(result)
  })

  cache_stats_after <- chatgpt_summary_cache$size()

  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("ChatGPT Summary Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("ChatGPT Summary Cache HIT - using cached summary for", taxonomic_group))
  }

  return(result)
}


# ChatGPT Image Selection functions with cache hit/miss logging
#
# ChatGPT image selection function with cache logging
cached_get_chatgpt_image_selection <- function(taxonomic_group, wikipedia_summary, image_descriptions) {
  cache_stats_before <- chatgpt_summary_cache$size()

  api_log_info(paste("ChatGPT Image Selection Cache lookup for", taxonomic_group))

  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_chatgpt_image_selection(taxonomic_group, wikipedia_summary, image_descriptions)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised ChatGPT image selection function failed, calling original:", e$message))
    # Re-source the openai functions to ensure they're available
    source("functions/openai_summary.R", local = TRUE)
    result <- get_chatgpt_image_selection(taxonomic_group, wikipedia_summary, image_descriptions)
    return(result)
  })

  cache_stats_after <- chatgpt_summary_cache$size()

  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("ChatGPT Image Selection Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("ChatGPT Image Selection Cache HIT - using cached selection for", taxonomic_group))
  }

  return(result)
}

# ChatGPT Common Name functions with cache hit/miss logging
#
# ChatGPT common name conversion function with cache logging
cached_get_chatgpt_common_name <- function(taxonomic_group) {
  cache_stats_before <- chatgpt_summary_cache$size()

  api_log_info(paste("ChatGPT Common Name Cache lookup for", taxonomic_group))

  # Ensure required functions are available in this context
  tryCatch({
    result <- memoised_get_chatgpt_common_name(taxonomic_group)
  }, error = function(e) {
    # If memoised function fails due to missing dependencies, call original function directly
    api_log_warn(paste("Memoised ChatGPT common name function failed, calling original:", e$message))
    # Re-source the openai functions to ensure they're available
    source("functions/openai_summary.R", local = TRUE)
    result <- get_chatgpt_common_name(taxonomic_group)
    return(result)
  })

  cache_stats_after <- chatgpt_summary_cache$size()

  if (cache_stats_after > cache_stats_before) {
    api_log_info(paste("ChatGPT Common Name Cache MISS - fetched from API for", taxonomic_group))
  } else {
    api_log_info(paste("ChatGPT Common Name Cache HIT - using cached name for", taxonomic_group))
  }

  return(result)
}

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
  wikipedia_images_cache$reset()
  unsplash_images_cache$reset()
  wikimedia_images_cache$reset()
  pixabay_images_cache$reset()
  chatgpt_summary_cache$reset()

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
    ),
    wikipedia_images = list(
      size = wikipedia_images_cache$size(),
      keys = length(wikipedia_images_cache$keys())
    ),
    unsplash_images = list(
      size = unsplash_images_cache$size(),
      keys = length(unsplash_images_cache$keys())
    ),
    pixabay_images = list(
      size = pixabay_images_cache$size(),
      keys = length(pixabay_images_cache$keys())
    ),
    wikimedia_images = list(
      size = wikimedia_images_cache$size(),
      keys = length(wikimedia_images_cache$keys())
    ),
    chatgpt_summaries = list(
      size = chatgpt_summary_cache$size(),
      keys = length(chatgpt_summary_cache$keys())
    )
  )
}

# Prune expired cache entries
prune_caches <- function() {
  api_log_info("Pruning expired cache entries...")

  info_panel_cache$prune()
  wikipedia_cache$prune()
  phylopic_cache$prune()
  wikipedia_images_cache$prune()
  unsplash_images_cache$prune()
  pixabay_images_cache$prune()
  wikimedia_images_cache$prune()
  chatgpt_summary_cache$prune()

  api_log_info("Cache pruning completed")
}

api_log_info("Cached API functions loaded successfully")
api_log_info("Available cached functions:")
api_log_info("  cached_get_wikipedia_intro() - Main Wikipedia function")
api_log_info("  cached_get_wikipedia_main_image() - Wikipedia image function")
api_log_info("  cached_get_unsplash_random_image() - Unsplash image function")
api_log_info("  cached_get_pixabay_random_image() - Pixabay image function")
api_log_info("  cached_get_wikimedia_image_enhanced() - Wikimedia/Wikipedia Commons image function")
api_log_info("  cached_get_silhouette_data() - Main PhyloPic function")
api_log_info("  cached_get_chatgpt_summary() - ChatGPT taxonomic summary function")
api_log_info("  cached_get_chatgpt_image_selection() - ChatGPT image selection from multiple options")
api_log_info("  cached_get_chatgpt_common_name() - ChatGPT taxonomic to common name conversion")
api_log_info("  clear_all_caches() - Clear all cached data")
api_log_info("  get_cache_stats() - View cache statistics")
api_log_info("  prune_caches() - Remove expired entries")