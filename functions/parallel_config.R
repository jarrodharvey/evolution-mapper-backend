# Parallel Processing Configuration for Plumber
# Enables multithreading to allow concurrent requests

library(future)
library(promises)

# Set number of workers (default 3, can be overridden by environment variable)
WORKERS <- as.integer(Sys.getenv("PLUMBER_WORKERS", "3"))

# Configure multisession strategy for cross-platform compatibility
future::plan(future::multisession(workers = WORKERS))

api_log_info(paste0("Plumber configured with ", WORKERS, " parallel workers for multithreading"))