#!/usr/bin/env Rscript

# Enhanced Evolution Mapper Server Provisioning Script
# This script provisions a DigitalOcean droplet with R, required packages, and deploys the API

library(analogsea)
library(logger)

# Set up logging
log_dir <- "logs"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}
log_appender(appender_tee(file.path(log_dir, "provision.log")))
log_threshold(INFO)

# Provision logging helper functions
prov_log_info <- function(...) {
  msg <- paste(..., sep = " ")
  cat(msg, "\n")
  log_info(msg)
}

prov_log_warn <- function(...) {
  msg <- paste(..., sep = " ")
  cat("⚠️ ", msg, "\n")
  log_warn(msg)
}

prov_log_error <- function(...) {
  msg <- paste(..., sep = " ")
  cat("❌ ", msg, "\n")
  log_error(msg)
}

prov_log_success <- function(...) {
  msg <- paste(..., sep = " ")
  cat("✅ ", msg, "\n")
  log_info(paste("SUCCESS:", msg))
}

# Configuration
REQUIRED_FILES <- c(
  "plumber.R",
  "functions/logging_config.R",
  "functions/caching_config.R",
  "functions/parallel_config.R",
  "functions/progress_tracking.R",
  "functions/wikipedia_api.R",
  "functions/phylopic_silhouettes.R",
  "functions/wikipedia_images.R",
  "functions/wikimedia_images.R",
  "functions/cached_api_functions.R",
  "functions/color_config.R",
  "functions/datelife_efficiency.R",
  "functions/modern_age_mapping.R",
  "functions/info_panel_system.R",
  "functions/tree_html_enhancement.R",
  "functions/rotl_tree_generation.R",
  "functions/datelife_tree_generation.R",
  "functions/hybrid_input_validation.R",
  "functions/hybrid_tree_conversion.R",
  "functions/hybrid_json_output.R",
  "functions/hybrid_visualization.R",
  "functions/hybrid_tree_controller.R",
  "functions/attribution_extractor.R",
  "data/species.sqlite"
)
FIREWALL_NAME <- "evolution-mapper-restricted"

# Load environment variables from .Renviron
readRenviron(".Renviron")

# Get configuration from environment
do_pat <- Sys.getenv("DO_PAT")
if (do_pat == "") {
  stop("DO_PAT not found in .Renviron file. Please add your DigitalOcean API token.")
}

# Optional domain configuration for reverse proxy
domain <- Sys.getenv("DO_DROPLET_DOMAIN")
if (domain != "") {
  cat("Domain configured for reverse proxy:", domain, "\n")
} else {
  cat("No domain configured - reverse proxy will be skipped\n")
}

# Set the API token
Sys.setenv(DO_PAT = do_pat)

# Function to validate required files exist
validate_project_files <- function() {
  prov_log_info("Validating project files...")
  missing_files <- c()
  
  for (file in REQUIRED_FILES) {
    if (!file.exists(file)) {
      missing_files <- c(missing_files, file)
    }
  }
  
  if (length(missing_files) > 0) {
    prov_log_error("Missing required files:", paste(missing_files, collapse = ", "))
    stop("Missing required files: ", paste(missing_files, collapse = ", "))
  }
  prov_log_success("All required project files found")
}

# Function to get droplet by name or use first available
get_target_droplet <- function(droplet_name = NULL) {
  prov_log_info("Connecting to droplet...")
  
  all_droplets <- droplets()
  if (length(all_droplets) == 0) {
    prov_log_error("No droplets found in your DigitalOcean account")
    stop("No droplets found in your DigitalOcean account")
  }
  
  if (!is.null(droplet_name)) {
    for (droplet in all_droplets) {
      if (droplet$name == droplet_name) {
        prov_log_info("Found target droplet:", droplet_name)
        return(droplet)
      }
    }
    prov_log_error("Droplet not found:", droplet_name)
    stop("Droplet '", droplet_name, "' not found")
  }
  
  # Use first droplet if no name specified
  droplet <- all_droplets[[1]]
  prov_log_info("Using droplet:", droplet$name, "at", droplet$networks$v4[[1]]$ip_address)
  return(droplet)
}

# Function to configure firewall
configure_firewall <- function(droplet, allowed_ip = NULL) {
  if (is.null(allowed_ip)) {
    cat("⚠️  No allowed IP specified - firewall configuration skipped\n")
    return()
  }

  # Verify doctl is available for firewall commands
  doctl_check <- system("which doctl > /dev/null 2>&1")
  if (doctl_check != 0) {
    stop("❌ CRITICAL: doctl command not found - required for firewall configuration")
  }

  cat("Configuring firewall for IP:", allowed_ip, "\n")
  
  # Check if firewall already exists
  existing_firewalls <- system("doctl compute firewall list --format ID,Name --no-header", intern = TRUE)
  firewall_exists <- any(grepl(FIREWALL_NAME, existing_firewalls))
  
  if (firewall_exists) {
    cat("Firewall", FIREWALL_NAME, "already exists - skipping creation\n")
  } else {
    # Create restrictive firewall
    create_cmd <- paste0(
      "doctl compute firewall create ",
      "--name '", FIREWALL_NAME, "' ",
      "--inbound-rules 'protocol:tcp,ports:22,address:", allowed_ip, "/32 ",
      "protocol:tcp,ports:8000,address:", allowed_ip, "/32 ",
      "protocol:tcp,ports:8000,address:10.126.0.0/20' ",
      "--outbound-rules 'protocol:tcp,ports:all,address:0.0.0.0/0 ",
      "protocol:udp,ports:all,address:0.0.0.0/0 ",
      "protocol:icmp,address:0.0.0.0/0'"
    )
    
    create_result <- system(create_cmd)
    if (create_result != 0) {
      stop("❌ CRITICAL: Failed to create firewall - command failed with exit code ", create_result)
    }
  }
  
  # Apply firewall to droplet
  apply_cmd <- paste0(
    "doctl compute firewall add-droplets ",
    "$(doctl compute firewall list --format ID,Name --no-header | grep '", FIREWALL_NAME, "' | awk '{print $1}') ",
    "--droplet-ids ", droplet$id
  )
  
  apply_result <- system(apply_cmd)
  if (apply_result != 0) {
    stop("❌ CRITICAL: Failed to apply firewall to droplet - command failed with exit code ", apply_result)
  }
  cat("✅ Firewall configured and applied\n")
}

# Function to check if domain is accessible (determines if reverse proxy needed)
check_domain_health <- function(domain) {
  if (is.null(domain) || domain == "") {
    cat("No domain configured - skipping domain health check\n")
    return(FALSE)  # No reverse proxy needed if no domain
  }
  
  cat("Checking domain health:", domain, "\n")
  
  # Test HTTPS first (preferred)
  https_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' --connect-timeout 10 'https://", domain, "/api/health'")
  https_code <- suppressWarnings(system(https_cmd, intern = TRUE, ignore.stderr = TRUE))
  
  if (length(https_code) > 0 && https_code[1] == "200") {
    cat("✅ Domain accessible via HTTPS - reverse proxy not needed\n")
    return(FALSE)
  }
  
  # Test HTTP as fallback
  http_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' --connect-timeout 10 'http://", domain, "/api/health'")
  http_code <- suppressWarnings(system(http_cmd, intern = TRUE, ignore.stderr = TRUE))
  
  if (length(http_code) > 0 && http_code[1] == "200") {
    cat("✅ Domain accessible via HTTP - reverse proxy not needed\n")
    return(FALSE)
  }
  
  cat("❌ Domain not accessible - reverse proxy setup needed\n")
  return(TRUE)
}

# Function to set up reverse proxy with Caddy
setup_reverse_proxy <- function(droplet, domain) {
  if (is.null(domain) || domain == "") {
    prov_log_error("No domain configured - cannot setup reverse proxy")
    stop("❌ CRITICAL: No domain configured for reverse proxy setup")
  }
  
  prov_log_info("🔧 Setting up reverse proxy for domain:", domain)
  
  # Install Caddy via snap
  prov_log_info("Installing Caddy...")
  tryCatch({
    install_result <- capture.output(droplet_ssh(droplet, "sudo snap install caddy --classic && echo 'INSTALL_SUCCESS' || echo 'INSTALL_FAILED'"))
    install_result <- paste(install_result, collapse = " ")
    
    if (!grepl("INSTALL_SUCCESS", install_result)) {
      prov_log_error("Caddy installation failed with output:", install_result)
      stop("❌ CRITICAL: Caddy installation failed")
    }
    
    prov_log_success("Caddy installed successfully")
  }, error = function(e) {
    prov_log_error("Failed to install Caddy:", e$message)
    stop("❌ CRITICAL: Caddy installation failed - ", e$message)
  })
  
  # Verify Caddy installation
  prov_log_info("Verifying Caddy installation...")
  tryCatch({
    version_result <- capture.output(droplet_ssh(droplet, "caddy version && echo 'VERSION_SUCCESS' || echo 'VERSION_FAILED'"))
    version_result <- paste(version_result, collapse = " ")
    
    if (!grepl("VERSION_SUCCESS", version_result)) {
      prov_log_error("Caddy verification failed - binary not accessible")
      stop("❌ CRITICAL: Caddy installation verification failed")
    }
    
    prov_log_success("Caddy installation verified")
  }, error = function(e) {
    prov_log_error("Caddy verification failed:", e$message)
    stop("❌ CRITICAL: Caddy installation verification failed - ", e$message)
  })
  
  # Configure Caddy with proper paths and format
  prov_log_info("Configuring Caddy...")
  caddyfile_content <- paste0(
    domain, " {\n",
    "\treverse_proxy localhost:8000\n",
    "}"
  )
  
  tryCatch({
    # Create proper directory structure for snap Caddy
    dir_result <- capture.output(droplet_ssh(droplet, "sudo mkdir -p /var/snap/caddy/common && echo 'DIR_SUCCESS' || echo 'DIR_FAILED'"))
    dir_result <- paste(dir_result, collapse = " ")
    
    if (!grepl("DIR_SUCCESS", dir_result)) {
      prov_log_error("Failed to create Caddy directory structure")
      stop("❌ CRITICAL: Failed to create Caddy configuration directory")
    }
    
    # Write Caddyfile to correct location
    caddyfile_result <- capture.output(droplet_ssh(droplet, paste0(
      'sudo tee /var/snap/caddy/common/Caddyfile > /dev/null << "EOF"\n', 
      caddyfile_content, 
      '\nEOF && echo "CADDYFILE_SUCCESS" || echo "CADDYFILE_FAILED"'
    )))
    caddyfile_result <- paste(caddyfile_result, collapse = " ")
    
    if (!grepl("CADDYFILE_SUCCESS", caddyfile_result)) {
      prov_log_error("Failed to create Caddyfile")
      stop("❌ CRITICAL: Failed to create Caddyfile")
    }
    
    # Convert Caddyfile to JSON format (required by snap Caddy)
    prov_log_info("Converting Caddyfile to JSON format...")
    json_result <- capture.output(droplet_ssh(droplet, paste0(
      'cd /var/snap/caddy/common && ',
      'sudo caddy adapt --config Caddyfile --adapter caddyfile --pretty > caddy.json && ',
      'echo "JSON_SUCCESS" || echo "JSON_FAILED"'
    )))
    json_result <- paste(json_result, collapse = " ")
    
    if (!grepl("JSON_SUCCESS", json_result)) {
      prov_log_error("Failed to convert Caddyfile to JSON:", json_result)
      stop("❌ CRITICAL: Failed to convert Caddyfile to JSON format")
    }
    
    # Verify JSON configuration was created
    verify_result <- capture.output(droplet_ssh(droplet, "ls -la /var/snap/caddy/common/caddy.json && echo 'VERIFY_SUCCESS' || echo 'VERIFY_FAILED'"))
    verify_result <- paste(verify_result, collapse = " ")
    
    if (!grepl("VERIFY_SUCCESS", verify_result)) {
      prov_log_error("Caddy JSON configuration not found after creation")
      stop("❌ CRITICAL: Caddy JSON configuration missing")
    }
    
    prov_log_success("Caddyfile configured and converted to JSON successfully")
  }, error = function(e) {
    prov_log_error("Failed to configure Caddy:", e$message)
    stop("❌ CRITICAL: Caddy configuration failed - ", e$message)
  })
  
  # Start Caddy service (not restart, since it may not be running)
  prov_log_info("Starting Caddy service...")
  tryCatch({
    # Stop any existing service first
    droplet_ssh(droplet, "sudo snap stop caddy.server 2>/dev/null || true")
    
    # Start the service
    start_result <- capture.output(droplet_ssh(droplet, "sudo snap start caddy.server && echo 'START_SUCCESS' || echo 'START_FAILED'"))
    start_result <- paste(start_result, collapse = " ")
    
    if (!grepl("START_SUCCESS", start_result)) {
      prov_log_error("Failed to start Caddy service:", start_result)
      stop("❌ CRITICAL: Failed to start Caddy service")
    }
    
    # Verify service is running
    Sys.sleep(3) # Brief wait for service to initialize
    status_result <- capture.output(droplet_ssh(droplet, "snap services caddy | grep caddy.server | grep active && echo 'STATUS_SUCCESS' || echo 'STATUS_FAILED'"))
    status_result <- paste(status_result, collapse = " ")
    
    if (!grepl("STATUS_SUCCESS", status_result)) {
      prov_log_error("Caddy service failed to start properly:", status_result)
      
      # Get detailed logs for troubleshooting
      logs_result <- capture.output(droplet_ssh(droplet, "journalctl -u snap.caddy.server --no-pager -n 10"))
      prov_log_error("Caddy service logs:", paste(logs_result, collapse = " "))
      
      stop("❌ CRITICAL: Caddy service is not running after start command")
    }
    
    prov_log_success("Caddy service started successfully")
  }, error = function(e) {
    prov_log_error("Failed to start Caddy service:", e$message)
    stop("❌ CRITICAL: Caddy service startup failed - ", e$message)
  })
  
  # Enable Caddy service for automatic startup on boot
  prov_log_info("Enabling Caddy service for automatic startup...")
  tryCatch({
    enable_result <- capture.output(droplet_ssh(droplet, "sudo systemctl enable snap.caddy.server && echo 'ENABLE_SUCCESS' || echo 'ENABLE_FAILED'"))
    enable_result <- paste(enable_result, collapse = " ")
    
    if (!grepl("ENABLE_SUCCESS", enable_result)) {
      prov_log_error("Failed to enable Caddy service for auto-start:", enable_result)
      stop("❌ CRITICAL: Failed to enable Caddy service for automatic startup")
    }
    
    # Verify service is enabled
    enabled_check <- capture.output(droplet_ssh(droplet, "systemctl is-enabled snap.caddy.server && echo 'VERIFY_SUCCESS' || echo 'VERIFY_FAILED'"))
    enabled_check <- paste(enabled_check, collapse = " ")
    
    if (!grepl("VERIFY_SUCCESS", enabled_check)) {
      prov_log_error("Caddy service enable verification failed:", enabled_check)
      stop("❌ CRITICAL: Caddy service is not enabled for automatic startup")
    }
    
    prov_log_success("Caddy service enabled for automatic startup on boot")
  }, error = function(e) {
    prov_log_error("Failed to enable Caddy service:", e$message)
    stop("❌ CRITICAL: Caddy service enable failed - ", e$message)
  })
  
  # Verify Caddy is listening on expected ports
  prov_log_info("Verifying Caddy is listening on ports 80/443...")
  Sys.sleep(5) # Allow time for SSL certificate provisioning to start
  
  tryCatch({
    port_result <- capture.output(droplet_ssh(droplet, "ss -tlnp | grep -E ':(80|443)' && echo 'PORTS_SUCCESS' || echo 'PORTS_FAILED'"))
    port_result <- paste(port_result, collapse = " ")
    
    if (!grepl("PORTS_SUCCESS", port_result)) {
      prov_log_error("Caddy is not listening on HTTP/HTTPS ports:", port_result)
      
      # Get service status for troubleshooting
      service_status <- capture.output(droplet_ssh(droplet, "snap services caddy"))
      prov_log_error("Caddy service status:", paste(service_status, collapse = " "))
      
      stop("❌ CRITICAL: Caddy is not listening on expected ports 80/443")
    }
    
    prov_log_success("Caddy is listening on HTTP/HTTPS ports")
  }, error = function(e) {
    prov_log_error("Failed to verify Caddy ports:", e$message)
    stop("❌ CRITICAL: Caddy port verification failed - ", e$message)
  })
  
  prov_log_success("Reverse proxy setup completed successfully")
  return(TRUE)
}

# Function to verify domain accessibility after reverse proxy setup
verify_domain_health <- function(domain, max_attempts = 6, wait_seconds = 10) {
  if (is.null(domain) || domain == "") {
    prov_log_info("No domain configured - skipping verification")
    return(TRUE)  # Skip if no domain configured
  }
  
  prov_log_info("Waiting for reverse proxy to initialize (SSL certificate provisioning)...")
  Sys.sleep(30)  # Initial wait for Caddy to fully start and provision SSL
  
  prov_log_info("Starting domain health verification for:", domain)
  
  for (attempt in 1:max_attempts) {
    prov_log_info("Attempt", attempt, "of", max_attempts, "- Testing domain:", domain)
    
    # Test HTTPS endpoint (Caddy should auto-provision SSL)
    https_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' --connect-timeout 15 'https://", domain, "/api/health'")
    https_code <- suppressWarnings(system(https_cmd, intern = TRUE, ignore.stderr = TRUE))
    
    if (length(https_code) > 0 && https_code[1] == "200") {
      prov_log_success("Domain health check successful via HTTPS")
      return(TRUE)
    }
    
    # Test HTTP as fallback
    http_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' --connect-timeout 15 'http://", domain, "/api/health'")
    http_code <- suppressWarnings(system(http_cmd, intern = TRUE, ignore.stderr = TRUE))
    
    if (length(http_code) > 0 && http_code[1] == "200") {
      prov_log_success("Domain health check successful via HTTP")
      return(TRUE)
    }
    
    # Log detailed failure information
    https_status <- if (length(https_code) > 0) https_code[1] else "no_response"
    http_status <- if (length(http_code) > 0) http_code[1] else "no_response"
    
    prov_log_warn(paste0(
      "Attempt ", attempt, " failed - HTTPS: ", https_status, 
      ", HTTP: ", http_status
    ))
    
    if (attempt < max_attempts) {
      prov_log_info("Waiting", wait_seconds, "seconds before retry...")
      Sys.sleep(wait_seconds)
    }
  }
  
  prov_log_error("❌ DOMAIN VERIFICATION FAILED")
  prov_log_error("Domain:", domain, "is not accessible after", max_attempts, "attempts")
  prov_log_error("Final HTTPS status:", if (exists("https_status")) https_status else "unknown")
  prov_log_error("Final HTTP status:", if (exists("http_status")) http_status else "unknown")
  prov_log_error("This indicates the reverse proxy is not working correctly")
  
  return(FALSE)
}

# Function to verify deployment success
verify_deployment <- function(droplet) {
  prov_log_info("Verifying deployment...")
  ip_address <- droplet$networks$v4[[1]]$ip_address

  # Wait longer for R service to start up (R packages take time to load)
  prov_log_info("Waiting 30 seconds for R service to load packages and start...")
  Sys.sleep(30)

  # Test health endpoint with retry logic
  health_success <- FALSE
  for (attempt in 1:5) {
    prov_log_info("Health check attempt", attempt, "of 5...")

    health_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' --connect-timeout 15 'http://", ip_address, ":8000/api/health'")
    health_code <- tryCatch({
      system(health_cmd, intern = TRUE)
    }, error = function(e) {
      c("000")
    })

    if (length(health_code) > 0 && health_code[1] == "200") {
      prov_log_success("Health check passed on attempt", attempt)
      health_success <- TRUE
      break
    } else {
      status_code <- if (length(health_code) > 0) health_code[1] else "000"
      prov_log_warn("Health check failed with HTTP", status_code, "on attempt", attempt)

      if (attempt < 5) {
        prov_log_info("Waiting 15 seconds before retry...")
        Sys.sleep(15)
      }
    }
  }

  if (!health_success) {
    final_status <- if (length(health_code) > 0) health_code[1] else "000"
    stop("❌ CRITICAL: Health check failed after 5 attempts - HTTP ", final_status, " (expected 200)")
  }

  # Test API key endpoint
  api_keys <- strsplit(Sys.getenv("EVOLUTION_API_KEYS"), ",")[[1]]
  if (length(api_keys) > 0) {
    prov_log_info("Testing API key authentication...")
    test_cmd <- paste0("curl -s -H 'X-API-Key: ", api_keys[1], "' -o /dev/null -w '%{http_code}' --connect-timeout 15 'http://", ip_address, ":8000/api/species?limit=1'")
    api_code <- tryCatch({
      system(test_cmd, intern = TRUE)
    }, error = function(e) {
      c("000")
    })

    if (length(api_code) > 0 && api_code[1] == "200") {
      prov_log_success("API key authentication test passed")
    } else {
      api_status <- if (length(api_code) > 0) api_code[1] else "000"
      stop("❌ CRITICAL: API key test failed - HTTP ", api_status, " (expected 200)")
    }
  }

  prov_log_success("All verification tests passed - deployment successful")
  return(TRUE)
}

# Main provisioning logic
main <- function(droplet_name = NULL, allowed_ip = NULL) {
  prov_log_info("🚀 Starting Evolution Mapper API provisioning...")
  
  # Validate project files
  validate_project_files()
  
  # Get target droplet
  droplet <- get_target_droplet(droplet_name)
  
  tryCatch({
    # Update R to current version first
    prov_log_info("Updating R to current version...")
    tryCatch({
      droplet_ssh(droplet, "sudo systemctl stop plumber-evolution-mapper || true")
      update_result <- capture.output(droplet_ssh(droplet, "sudo apt update && echo 'SUCCESS' || echo 'FAILED'"))
      update_result <- paste(update_result, collapse = " ")
      if (!grepl("SUCCESS", update_result)) {
        stop("Failed to update package lists")
      }
      
      install_result <- capture.output(droplet_ssh(droplet, "sudo apt install -y software-properties-common dirmngr && echo 'SUCCESS' || echo 'FAILED'"))
      install_result <- paste(install_result, collapse = " ")
      if (!grepl("SUCCESS", install_result)) {
        stop("Failed to install software-properties-common and dirmngr")
      }
      
      # Detect Ubuntu version and set appropriate CRAN repository
      prov_log_info("Detecting Ubuntu version for CRAN repository configuration...")
      ubuntu_version <- capture.output(droplet_ssh(droplet, "lsb_release -cs"))
      ubuntu_version <- paste(ubuntu_version, collapse = " ")
      ubuntu_version <- trimws(ubuntu_version)
      prov_log_info("Detected Ubuntu codename:", ubuntu_version)
      
      # Map Ubuntu codenames to CRAN repository names
      cran_repo_map <- list(
        "jammy" = "jammy-cran40",      # Ubuntu 22.04 LTS
        "focal" = "focal-cran40",      # Ubuntu 20.04 LTS  
        "noble" = "noble-cran40",      # Ubuntu 24.04 LTS
        "mantic" = "mantic-cran40",    # Ubuntu 23.10
        "lunar" = "lunar-cran40",      # Ubuntu 23.04
        "kinetic" = "kinetic-cran40",  # Ubuntu 22.10
        "impish" = "impish-cran40",    # Ubuntu 21.10
        "hirsute" = "hirsute-cran40",  # Ubuntu 21.04
        "groovy" = "groovy-cran40",    # Ubuntu 20.10
        "plucky" = "noble-cran40"      # Ubuntu 25.04 - use noble (24.04) as fallback
      )
      
      # Get the appropriate CRAN repository suffix
      cran_suffix <- cran_repo_map[[ubuntu_version]]
      if (is.null(cran_suffix)) {
        prov_log_error("Ubuntu version", ubuntu_version, "not supported by CRAN - refusing to use potentially outdated Ubuntu repositories")
        stop("Unsupported Ubuntu version for CRAN R installation: ", ubuntu_version)
      } else {
        prov_log_info("Using CRAN repository suffix:", cran_suffix)
        
        # Add CRAN GPG key
        gpg_result <- capture.output(droplet_ssh(droplet, "wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc && echo 'SUCCESS' || echo 'FAILED'"))
        gpg_result <- paste(gpg_result, collapse = " ")
        if (!grepl("SUCCESS", gpg_result)) {
          stop("Failed to add CRAN GPG key")
        }
        
        # Add the appropriate CRAN repository
        cran_repo_line <- paste0("deb https://cloud.r-project.org/bin/linux/ubuntu ", cran_suffix, "/")
        prov_log_info("Adding CRAN repository:", cran_repo_line)
        droplet_ssh(droplet, paste0("echo \"", cran_repo_line, "\" | sudo tee /etc/apt/sources.list.d/cran-r.list"))
        
        # Update package lists with new repository
        update2_result <- capture.output(droplet_ssh(droplet, "sudo apt update && echo 'SUCCESS' || echo 'FAILED'"))
        update2_result <- paste(update2_result, collapse = " ")
        if (!grepl("SUCCESS", update2_result)) {
          stop("Failed to update package lists after adding CRAN repository")
        }
        
        # Install R from CRAN repository
        upgrade_result <- capture.output(droplet_ssh(droplet, "sudo apt install -y r-base r-base-dev && echo 'SUCCESS' || echo 'FAILED'"))
        upgrade_result <- paste(upgrade_result, collapse = " ")
        if (!grepl("SUCCESS", upgrade_result)) {
          prov_log_error("CRAN R installation failed - attempting fallback to Ubuntu repositories")

          # Remove CRAN repository as fallback
          prov_log_info("Removing CRAN repository to use Ubuntu R packages...")
          droplet_ssh(droplet, "sudo rm -f /etc/apt/sources.list.d/cran-r.list")
          droplet_ssh(droplet, "sudo apt update")

          # Install R from Ubuntu repositories as fallback
          prov_log_info("Installing R from Ubuntu repositories as fallback...")
          ubuntu_r_result <- capture.output(droplet_ssh(droplet, "sudo apt install -y r-base r-base-dev && echo 'SUCCESS' || echo 'FAILED'"))
          ubuntu_r_result <- paste(ubuntu_r_result, collapse = " ")
          if (!grepl("SUCCESS", ubuntu_r_result)) {
            stop("Failed to install R from both CRAN and Ubuntu repositories")
          }
          prov_log_success("R installed successfully from Ubuntu repositories")
        }
      }
      
      prov_log_success("R update completed successfully")
    }, error = function(e) {
      stop("❌ R update failed: ", e$message)
    })
    
    # Verify R version
    r_version_result <- tryCatch({
      version_output <- capture.output(droplet_ssh(droplet, "R --version | head -1"))
      paste(version_output, collapse = " ")
    }, error = function(e) {
      stop("❌ Failed to verify R version: ", e$message)
    })
    cat("✅ R version verified:", r_version_result, "\n")
    
    # Install system dependencies (including gfortran for Hmisc and libsodium-dev for plumber)
    prov_log_info("Installing system dependencies...")
    system_deps <- c("libcurl4-openssl-dev", "libssl-dev", "libxml2-dev", "libsqlite3-dev", "pandoc", "librsvg2-dev", "gfortran", "libsodium-dev")
    
    # Install memory-intensive packages as pre-compiled Ubuntu binaries to avoid OOM issues
    prov_log_info("Installing memory-intensive R packages as Ubuntu binaries...")
    ubuntu_r_packages <- c("r-cran-rsqlite", "r-cran-dbi")
    for (pkg in ubuntu_r_packages) {
      prov_log_info("Installing Ubuntu binary package:", pkg)
      pkg_result <- capture.output(droplet_ssh(droplet, paste0("sudo apt-get install -y ", pkg, " && echo 'SUCCESS' || echo 'FAILED'")))
      pkg_result <- paste(pkg_result, collapse = " ")
      if (!grepl("SUCCESS", pkg_result)) {
        stop("❌ CRITICAL: Failed to install Ubuntu binary package: ", pkg)
      }
      prov_log_success("Successfully installed Ubuntu binary package:", pkg)
    }
    
    tryCatch({
      for (dep in system_deps) {
        prov_log_info("Installing system dependency:", dep)
        result <- capture.output(droplet_ssh(droplet, paste0("sudo apt install -y ", dep, " && echo 'SUCCESS' || echo 'FAILED'")))
        result <- paste(result, collapse = " ")
        if (!grepl("SUCCESS", result)) {
          stop("Failed to install system dependency: ", dep)
        }
      }
      prov_log_success("All system dependencies installed successfully")
    }, error = function(e) {
      stop("❌ System dependency installation failed: ", e$message)
    })
    
    # Install R packages system-wide for all users (required for service to work)
    prov_log_info("Installing R packages system-wide...")

    # Create system library directory if it doesn't exist
    droplet_ssh(droplet, "sudo mkdir -p /usr/local/lib/R/site-library")
    droplet_ssh(droplet, "sudo chown -R root:staff /usr/local/lib/R/site-library")
    droplet_ssh(droplet, "sudo chmod 755 /usr/local/lib/R/site-library")

    # Check if we can skip package installation by verifying key packages exist
    prov_log_info("Checking existing R package installation...")
    key_packages <- c("plumber", "datelife", "bold", "rphylopic", "tidywikidatar", "RSQLite")
    all_packages_exist <- TRUE

    tryCatch({
      for (pkg in key_packages) {
        check_result <- capture.output(droplet_ssh(droplet, paste0(
          'R -e "if (require(', pkg, ', lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { cat(\'EXISTS\') } else { cat(\'MISSING\') }"'
        )))
        check_result <- paste(check_result, collapse = " ")
        if (!grepl("EXISTS", check_result)) {
          all_packages_exist <- FALSE
          prov_log_info("Package missing or needs update:", pkg)
          break
        }
      }

      if (all_packages_exist) {
        prov_log_success("All key packages already installed - skipping package installation")
      } else {
        prov_log_info("Some packages missing - proceeding with full installation")
      }

      # Force check for tidywikidatar specifically since it's critical
      prov_log_info("Double-checking tidywikidatar package specifically...")
      tidywiki_check_result <- tryCatch({
        result <- capture.output(droplet_ssh(droplet, paste0(
          'R -e "if (require(tidywikidatar, lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { cat(\'TIDYWIKI_EXISTS\') } else { cat(\'TIDYWIKI_MISSING\') }"'
        )))
        paste(result, collapse = " ")
      }, error = function(e) {
        "TIDYWIKI_ERROR"
      })

      if (!grepl("TIDYWIKI_EXISTS", tidywiki_check_result)) {
        prov_log_warn("tidywikidatar is missing - forcing package installation...")
        all_packages_exist <- FALSE
      }
    }, error = function(e) {
      prov_log_error("Failed to verify existing package installation:", e$message)
      stop("❌ CRITICAL: Cannot verify package installation status - ", e$message)
    })
    
    # Set proper permissions for system library (critical for non-root user access)
    prov_log_info("Setting proper permissions for system library...")
    droplet_ssh(droplet, "sudo find /usr/local/lib/R/site-library -type d -exec chmod 755 {} \\;")
    droplet_ssh(droplet, "sudo find /usr/local/lib/R/site-library -type f -exec chmod 644 {} \\;")
    prov_log_success("System library permissions configured")

    # Only install packages if not all key packages exist
    if (!all_packages_exist) {
      # CRAN packages that work with current R
      # CRAN packages (excluding memory-intensive ones that are installed as Ubuntu binaries)
      cran_packages <- c(
        "plumber", "rlang", "rotl", "ape", "collapsibleTree", "htmlwidgets",
        "RSQLite", "DBI", "dplyr", "httr", "httr2", "logger", "memoise",
        "cachem", "future", "promises", "remotes", "Hmisc", "taxize",
        "rphylopic", "phylobase", "jsonlite", "colorspace", "tidywikidatar",
        "rentrez"
      )
    
    for (pkg in cran_packages) {
      # Check if package is already installed system-wide
      prov_log_info("Checking if package already installed system-wide:", pkg)
      check_result <- tryCatch({
        result <- capture.output(droplet_ssh(droplet, paste0(
          'R -e "if (require(', pkg, ', lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { cat(\'ALREADY_INSTALLED\') } else { cat(\'NOT_INSTALLED\') }"'
        )))
        paste(result, collapse = " ")
      }, error = function(e) {
        "NOT_INSTALLED"
      })
      
      if (grepl("ALREADY_INSTALLED", check_result)) {
        prov_log_info("Package already installed, skipping:", pkg)
        next
      }
      
      prov_log_info("Installing R package system-wide:", pkg)
      tryCatch({
        # Install to system library so all users can access it
        install_result <- capture.output(droplet_ssh(droplet, paste0(
          'sudo R -e "install.packages(\'', pkg, '\', repos=\'https://cloud.r-project.org\', lib=\'/usr/local/lib/R/site-library\', quiet=FALSE); ',
          'if (require(', pkg, ', lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { cat(\'VERIFY_SUCCESS\') } else { cat(\'VERIFY_FAILED\') }"'
        )))
        install_result <- paste(install_result, collapse = " ")
        
        if (grepl("VERIFY_FAILED", install_result) || !grepl("VERIFY_SUCCESS", install_result)) {
          prov_log_error("Package installation verification failed for:", pkg)
          prov_log_error("Installation output:", install_result)
          stop("Package installation verification failed for: ", pkg)
        }
        
        prov_log_success("Successfully installed system-wide:", pkg)
      }, error = function(e) {
        prov_log_error("Failed to install R package system-wide:", pkg, "-", e$message)
        stop("❌ Failed to install R package '", pkg, "' system-wide: ", e$message)
      })
    }
    
    # GitHub packages (removed from CRAN) - install system-wide
    prov_log_info("Installing packages from GitHub system-wide...")
    prov_log_info("⏳ NOTE: GitHub package installation may take 15-30 minutes due to complex dependencies (especially datelife with Bioconductor packages). Please be patient...")
    github_packages <- list(
      list(name = "bold", repo = "ropensci/bold"),
      list(name = "datelife", repo = "phylotastic/datelife")
    )
    
    # Install R package system dependencies proactively using RSPM API
    prov_log_info("Checking for additional system dependencies for R packages...")
    all_r_packages <- c(cran_packages, sapply(github_packages, function(x) x$name))
    
    tryCatch({
      # Use remotes package to get system requirements from RSPM
      sys_deps_result <- capture.output(droplet_ssh(droplet, paste0(
        'sudo R -e "',
        'if (!require(remotes, quietly=TRUE)) install.packages(\\"remotes\\", repos=\\"https://cloud.r-project.org\\"); ',
        'library(remotes); ',
        'packages <- c(\\"', paste(all_r_packages, collapse = '\\", \\"'), '\\"); ',
        'deps <- character(0); ',
        'for (pkg in packages) { ',
        '  tryCatch({ ',
        '    pkg_deps <- system_requirements(\\"ubuntu\\", \\"20.04\\", package=pkg); ',
        '    if (length(pkg_deps) > 0) deps <- c(deps, pkg_deps); ',
        '  }, error = function(e) { }); ',
        '}; ',
        'unique_deps <- unique(deps); ',
        'if (length(unique_deps) > 0) { ',
        '  cat(\\"SYSTEM_DEPS:\\", paste(unique_deps, collapse=\\" \\")); ',
        '} else { ',
        '  cat(\\"NO_ADDITIONAL_DEPS\\"); ',
        '}"'
      )))
      
      sys_deps_output <- paste(sys_deps_result, collapse = " ")
      
      if (grepl("SYSTEM_DEPS:", sys_deps_output)) {
        # Extract dependencies after "SYSTEM_DEPS:"
        deps_match <- regmatches(sys_deps_output, regexpr("SYSTEM_DEPS: .*", sys_deps_output))
        deps_line <- gsub("SYSTEM_DEPS: ", "", deps_match)
        individual_deps <- unique(unlist(strsplit(deps_line, " ")))
        individual_deps <- individual_deps[individual_deps != ""]
        
        prov_log_info("Installing", length(individual_deps), "additional system dependencies for R packages...")
        for (dep in individual_deps) {
          prov_log_info("Installing system dependency:", dep)
          droplet_ssh(droplet, paste0("sudo apt-get install -y ", dep))
        }
        prov_log_success("Additional R package system dependencies installed")
      } else {
        prov_log_info("No additional system dependencies required beyond base installation")
      }
      
    }, error = function(e) {
      prov_log_error("Failed to determine system dependencies for R packages:", e$message)
      stop("❌ CRITICAL: Cannot verify required system dependencies - ", e$message)
    })
    
    tryCatch({
      for (pkg_info in github_packages) {
        # Check if GitHub package is already installed system-wide
        prov_log_info("Checking if GitHub package already installed system-wide:", pkg_info$name)
        check_result <- tryCatch({
          result <- capture.output(droplet_ssh(droplet, paste0(
            'R -e "if (require(', pkg_info$name, ', lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { cat(\'ALREADY_INSTALLED\') } else { cat(\'NOT_INSTALLED\') }"'
          )))
          paste(result, collapse = " ")
        }, error = function(e) {
          "NOT_INSTALLED"
        })
        
        if (grepl("ALREADY_INSTALLED", check_result)) {
          prov_log_info("GitHub package already installed, skipping:", pkg_info$name)
          next
        }
        
        prov_log_info("Installing GitHub package system-wide:", pkg_info$name, "from", pkg_info$repo)
        
        github_install_result <- capture.output(droplet_ssh(droplet, paste0(
          'sudo R -e "library(remotes, lib.loc=\'/usr/local/lib/R/site-library\'); ',
          'install_github(\'', pkg_info$repo, '\', lib=\'/usr/local/lib/R/site-library\', quiet=FALSE); ',
          'if (require(', pkg_info$name, ', lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { cat(\'VERIFY_SUCCESS\') } else { cat(\'VERIFY_FAILED\') }"'
        )))
        github_install_result <- paste(github_install_result, collapse = " ")
        
        if (grepl("VERIFY_FAILED", github_install_result) || !grepl("VERIFY_SUCCESS", github_install_result)) {
          prov_log_error("GitHub package installation verification failed for:", pkg_info$name)
          prov_log_error("Installation output:", github_install_result)
          stop("GitHub package installation verification failed for: ", pkg_info$name)
        }
        
        prov_log_success("Successfully installed system-wide:", pkg_info$name)
      }
      prov_log_success("All GitHub packages installed system-wide successfully")
    }, error = function(e) {
      prov_log_error("GitHub package installation failed:", e$message)
      stop("❌ GitHub package installation failed: ", e$message)
    })
    
    # Perform comprehensive package verification system-wide
    prov_log_info("Performing comprehensive package verification system-wide...")
    target_packages <- c("datelife", "bold", "taxize", "Hmisc", "rphylopic", "remotes", "plumber", "rlang")
    
    tryCatch({
      for (pkg in target_packages) {
        prov_log_info("Verifying system-wide package:", pkg)
        
        # Get detailed package information from system library
        verify_result <- capture.output(droplet_ssh(droplet, paste0(
          'R -e "if (require(', pkg, ', lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { ',
          'version <- as.character(packageVersion(\'', pkg, '\')); ',
          'cat(\'SUCCESS\', version); ',
          '} else { cat(\'FAILED\') }"'
        )))
        verify_result <- paste(verify_result, collapse = " ")
        
        if (!grepl("SUCCESS", verify_result)) {
          stop("Package verification failed for: ", pkg)
        }
        
        # Extract version from result
        version_info <- strsplit(verify_result, " ")[[1]]
        if (length(version_info) > 1) {
          prov_log_success("Verified system-wide:", pkg, "version", version_info[2])
        } else {
          prov_log_success("Verified system-wide:", pkg)
        }
      }
      prov_log_success("All", length(target_packages), "critical packages verified system-wide successfully")
    }, error = function(e) {
      stop("❌ Package verification failed: ", e$message)
    })

    } else {
      # Force installation of tidywikidatar if missing
      prov_log_info("Force-checking for tidywikidatar package...")
      tidywiki_check_result <- tryCatch({
        result <- capture.output(droplet_ssh(droplet, paste0(
          'R -e "if (require(tidywikidatar, lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { cat(\'TIDYWIKI_EXISTS\') } else { cat(\'TIDYWIKI_MISSING\') }"'
        )))
        paste(result, collapse = " ")
      }, error = function(e) {
        "TIDYWIKI_ERROR"
      })

      if (!grepl("TIDYWIKI_EXISTS", tidywiki_check_result)) {
        prov_log_warn("tidywikidatar is missing - installing it now...")

        # Install tidywikidatar specifically
        tryCatch({
          install_result <- capture.output(droplet_ssh(droplet, paste0(
            'sudo R -e "install.packages(\'tidywikidatar\', repos=\'https://cloud.r-project.org\', lib=\'/usr/local/lib/R/site-library\', quiet=FALSE); ',
            'if (require(tidywikidatar, lib.loc=\'/usr/local/lib/R/site-library\', quietly=TRUE)) { cat(\'VERIFY_SUCCESS\') } else { cat(\'VERIFY_FAILED\') }"'
          )))
          install_result <- paste(install_result, collapse = " ")

          if (grepl("VERIFY_FAILED", install_result) || !grepl("VERIFY_SUCCESS", install_result)) {
            prov_log_error("tidywikidatar installation verification failed")
            prov_log_error("Installation output:", install_result)
            stop("tidywikidatar installation verification failed")
          }

          prov_log_success("Successfully installed tidywikidatar")
        }, error = function(e) {
          prov_log_error("Failed to install tidywikidatar:", e$message)
          stop("❌ Failed to install tidywikidatar: ", e$message)
        })
      } else {
        prov_log_success("tidywikidatar is already available")
      }

      prov_log_success("All R packages verified and ready")
    }

    # Deploy the API with selective file upload
    cat("Deploying Evolution Mapper API...\n")
    
    # Create plumber user if it doesn't exist
    prov_log_info("Creating plumber user...")
    tryCatch({
      user_check <- capture.output(droplet_ssh(droplet, "id plumber 2>/dev/null && echo 'EXISTS' || echo 'MISSING'"))
      user_check <- paste(user_check, collapse = " ")
      
      if (grepl("MISSING", user_check)) {
        droplet_ssh(droplet, "sudo useradd -r -s /bin/false -d /var/plumber plumber")
        prov_log_success("Plumber user created")
      } else {
        prov_log_info("Plumber user already exists")
      }
    }, error = function(e) {
      prov_log_error("Failed to create plumber user:", e$message)
      stop("❌ CRITICAL: Cannot create required plumber user - ", e$message)
    })
    
    # Create deployment directory structure
    tryCatch({
      droplet_ssh(droplet, "sudo mkdir -p /var/plumber/evolution-mapper")
      droplet_ssh(droplet, "sudo chown -R plumber:plumber /var/plumber")
      
      # Verify directory creation
      dir_check <- capture.output(droplet_ssh(droplet, "ls -ld /var/plumber/evolution-mapper"))
      dir_check <- paste(dir_check, collapse = " ")
      if (!grepl("plumber plumber", dir_check)) {
        stop("Failed to create or set permissions for deployment directory")
      }
      
      prov_log_success("Deployment directory created successfully")
    }, error = function(e) {
      stop("❌ Failed to create deployment directory: ", e$message)
    })
    
    # Upload core files (excluding .claude/, screenshots/, unnecessary files)
    prov_log_info("Uploading core application files...")
    tryCatch({
      droplet_upload(droplet, "plumber.R", "/tmp/plumber.R")
      droplet_ssh(droplet, "mv /tmp/plumber.R /var/plumber/evolution-mapper/plumber.R")
      
      # Verify file upload
      file_check <- capture.output(droplet_ssh(droplet, "ls -la /var/plumber/evolution-mapper/plumber.R"))
      file_check <- paste(file_check, collapse = " ")
      if (!grepl("plumber.R", file_check)) {
        stop("plumber.R file not found after upload")
      }
      
      prov_log_success("plumber.R uploaded successfully")
    }, error = function(e) {
      stop("❌ Failed to upload plumber.R: ", e$message)
    })
    
    # Upload functions directory
    tryCatch({
      tar_result <- system("tar -czf /tmp/functions.tar.gz --exclude='.*' --exclude='*.tmp' functions/")
      if (tar_result != 0) {
        stop("Failed to create functions tar archive")
      }
      
      droplet_upload(droplet, "/tmp/functions.tar.gz", "/tmp/functions.tar.gz")
      droplet_ssh(droplet, "cd /var/plumber/evolution-mapper && tar -xzf /tmp/functions.tar.gz && rm /tmp/functions.tar.gz")
      
      # Verify functions directory exists
      functions_check <- capture.output(droplet_ssh(droplet, "ls -ld /var/plumber/evolution-mapper/functions"))
      functions_check <- paste(functions_check, collapse = " ")
      if (!grepl("functions", functions_check)) {
        stop("Functions directory not found after upload and extraction")
      }
      
      prov_log_success("Functions directory uploaded successfully")
    }, error = function(e) {
      stop("❌ Failed to upload functions directory: ", e$message)
    })
    
    # Upload and create data directory with database
    prov_log_info("Uploading database...")
    tryCatch({
      data_tar_result <- system("tar -czf /tmp/data.tar.gz data/")
      if (data_tar_result != 0) {
        stop("Failed to create data tar archive")
      }
      
      droplet_upload(droplet, "/tmp/data.tar.gz", "/tmp/data.tar.gz")
      droplet_ssh(droplet, "cd /var/plumber/evolution-mapper && tar -xzf /tmp/data.tar.gz && rm /tmp/data.tar.gz")
      
      # Verify database file exists
      db_check <- capture.output(droplet_ssh(droplet, "ls -la /var/plumber/evolution-mapper/data/species.sqlite"))
      db_check <- paste(db_check, collapse = " ")
      if (!grepl("species.sqlite", db_check)) {
        stop("Database file not found after upload")
      }
      
      prov_log_success("Database uploaded successfully")
    }, error = function(e) {
      stop("❌ Failed to upload database: ", e$message)
    })
    
    # Create required directories for API operation
    prov_log_info("Creating required directories...")
    required_dirs <- c("progress", "logs", "cache", "cache/info_panels", "cache/wikipedia", "cache/phylopic", "image_overrides")

    tryCatch({
      for (dir_name in required_dirs) {
        full_path <- paste0("/var/plumber/evolution-mapper/", dir_name)

        prov_log_info("Creating directory:", dir_name)
        droplet_ssh(droplet, paste0("mkdir -p ", full_path))

        # Set appropriate permissions based on directory type
        if (grepl("cache", dir_name)) {
          # Cache directories need write access for the plumber user
          droplet_ssh(droplet, paste0("chmod 755 ", full_path))
          droplet_ssh(droplet, paste0("chown plumber:plumber ", full_path))
        } else {
          # Other directories can have standard permissions
          droplet_ssh(droplet, paste0("chmod 755 ", full_path))
        }

        # Verify directory creation
        dir_check <- capture.output(droplet_ssh(droplet, paste0("ls -ld ", full_path)))
        dir_check <- paste(dir_check, collapse = " ")
        if (!grepl(basename(dir_name), dir_check)) {
          stop("Directory not created successfully: ", dir_name)
        }

        prov_log_success("Directory created successfully:", dir_name)
      }

      # Ensure plumber user owns all directories
      droplet_ssh(droplet, "chown -R plumber:plumber /var/plumber/evolution-mapper")

      prov_log_success("All required directories created successfully")
    }, error = function(e) {
      stop("❌ Failed to create required directories: ", e$message)
    })
    
    # Setup systemd service
    prov_log_info("Setting up systemd service...")
    systemd_service <- '
[Unit]
Description=Plumber Evolution Mapper API
After=network.target

[Service]
Type=simple
User=plumber
WorkingDirectory=/var/plumber/evolution-mapper
ExecStartPre=/bin/bash -c "mkdir -p /var/plumber/evolution-mapper/{logs,cache,cache/info_panels,cache/wikipedia,cache/phylopic,progress,image_overrides} && chown -R plumber:plumber /var/plumber/evolution-mapper"
ExecStart=/usr/bin/Rscript /var/plumber/evolution-mapper/run.R
Restart=on-failure
RestartSec=5
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
'
    tryCatch({
      droplet_ssh(droplet, paste0('sudo tee /etc/systemd/system/plumber-evolution-mapper.service > /dev/null << "EOF"', systemd_service, 'EOF'))
      
      # Verify service file creation
      service_check <- capture.output(droplet_ssh(droplet, "ls -la /etc/systemd/system/plumber-evolution-mapper.service"))
      service_check <- paste(service_check, collapse = " ")
      if (!grepl("plumber-evolution-mapper.service", service_check)) {
        stop("Systemd service file not created")
      }
      
      reload_result <- capture.output(droplet_ssh(droplet, "sudo systemctl daemon-reload && echo 'SUCCESS' || echo 'FAILED'"))
      reload_result <- paste(reload_result, collapse = " ")
      if (!grepl("SUCCESS", reload_result)) {
        stop("Failed to reload systemd daemon")
      }
      
      enable_result <- capture.output(droplet_ssh(droplet, "sudo systemctl enable plumber-evolution-mapper && echo 'SUCCESS' || echo 'FAILED'"))
      enable_result <- paste(enable_result, collapse = " ")
      if (!grepl("SUCCESS", enable_result)) {
        stop("Failed to enable systemd service")
      }
      
      prov_log_success("Systemd service configured successfully")
    }, error = function(e) {
      stop("❌ Failed to setup systemd service: ", e$message)
    })
    
    # Fix missing rlang dependency
    prov_log_info("Fixing rlang dependency in plumber.R...")
    rlang_fix_result <- capture.output(droplet_ssh(droplet, 'cd /var/plumber/evolution-mapper && echo "library(rlang)" > temp_fix.txt && echo "" >> temp_fix.txt && cat plumber.R >> temp_fix.txt && mv temp_fix.txt plumber.R && echo "SUCCESS" || echo "FAILED"'))
    rlang_fix_result <- paste(rlang_fix_result, collapse = " ")
    if (!grepl("SUCCESS", rlang_fix_result)) {
      stop("❌ CRITICAL: Failed to fix rlang dependency in plumber.R")
    }

    # Verify the modification was successful
    verification_result <- capture.output(droplet_ssh(droplet, "head -1 /var/plumber/evolution-mapper/plumber.R"))
    verification_result <- paste(verification_result, collapse = " ")
    if (!grepl("library\\(rlang\\)", verification_result)) {
      stop("❌ CRITICAL: plumber.R modification verification failed - rlang library not found at beginning of file")
    }
    prov_log_success("plumber.R rlang dependency fixed and verified")
    
    # Create run.R script for systemd with system library path
    prov_log_info("Creating run.R script...")
    run_r_content <- paste0(
      "# Evolution Mapper API Startup Script\n",
      "# Environment variables are set by systemd service\n",
      "setwd(\"/var/plumber/evolution-mapper\")\n\n",
      "# Set library path to include system library (required for service user)\n",
      ".libPaths(c('/usr/local/lib/R/site-library', .libPaths()))\n\n",
      "# Load required libraries\n",
      "library(rlang)\n",
      "library(plumber)\n\n",
      "# Verify all packages are available\n",
      "required_packages <- c('datelife', 'bold', 'taxize', 'Hmisc', 'rphylopic')\n",
      "for (pkg in required_packages) {\n",
      "  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {\n",
      "    stop('Required package not available: ', pkg)\n",
      "  }\n",
      "}\n",
      "cat('✅ All required packages loaded successfully\\n')\n\n",
      "# Start API server\n",
      "pr <- pr('plumber.R')\n",
      "pr$setDocs(TRUE)\n",
      "cat('🚀 Starting Evolution Mapper API on port 8000...\\n')\n",
      "pr$run(port=8000, host='0.0.0.0')\n"
    )
    
    # Create run.R file locally and upload it instead of using HEREDOC
    temp_run_r <- tempfile(fileext = ".R")
    writeLines(strsplit(run_r_content, "\n")[[1]], temp_run_r)

    tryCatch({
      droplet_upload(droplet, temp_run_r, "/tmp/run.R")
      droplet_ssh(droplet, "mv /tmp/run.R /var/plumber/evolution-mapper/run.R")

      run_r_result <- capture.output(droplet_ssh(droplet, "ls -la /var/plumber/evolution-mapper/run.R && echo 'SUCCESS' || echo 'FAILED'"))
      run_r_result <- paste(run_r_result, collapse = " ")
      if (!grepl("SUCCESS", run_r_result)) {
        stop("Failed to create run.R script")
      }
    }, finally = {
      if (file.exists(temp_run_r)) {
        unlink(temp_run_r)
      }
    })

    # Verify run.R was created successfully
    run_r_check <- capture.output(droplet_ssh(droplet, "ls -la /var/plumber/evolution-mapper/run.R && grep -q 'plumber' /var/plumber/evolution-mapper/run.R && echo 'VERIFIED' || echo 'MISSING'"))
    run_r_check <- paste(run_r_check, collapse = " ")
    if (!grepl("VERIFIED", run_r_check)) {
      stop("❌ CRITICAL: run.R script verification failed - file missing or invalid")
    }
    prov_log_success("run.R script created and verified")
    
    # Upload local .Renviron to server (ensures server matches local configuration)
    prov_log_info("Syncing local .Renviron to server...")
    if (file.exists(".Renviron")) {
      tryCatch({
        droplet_upload(droplet, ".Renviron", "/tmp/.Renviron")
        droplet_ssh(droplet, "mv /tmp/.Renviron /var/plumber/evolution-mapper/.Renviron")
        
        # Verify .Renviron upload
        renviron_check <- capture.output(droplet_ssh(droplet, "ls -la /var/plumber/evolution-mapper/.Renviron"))
        renviron_check <- paste(renviron_check, collapse = " ")
        if (!grepl(".Renviron", renviron_check)) {
          stop(".Renviron file not found after upload")
        }
        
        prov_log_success("Local .Renviron synced to server")
      }, error = function(e) {
        stop("❌ Failed to upload .Renviron: ", e$message)
      })
    } else {
      stop("❌ No local .Renviron found - this is required for deployment")
    }
    
    # Configure environment variables
    prov_log_info("Configuring environment variables...")
    evolution_api_keys <- Sys.getenv("EVOLUTION_API_KEYS")
    if (evolution_api_keys == "") {
      stop("❌ EVOLUTION_API_KEYS not found in .Renviron. Please set secure API keys before deployment.")
    }
    
    # Validate API keys format
    if (nchar(evolution_api_keys) < 10) {
      stop("❌ EVOLUTION_API_KEYS appear to be too short or invalid")
    }

    # Ensure API keys are not example/placeholder values
    example_patterns <- c("your-dev-key", "your-key", "example", "test-key", "placeholder", "demo-key")
    for (pattern in example_patterns) {
      if (grepl(pattern, evolution_api_keys, ignore.case = TRUE)) {
        stop("❌ CRITICAL: EVOLUTION_API_KEYS contains example/placeholder values (", pattern, ") - real API keys required")
      }
    }
    
    cors_origins <- Sys.getenv("CORS_ALLOWED_ORIGINS", "http://localhost:3000")
    if (cors_origins == "") {
      stop("❌ CORS_ALLOWED_ORIGINS cannot be empty")
    }
    
    prov_log_info("Configured CORS origins:", cors_origins)
    prov_log_info("Configured API keys:", substr(evolution_api_keys, 1, 10), "...")
    
    # Set up systemd environment configuration
    cat("Setting up systemd environment...\n")
    tryCatch({
      droplet_ssh(droplet, "sudo mkdir -p /etc/systemd/system/plumber-evolution-mapper.service.d")
      
      # Verify directory creation
      env_dir_check <- capture.output(droplet_ssh(droplet, "ls -ld /etc/systemd/system/plumber-evolution-mapper.service.d"))
      env_dir_check <- paste(env_dir_check, collapse = " ")
      if (!grepl("plumber-evolution-mapper.service.d", env_dir_check)) {
        stop("Failed to create systemd environment directory")
      }
      
      env_config <- paste0(
        "[Service]\n",
        "Environment=\"EVOLUTION_API_KEYS=", evolution_api_keys, "\"\n",
        "Environment=\"CORS_ALLOWED_ORIGINS=", cors_origins, "\"\n",
        "Environment=\"HOME=/var/plumber/evolution-mapper\"\n",
        "Environment=\"R_LIBS_SITE=/usr/local/lib/R/site-library\"\n"
      )
      
      droplet_ssh(droplet, paste0('sudo tee /etc/systemd/system/plumber-evolution-mapper.service.d/environment.conf > /dev/null << "EOF"\n', env_config, 'EOF'))
      
      # Verify environment config creation
      env_config_check <- capture.output(droplet_ssh(droplet, "ls -la /etc/systemd/system/plumber-evolution-mapper.service.d/environment.conf"))
      env_config_check <- paste(env_config_check, collapse = " ")
      if (!grepl("environment.conf", env_config_check)) {
        stop("Failed to create systemd environment configuration")
      }
      
      cat("✅ Systemd environment configured successfully\n")
    }, error = function(e) {
      stop("❌ Failed to setup systemd environment: ", e$message)
    })
    
    # Reload and start the service
    cat("Starting API service...\n")
    tryCatch({
      daemon_reload_result <- capture.output(droplet_ssh(droplet, "sudo systemctl daemon-reload && echo 'SUCCESS' || echo 'FAILED'"))
      daemon_reload_result <- paste(daemon_reload_result, collapse = " ")
      if (!grepl("SUCCESS", daemon_reload_result)) {
        stop("Failed to reload systemd daemon")
      }
      
      # Stop any existing service first, then start fresh
      droplet_ssh(droplet, "sudo systemctl stop plumber-evolution-mapper 2>/dev/null || true")

      # Set proper ownership on all files to ensure plumber user can access them
      prov_log_info("Setting proper ownership on all application files...")
      droplet_ssh(droplet, "sudo chown -R plumber:plumber /var/plumber/evolution-mapper")

      # Try to start the service manually first to check for errors
      prov_log_info("Testing manual service start to check for errors...")
      manual_test_result <- tryCatch({
        result <- capture.output(droplet_ssh(droplet, "cd /var/plumber/evolution-mapper && timeout 10 sudo -u plumber Rscript run.R 2>&1 || echo 'MANUAL_TEST_COMPLETE'"))
        paste(result, collapse = " ")
      }, error = function(e) {
        prov_log_warn("Manual test SSH error (expected):", e$message)
        "MANUAL_TEST_SSH_ERROR"
      })

      if (manual_test_result != "MANUAL_TEST_SSH_ERROR") {
        prov_log_info("Manual service test output:", manual_test_result)
      }

      prov_log_info("Starting service via systemd...")
      start_result <- tryCatch({
        result <- capture.output(droplet_ssh(droplet, "sudo systemctl start plumber-evolution-mapper && echo 'SUCCESS' || echo 'FAILED'"))
        paste(result, collapse = " ")
      }, error = function(e) {
        prov_log_warn("SSH error during service start, but service may have started:", e$message)
        "MAYBE_SUCCESS"
      })

      if (!grepl("SUCCESS", start_result) && start_result != "MAYBE_SUCCESS") {
        prov_log_warn("Service start command failed, but will verify via HTTP health check...")
      } else {
        prov_log_success("Service start command completed")
      }

      # Brief wait for service to initialize
      prov_log_info("Waiting for service to initialize...")
      Sys.sleep(5)
      
      cat("✅ API service started successfully\n")
    }, error = function(e) {
      stop("❌ Failed to start API service: ", e$message)
    })
    
    # Wait for service to start and verify it's running
    cat("Waiting for service to start...\n")
    Sys.sleep(10)
    
    # Skip systemctl verification due to SSH connection issues
    # Will rely on HTTP health check for verification
    prov_log_info("Skipping systemctl verification due to SSH limitations")
    
    cat("✅ API deployed successfully!\n")
    
    # Configure firewall if IP provided
    if (!is.null(allowed_ip)) {
      configure_firewall(droplet, allowed_ip)
    }
    
    # Verify deployment with strict error handling
    prov_log_info("Performing deployment verification...")
    tryCatch({
      verify_deployment(droplet)  # Now calls stop() internally on failure
      prov_log_success("Deployment verification passed")
    }, error = function(e) {
      stop("❌ Deployment verification failed: ", e$message)
    })
    
    # Check if reverse proxy setup is needed
    domain <- Sys.getenv("DO_DROPLET_DOMAIN")
    if (check_domain_health(domain)) {
      prov_log_info("🔧 Domain not accessible - reverse proxy setup required")
      
      tryCatch({
        # Attempt reverse proxy setup - this will stop() on failure
        reverse_proxy_success <- setup_reverse_proxy(droplet, domain)
        
        if (!reverse_proxy_success) {
          prov_log_error("Reverse proxy setup returned FALSE - configuration failed")
          stop("❌ CRITICAL: Reverse proxy setup failed")
        }
        
        prov_log_info("⏳ Verifying reverse proxy is working...")
        if (!verify_domain_health(domain)) {
          prov_log_error("Domain verification failed after reverse proxy setup")
          prov_log_error("Domain:", domain, "is still not accessible")
          stop("❌ CRITICAL: Reverse proxy setup completed but domain is not accessible")
        }
        
        prov_log_success("Reverse proxy setup and verification successful!")
      }, error = function(e) {
        prov_log_error("Reverse proxy setup failed with error:", e$message)
        stop("❌ CRITICAL: Provisioning failed during reverse proxy setup - ", e$message)
      })
    } else {
      prov_log_info("✅ Domain already accessible - reverse proxy not needed")
    }
    
    # Print summary
    ip_address <- droplet$networks$v4[[1]]$ip_address
    cat("\n🎉 === Deployment Summary ===\n")
    cat("Server IP:", ip_address, "\n")
    
    if (!is.null(domain) && domain != "") {
      cat("Domain: ", domain, "\n")
      cat("API Base URL: https://", domain, "/api/\n", sep = "")
      cat("Documentation: https://", domain, "/__docs__/\n", sep = "")
      cat("Health Check: https://", domain, "/api/health\n", sep = "")
      cat("Fallback IP URL: http://", ip_address, ":8000/api/\n", sep = "")
    } else {
      cat("API Base URL: http://", ip_address, ":8000/api/\n", sep = "")
      cat("Documentation: http://", ip_address, ":8000/__docs__/\n", sep = "")
      cat("Health Check: http://", ip_address, ":8000/api/health\n", sep = "")
    }
    
    if (!is.null(allowed_ip)) {
      cat("🔒 Firewall configured - access restricted to:", allowed_ip, "\n")
    }
    
    cat("\n✅ Provisioning completed successfully!\n")
    
  }, error = function(e) {
    prov_log_error("❌ CRITICAL: Provisioning failed:", e$message)
    prov_log_error("Check the logs above for details")
    stop("❌ PROVISIONING FAILED: ", e$message)
  })
}

# Command line argument parsing (optional)
args <- commandArgs(trailingOnly = TRUE)
droplet_name <- if (length(args) > 0) args[1] else NULL
allowed_ip <- if (length(args) > 1) args[2] else NULL

# Run main provisioning
main(droplet_name, allowed_ip)