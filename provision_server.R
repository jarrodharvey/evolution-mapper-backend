#!/usr/bin/env Rscript

# Enhanced Evolution Mapper Server Provisioning Script
# This script provisions a DigitalOcean droplet with R, required packages, and deploys the API

library(analogsea)

# Configuration
REQUIRED_FILES <- c("plumber.R", "functions/tree_generation.R", "data/species.sqlite")
FIREWALL_NAME <- "evolution-mapper-restricted"

# Load environment variables from .Renviron
readRenviron(".Renviron")

# Get configuration from environment
do_pat <- Sys.getenv("DO_PAT")
if (do_pat == "") {
  stop("DO_PAT not found in .Renviron file. Please add your DigitalOcean API token.")
}

# Set the API token
Sys.setenv(DO_PAT = do_pat)

# Function to validate required files exist
validate_project_files <- function() {
  cat("Validating project files...\n")
  missing_files <- c()
  
  for (file in REQUIRED_FILES) {
    if (!file.exists(file)) {
      missing_files <- c(missing_files, file)
    }
  }
  
  if (length(missing_files) > 0) {
    stop("Missing required files: ", paste(missing_files, collapse = ", "))
  }
  cat("✅ All required project files found\n")
}

# Function to get droplet by name or use first available
get_target_droplet <- function(droplet_name = NULL) {
  cat("Connecting to droplet...\n")
  
  all_droplets <- droplets()
  if (length(all_droplets) == 0) {
    stop("No droplets found in your DigitalOcean account")
  }
  
  if (!is.null(droplet_name)) {
    for (droplet in all_droplets) {
      if (droplet$name == droplet_name) {
        return(droplet)
      }
    }
    stop("Droplet '", droplet_name, "' not found")
  }
  
  # Use first droplet if no name specified
  droplet <- all_droplets[[1]]
  cat("Using droplet:", droplet$name, "at", droplet$networks$v4[[1]]$ip_address, "\n")
  return(droplet)
}

# Function to configure firewall
configure_firewall <- function(droplet, allowed_ip = NULL) {
  if (is.null(allowed_ip)) {
    cat("⚠️  No allowed IP specified - firewall configuration skipped\n")
    return()
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
    
    system(create_cmd)
  }
  
  # Apply firewall to droplet
  apply_cmd <- paste0(
    "doctl compute firewall add-droplets ",
    "$(doctl compute firewall list --format ID,Name --no-header | grep '", FIREWALL_NAME, "' | awk '{print $1}') ",
    "--droplet-ids ", droplet$id
  )
  
  system(apply_cmd)
  cat("✅ Firewall configured and applied\n")
}

# Function to verify deployment success
verify_deployment <- function(droplet) {
  cat("Verifying deployment...\n")
  ip_address <- droplet$networks$v4[[1]]$ip_address
  
  # Wait a bit for service to start
  Sys.sleep(10)
  
  # Test health endpoint
  health_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' 'http://", ip_address, ":8000/api/health'")
  health_code <- system(health_cmd, intern = TRUE)
  
  if (health_code != "200") {
    cat("❌ Health check failed - HTTP", health_code, "\n")
    return(FALSE)
  }
  
  # Test API key endpoint
  api_keys <- strsplit(Sys.getenv("EVOLUTION_API_KEYS"), ",")[[1]]
  if (length(api_keys) > 0) {
    test_cmd <- paste0("curl -s -H 'X-API-Key: ", api_keys[1], "' -o /dev/null -w '%{http_code}' 'http://", ip_address, ":8000/api/species?limit=1'")
    api_code <- system(test_cmd, intern = TRUE)
    
    if (api_code != "200") {
      cat("❌ API key test failed - HTTP", api_code, "\n")
      return(FALSE)
    }
  }
  
  cat("✅ All tests passed - deployment successful\n")
  return(TRUE)
}

# Main provisioning logic
main <- function(droplet_name = NULL, allowed_ip = NULL) {
  cat("🚀 Starting Evolution Mapper API provisioning...\n\n")
  
  # Validate project files
  validate_project_files()
  
  # Get target droplet
  droplet <- get_target_droplet(droplet_name)
  
  tryCatch({
    # Install system dependencies
    cat("Installing system dependencies...\n")
    ubuntu_apt_get_install(droplet, "libcurl4-openssl-dev", "libssl-dev", "libxml2-dev", "libsqlite3-dev", "pandoc")
    
    # Install R packages
    required_packages <- c("plumber", "rlang", "rotl", "ape", "collapsibleTree", "htmlwidgets", "RSQLite", "DBI", "dplyr")
    
    cat("Installing R packages:", paste(required_packages, collapse = ", "), "\n")
    for (pkg in required_packages) {
      cat("Installing", pkg, "...\n")
      tryCatch({
        install_r_package(droplet, pkg)
        cat("✅ Successfully installed", pkg, "\n")
      }, error = function(e) {
        cat("⚠️  Failed to install", pkg, "-", e$message, "\n")
      })
    }
    
    # Deploy the API
    cat("Deploying Evolution Mapper API...\n")
    do_deploy_api(
      droplet = droplet,
      path = "evolution-mapper",
      localPath = ".",
      port = 8000,
      forward = TRUE,
      docs = TRUE
    )
    
    # Fix missing rlang dependency
    cat("Fixing rlang dependency in plumber.R...\n")
    droplet_ssh(droplet, 'cd /var/plumber/evolution-mapper && echo "library(rlang)" > temp_fix.txt && echo "" >> temp_fix.txt && cat plumber.R >> temp_fix.txt && mv temp_fix.txt plumber.R')
    
    # Create run.R script for systemd
    cat("Creating run.R script...\n")
    run_r_content <- paste0(
      "# Environment variables are set by systemd service\n",
      "setwd(\"/var/plumber/evolution-mapper\")\n",
      "library(rlang)\n",
      "library(plumber)\n",
      "pr <- pr(\"plumber.R\")\n",
      "pr$setDocs(TRUE)\n",
      "pr$run(port=8000, host=\"0.0.0.0\")\n"
    )
    
    droplet_ssh(droplet, paste0('cat > /var/plumber/evolution-mapper/run.R << "EOF"\n', run_r_content, 'EOF'))
    
    # Configure environment variables
    cat("Configuring environment variables...\n")
    evolution_api_keys <- Sys.getenv("EVOLUTION_API_KEYS")
    if (evolution_api_keys == "") {
      stop("EVOLUTION_API_KEYS not found in .Renviron. Please set secure API keys before deployment.")
    }
    
    cors_origins <- Sys.getenv("CORS_ALLOWED_ORIGINS", "http://localhost:3000")
    
    # Set up systemd environment
    env_setup_commands <- c(
      "sudo mkdir -p /etc/systemd/system/plumber-evolution-mapper.service.d",
      paste0("sudo bash -c 'cat > /etc/systemd/system/plumber-evolution-mapper.service.d/environment.conf << \"EOF\""),
      "[Service]",
      paste0("Environment=\"EVOLUTION_API_KEYS=", evolution_api_keys, "\""),
      paste0("Environment=\"CORS_ALLOWED_ORIGINS=", cors_origins, "\""),
      "Environment=\"HOME=/var/plumber/evolution-mapper\"",
      "EOF'",
      "sudo systemctl daemon-reload",
      "sudo systemctl restart plumber-evolution-mapper"
    )
    
    for (cmd in env_setup_commands) {
      if (cmd != "" && !grepl("^#", cmd)) {
        droplet_ssh(droplet, cmd)
      }
    }
    
    cat("✅ API deployed successfully!\n")
    
    # Configure firewall if IP provided
    if (!is.null(allowed_ip)) {
      configure_firewall(droplet, allowed_ip)
    }
    
    # Verify deployment
    if (!verify_deployment(droplet)) {
      stop("Deployment verification failed")
    }
    
    # Print summary
    ip_address <- droplet$networks$v4[[1]]$ip_address
    cat("\n🎉 === Deployment Summary ===\n")
    cat("Server IP:", ip_address, "\n")
    cat("API Base URL: http://", ip_address, ":8000/api/\n", sep = "")
    cat("Documentation: http://", ip_address, ":8000/__docs__/\n", sep = "")
    cat("Health Check: http://", ip_address, ":8000/api/health\n", sep = "")
    
    if (!is.null(allowed_ip)) {
      cat("🔒 Firewall configured - access restricted to:", allowed_ip, "\n")
    }
    
    cat("\n✅ Provisioning completed successfully!\n")
    
  }, error = function(e) {
    cat("❌ Error during provisioning:", e$message, "\n")
    cat("Check the logs above for details\n")
    return(FALSE)
  })
}

# Command line argument parsing (optional)
args <- commandArgs(trailingOnly = TRUE)
droplet_name <- if (length(args) > 0) args[1] else NULL
allowed_ip <- if (length(args) > 1) args[2] else NULL

# Run main provisioning
main(droplet_name, allowed_ip)