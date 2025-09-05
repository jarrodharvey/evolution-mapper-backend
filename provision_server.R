#!/usr/bin/env Rscript

# Enhanced Evolution Mapper Server Provisioning Script
# This script provisions a DigitalOcean droplet with R, required packages, and deploys the API

library(analogsea)

# Configuration
REQUIRED_FILES <- c(
  "plumber.R", 
  "functions/rotl_tree_generation.R", 
  "functions/datelife_tree_generation.R", 
  "functions/hybrid_tree_generation.R",
  "functions/logging_config.R",
  "functions/progress_tracking.R",
  "functions/parallel_config.R",
  "functions/wikipedia_api.R",
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
  
  if (length(https_code) > 0 && https_code == "200") {
    cat("✅ Domain accessible via HTTPS - reverse proxy not needed\n")
    return(FALSE)
  }
  
  # Test HTTP as fallback
  http_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' --connect-timeout 10 'http://", domain, "/api/health'")
  http_code <- suppressWarnings(system(http_cmd, intern = TRUE, ignore.stderr = TRUE))
  
  if (length(http_code) > 0 && http_code == "200") {
    cat("✅ Domain accessible via HTTP - reverse proxy not needed\n")
    return(FALSE)
  }
  
  cat("❌ Domain not accessible - reverse proxy setup needed\n")
  return(TRUE)
}

# Function to set up reverse proxy with Caddy
setup_reverse_proxy <- function(droplet, domain) {
  if (is.null(domain) || domain == "") {
    cat("⚠️  No domain configured - skipping reverse proxy setup\n")
    return(FALSE)
  }
  
  cat("🔧 Setting up reverse proxy for domain:", domain, "\n")
  
  # Install Caddy via snap
  cat("Installing Caddy...\n")
  tryCatch({
    droplet_ssh(droplet, "sudo snap install caddy --classic")
    cat("✅ Caddy installed successfully\n")
  }, error = function(e) {
    cat("❌ Failed to install Caddy:", e$message, "\n")
    stop("Caddy installation failed")
  })
  
  # Configure Caddy
  cat("Configuring Caddy...\n")
  caddyfile_content <- paste0(
    domain, " {\n",
    "    reverse_proxy localhost:8000\n",
    "}"
  )
  
  tryCatch({
    droplet_ssh(droplet, "sudo mkdir -p /var/snap/caddy/current")
    droplet_ssh(droplet, paste0('sudo tee /var/snap/caddy/current/Caddyfile > /dev/null << "EOF"\n', caddyfile_content, '\nEOF'))
    cat("✅ Caddyfile configured\n")
  }, error = function(e) {
    cat("❌ Failed to configure Caddy:", e$message, "\n")
    stop("Caddy configuration failed")
  })
  
  # Restart Caddy
  cat("Restarting Caddy...\n")
  tryCatch({
    droplet_ssh(droplet, "sudo snap restart caddy")
    cat("✅ Caddy restarted successfully\n")
  }, error = function(e) {
    cat("❌ Failed to restart Caddy:", e$message, "\n")
    stop("Caddy restart failed")
  })
  
  return(TRUE)
}

# Function to verify domain accessibility after reverse proxy setup
verify_domain_health <- function(domain, max_attempts = 6, wait_seconds = 10) {
  if (is.null(domain) || domain == "") {
    return(TRUE)  # Skip if no domain configured
  }
  
  cat("Waiting for reverse proxy to initialize...\n")
  Sys.sleep(30)  # Initial wait for Caddy to fully start
  
  for (attempt in 1:max_attempts) {
    cat("Attempt", attempt, "of", max_attempts, "- Testing domain:", domain, "\n")
    
    # Test HTTPS endpoint (Caddy should auto-provision SSL)
    https_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' --connect-timeout 15 'https://", domain, "/api/health'")
    https_code <- suppressWarnings(system(https_cmd, intern = TRUE, ignore.stderr = TRUE))
    
    if (length(https_code) > 0 && https_code == "200") {
      cat("✅ Domain health check successful via HTTPS\n")
      return(TRUE)
    }
    
    # Test HTTP as fallback
    http_cmd <- paste0("curl -s -o /dev/null -w '%{http_code}' --connect-timeout 15 'http://", domain, "/api/health'")
    http_code <- suppressWarnings(system(http_cmd, intern = TRUE, ignore.stderr = TRUE))
    
    if (length(http_code) > 0 && http_code == "200") {
      cat("✅ Domain health check successful via HTTP\n")
      return(TRUE)
    }
    
    if (attempt < max_attempts) {
      cat("❌ Domain not responding, waiting", wait_seconds, "seconds before retry...\n")
      Sys.sleep(wait_seconds)
    }
  }
  
  cat("❌ Domain health check failed after", max_attempts, "attempts\n")
  return(FALSE)
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
    # Update R to current version first
    cat("Updating R to current version...\n")
    tryCatch({
      droplet_ssh(droplet, "sudo systemctl stop plumber-evolution-mapper || true")
      droplet_ssh(droplet, "sudo apt update")
      if (droplet_ssh(droplet, "echo $?") != "0") {
        stop("Failed to update package lists")
      }
      
      droplet_ssh(droplet, "sudo apt install -y software-properties-common dirmngr")
      if (droplet_ssh(droplet, "echo $?") != "0") {
        stop("Failed to install software-properties-common and dirmngr")
      }
      
      droplet_ssh(droplet, "wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc")
      if (droplet_ssh(droplet, "echo $?") != "0") {
        stop("Failed to add CRAN GPG key")
      }
      
      droplet_ssh(droplet, "echo \"deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/\" | sudo tee /etc/apt/sources.list.d/cran-r.list")
      droplet_ssh(droplet, "sudo apt update")
      if (droplet_ssh(droplet, "echo $?") != "0") {
        stop("Failed to update package lists after adding CRAN repository")
      }
      
      droplet_ssh(droplet, "sudo apt upgrade -y r-base r-base-dev")
      if (droplet_ssh(droplet, "echo $?") != "0") {
        stop("Failed to upgrade R base packages")
      }
      
      cat("✅ R update completed successfully\n")
    }, error = function(e) {
      stop("❌ R update failed: ", e$message)
    })
    
    # Verify R version
    r_version_result <- tryCatch({
      droplet_ssh(droplet, "R --version | head -1")
    }, error = function(e) {
      stop("❌ Failed to verify R version: ", e$message)
    })
    cat("✅ R version verified:", r_version_result, "\n")
    
    # Install system dependencies (including gfortran for Hmisc)
    cat("Installing system dependencies...\n")
    system_deps <- c("libcurl4-openssl-dev", "libssl-dev", "libxml2-dev", "libsqlite3-dev", "pandoc", "librsvg2-dev", "gfortran")
    
    tryCatch({
      for (dep in system_deps) {
        cat("Installing system dependency:", dep, "\n")
        result <- droplet_ssh(droplet, paste0("sudo apt install -y ", dep, " && echo 'SUCCESS' || echo 'FAILED'"))
        if (!grepl("SUCCESS", result)) {
          stop("Failed to install system dependency: ", dep)
        }
      }
      cat("✅ All system dependencies installed successfully\n")
    }, error = function(e) {
      stop("❌ System dependency installation failed: ", e$message)
    })
    
    # Install R packages with systematic verification
    cat("Installing R packages systematically...\n")
    
    # CRAN packages that work with current R
    cran_packages <- c(
      "plumber", "rlang", "rotl", "ape", "collapsibleTree", "htmlwidgets", 
      "RSQLite", "DBI", "dplyr", "colorspace", "jsonlite", "httr", "httr2",
      "logger", "memoise", "cachem", "future", "promises", "remotes",
      "Hmisc", "taxize", "rphylopic"
    )
    
    for (pkg in cran_packages) {
      cat("Checking R package:", pkg, "\n")
      tryCatch({
        # Check if package is already installed and get available version
        check_result <- droplet_ssh(droplet, paste0(
          'R -e "',
          'installed_version <- NULL; ',
          'available_version <- NULL; ',
          'if (require(', pkg, ', quietly=TRUE)) { installed_version <- as.character(packageVersion(\'', pkg, '\')); }; ',
          'available_version <- available.packages(repos=\'https://cloud.r-project.org\')[\'', pkg, '\', \'Version\']; ',
          'if (!is.null(installed_version) && !is.null(available_version) && installed_version == available_version) { ',
          'cat(\'CURRENT\'); ',
          '} else if (!is.null(installed_version)) { ',
          'cat(\'OUTDATED\', installed_version, available_version); ',
          '} else { ',
          'cat(\'MISSING\'); ',
          '}"'
        ))
        
        if (grepl("CURRENT", check_result)) {
          cat("✅ Package", pkg, "is already current - skipping installation\n")
        } else if (grepl("MISSING", check_result)) {
          cat("Installing missing package:", pkg, "\n")
          install_result <- droplet_ssh(droplet, paste0(
            'R -e "install.packages(\'', pkg, '\', repos=\'https://cloud.r-project.org\'); ',
            'if (require(', pkg, ', quietly=TRUE)) { cat(\'SUCCESS\') } else { cat(\'FAILED\') }"'
          ))
          
          if (!grepl("SUCCESS", install_result)) {
            stop("Package installation verification failed")
          }
          
          cat("✅ Successfully installed:", pkg, "\n")
        } else if (grepl("OUTDATED", check_result)) {
          versions <- strsplit(check_result, " ")[[1]]
          cat("Updating package", pkg, "from version", versions[2], "to", versions[3], "\n")
          install_result <- droplet_ssh(droplet, paste0(
            'R -e "install.packages(\'', pkg, '\', repos=\'https://cloud.r-project.org\'); ',
            'if (require(', pkg, ', quietly=TRUE)) { cat(\'SUCCESS\') } else { cat(\'FAILED\') }"'
          ))
          
          if (!grepl("SUCCESS", install_result)) {
            stop("Package update verification failed")
          }
          
          cat("✅ Successfully updated:", pkg, "\n")
        }
      }, error = function(e) {
        stop("❌ Failed to process R package '", pkg, "': ", e$message)
      })
    }
    
    # GitHub packages (removed from CRAN)
    cat("Installing packages from GitHub...\n")
    github_packages <- list(
      list(name = "bold", repo = "ropensci/bold"),
      list(name = "datelife", repo = "phylotastic/datelife")
    )
    
    tryCatch({
      for (pkg_info in github_packages) {
        cat("Checking GitHub package:", pkg_info$name, "\n")
        
        # Check if package is already installed
        check_result <- droplet_ssh(droplet, paste0(
          'R -e "if (require(', pkg_info$name, ', quietly=TRUE)) { cat(\'INSTALLED\') } else { cat(\'MISSING\') }"'
        ))
        
        if (grepl("INSTALLED", check_result)) {
          cat("✅ Package", pkg_info$name, "is already installed - skipping GitHub installation\n")
        } else {
          cat("Installing missing GitHub package:", pkg_info$name, "from", pkg_info$repo, "\n")
          
          github_install_result <- droplet_ssh(droplet, paste0(
            'R -e "library(remotes); install_github(\'', pkg_info$repo, '\'); ',
            'if (require(', pkg_info$name, ', quietly=TRUE)) { cat(\'SUCCESS\') } else { cat(\'FAILED\') }"'
          ))
          
          if (!grepl("SUCCESS", github_install_result)) {
            stop("GitHub package installation verification failed for: ", pkg_info$name)
          }
          
          cat("✅ Successfully installed:", pkg_info$name, "\n")
        }
      }
      cat("✅ All GitHub packages installed successfully\n")
    }, error = function(e) {
      stop("❌ GitHub package installation failed: ", e$message)
    })
    
    # Perform comprehensive package verification
    cat("Performing comprehensive package verification...\n")
    target_packages <- c("datelife", "bold", "taxize", "Hmisc", "rphylopic", "remotes", "plumber", "rlang")
    
    tryCatch({
      for (pkg in target_packages) {
        cat("Verifying package:", pkg, "\n")
        
        # Get detailed package information
        verify_result <- droplet_ssh(droplet, paste0(
          'R -e "if (require(', pkg, ', quietly=TRUE)) { ',
          'version <- as.character(packageVersion(\'', pkg, '\')); ',
          'cat(\'SUCCESS\', version); ',
          '} else { cat(\'FAILED\') }"'
        ))
        
        if (!grepl("SUCCESS", verify_result)) {
          stop("Package verification failed for: ", pkg)
        }
        
        # Extract version from result
        version_info <- strsplit(verify_result, " ")[[1]]
        if (length(version_info) > 1) {
          cat("✅ Verified:", pkg, "version", version_info[2], "\n")
        } else {
          cat("✅ Verified:", pkg, "\n")
        }
      }
      cat("✅ All", length(target_packages), "critical packages verified successfully\n")
    }, error = function(e) {
      stop("❌ Package verification failed: ", e$message)
    })
    
    # Deploy the API with selective file upload
    cat("Deploying Evolution Mapper API...\n")
    
    # Create deployment directory structure
    tryCatch({
      droplet_ssh(droplet, "sudo mkdir -p /var/plumber/evolution-mapper")
      droplet_ssh(droplet, "sudo chown -R plumber:plumber /var/plumber")
      
      # Verify directory creation
      dir_check <- droplet_ssh(droplet, "ls -ld /var/plumber/evolution-mapper")
      if (!grepl("plumber plumber", dir_check)) {
        stop("Failed to create or set permissions for deployment directory")
      }
      
      cat("✅ Deployment directory created successfully\n")
    }, error = function(e) {
      stop("❌ Failed to create deployment directory: ", e$message)
    })
    
    # Upload core files (excluding .claude/, screenshots/, unnecessary files)
    cat("Uploading core application files...\n")
    tryCatch({
      droplet_upload(droplet, "plumber.R", "/tmp/plumber.R")
      droplet_ssh(droplet, "mv /tmp/plumber.R /var/plumber/evolution-mapper/plumber.R")
      
      # Verify file upload
      file_check <- droplet_ssh(droplet, "ls -la /var/plumber/evolution-mapper/plumber.R")
      if (!grepl("plumber.R", file_check)) {
        stop("plumber.R file not found after upload")
      }
      
      cat("✅ plumber.R uploaded successfully\n")
    }, error = function(e) {
      stop("❌ Failed to upload plumber.R: ", e$message)
    })
    
    # Upload functions directory
    tryCatch({
      tar_result <- system("tar -czf /tmp/functions.tar.gz functions/ --exclude='.*' --exclude='*.tmp'")
      if (tar_result != 0) {
        stop("Failed to create functions tar archive")
      }
      
      droplet_upload(droplet, "/tmp/functions.tar.gz", "/tmp/functions.tar.gz")
      droplet_ssh(droplet, "cd /var/plumber/evolution-mapper && tar -xzf /tmp/functions.tar.gz && rm /tmp/functions.tar.gz")
      
      # Verify functions directory exists
      functions_check <- droplet_ssh(droplet, "ls -ld /var/plumber/evolution-mapper/functions")
      if (!grepl("functions", functions_check)) {
        stop("Functions directory not found after upload and extraction")
      }
      
      cat("✅ Functions directory uploaded successfully\n")
    }, error = function(e) {
      stop("❌ Failed to upload functions directory: ", e$message)
    })
    
    # Upload and create data directory with database
    cat("Uploading database...\n")
    tryCatch({
      data_tar_result <- system("tar -czf /tmp/data.tar.gz data/")
      if (data_tar_result != 0) {
        stop("Failed to create data tar archive")
      }
      
      droplet_upload(droplet, "/tmp/data.tar.gz", "/tmp/data.tar.gz")
      droplet_ssh(droplet, "cd /var/plumber/evolution-mapper && tar -xzf /tmp/data.tar.gz && rm /tmp/data.tar.gz")
      
      # Verify database file exists
      db_check <- droplet_ssh(droplet, "ls -la /var/plumber/evolution-mapper/data/species.sqlite")
      if (!grepl("species.sqlite", db_check)) {
        stop("Database file not found after upload")
      }
      
      cat("✅ Database uploaded successfully\n")
    }, error = function(e) {
      stop("❌ Failed to upload database: ", e$message)
    })
    
    # Create progress directory for progress tokens
    cat("Creating progress directory...\n")
    tryCatch({
      droplet_ssh(droplet, "mkdir -p /var/plumber/evolution-mapper/progress")
      droplet_ssh(droplet, "chmod 755 /var/plumber/evolution-mapper/progress")
      
      # Verify progress directory creation
      progress_check <- droplet_ssh(droplet, "ls -ld /var/plumber/evolution-mapper/progress")
      if (!grepl("progress", progress_check)) {
        stop("Progress directory not created successfully")
      }
      
      cat("✅ Progress directory created successfully\n")
    }, error = function(e) {
      stop("❌ Failed to create progress directory: ", e$message)
    })
    
    # Setup systemd service
    cat("Setting up systemd service...\n")
    systemd_service <- '
[Unit]
Description=Plumber Evolution Mapper API
After=network.target

[Service]
Type=simple
User=plumber
WorkingDirectory=/var/plumber/evolution-mapper
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
      service_check <- droplet_ssh(droplet, "ls -la /etc/systemd/system/plumber-evolution-mapper.service")
      if (!grepl("plumber-evolution-mapper.service", service_check)) {
        stop("Systemd service file not created")
      }
      
      droplet_ssh(droplet, "sudo systemctl daemon-reload")
      reload_result <- droplet_ssh(droplet, "echo $?")
      if (reload_result != "0") {
        stop("Failed to reload systemd daemon")
      }
      
      droplet_ssh(droplet, "sudo systemctl enable plumber-evolution-mapper")
      enable_result <- droplet_ssh(droplet, "echo $?")
      if (enable_result != "0") {
        stop("Failed to enable systemd service")
      }
      
      cat("✅ Systemd service configured successfully\n")
    }, error = function(e) {
      stop("❌ Failed to setup systemd service: ", e$message)
    })
    
    # Fix missing rlang dependency
    cat("Fixing rlang dependency in plumber.R...\n")
    droplet_ssh(droplet, 'cd /var/plumber/evolution-mapper && echo "library(rlang)" > temp_fix.txt && echo "" >> temp_fix.txt && cat plumber.R >> temp_fix.txt && mv temp_fix.txt plumber.R')
    
    # Create run.R script for systemd
    cat("Creating run.R script...\n")
    run_r_content <- paste0(
      "# Evolution Mapper API Startup Script\n",
      "# Environment variables are set by systemd service\n",
      "setwd(\"/var/plumber/evolution-mapper\")\n\n",
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
    
    droplet_ssh(droplet, paste0('cat > /var/plumber/evolution-mapper/run.R << "EOF"\n', run_r_content, 'EOF'))
    
    # Upload local .Renviron to server (ensures server matches local configuration)
    cat("Syncing local .Renviron to server...\n")
    if (file.exists(".Renviron")) {
      tryCatch({
        droplet_upload(droplet, ".Renviron", "/tmp/.Renviron")
        droplet_ssh(droplet, "mv /tmp/.Renviron /var/plumber/evolution-mapper/.Renviron")
        
        # Verify .Renviron upload
        renviron_check <- droplet_ssh(droplet, "ls -la /var/plumber/evolution-mapper/.Renviron")
        if (!grepl(".Renviron", renviron_check)) {
          stop(".Renviron file not found after upload")
        }
        
        cat("✅ Local .Renviron synced to server\n")
      }, error = function(e) {
        stop("❌ Failed to upload .Renviron: ", e$message)
      })
    } else {
      stop("❌ No local .Renviron found - this is required for deployment")
    }
    
    # Configure environment variables
    cat("Configuring environment variables...\n")
    evolution_api_keys <- Sys.getenv("EVOLUTION_API_KEYS")
    if (evolution_api_keys == "") {
      stop("❌ EVOLUTION_API_KEYS not found in .Renviron. Please set secure API keys before deployment.")
    }
    
    # Validate API keys format
    if (nchar(evolution_api_keys) < 10) {
      stop("❌ EVOLUTION_API_KEYS appear to be too short or invalid")
    }
    
    cors_origins <- Sys.getenv("CORS_ALLOWED_ORIGINS", "http://localhost:3000")
    if (cors_origins == "") {
      stop("❌ CORS_ALLOWED_ORIGINS cannot be empty")
    }
    
    cat("Configured CORS origins:", cors_origins, "\n")
    cat("Configured API keys:", substr(evolution_api_keys, 1, 10), "...\n")
    
    # Set up systemd environment configuration
    cat("Setting up systemd environment...\n")
    tryCatch({
      droplet_ssh(droplet, "sudo mkdir -p /etc/systemd/system/plumber-evolution-mapper.service.d")
      
      # Verify directory creation
      env_dir_check <- droplet_ssh(droplet, "ls -ld /etc/systemd/system/plumber-evolution-mapper.service.d")
      if (!grepl("plumber-evolution-mapper.service.d", env_dir_check)) {
        stop("Failed to create systemd environment directory")
      }
      
      env_config <- paste0(
        "[Service]\n",
        "Environment=\"EVOLUTION_API_KEYS=", evolution_api_keys, "\"\n",
        "Environment=\"CORS_ALLOWED_ORIGINS=", cors_origins, "\"\n",
        "Environment=\"HOME=/var/plumber/evolution-mapper\"\n"
      )
      
      droplet_ssh(droplet, paste0('sudo tee /etc/systemd/system/plumber-evolution-mapper.service.d/environment.conf > /dev/null << "EOF"\n', env_config, 'EOF'))
      
      # Verify environment config creation
      env_config_check <- droplet_ssh(droplet, "ls -la /etc/systemd/system/plumber-evolution-mapper.service.d/environment.conf")
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
      droplet_ssh(droplet, "sudo systemctl daemon-reload")
      daemon_reload_result <- droplet_ssh(droplet, "echo $?")
      if (daemon_reload_result != "0") {
        stop("Failed to reload systemd daemon")
      }
      
      droplet_ssh(droplet, "sudo systemctl restart plumber-evolution-mapper")
      restart_result <- droplet_ssh(droplet, "echo $?")
      if (restart_result != "0") {
        stop("Failed to restart plumber service")
      }
      
      cat("✅ API service started successfully\n")
    }, error = function(e) {
      stop("❌ Failed to start API service: ", e$message)
    })
    
    # Wait for service to start and verify it's running
    cat("Waiting for service to start...\n")
    Sys.sleep(10)
    
    tryCatch({
      # Check service status
      service_status <- droplet_ssh(droplet, "sudo systemctl is-active plumber-evolution-mapper")
      if (!grepl("active", service_status)) {
        # Get detailed status for debugging
        detailed_status <- droplet_ssh(droplet, "sudo systemctl status plumber-evolution-mapper --no-pager")
        stop("Service is not active. Status: ", service_status, "\nDetailed status: ", detailed_status)
      }
      
      cat("✅ Service is active and running\n")
    }, error = function(e) {
      stop("❌ Service failed to start properly: ", e$message)
    })
    
    cat("✅ API deployed successfully!\n")
    
    # Configure firewall if IP provided
    if (!is.null(allowed_ip)) {
      configure_firewall(droplet, allowed_ip)
    }
    
    # Verify deployment with strict error handling
    cat("Performing deployment verification...\n")
    tryCatch({
      if (!verify_deployment(droplet)) {
        stop("Deployment verification failed - API is not responding correctly")
      }
      cat("✅ Deployment verification passed\n")
    }, error = function(e) {
      stop("❌ Deployment verification failed: ", e$message)
    })
    
    # Check if reverse proxy setup is needed
    domain <- Sys.getenv("DO_DROPLET_DOMAIN")
    if (check_domain_health(domain)) {
      cat("\n🔧 Setting up reverse proxy...\n")
      if (setup_reverse_proxy(droplet, domain)) {
        cat("\n⏳ Verifying reverse proxy setup...\n")
        if (!verify_domain_health(domain)) {
          stop("❌ Reverse proxy setup failed - domain not accessible after configuration")
        }
        cat("✅ Reverse proxy setup successful!\n")
      }
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