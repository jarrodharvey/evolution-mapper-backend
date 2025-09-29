# Evolution Mapper API

A phylogenetic tree generation API built with R Plumber, providing interactive CollapsibleTree visualizations from species data.

## Features

- **Species Database**: 90,276+ unique species with Open Tree of Life IDs
- **Triple Tree Types**: Topology-only trees (fast, any species), dated trees (chronogram ages, limited coverage), and hybrid trees (ROTL topology + DateLife ages where available)
- **Consistent Input Format**: All APIs require paired common and scientific names for accurate species matching
- **Interactive Trees**: Color-coded CollapsibleTree HTML visualizations with mobile-friendly info panels
- **Mobile-First Design**: Info panel system replaces hover tooltips for mobile compatibility
- **Wikipedia Integration**: Contextual information about ancestral taxonomic groups
- **PhyloPic Silhouettes**: Species silhouette images where available
- **Image Override System**: Custom species images via local override system
- **Intelligent Caching**: Multi-tier caching system for Wikipedia, PhyloPic, and info panels
- **REST API**: Clean endpoints for integration with any frontend
- **API Key Authentication**: Secure access control for all endpoints
- **Rate Limiting**: 60 requests per minute per IP address
- **Input Validation**: SQL injection protection and parameter sanitization
- **Graceful Fallback**: Partial coverage detection for seamless user experience
- **Reverse Proxy Support**: Automatic HTTPS setup with Caddy for production domains

## API Endpoints

**Note**: All endpoints except `/api/health` require API key authentication via `X-API-Key` header for security.

### Health Check
```
GET /api/health
```
No authentication required.

### Search Species
```
GET /api/species?search=whale&limit=7
Headers: X-API-Key: your-api-key
```
Search species by name (case-insensitive). Optional parameters:
- `search`: Search term for species names
- `limit`: Max results (default 50, max 100)

### Generate Phylogenetic Tree (Topology Only)
```
POST /api/tree
Headers: X-API-Key: your-api-key
Content-Type: application/x-www-form-urlencoded
Body: common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus
```
Returns interactive CollapsibleTree HTML with topology only (no ages). **Requires both common and scientific names** for accurate species matching and consistent visualization labels.

### Generate Hybrid Phylogenetic Tree (RECOMMENDED)
```
POST /api/full-tree-dated
Headers: X-API-Key: your-api-key
Content-Type: application/x-www-form-urlencoded
Body: common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus
```
Returns hybrid tree combining ROTL topology (complete coverage) with DateLife age data where available. **Always generates complete trees** - no fallback needed. Features mobile-friendly info panels with Wikipedia content and PhyloPic silhouettes.

**Optional Parameters:**
- `expansion_speed`: Animation speed for tree expansion (default: 1000ms)
- `allow_partial_response`: Allow tree generation with subset of species (default: false)
- `as_json`: Return structured JSON data instead of HTML visualization (default: false)

### Generate Dated Phylogenetic Tree
```
POST /api/dated-tree
Headers: X-API-Key: your-api-key
Content-Type: application/x-www-form-urlencoded
Body: common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus
```
Returns age-calibrated tree using DateLife chronogram database. **Requires both common and scientific names** - uses scientific names for chronogram lookup while preserving user-provided common names in visualization.

**Coverage Limitations**: DateLife has extremely limited species coverage. Most species return no chronogram data. Recommend using `/api/full-tree-dated` instead for guaranteed complete trees.

**Partial Response Mode**:
```
Body: common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus&allow_partial_response=true
```
Allows tree generation with subset of species when some lack chronogram data.

### Random Tree (Testing)
```
GET /api/random-tree?count=4
Headers: X-API-Key: your-api-key
```
Generates tree with random species for testing.

### Wikipedia Information
```
GET /api/wikipedia_truncated_intro?taxonomic_group=Mammalia&truncate_length=300
Headers: X-API-Key: your-api-key
```
Returns Wikipedia article introductions for taxonomic groups. Parameters:
- `taxonomic_group` (required): Name of taxonomic group
- `truncate_length` (optional): Max characters (50-1000, default 300)

### Citations
```
GET /api/citations
Headers: X-API-Key: your-api-key
```
Returns citation information for data sources (Open Tree of Life, DateLife, Wikipedia, PhyloPic).

### Image Attributions
```
GET /api/attributions
Headers: X-API-Key: your-api-key
```
Returns attribution information for all images used in the current session, including licensing details.

### Legend Information
```
GET /api/legend
Headers: X-API-Key: your-api-key
```
Returns color coding information for tree visualization nodes.

### Debug Tree (Development)
```
GET /api/debug-tree?count=3
Headers: X-API-Key: your-api-key
```
Debug endpoint for tree generation testing.

## Input Format

### Paired Species Names

Both `/api/tree` and `/api/dated-tree` require **paired inputs**:
- `common_names`: Comma-separated list of user-friendly species names
- `scientific_names`: Comma-separated list of scientific names (same order as common names)

**Benefits of Paired Format**:
- **Consistent Visualization**: User-provided common names appear exactly as specified in the tree
- **Accurate Database Matching**: Precise species identification using both name types
- **Enhanced User Experience**: Predictable output with user-specified naming
- **API Consistency**: Both endpoints handle input identically

**Getting Paired Data**:
Use `/api/species` endpoint to search and obtain both common and scientific names:
```bash
# Search for species to get both common and scientific names
curl -H "X-API-Key: demo-key-12345" "http://localhost:8000/api/species?search=whale&limit=3"
```

## Project Structure

```
backend/
├── plumber.R                        # Main API server with CORS and authentication
├── functions/
│   ├── rotl_tree_generation.R       # Topology-only trees (Open Tree of Life)
│   ├── datelife_tree_generation.R   # Dated trees (DateLife chronograms)
│   ├── hybrid_tree_controller.R     # Main hybrid tree endpoint controller
│   ├── hybrid_tree_conversion.R     # Tree format conversion utilities
│   ├── hybrid_input_validation.R    # Input validation for hybrid trees
│   ├── hybrid_json_output.R         # JSON output format generation
│   ├── hybrid_visualization.R       # Hybrid tree visualization logic
│   ├── info_panel_system.R          # Mobile-friendly info panels
│   ├── tree_html_enhancement.R      # Advanced tree visualization
│   ├── wikipedia_api.R              # Wikipedia integration
│   ├── phylopic_silhouettes.R       # PhyloPic integration
│   ├── wikimedia_images.R           # Wikimedia Commons integration
│   ├── wikipedia_images.R           # Wikipedia image handling
│   ├── attribution_extractor.R      # Image attribution system
│   ├── cached_api_functions.R       # Cached API wrappers
│   ├── color_config.R               # Centralized color schemes
│   ├── logging_config.R             # Centralized logging
│   ├── progress_tracking.R          # Progress tracking for long operations
│   ├── parallel_config.R            # Parallel processing configuration
│   ├── caching_config.R             # Multi-tier caching system
│   ├── datelife_efficiency.R       # DateLife optimization utilities
│   ├── modern_age_mapping.R         # Age data mapping system
│   └── show_function_relationships.R # Development utility for function analysis
├── data/
│   └── species.sqlite               # Species database (90k+ records)
├── cache/                           # Multi-tier cache storage
│   ├── info_panels/                 # Info panel cache
│   ├── wikipedia/                   # Wikipedia content cache
│   └── phylopic/                    # PhyloPic image cache
├── image_overrides/                 # Custom species images
├── logs/                            # API and system logs
├── progress/                        # Progress tracking tokens
├── tests/                           # Test scripts and HTML files
├── sh/                              # Utility shell scripts
│   ├── restart_server.sh            # Server restart script
│   ├── clear_cache.sh               # Cache management
│   ├── logs.sh                      # Log viewing
│   └── generate_random.sh           # Test data generation
├── provision_server.R               # Enhanced DigitalOcean deployment
├── .Renviron.example                # Environment configuration template
├── CLAUDE.md                        # Claude Code development guide
└── README.md
```

## Dependencies

### R Packages

**Core Dependencies:**
- `plumber` - API framework
- `rlang` - Required for %||% operator
- `rotl` - Open Tree of Life integration
- `datelife` - Chronogram database access for dated trees
- `ape` - Phylogenetic tree handling
- `collapsibleTree` - Interactive tree visualization
- `htmlwidgets` - Widget framework
- `RSQLite`, `DBI` - Database access
- `dplyr` - Data manipulation
- `httr`, `httr2` - HTTP clients for external APIs
- `logger` - Centralized logging system

**Performance & Caching:**
- `memoise` - Function memoization
- `cachem` - Cache management
- `future`, `promises` - Parallel processing
- `remotes` - Package installation utilities

**Phylogenetic & Taxonomic:**
- `bold` - Barcode of Life integration
- `taxize` - Taxonomic data access
- `rphylopic` - PhyloPic silhouette integration
- `phylobase` - Phylogenetic utilities
- `Hmisc` - Statistical utilities

**Data Processing:**
- `jsonlite` - JSON handling
- `colorspace` - Color manipulation

### System Dependencies (Production)
- `pandoc` - Document conversion
- `libcurl4-openssl-dev` - HTTP client library
- `libssl-dev` - SSL/TLS library
- `libxml2-dev` - XML parsing
- `libsqlite3-dev` - SQLite database
- `librsvg2-dev` - SVG rendering
- `gfortran` - Fortran compiler
- `libsodium-dev` - Cryptographic library

## Local Development

### Setup Environment Configuration

1. Copy `.Renviron.example` to `.Renviron`
2. Configure environment variables:
```bash
# .Renviron
EVOLUTION_API_KEYS=your-key-1,your-key-2,your-key-3
CORS_ALLOWED_ORIGINS=http://localhost:3000,https://your-domain.com
DO_PAT=your-digitalocean-api-token
DO_DROPLET_IP=your-droplet-ip-address
DO_DROPLET_DOMAIN=your-domain.com
```

**Important**: API keys in `.Renviron` are real. Example keys in documentation are invalid and will not work.

### Fix SSL Issues (macOS)

If you encounter SSL connection errors like "SSL connect error: Connection reset by peer" after updating R packages, the R curl package may need to be recompiled against system libraries:

```r
# In R console:
options(repos = c(CRAN = 'https://cloud.r-project.org/'))

# Remove existing curl package
remove.packages('curl')

# Set environment variables to force system library usage
Sys.setenv(
  'INCLUDE_DIR' = '/opt/homebrew/include',  # For Apple Silicon Macs
  'LIB_DIR' = '/opt/homebrew/lib',          # For Intel Macs use /usr/local/...
  'AUTOBREW' = '0',
  'FORCE_AUTOBREW' = '0',
  'DISABLE_AUTOBREW' = '1'
)

# Reinstall curl from source with system libraries
install.packages('curl', type = 'source', 
                 configure.args = '--with-curl-config=/opt/homebrew/bin/curl-config')

# Test the fix
library(httr)
GET('https://en.wikipedia.org/api/rest_v1/page/summary/test')  # Should return 200
```

**Requirements:**
- Homebrew curl and OpenSSL: `brew install curl openssl`
- Look for "Found INCLUDE_DIR and/or LIB_DIR!" and "Using PKG_LIBS=-L/opt/homebrew/lib -lcurl" in output

This ensures R curl uses the same SSL libraries as system curl, fixing compatibility issues.

### Start Server

```bash
# Quick start using restart script (recommended)
sh/restart_server.sh

# Manual start
R -e "library(plumber); pr('plumber.R') %>% pr_run(port = 8000)"
```

```r
# Install all dependencies (run once)
install.packages(c("plumber", "rlang", "rotl", "ape", "collapsibleTree",
                   "htmlwidgets", "RSQLite", "DBI", "dplyr", "datelife",
                   "httr", "httr2", "logger", "memoise", "cachem", "future",
                   "promises", "remotes", "Hmisc", "taxize", "rphylopic",
                   "phylobase", "jsonlite", "colorspace"))

# Install GitHub packages
remotes::install_github("ropensci/bold")
remotes::install_github("phylotastic/datelife")

# Manual server start
library(plumber)
pr("plumber.R") %>% pr_run(port = 8000)
```

## Testing

```bash
# Health check (no API key required)
curl http://localhost:8000/api/health

# Search species with API key in header
curl -H "X-API-Key: demo-key-12345" "http://localhost:8000/api/species?search=whale&limit=7"

# Legend information
curl -H "X-API-Key: demo-key-12345" "http://localhost:8000/api/legend"

# Generate topology tree (paired names required)
curl -X POST -H "X-API-Key: demo-key-12345" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/tree

# Generate hybrid tree (RECOMMENDED - complete coverage + ages where available)
curl -X POST -H "X-API-Key: demo-key-12345" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/full-tree-dated

# Generate hybrid tree with JSON output
curl -X POST -H "X-API-Key: demo-key-12345" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus&as_json=true" http://localhost:8000/api/full-tree-dated

# Generate dated tree (paired names required)
curl -X POST -H "X-API-Key: demo-key-12345" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus" http://localhost:8000/api/dated-tree

# Dated tree with partial response allowed
curl -X POST -H "X-API-Key: demo-key-12345" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus&allow_partial_response=true" http://localhost:8000/api/dated-tree

# Random tree
curl -H "X-API-Key: demo-key-12345" "http://localhost:8000/api/random-tree?count=3"

# Generate random tree and save to HTML file for viewing
curl -H "X-API-Key: demo-key-12345" "http://localhost:8000/api/random-tree?count=5" | jq -r '.html[0]' > random_tree.html
```

## Production Deployment

### Enhanced DigitalOcean Deployment

Use the enhanced provisioning script for robust deployment:

```bash
# Deploy to first available droplet with firewall protection
Rscript provision_server.R "" "YOUR_IP_ADDRESS"

# Deploy to specific droplet
Rscript provision_server.R "droplet-name" "SOURCE_IP_ADDRESS"
```

**Prerequisites:**
1. DigitalOcean API token in `.Renviron` as `DO_PAT`
2. API keys configured in `.Renviron` as `EVOLUTION_API_KEYS`
3. Optional: Domain configuration for automatic HTTPS setup
4. R packages: `analogsea`, `logger`

**Enhanced Features:**
- **Intelligent Package Management**: Skips reinstallation if packages already exist
- **Ubuntu Version Detection**: Automatically selects correct CRAN repository
- **Complete Directory Structure**: Creates all required cache, log, and override directories
- **Reverse Proxy Setup**: Automatic HTTPS with Caddy for configured domains
- **Robust Error Handling**: Comprehensive validation and rollback capabilities
- **System Dependencies**: Automatically installs all required Ubuntu packages
- **Service Management**: Systemd service with automatic directory recreation

The script automatically:
- Updates R to current version from CRAN
- Installs all system and R dependencies
- Creates complete directory structure with proper permissions
- Deploys API with systemd service
- Configures secure firewall (SSH + API access only)
- Sets up reverse proxy with automatic HTTPS (if domain configured)
- Verifies deployment with comprehensive health checks

### Production API Access

**Server**: `DROPLET_ADDRESS:8000`

```bash
# Health check
curl "http://DROPLET_ADDRESS:8000/api/health"

# Search species
curl -H "X-API-Key: demo-key-12345" "http://DROPLET_ADDRESS:8000/api/species?search=human&limit=3"

# Generate hybrid tree (recommended)
curl -X POST -H "X-API-Key: demo-key-12345" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus" "http://DROPLET_ADDRESS:8000/api/full-tree-dated"

# Generate topology tree (fast, no ages)
curl -X POST -H "X-API-Key: demo-key-12345" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus" "http://DROPLET_ADDRESS:8000/api/tree"
```

### Manual Deployment

Alternative deployment options include `plumberDeploy` or Docker containerization.

## Color Coding

- **Deep Purple (#8E44AD)**: Root node ("Common ancestor - click me!")
- **Blue (#3498DB)**: Unnamed evolutionary ancestors  
- **Orange (#F39C12)**: Named taxonomic groups
- **Green (#27AE60)**: Species (leaf nodes)

## Recent Major Features

### Enhanced Deployment System (Latest)
- **Intelligent Package Management**: Optimized deployment with package existence checking
- **Ubuntu Version Detection**: Automatic CRAN repository selection for all Ubuntu versions
- **Complete Directory Structure**: Automated creation of cache, logs, and override directories
- **Reverse Proxy Integration**: Automatic HTTPS setup with Caddy for production domains
- **Enhanced Error Handling**: Comprehensive validation and recovery mechanisms

### Hybrid Tree System
The `/api/full-tree-dated` endpoint provides the best of both worlds:
- **Complete Coverage**: Always generates full trees using ROTL topology
- **Age Information**: Incorporates DateLife chronogram data where available
- **Mobile-First**: Info panel system replaces hover tooltips
- **Rich Content**: Wikipedia content and PhyloPic silhouettes
- **Image Override System**: Custom species images with automatic fallback
- **No Fallback Needed**: Unlike pure DateLife approach, always succeeds

### Multi-Tier Caching System
- **Wikipedia Cache**: 7-day cache for taxonomic content (200MB)
- **PhyloPic Cache**: 30-day cache for species silhouettes (300MB)
- **Info Panel Cache**: 24-hour cache for complete panel data (500MB)
- **Intelligent Cache Management**: Automatic size and age-based eviction

### Performance Optimizations
- **Parallel Processing**: Wikipedia and PhyloPic data fetched concurrently
- **Optimized Package Installation**: Skip reinstallation when packages exist
- **Progress Tracking**: Server-sent events for long-running operations
- **Centralized Logging**: Structured logging with file and console output
- **Database Optimization**: Indexed species lookup for 90k+ records

### Image Attribution System
- **Comprehensive Tracking**: License and attribution data for all images
- **Multiple Sources**: PhyloPic, Wikimedia Commons, Wikipedia integration
- **License Compliance**: Automatic attribution extraction and display
- **Override Support**: Custom image system with proper attribution tracking

### Security & Production Ready
- **API Key Authentication**: Secure access control
- **Rate Limiting**: 60 requests per minute per IP
- **Input Validation**: SQL injection protection
- **Automated HTTPS**: Reverse proxy with automatic SSL certificate provisioning
- **Enhanced Deployment**: One-command DigitalOcean deployment with domain support
- **Firewall Configuration**: Restrictive access control for production environments