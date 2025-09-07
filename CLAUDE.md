# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Start Development Server
```r
library(plumber)
pr("plumber.R") %>% pr_run(port = 8000)
```

### Install Dependencies

**R Packages:**
```r
install.packages(c("plumber", "rlang", "rotl", "ape", "collapsibleTree", 
                   "htmlwidgets", "RSQLite", "DBI", "dplyr", "datelife", "httr", "logger"))
```

**System Dependencies (for production):**
- `pandoc` - Required for HTML widget generation
- `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libsqlite3-dev` - R package compilation

### Production Security Features
- **API Key Authentication**: Required for all endpoints except health check
- **Rate Limiting**: 60 requests per minute per IP address
- **Input Validation**: SQL injection protection and parameter sanitization
- **Error Handling**: Structured error responses without exposing internal details

### Testing Endpoints
```bash
# Health check (no API key required)
curl http://localhost:8000/api/health

# Search species with API key in header
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/species?search=whale&limit=7"

# Generate hybrid tree (RECOMMENDED - combines ROTL topology + DateLife ages)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/full-tree-dated

# Generate topology-only tree (requires both common and scientific names)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/tree

# Generate dated tree with chronogram ages (requires both common and scientific names)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus" http://localhost:8000/api/dated-tree

# Generate partial tree when some species missing (allow_partial_response=true)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus&allow_partial_response=true" http://localhost:8000/api/dated-tree

# Random tree
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/random-tree?count=3"

# Get legend information for tree colors
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/legend"

# Get Wikipedia information for taxonomic groups
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/wikipedia_truncated_intro?taxonomic_group=Mammalia"

# Get Wikipedia info with custom truncation length
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/wikipedia_truncated_intro?taxonomic_group=Primates&truncate_length=200"

# Get citations for data sources
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/citations"
```

### API Key Configuration

**Setup (.Renviron file):**
1. Copy `.Renviron.example` to `.Renviron`
2. Edit `.Renviron` with your API keys:
```bash
# .Renviron
EVOLUTION_API_KEYS=your-key-1,your-key-2,your-key-3
```

**Development Keys:**
Configure your own secure API keys in `.Renviron`. For development, you can use keys like:
- `your-dev-key-123`
- `your-test-key-456`
- `your-api-key-789`

**Security Notes:**
- `.Renviron` is excluded from version control (.gitignore)
- Use `.Renviron.example` as a template for new deployments
- For production: generate secure, unique API keys

### Deploying Changes to Production

**For code changes (preferred method):**
```r
# Quick deployment of specific file changes
library(analogsea)
droplet <- droplets()[[1]]  # or specify your droplet

# Upload updated files
droplet_upload(droplet, 
               local = "functions/tree_generation.R", 
               remote = "/var/plumber/evolution-mapper/functions/tree_generation.R")

# Restart the service
droplet_ssh(droplet, "sudo systemctl restart plumber-evolution-mapper")
```

**For full redeployment (slower, use only when needed):**
```r
source("provision_server.R")
```

### Code Quality
```r
# Install and run linter
install.packages("lintr")
library(lintr)
lint("plumber.R")
lint("functions/rotl_tree_generation.R")
lint("functions/hybrid_tree_generation.R")
```

### Server Restart Required
When making changes to any R files, restart the server for changes to take effect:
```r
# Stop current server (Ctrl+C), then restart
library(plumber)
pr("plumber.R") %>% pr_run(port = 8000)
```

### Kill Process on Port 8000
```bash
# If port 8000 is occupied
lsof -ti:8000 | xargs kill -9
```

## Architecture Overview

### Core Components
- **plumber.R**: Main API server with comprehensive REST endpoints and CORS configuration
- **functions/rotl_tree_generation.R**: Core phylogenetic tree logic using Open Tree of Life (topology only)
- **functions/datelife_tree_generation.R**: Dated tree generation using DateLife chronograms with ancestor ages
- **functions/hybrid_tree_generation.R**: Hybrid trees combining ROTL topology with DateLife age data
- **functions/info_panel_system.R**: Mobile-friendly info panels with Wikipedia/PhyloPic integration
- **functions/tree_html_enhancement.R**: Advanced tree visualization customization
- **functions/wikipedia_api.R**: Wikipedia integration for taxonomic information
- **functions/phylopic_silhouettes.R**: PhyloPic integration for species silhouettes
- **functions/color_config.R**: Centralized color scheme configuration
- **functions/logging_config.R**: Centralized logging configuration using logger package
- **functions/progress_tracking.R**: Progress tracking for long-running operations
- **functions/parallel_config.R**: Parallel processing configuration
- **functions/caching_config.R**: Caching configuration for external API calls
- **functions/cached_api_functions.R**: Cached implementations of external API calls
- **functions/datelife_efficiency.R**: Optimized DateLife operations
- **functions/modern_age_mapping.R**: Age mapping and geological period utilities
- **data/species.sqlite**: Species database (90,276+ records with OTT IDs, common names, scientific names)
- **logs/**: Log file directory for API operations

### API Features Overview
- **Topology Trees**: Fast generation using common names, works for any species combination
- **Dated Trees**: Age-calibrated trees using scientific names, limited to species with chronogram data
- **Hybrid Trees**: Best of both worlds, ROTL topology with DateLife ages where available
- **Info Panel System**: Mobile-friendly clickable info panels replacing tooltips
- **Parallel Processing**: Performance-optimized data fetching for Wikipedia and PhyloPic content
- **Progress Tracking**: Server-sent events for real-time progress updates on long-running operations
- **Caching System**: Intelligent caching of external API calls (Wikipedia, PhyloPic, DateLife)
- **Logging Infrastructure**: Centralized logging using logger package with file and console output
- **Fallback Strategy**: Frontend can attempt dated trees first, fall back to topology trees
- **Interactive Visualization**: Color-coded CollapsibleTree with age tooltips, geological periods, and species silhouettes

### API Endpoints
- **GET /api/health**: Health check endpoint (no API key required)
- **GET /api/species?search=term&limit=N**: Search species by name (case-insensitive, default limit 50, max 100)
- **GET /api/random-species?count=N**: Get random species for testing
- **POST /api/tree**: Generate topology-only phylogenetic tree from paired species lists (common + scientific names)
- **POST /api/dated-tree**: Generate dated phylogenetic tree with chronogram ages (common + scientific names required, limited coverage)
- **POST /api/full-tree-dated**: Generate hybrid tree combining ROTL topology with DateLife age data where available
- **GET /api/random-tree?count=N**: Generate random tree for testing (topology only)
- **GET /api/legend**: Get legend information for tree visualization colors and node types
- **GET /api/wikipedia_truncated_intro?taxonomic_group=name&truncate_length=N**: Get truncated Wikipedia introduction for taxonomic groups
- **GET /api/citations**: Get citation information for data sources
- **GET /api/debug-tree?count=N**: Debug endpoint for tree generation testing

### Hybrid Tree API (/api/full-tree-dated)
**NEWEST FEATURE**: The hybrid tree system provides the best of both worlds - complete species coverage from ROTL with age information from DateLife where available.

**Key Advantages:**
- Always generates a complete tree (no missing species like pure DateLife approach)
- Incorporates age data where available from DateLife chronogram database
- Uses ROTL topology as the reliable backbone for all species relationships
- Provides graceful degradation when age data is unavailable
- Enhanced mobile-friendly visualization with info panels instead of tooltips

**Usage Pattern:**
```bash
# Generate hybrid tree with info panels and age data where available
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/full-tree-dated
```

**Enhanced Features:**
- **Info Panel System**: Clickable info icons instead of hover tooltips (mobile-friendly)
- **Wikipedia Integration**: Contextual information about ancestral taxonomic groups  
- **PhyloPic Silhouettes**: Species silhouette images where available
- **Parallel Processing**: Performance-optimized data fetching for external APIs
- **Age Visualization**: Ancestor ages displayed in millions of years (Mya) with geological context
- **Smart Layout**: Dynamic link lengths optimized for age information display

### Dated Tree API (/api/dated-tree)
**MAJOR FEATURE**: Added in current session - provides age-calibrated phylogenetic trees using DateLife chronogram database.

**Usage Pattern:**
```bash
# Generate hybrid tree (ROTL topology + DateLife ages where available)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/full-tree-dated

# Generate dated tree with chronogram ages (requires scientific names)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus" http://localhost:8000/api/dated-tree

# Allow partial trees when some species missing from chronogram data
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus&allow_partial_response=true" http://localhost:8000/api/dated-tree

# Get random species for testing
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/random-species?count=5"
```

**Key Features:**
- Uses DateLife R package to query chronogram database
- Requires both common and scientific names (paired inputs)
- Returns ancestor ages in millions of years (Mya)
- Includes geological period information in tooltips
- Handles partial coverage gracefully with detailed error responses
- Frontend can detect partial coverage and fall back to topology trees
- Preserves user-provided common names in tree visualization

**Response Types:**
- **Complete Coverage**: Full dated tree with all species
- **Partial Coverage**: Error response with covered/missing species lists
- **No Coverage**: Error response suggesting fallback to /api/tree

**Coverage Limitations:**
- DateLife has extremely limited species coverage
- Most individual species return 0 chronograms
- Coverage mainly limited to specific taxonomic groups with published molecular clock studies
- Recommended to always implement fallback to topology-only trees

### Key Functions

**Core Tree Generation:**
- `convert_rotl_to_hierarchy()`: Converts phylogenetic tree from Open Tree of Life to hierarchical structure
- `generate_tree_html()`: Creates CollapsibleTree HTML visualization with color coding
- `generate_hybrid_tree_html()`: NEW - Creates hybrid trees with ROTL topology + DateLife ages
- `search_species()`: Searches SQLite database for species with optional search term and limit
- `get_species_from_db()`: Queries SQLite database for species data by common name
- `trace_path_to_root()`: Walks tree structure from species to root ancestor

**Enhancement Functions:**
- `generate_info_panel_html()`: Creates mobile-friendly info panels for ancestor nodes
- `fetch_wikipedia_info()`: Retrieves Wikipedia content for taxonomic groups
- `fetch_phylopic_silhouettes()`: Gets species silhouette images from PhyloPic API
- `calculate_dynamic_link_length_hybrid()`: Optimizes tree layout for hybrid visualizations
- `clean_scientific_names()`: Sanitizes scientific names for database queries

### Data Flow

**Topology-Only Trees (/api/tree):**
1. API receives paired species lists (common + scientific names)
2. Database lookup using paired names to get accurate OTT IDs
3. rotl library fetches phylogenetic tree from Open Tree of Life  
4. Tree converted to hierarchical structure preserving user-provided common names
5. CollapsibleTree generates interactive HTML visualization
6. Color-coded nodes: Deep Purple (root), Blue (unnamed ancestors), Orange (taxonomic groups), Green (species)

**Dated Trees (/api/dated-tree):**
1. API receives paired species lists (common + scientific names)
2. DateLife searches chronogram database using scientific names for published age data
3. If partial coverage: returns JSON with missing species list for frontend handling
4. If full coverage: generates median consensus matrix and phylo tree with ages
5. CollapsibleTree generates interactive HTML with age information in tooltips, preserving user-provided common names
6. Frontend falls back to /api/tree if DateLife coverage insufficient

**Hybrid Trees (/api/full-tree-dated):**
1. API receives paired species lists (common + scientific names)
2. ROTL provides complete phylogenetic topology for all species (always succeeds)
3. DateLife searches chronogram database in parallel for age data where available
4. Hybrid tree merges ROTL structure with DateLife ages using median consensus
5. Info panel system generates mobile-friendly clickable panels with:
   - Wikipedia content fetched in parallel for taxonomic groups
   - PhyloPic silhouettes fetched in parallel for species
   - Age information with geological context where available
6. CollapsibleTree generates enhanced HTML with info panels, preserving user-provided common names
7. Always returns complete tree (no fallback needed - major advantage over pure DateLife approach)

### Database Schema
```sql
CREATE TABLE species (
  ott INTEGER,        -- Open Tree of Life ID
  common TEXT,        -- Common name (e.g., "Human")  
  scientific TEXT     -- Scientific name (e.g., "Homo sapiens")
);
```

## Development Notes

### Performance Considerations
- **Parallel Processing**: Wikipedia and PhyloPic data fetching uses parallel processing for improved performance
- **Database Optimization**: SQLite database with indexed queries for fast species lookup
- **External API Limits**: Rate-limited calls to Wikipedia and PhyloPic APIs to avoid service disruption
- **Memory Management**: Large phylogenetic trees are processed efficiently with streaming where possible
- **Error Handling**: Robust error recovery for external API failures (Wikipedia, PhyloPic, DateLife)

### Mobile-First Design
- **Info Panels**: Replaces hover-based tooltips with clickable info panels for mobile compatibility
- **Responsive Layout**: CollapsibleTree visualizations adapt to different screen sizes
- **Touch-Friendly**: Info panel system designed for touch interfaces
- **Progressive Enhancement**: Trees work with basic functionality even when external APIs fail

### Tree Generation Logic
- Minimum 2 species required for tree generation
- Uses `tol_induced_subtree()` from rotl to get phylogenetic relationships
- Handles missing OTT IDs gracefully (returns error for insufficient valid species)
- Converts scientific names to readable ancestor labels via `convert_to_readable_name()`

### API Response Format
All endpoints return JSON with `success` boolean and either `error` message or relevant data fields.

### Color Coding System
- **Deep Purple (#8E44AD)**: Root node ("Common ancestor - click me!")
- **Blue (#3498DB)**: Unnamed evolutionary ancestors  
- **Orange (#F39C12)**: Named taxonomic groups (families, orders, etc.)
- **Green (#27AE60)**: Species (leaf nodes)
- **Color variations**: Enhanced with opacity and gradients in hybrid trees for age visualization

### Wikipedia API Documentation
**NEW ENDPOINT**: `/api/wikipedia_truncated_intro` provides Wikipedia article introductions for taxonomic groups.

**Purpose**: Gives frontend applications contextual information about ancestral taxonomic groups in phylogenetic trees.

**Parameters:**
- `taxonomic_group` (required): Name of the taxonomic group (e.g., "Mammalia", "Primates", "Canidae")
- `truncate_length` (optional): Maximum introduction length in characters (50-1000, default 300)

**Features:**
- Searches Wikipedia using REST API for accurate article matching
- Intelligent text truncation at sentence boundaries when possible
- Returns Wikipedia URL for full article access
- Handles both scientific and common taxonomic names
- Comprehensive error handling for missing articles

**Response Format:**
- **Success**: `taxonomic_group`, `wikipedia_title`, `introduction`, `url`, `page_id`, `truncated`
- **Error**: `success: false`, `error` message, `taxonomic_group`

**Example Usage:**
```bash
# Basic usage
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/wikipedia_truncated_intro?taxonomic_group=Mammalia"

# With custom length
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/wikipedia_truncated_intro?taxonomic_group=Primates&truncate_length=200"
```

### DateLife Coverage Limitations
**IMPORTANT**: DateLife has extremely limited coverage in the current chronogram database:
- Most individual species return 0 chronograms when queried alone
- Coverage appears to be limited to specific taxonomic groups with published molecular clock studies
- Even common model organisms (Human, Dog, Cat, Mouse) often lack individual coverage
- Some species pairs may have data when queried together due to shared studies

**Recommended Usage Pattern:**
1. Frontend attempts `/api/dated-tree` with scientific names
2. If response includes `missing_species`, display informative message to user
3. Frontend falls back to `/api/tree` for topology-only visualization
4. User gets clear feedback about why age data is unavailable

**Testing DateLife Coverage:**
```r
library(datelife)
# Test if species have chronogram data
result <- get_datelife_result(input = c("Homo sapiens", "Canis lupus"))
cat("Chronograms found:", length(result))
```

### Testing Infrastructure
Manual testing via curl commands and shell scripts in `tests/` directory. Key test files:

```bash
# Run API integration tests
./tests/test_clupeocephala_api.sh

# Shell test runner
./sh/test.sh
```

```r
# Test topology-only trees
source("functions/rotl_tree_generation.R")
test_species <- c("Human", "Dog", "Cat")
result <- generate_tree_html(test_species)

# Test dated trees (limited coverage)
source("functions/datelife_tree_generation.R")
test_species_sci <- c("Homo sapiens", "Canis lupus")
datelife_result <- get_datelife_result(input = test_species_sci)

# Test hybrid trees
source("functions/hybrid_tree_generation.R")
common_names <- c("Human", "Dog", "Cat")
scientific_names <- c("Homo sapiens", "Canis lupus", "Felis catus")
hybrid_result <- generate_hybrid_tree_html(common_names, scientific_names)

# Performance testing
source("tests/cache_performance_logging.R")  # Benchmarks caching improvements
source("tests/parallel_side_effects_investigation.R")  # Parallel processing tests
```

**Test Organization:**
- Save all test files to `tests/` directory (never to root)
- API integration tests use shell scripts with curl
- Function-level tests use R scripts
- Performance benchmarks in dedicated test files

### Logging Standards
- **Never use `cat()`** for logging - use the project's logger implementation
- Use functions from `functions/logging_config.R`: `api_log_info()`, `api_log_warn()`, `api_log_error()`
- Logs are written to `logs/api.log` and console
- Logger uses namespace `evolution.api` for consistent formatting

### API Key Management
- **Never hardcode API keys** - always use environment variables
- API keys stored in `.Renviron` (excluded from git)
- Use `.Renviron.example` as template for setup
- Example keys in documentation (like `your-dev-key-123`) are invalid and will not work

### Environment Configuration
The `.Renviron` file contains several important configuration options:
- `EVOLUTION_API_KEYS`: Comma-separated list of valid API keys
- `CORS_ALLOWED_ORIGINS`: Allowed origins for CORS (development and production URLs)
- `DO_PAT`: DigitalOcean API token for server provisioning
- `DO_DROPLET_IP`: Production server IP address
- `DO_DROPLET_DOMAIN`: Optional domain for HTTPS setup

### Development Best Practices
- **Restart Required**: Always restart the R server after making code changes
- **Port Management**: Use `lsof -ti:8000 | xargs kill -9` to kill processes on port 8000
- **Logging**: Use `api_log_info()`, `api_log_warn()`, `api_log_error()` functions instead of `cat()`
- **Test Organization**: Save all test files to `tests/` directory, never to root
- **API Key Security**: Real API keys are in `.Renviron`, example keys in docs will not work