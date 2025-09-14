# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Essential Commands

### Start Development Server
```r
library(plumber)
pr("plumber.R") %>% pr_run(port = 8000)
```

### Install Dependencies
```r
install.packages(c("plumber", "rlang", "rotl", "ape", "collapsibleTree",
                   "htmlwidgets", "RSQLite", "DBI", "dplyr", "datelife", "httr", "logger"))
```

**System Dependencies (production):**
- `pandoc`, `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libsqlite3-dev`

### Kill Process on Port 8000
```bash
lsof -ti:8000 | xargs kill -9
```

### Code Quality
```r
# Run linter on key files
library(lintr)
lint("plumber.R")
lint("functions/rotl_tree_generation.R")
lint("functions/hybrid_tree_generation.R")
```

### API Testing
```bash
# Health check (no API key required)
curl http://localhost:8000/api/health

# Search species
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/species?search=whale&limit=7"

# Generate hybrid tree (RECOMMENDED - complete coverage + ages where available)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/full-tree-dated

# Generate topology-only tree (fast, no ages)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/tree

# Generate dated tree (limited species coverage)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus" http://localhost:8000/api/dated-tree
```

### API Key Setup
1. Copy `.Renviron.example` to `.Renviron`
2. Edit `.Renviron` with your API keys:
```bash
EVOLUTION_API_KEYS=your-key-1,your-key-2,your-key-3
```

**Important:** API keys in `.Renviron` are real. Example keys like `your-dev-key-123` in documentation are invalid.

### Production Deployment
```r
# Quick file updates
library(analogsea)
droplet <- droplets()[[1]]
droplet_upload(droplet, "functions/tree_generation.R", "/var/plumber/evolution-mapper/functions/tree_generation.R")
droplet_ssh(droplet, "sudo systemctl restart plumber-evolution-mapper")

# Full redeployment
source("provision_server.R")
```

**Important:** Always restart the R server after making code changes for them to take effect.

## Architecture Overview

### Core System Design
This is a **phylogenetic tree generation API** that creates interactive evolutionary trees from species data using three complementary approaches:

1. **Topology Trees** (`/api/tree`): Fast generation using Open Tree of Life - works for any species combination
2. **Dated Trees** (`/api/dated-tree`): Age-calibrated trees using DateLife chronograms - limited species coverage
3. **Hybrid Trees** (`/api/full-tree-dated`): **RECOMMENDED** - Combines ROTL topology with DateLife ages where available

### Key Components
- **plumber.R**: Main API server with CORS, authentication, rate limiting
- **functions/rotl_tree_generation.R**: Topology-only trees via Open Tree of Life
- **functions/datelife_tree_generation.R**: Age-calibrated trees via DateLife chronograms
- **functions/hybrid_tree_generation.R**: **Primary endpoint** - combines topology + ages
- **functions/info_panel_system.R**: Mobile-friendly clickable info panels (replaces tooltips)
- **functions/wikipedia_api.R**: Wikipedia integration for taxonomic context
- **functions/phylopic_silhouettes.R**: Species silhouette images via PhyloPic API
- **data/species.sqlite**: 90,276+ species with OTT IDs, common/scientific names
- **Configuration modules**: logging, caching, parallel processing, colors

### Data Flow Architecture

**Input Requirements:** All tree endpoints require **paired species lists**:
- `common_names`: User-friendly names (e.g., "Human,Dog,Cat")
- `scientific_names`: Scientific names in same order (e.g., "Homo sapiens,Canis lupus,Felis catus")

**Three-Tier Tree Generation:**
1. **Topology-Only** → ROTL API → CollapsibleTree HTML (fast, any species)
2. **Dated Trees** → DateLife chronograms → Age-calibrated trees (limited coverage)
3. **Hybrid Trees** → ROTL topology + DateLife ages + Wikipedia/PhyloPic → Enhanced mobile-friendly trees

**Mobile-First Architecture:**
- **Info Panels**: Clickable panels replace hover tooltips for mobile compatibility
- **Parallel Processing**: Wikipedia and PhyloPic data fetched concurrently for performance
- **Progressive Enhancement**: Core functionality works even when external APIs fail

### Critical System Understanding

**Hybrid Tree System (`/api/full-tree-dated`) is the PRIMARY endpoint:**
- **Always succeeds** - uses ROTL topology as reliable backbone
- **Incorporates age data where available** from DateLife chronograms
- **Mobile-optimized** with clickable info panels instead of tooltips
- **Enhanced content** via parallel Wikipedia/PhyloPic API calls

**DateLife Coverage Limitations:**
- **Extremely limited** species coverage in chronogram database
- Most individual species return 0 chronograms
- Coverage limited to specific taxonomic groups with published molecular clock studies
- **Always implement fallback** to topology-only trees for production use

**Security & Production:**
- **API Key Authentication**: Required for all endpoints except `/api/health`
- **Rate Limiting**: 60 requests per minute per IP
- **Input Validation**: SQL injection protection and parameter sanitization

### Key Implementation Functions

**Primary Tree Generation:**
- `generate_hybrid_tree_html()`: Main endpoint - combines ROTL topology + DateLife ages
- `generate_tree_html()`: Topology-only trees via Open Tree of Life
- `convert_rotl_to_hierarchy()`: Converts ROTL tree to CollapsibleTree format
- `generate_info_panel_html()`: Mobile-friendly clickable info panels

**Database & Enhancement:**
- `search_species()`: SQLite species search with pagination
- `fetch_wikipedia_info()`: Parallel Wikipedia content retrieval
- `fetch_phylopic_silhouettes()`: Parallel PhyloPic image retrieval

### Database Schema
```sql
CREATE TABLE species (
  ott INTEGER,        -- Open Tree of Life ID
  common TEXT,        -- Common name (e.g., "Human")
  scientific TEXT     -- Scientific name (e.g., "Homo sapiens")
);
```
90,276+ records with indexed queries for fast species lookup.

## Development Standards & Key Insights

### Critical Implementation Details
- **Minimum 2 species** required for tree generation
- **Paired input format**: common_names + scientific_names in same order
- Uses `tol_induced_subtree()` from rotl for phylogenetic relationships
- **Info Panel System**: Mobile-friendly clickable panels replace hover tooltips
- **Color Coding**: Deep Purple (root), Blue (ancestors), Orange (taxonomic groups), Green (species)

### Performance Architecture
- **Parallel Processing**: Wikipedia and PhyloPic data fetched concurrently
- **Intelligent Caching**: External API calls cached for performance
- **Progressive Enhancement**: Core functionality works even when external APIs fail
- **Database Optimization**: SQLite with indexed queries for 90k+ species lookup

### Essential Development Practices
- **Never use `cat()` for logging** - use `api_log_info()`, `api_log_warn()`, `api_log_error()`
- **Save test files to `tests/` directory** - never to root
- **Real API keys in `.Renviron`** - example keys in docs are invalid
- **Always restart R server** after code changes for them to take effect

### Testing Approaches
```r
# Test core functions directly
source("functions/hybrid_tree_generation.R")
result <- generate_hybrid_tree_html(c("Human", "Dog"), c("Homo sapiens", "Canis lupus"))

# Test DateLife coverage
library(datelife)
result <- get_datelife_result(input = c("Homo sapiens", "Canis lupus"))
```

```bash
# API integration tests
./tests/test_clupeocephala_api.sh
```