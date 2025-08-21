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
                   "htmlwidgets", "RSQLite", "DBI", "dplyr", "datelife"))
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

# Generate topology-only tree (no ages, uses common names)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "species=Human,Dog,Cat" http://localhost:8000/api/tree

# Generate dated tree with chronogram ages (requires scientific names, limited coverage)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "species=Homo sapiens,Canis lupus" http://localhost:8000/api/dated-tree

# Generate partial tree when some species missing (allow_partial_response=true)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "species=Homo sapiens,Canis lupus,Felis catus&allow_partial_response=true" http://localhost:8000/api/dated-tree

# Random tree
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/random-tree?count=3"

# Get legend information for tree colors
curl -H "X-API-Key: YOUR-API-KEY" "http://localhost:8000/api/legend"
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
lint("functions/tree_generation.R")
```

## Architecture Overview

### Core Components
- **plumber.R**: Main API server with REST endpoints (`/api/health`, `/api/legend`, `/api/species`, `/api/tree`, `/api/dated-tree`, `/api/random-tree`)
- **functions/rotl_tree_generation.R**: Core phylogenetic tree logic using Open Tree of Life (topology only)
- **functions/datelife_tree_generation.R**: Dated tree generation using DateLife chronograms with ancestor ages
- **data/species.sqlite**: Species database (90,276+ records with OTT IDs, common names, scientific names)

### API Features Overview
- **Topology Trees**: Fast generation using common names, works for any species combination
- **Dated Trees**: Age-calibrated trees using scientific names, limited to species with chronogram data
- **Fallback Strategy**: Frontend can attempt dated trees first, fall back to topology trees
- **Interactive Visualization**: Color-coded CollapsibleTree with age tooltips and geological periods

### API Endpoints
- **GET /api/species?search=term&limit=N**: Search species by name (case-insensitive, default limit 50, max 100)
- **POST /api/tree**: Generate topology-only phylogenetic tree from species list (common names)
- **POST /api/dated-tree**: Generate dated phylogenetic tree with chronogram ages (scientific names required, limited coverage)
- **GET /api/random-tree?count=N**: Generate random tree for testing (topology only)
- **GET /api/legend**: Get legend information for tree visualization colors and node types
- **GET /api/health**: Health check endpoint

### New Dated Tree API (/api/dated-tree)
**MAJOR FEATURE**: Added in current session - provides age-calibrated phylogenetic trees using DateLife chronogram database.

**Usage Pattern:**
```bash
# Generate dated tree with chronogram ages (requires scientific names)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "species=Homo sapiens,Canis lupus" http://localhost:8000/api/dated-tree

# Allow partial trees when some species missing from chronogram data
curl -X POST -H "X-API-Key: YOUR-API-Key" -d "species=Homo sapiens,Canis lupus,Felis catus&allow_partial_response=true" http://localhost:8000/api/dated-tree
```

**Key Features:**
- Uses DateLife R package to query chronogram database
- Requires scientific names (not common names)
- Returns ancestor ages in millions of years (Mya)
- Includes geological period information in tooltips
- Handles partial coverage gracefully with detailed error responses
- Frontend can detect partial coverage and fall back to topology trees

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
- `convert_rotl_to_hierarchy()`: Converts phylogenetic tree from Open Tree of Life to hierarchical structure
- `generate_tree_html()`: Creates CollapsibleTree HTML visualization with color coding
- `search_species()`: Searches SQLite database for species with optional search term and limit
- `get_species_from_db()`: Queries SQLite database for species data by common name
- `trace_path_to_root()`: Walks tree structure from species to root ancestor

### Data Flow

**Topology-Only Trees (/api/tree):**
1. API receives species list (common names)
2. Database lookup to get OTT IDs and scientific names
3. rotl library fetches phylogenetic tree from Open Tree of Life  
4. Tree converted to hierarchical structure with readable ancestor names
5. CollapsibleTree generates interactive HTML visualization
6. Color-coded nodes: Red (root), Blue (unnamed ancestors), Orange (taxonomic groups), Green (species)

**Dated Trees (/api/dated-tree):**
1. API receives species list (scientific names required)
2. DateLife searches chronogram database for published age data
3. If partial coverage: returns JSON with missing species list for frontend handling
4. If full coverage: generates median consensus matrix and phylo tree with ages
5. CollapsibleTree generates interactive HTML with age information in tooltips
6. Frontend falls back to /api/tree if DateLife coverage insufficient

### Database Schema
```sql
CREATE TABLE species (
  ott INTEGER,        -- Open Tree of Life ID
  common TEXT,        -- Common name (e.g., "Human")  
  scientific TEXT     -- Scientific name (e.g., "Homo sapiens")
);
```

## Development Notes

### Tree Generation Logic
- Minimum 2 species required for tree generation
- Uses `tol_induced_subtree()` from rotl to get phylogenetic relationships
- Handles missing OTT IDs gracefully (returns error for insufficient valid species)
- Converts scientific names to readable ancestor labels via `convert_to_readable_name()`

### API Response Format
All endpoints return JSON with `success` boolean and either `error` message or relevant data fields.

### Color Coding System
- **Red (#E74C3C)**: Root node ("Common ancestor - click me!")
- **Blue (#3498DB)**: Unnamed evolutionary ancestors  
- **Orange (#F39C12)**: Named taxonomic groups (families, orders, etc.)
- **Green (#27AE60)**: Species (leaf nodes)

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
No formal test suite exists. Use curl commands above for manual endpoint testing. For function-level testing:
```r
# Test topology-only trees
source("functions/rotl_tree_generation.R")
test_species <- c("Human", "Dog", "Cat")
result <- generate_tree_html(test_species)

# Test dated trees (limited coverage)
source("functions/datelife_tree_generation.R")
test_species_sci <- c("Homo sapiens", "Canis lupus")
datelife_result <- get_datelife_result(input = test_species_sci)
```