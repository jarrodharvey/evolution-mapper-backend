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
                   "htmlwidgets", "RSQLite", "DBI", "dplyr", "datelife", "httr", "logger", "rentrez"))
```

**System Dependencies (production):**
- `pandoc`, `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libsqlite3-dev`

### Server Management
```bash
# Start/restart server (preferred method)
sh/restart_server.sh

# Kill process on port 8000 (manual method)
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

# Generate hybrid tree as JSON structure (for programmatic use)
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus&as_json=true" http://localhost:8000/api/full-tree-dated

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
CORS_ALLOWED_ORIGINS=http://localhost:3000,https://your-domain.com
DO_PAT=your-digitalocean-api-token
DO_DROPLET_IP=your-droplet-ip-address
DO_DROPLET_DOMAIN=your-domain.com
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

## JSON Output Format

### Using JSON Output

The `/api/full-tree-dated` endpoint supports both HTML and JSON output formats:

```bash
# HTML output (default) - interactive visualization
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus" http://localhost:8000/api/full-tree-dated

# JSON output - structured data for programmatic use
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus&as_json=true" http://localhost:8000/api/full-tree-dated
```

### JSON Response Structure

When `as_json=true` is specified, the response includes a `tree_json` field instead of `html`:

```json
{
  "success": true,
  "species_count": 2,
  "tree_type": "hybrid_rotl_datelife",
  "output_format": "json",
  "tree_json": {
    "node_label": "Common ancestor",
    "node_type": "root",
    "color": "#8E44AD",
    "has_age": false,
    "age_info": "age unavailable",
    "age_numeric": null,
    "node_shape": "circle",
    "image_url": null,
    "image_type": "none",
    "wikipedia_text": "Common ancestor description...",
    "wikipedia_url": null,
    "phylopic_uuid": null,
    "phylopic_url": null,
    "children": [
      {
        "node_label": "Boreoeutheria",
        "node_type": "taxonomic",
        "color": "#F39C12",
        "image_url": "https://commons.wikimedia.org/...",
        "image_type": "wikimedia",
        "wikipedia_text": "Boreoeutheria description...",
        "wikipedia_url": "https://en.wikipedia.org/wiki/Boreoeutheria",
        "children": [
          {
            "node_label": "Human",
            "node_type": "species",
            "color": "#27AE60",
            "has_age": true,
            "age_info": "present (0 Mya)",
            "age_numeric": 0
          },
          {
            "node_label": "Dog",
            "node_type": "species",
            "color": "#27AE60",
            "has_age": true,
            "age_info": "present (0 Mya)",
            "age_numeric": 0
          }
        ]
      }
    ]
  }
}
```

### Node Metadata Fields

Each node in the JSON tree includes comprehensive metadata:

**Core Properties:**
- `node_label`: Display name with age information if available
- `node_type`: `"root"`, `"taxonomic"`, `"ancestor"`, or `"species"`
- `color`: Hex color code for visualization
- `has_age`: Boolean indicating if age data is available
- `age_info`: Human-readable age description (e.g., "65.2 Mya")
- `age_numeric`: Numeric age value for calculations

**Visual Properties:**
- `node_shape`: `"circle"` or PhyloPic URL for custom shapes
- `image_url`: Primary image URL (PhyloPic, Wikimedia, etc.)
- `image_type`: `"phylopic"`, `"wikimedia"`, `"unsplash"`, `"pixabay"`, or `"none"`
- `image_attribution`: Attribution text for images

**Content Properties:**
- `wikipedia_text`: Truncated Wikipedia introduction text
- `wikipedia_url`: Link to Wikipedia article
- `wikipedia_title`: Wikipedia page title
- `phylopic_uuid`: PhyloPic silhouette UUID
- `phylopic_url`: Direct link to PhyloPic SVG image
- `phylopic_attribution`: PhyloPic attribution information

**Tree Structure:**
- `children`: Array of child nodes (recursive structure)

### Use Cases for JSON Output

**Custom Visualizations:**
- Build D3.js, React, or other interactive visualizations
- Create mobile app tree displays
- Generate static diagrams or charts

**Data Analysis:**
- Extract age information for statistical analysis
- Build phylogenetic distance matrices
- Integrate with bioinformatics pipelines

**Content Management:**
- Store structured tree data in databases
- Generate reports or documentation
- Create educational materials

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
tests/test_chatgpt_logging.sh

# Save test outputs to tests/ directory
curl -X POST -H "X-API-Key: YOUR-API-KEY" -d "common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus" http://localhost:8000/api/full-tree-dated > tests/test_output.html

# Use frontend for visual testing (restart backend first)
sh/restart_server.sh
# Then navigate to http://localhost:3000 with playwright
```

### Utility Scripts
```bash
# Test hybrid tree generation (HTML output)
sh/test.sh --simple

# Test hybrid tree generation (JSON output)
sh/test.sh --as-json --simple

# Generate random hybrid tree with specific species
sh/test.sh --species="Human (Homo sapiens), Dog (Canis lupus), Cat (Felis catus)"

# Test with JSON output and custom file
sh/test.sh --json --simple 3 my_tree.json

# Clear API cache
sh/clear_cache.sh

# View server logs
sh/logs.sh

# Generate random test data
sh/generate_random.sh

# Fetch documentation
sh/fetch_docs.sh
```

### Test Script Features
The `sh/test.sh` script supports both HTML and JSON output formats:
- `--json` or `--as-json`: Get JSON tree structure instead of HTML
- `--simple`: Use predefined species set (Chicken, Human, Chimpanzee)
- `--species="list"`: Use custom species in format "Common (Scientific)"
- `--progress`: Use real-time progress monitoring
- `--expansion-speed=N`: Set tree expansion speed in milliseconds

The JSON output opens in your default browser and displays the structured tree data with full metadata.