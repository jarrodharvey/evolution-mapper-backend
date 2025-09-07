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
- **REST API**: Clean endpoints for integration with any frontend
- **API Key Authentication**: Secure access control for all endpoints
- **Rate Limiting**: 60 requests per minute per IP address
- **Input Validation**: SQL injection protection and parameter sanitization
- **Graceful Fallback**: Partial coverage detection for seamless user experience

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
│   ├── hybrid_tree_generation.R     # Hybrid trees (ROTL + DateLife)
│   ├── info_panel_system.R          # Mobile-friendly info panels
│   ├── tree_html_enhancement.R      # Advanced tree visualization
│   ├── wikipedia_api.R              # Wikipedia integration
│   ├── phylopic_silhouettes.R       # PhyloPic integration
│   ├── color_config.R               # Centralized color schemes
│   ├── logging_config.R             # Centralized logging
│   ├── progress_tracking.R          # Progress tracking for long operations
│   ├── parallel_config.R            # Parallel processing configuration
│   ├── caching_config.R             # Caching for external APIs
│   └── tree_generation.R            # Legacy tree generation logic
├── data/
│   └── species.sqlite               # Species database (90k+ records)
├── logs/                            # Log files
├── tests/                           # Test scripts and files
├── provision_server.R               # Automated DigitalOcean deployment
├── .Renviron.example                # Environment configuration template
└── README.md
```

## Dependencies

Required R packages:
- `plumber` - API framework
- `rlang` - Required for %||% operator
- `rotl` - Open Tree of Life integration
- `datelife` - Chronogram database access for dated trees
- `ape` - Phylogenetic tree handling
- `collapsibleTree` - Interactive tree visualization
- `htmlwidgets` - Widget framework
- `RSQLite`, `DBI` - Database access
- `dplyr` - Data manipulation
- `httr` - HTTP client for Wikipedia and PhyloPic APIs
- `logger` - Centralized logging system

## Local Development

### Setup API Keys

1. Copy `.Renviron.example` to `.Renviron`
2. Edit `.Renviron` with your API keys:
```bash
# .Renviron
EVOLUTION_API_KEYS=your-key-1,your-key-2,your-key-3
```

**Development Keys (included in .Renviron):**
- `demo-key-12345`
- `research-key-67890` 
- `dev-key-abcde`

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

```r
# Install dependencies
install.packages(c("plumber", "rlang", "rotl", "ape", "collapsibleTree", 
                   "htmlwidgets", "RSQLite", "DBI", "dplyr", "datelife", "httr", "logger"))

# Run API server
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

### Automated DigitalOcean Deployment

Use the included provisioning script for one-command deployment:

```bash
# Deploy to first available droplet with firewall protection
Rscript provision_server.R "" "YOUR_IP_ADDRESS"

# Deploy to specific droplet
Rscript provision_server.R "droplet-name" "SOURCE_IP_ADDRESS"
```

**Prerequisites:**
1. DigitalOcean API token in `.Renviron` as `DO_PAT`
2. API keys configured in `.Renviron` as `EVOLUTION_API_KEYS`
3. R packages: `analogsea`, `plumberDeploy`

The script automatically:
- Installs R and system dependencies
- Deploys the API with systemd service
- Configures secure firewall (SSH + API access only)
- Verifies deployment with health checks

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

### Hybrid Tree System (Latest)
The `/api/full-tree-dated` endpoint provides the best of both worlds:
- **Complete Coverage**: Always generates full trees using ROTL topology
- **Age Information**: Incorporates DateLife chronogram data where available
- **Mobile-First**: Info panel system replaces hover tooltips
- **Rich Content**: Wikipedia content and PhyloPic silhouettes
- **No Fallback Needed**: Unlike pure DateLife approach, always succeeds

### Performance Optimizations
- **Parallel Processing**: Wikipedia and PhyloPic data fetched concurrently
- **Intelligent Caching**: External API calls cached to reduce load times
- **Progress Tracking**: Server-sent events for long-running operations
- **Centralized Logging**: Structured logging with file and console output

### Security & Production Ready
- **API Key Authentication**: Secure access control
- **Rate Limiting**: 60 requests per minute per IP
- **Input Validation**: SQL injection protection
- **Automated Deployment**: One-command DigitalOcean deployment