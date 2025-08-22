# Evolution Mapper API

A phylogenetic tree generation API built with R Plumber, providing interactive CollapsibleTree visualizations from species data.

## Features

- **Species Database**: 90,276+ unique species with Open Tree of Life IDs
- **Dual Tree Types**: Topology-only trees (fast, any species) and dated trees (chronogram ages, limited coverage)
- **Consistent Input Format**: Both APIs require paired common and scientific names for accurate species matching
- **Interactive Trees**: Color-coded CollapsibleTree HTML visualizations with age tooltips
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

### Generate Dated Phylogenetic Tree
```
POST /api/dated-tree
Headers: X-API-Key: your-api-key
Content-Type: application/x-www-form-urlencoded
Body: common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus
```
Returns age-calibrated tree using DateLife chronogram database. **Requires both common and scientific names** - uses scientific names for chronogram lookup while preserving user-provided common names in visualization.

**Coverage Limitations**: DateLife has extremely limited species coverage. Most species return no chronogram data. Frontend should attempt this endpoint first, then fall back to `/api/tree` if partial/no coverage.

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

### Legend Information
```
GET /api/legend
Headers: X-API-Key: your-api-key
```
Returns color coding information for tree visualization nodes.

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
├── plumber.R                        # Main API server
├── functions/
│   ├── rotl_tree_generation.R       # Topology-only trees (Open Tree of Life)
│   ├── datelife_tree_generation.R   # Dated trees (DateLife chronograms)
│   └── tree_generation.R            # Legacy tree generation logic
├── data/
│   └── species.sqlite               # Species database (90k+ records)
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

### Start Server

```r
# Install dependencies
install.packages(c("plumber", "rlang", "rotl", "ape", "collapsibleTree", 
                   "htmlwidgets", "RSQLite", "DBI", "dplyr", "datelife"))

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

# Generate tree (paired names required)
curl -X POST -H "X-API-Key: demo-key-12345" -d "common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus" "http://DROPLET_ADDRESS:8000/api/tree"
```

### Manual Deployment

Alternative deployment options include `plumberDeploy` or Docker containerization.

## Color Coding

- **Red**: Root node ("Common ancestor - click me!")
- **Blue**: Unnamed evolutionary ancestors  
- **Orange**: Named taxonomic groups
- **Green**: Species (leaf nodes)