# Evolution Mapper API

A phylogenetic tree generation API built with R Plumber, providing interactive CollapsibleTree visualizations from species data.

## Features

- **Species Database**: 90,276+ unique species with Open Tree of Life IDs
- **Interactive Trees**: Color-coded CollapsibleTree HTML visualizations
- **REST API**: Clean endpoints for integration with any frontend
- **API Key Authentication**: Secure access control for all endpoints
- **Rate Limiting**: 60 requests per minute per IP address
- **Input Validation**: SQL injection protection and parameter sanitization
- **Self-contained**: No external dependencies beyond R packages

## API Endpoints

**Note**: All endpoints except `/api/health` require API key authentication via `X-API-Key` header or `api_key` query parameter.

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

### Generate Phylogenetic Tree
```
POST /api/tree
Headers: X-API-Key: your-api-key
Content-Type: application/x-www-form-urlencoded
Body: species=Human,Dog,Cat
```
Returns interactive CollapsibleTree HTML.

### Random Tree (Testing)
```
GET /api/random-tree?count=4
Headers: X-API-Key: your-api-key
```
Generates tree with random species for testing.

## Project Structure

```
backend/
├── plumber.R              # Main API server
├── functions/
│   └── tree_generation.R  # Core tree generation logic
├── data/
│   └── species.sqlite     # Species database (90k+ records)
└── README.md
```

## Dependencies

Required R packages:
- `plumber` - API framework
- `rotl` - Open Tree of Life integration
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
install.packages(c("plumber", "rotl", "ape", "collapsibleTree", 
                   "htmlwidgets", "RSQLite", "DBI", "dplyr"))

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

# Search species with API key as query parameter
curl "http://localhost:8000/api/species?api_key=demo-key-12345&search=human&limit=3"

# Generate tree
curl -X POST -H "X-API-Key: demo-key-12345" -d "species=Human,Dog,Cat" http://localhost:8000/api/tree

# Random tree
curl -H "X-API-Key: demo-key-12345" "http://localhost:8000/api/random-tree?count=3"

# Generate random tree and save to HTML file for viewing
curl -H "X-API-Key: demo-key-12345" "http://localhost:8000/api/random-tree?count=5" | jq -r '.html[0]' > random_tree.html
```

## Deployment

This API is designed to work with `plumberDeploy` for cloud deployment or can be containerized with Docker.

## Color Coding

- **Red**: Root node ("Common ancestor - click me!")
- **Blue**: Unnamed evolutionary ancestors  
- **Orange**: Named taxonomic groups
- **Green**: Species (leaf nodes)