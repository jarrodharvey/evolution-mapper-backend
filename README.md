# Evolution Mapper API

A phylogenetic tree generation API built with R Plumber, providing interactive CollapsibleTree visualizations from species data.

## Features

- **Species Database**: 90,276+ unique species with Open Tree of Life IDs
- **Interactive Trees**: Color-coded CollapsibleTree HTML visualizations
- **REST API**: Clean endpoints for integration with any frontend
- **Self-contained**: No external dependencies beyond R packages

## API Endpoints

### Health Check
```
GET /api/health
```

### Get All Species Names
```
GET /api/species
```
Returns all unique common names for frontend species picker.

### Generate Phylogenetic Tree
```
POST /api/tree
Content-Type: application/x-www-form-urlencoded
Body: species=Common Name 1,Common Name 2,Common Name 3
```
Returns interactive CollapsibleTree HTML.

### Random Tree (Testing)
```
GET /api/random-tree?count=4
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
# Health check
curl http://localhost:8000/api/health

# Generate tree
curl -X POST -d "species=Human,Dog,Cat" http://localhost:8000/api/tree

# Random tree
curl http://localhost:8000/api/random-tree?count=3
```

## Deployment

This API is designed to work with `plumberDeploy` for cloud deployment or can be containerized with Docker.

## Color Coding

- **Red**: Root node ("Common ancestor - click me!")
- **Blue**: Unnamed evolutionary ancestors  
- **Orange**: Named taxonomic groups
- **Green**: Species (leaf nodes)