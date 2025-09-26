#!/bin/bash

# foodweb.sh - Generate and display function relationship diagram
# Uses the show_function_relationships.R function from the functions directory

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}Evolution Mapper Function Dependency Visualization${NC}"
echo -e "${BLUE}=================================================${NC}"

# Check if we're in the right directory
if [ ! -f "plumber.R" ]; then
    echo -e "${YELLOW}Warning: plumber.R not found. Make sure you're running from the project root.${NC}"
fi

echo -e "${YELLOW}Generating function relationship diagram...${NC}"

# Run the R function
Rscript -e "
source('functions/show_function_relationships.R')
show_function_relationships()
"

echo -e "${GREEN}✓ Analysis complete!${NC}"
echo ""
echo "The function dependency graph shows:"
echo "- Which functions call other functions (arrows)"
echo "- Hierarchical layout (top-level functions at top)"
echo "- Function names with call relationships"
echo ""
echo "This helps you understand:"
echo "- Entry points to your codebase"
echo "- Function interdependencies"
echo "- Potential refactoring opportunities"