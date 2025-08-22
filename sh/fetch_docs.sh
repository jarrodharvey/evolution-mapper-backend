#!/bin/bash

# fetch_docs.sh - Fetches and formats API documentation from local Plumber server

set -e

# Source .Renviron to get API keys
if [ -f ".Renviron" ]; then
    export $(grep -v '^#' .Renviron | xargs)
else
    echo "Error: .Renviron file not found"
    exit 1
fi

# Get first API key from comma-separated list
API_KEY=$(echo "$EVOLUTION_API_KEYS" | cut -d',' -f1)

if [ -z "$API_KEY" ]; then
    echo "Error: No API key found in .Renviron"
    exit 1
fi

# Check if server is running
if ! curl -s --connect-timeout 5 "http://localhost:8000/api/health" > /dev/null; then
    echo "Error: Server at http://localhost:8000 is not responding"
    echo "Make sure the Plumber server is running on port 8000"
    exit 1
fi

# Display API endpoints table
echo "API ENDPOINTS"
echo "============="
printf "%-10s %-30s %-50s\n" "METHOD" "ENDPOINT" "DESCRIPTION"
echo "--------------------------------------------------------------------------------------"
printf "%-10s %-30s %-50s\n" "GET" "/api/health" "Health check endpoint (no auth required)"
printf "%-10s %-30s %-50s\n" "GET" "/api/species" "Search species by name (?search=term&limit=N)"
printf "%-10s %-30s %-50s\n" "POST" "/api/tree" "Generate topology tree (common + scientific names)"
printf "%-10s %-30s %-50s\n" "POST" "/api/dated-tree" "Generate dated tree with ages (common + scientific names)"
printf "%-10s %-30s %-50s\n" "GET" "/api/random-tree" "Generate random tree (?count=N)"
printf "%-10s %-30s %-50s\n" "GET" "/api/legend" "Get tree visualization legend"

echo
echo "TESTING COMMANDS"
echo "================"
echo "# Health check (no API key required)"
echo "curl http://localhost:8000/api/health"
echo
echo "# Search species"
echo "curl -H \"X-API-Key: $API_KEY\" \"http://localhost:8000/api/species?search=whale&limit=5\""
echo
echo "# Generate topology tree (paired names required)"
echo "curl -X POST -H \"X-API-Key: $API_KEY\" -d \"common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus\" http://localhost:8000/api/tree"
echo
echo "# Generate dated tree (paired names required)"
echo "curl -X POST -H \"X-API-Key: $API_KEY\" -d \"common_names=Human,Dog&scientific_names=Homo sapiens,Canis lupus\" http://localhost:8000/api/dated-tree"
echo
echo "# Random tree"
echo "curl -H \"X-API-Key: $API_KEY\" \"http://localhost:8000/api/random-tree?count=3\""