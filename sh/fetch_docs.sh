#!/bin/bash

# fetch_docs.sh - Fetches and formats API documentation from local Plumber server

set -e

# Function to format OpenAPI JSON into human-readable format
format_openapi_docs() {
    local json_data="$1"
    
    echo "API DOCUMENTATION (from OpenAPI spec)"
    echo "====================================="
    echo
    
    # Extract API info
    local title=$(echo "$json_data" | jq -r '.info.title // "Unknown API"')
    local description=$(echo "$json_data" | jq -r '.info.description // ""')
    local version=$(echo "$json_data" | jq -r '.info.version // ""')
    
    echo "Title: $title"
    if [[ "$description" != "" ]]; then
        echo "Description: $description"
    fi
    if [[ "$version" != "" ]]; then
        echo "Version: $version"
    fi
    echo
    
    echo "API ENDPOINTS"
    echo "============="
    printf "%-10s %-35s %-50s\n" "METHOD" "ENDPOINT" "DESCRIPTION"
    echo "---------------------------------------------------------------------------------------------"
    
    # Parse endpoints from OpenAPI spec
    echo "$json_data" | jq -r '.paths | to_entries[] | .key as $path | .value | to_entries[] | .key as $method | .value as $endpoint_data | [$method | ascii_upcase, $path, ($endpoint_data.summary // $endpoint_data.description // "No description")] | @tsv' | while IFS=$'\t' read -r method endpoint summary; do
        # Truncate long descriptions
        if [[ ${#summary} -gt 50 ]]; then
            summary="${summary:0:47}..."
        fi
        printf "%-10s %-35s %-50s\n" "$method" "$endpoint" "$summary"
    done
    
    echo
    echo "PARAMETERS AND SCHEMAS"
    echo "======================"
    
    # Show endpoint details with parameters
    echo "$json_data" | jq -r '.paths | to_entries[] | .key as $path | .value | to_entries[] | .key as $method | .value as $endpoint_data | select($endpoint_data.parameters or $endpoint_data.requestBody) | [$method | ascii_upcase, $path, ($endpoint_data.summary // "No description")] | @tsv' | while IFS=$'\t' read -r method endpoint summary; do
        echo
        echo "$method $endpoint - $summary"
        echo "$(printf '%*s' $((${#method} + ${#endpoint} + ${#summary} + 6)) '' | tr ' ' '-')"
        
        # Show parameters
        echo "$json_data" | jq -r --arg path "$endpoint" --arg method "$method" '.paths[$path][$method].parameters[]? | "  Parameter: \(.name) (\(.in)) - \(.description // "No description") [\(.required // false | if . then "required" else "optional" end)]"'
        
        # Show request body schema if exists
        echo "$json_data" | jq -r --arg path "$endpoint" --arg method "$method" '.paths[$path][$method].requestBody.content."application/x-www-form-urlencoded".schema.properties // empty | to_entries[] | "  Body param: \(.key) - \(.value.description // "No description")"'
    done
}

# Function to generate testing commands from OpenAPI spec
generate_testing_commands() {
    local json_data="$1"
    local api_key="$2"
    
    echo "TESTING COMMANDS"
    echo "================"
    
    # Generate commands for each endpoint
    echo "$json_data" | jq -r '.paths | to_entries[] | .key as $path | .value | to_entries[] | .key as $method | .value as $endpoint_data | [$method | ascii_upcase, $path, ($endpoint_data.summary // $endpoint_data.description // "No description"), $endpoint_data] | @base64' | while read -r line; do
        # Decode the base64 encoded JSON data
        local decoded=$(echo "$line" | base64 --decode)
        local method=$(echo "$decoded" | jq -r '.[0]')
        local endpoint=$(echo "$decoded" | jq -r '.[1]')
        local summary=$(echo "$decoded" | jq -r '.[2]')
        local endpoint_data=$(echo "$decoded" | jq -r '.[3]')
        
        # Generate comment from summary
        echo "# $summary"
        
        # Start building curl command
        local curl_cmd="curl"
        
        # Add method if not GET
        if [[ "$method" != "GET" ]]; then
            curl_cmd="$curl_cmd -X $method"
        fi
        
        # Add API key header unless it's health endpoint
        if [[ "$endpoint" != "/api/health" ]]; then
            curl_cmd="$curl_cmd -H \"X-API-Key: $api_key\""
        fi
        
        # Handle request body parameters for POST endpoints
        if [[ "$method" == "POST" ]]; then
            local post_data=""
            case "$endpoint" in
                "/api/tree"|"/api/dated-tree"|"/api/full-tree-dated")
                    post_data="common_names=Human,Dog,Cat&scientific_names=Homo sapiens,Canis lupus,Felis catus"
                    ;;
            esac
            if [[ -n "$post_data" ]]; then
                curl_cmd="$curl_cmd -d \"$post_data\""
            fi
        fi
        
        # Handle query parameters for GET endpoints
        local query_params=""
        case "$endpoint" in
            "/api/echo")
                query_params="?msg=hello"
                ;;
            "/api/species")
                query_params="?search=whale&limit=5"
                ;;
            "/api/random-species"|"/api/random-tree"|"/api/debug-tree")
                if [[ "$endpoint" == "/api/random-species" ]]; then
                    query_params="?count=5"
                else
                    query_params="?count=3"
                fi
                ;;
            "/api/citations")
                # No query parameters needed for citations
                query_params=""
                ;;
        esac
        
        # Complete the curl command
        curl_cmd="$curl_cmd \"http://localhost:8000$endpoint$query_params\" | jq"
        
        echo "$curl_cmd"
        echo
    done
}

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

# Fetch OpenAPI specification and format it
echo "Fetching OpenAPI specification from server..."
OPENAPI_JSON=$(curl -s -H "X-API-Key: $API_KEY" "http://localhost:8000/openapi.json")

if [[ $? -eq 0 && "$OPENAPI_JSON" != "" ]]; then
    format_openapi_docs "$OPENAPI_JSON"
else
    echo "Error: Could not fetch OpenAPI specification"
    echo "Falling back to manual endpoint list..."
    
    # Fallback to manual endpoint list
    echo "API ENDPOINTS"
    echo "============="
    printf "%-10s %-35s %-50s\n" "METHOD" "ENDPOINT" "DESCRIPTION"
    echo "---------------------------------------------------------------------------------------------"
    printf "%-10s %-35s %-50s\n" "GET" "/api/health" "Health check endpoint (no auth required)"
    printf "%-10s %-35s %-50s\n" "GET" "/api/legend" "Get tree visualization legend"
    printf "%-10s %-35s %-50s\n" "GET" "/api/echo" "Echo test endpoint (?msg=text)"
    printf "%-10s %-35s %-50s\n" "GET" "/api/species" "Search species by name (?search=term&limit=N)"
    printf "%-10s %-35s %-50s\n" "POST" "/api/tree" "Generate topology tree (common + scientific names)"
    printf "%-10s %-35s %-50s\n" "GET" "/api/random-species" "Get random species (?count=N)"
    printf "%-10s %-35s %-50s\n" "GET" "/api/random-tree" "Generate random tree (?count=N)"
    printf "%-10s %-35s %-50s\n" "POST" "/api/dated-tree" "Generate dated tree with ages (common + scientific names)"
    printf "%-10s %-35s %-50s\n" "POST" "/api/full-tree-dated" "Generate full dated tree (experimental)"
    printf "%-10s %-35s %-50s\n" "GET" "/api/debug-tree" "Debug tree generation (?count=N)"
fi

echo
generate_testing_commands "$OPENAPI_JSON" "$API_KEY"