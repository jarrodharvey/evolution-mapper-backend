#!/bin/bash

# Generate JSON file with paired species data for testing
# Usage: ./sh/generate_random_json.sh [count] [output_file]

# Get API key from .Renviron file
if [[ -f ".Renviron" ]]; then
    API_KEY=$(grep "^EVOLUTION_API_KEYS=" .Renviron | cut -d'=' -f2 | cut -d',' -f1)
    if [[ -z "$API_KEY" ]]; then
        echo "Error: No API key found in .Renviron file" >&2
        echo "Please set EVOLUTION_API_KEYS in .Renviron (see .Renviron.example)" >&2
        exit 1
    fi
else
    echo "Error: .Renviron file not found" >&2
    echo "Please create .Renviron from .Renviron.example and set your API keys" >&2
    exit 1
fi

# Set defaults
COUNT=${1:-5}
OUTPUT_FILE=${2:-"claude/random_species.json"}

# Validate count parameter
if [[ $COUNT -lt 2 || $COUNT -gt 20 ]]; then
    echo "Error: Count must be between 2 and 20" >&2
    exit 1
fi

echo "Generating random species list with $COUNT species..."
echo "Output will be saved to: $OUTPUT_FILE"

# Get random species from the database
RESPONSE=$(curl -s -H "X-API-Key: $API_KEY" "http://localhost:8000/api/species?limit=$COUNT")

# Check if the API call was successful
if echo "$RESPONSE" | jq -r '.success' | grep -q "true"; then
    # Extract species data and create paired arrays
    SPECIES_DATA=$(echo "$RESPONSE" | jq '.species')
    
    # Create common names array
    COMMON_NAMES=$(echo "$SPECIES_DATA" | jq -r '[.[].common] | join(",")')
    
    # Create scientific names array  
    SCIENTIFIC_NAMES=$(echo "$SPECIES_DATA" | jq -r '[.[].scientific] | join(",")')
    
    # Create the output JSON structure
    OUTPUT_JSON=$(cat <<EOF
{
  "count": $COUNT,
  "common_names": "$COMMON_NAMES",
  "scientific_names": "$SCIENTIFIC_NAMES",
  "species": $SPECIES_DATA,
  "curl_topology_tree": "curl -X POST -H \"X-API-Key: $API_KEY\" -d \"common_names=$COMMON_NAMES&scientific_names=$SCIENTIFIC_NAMES\" http://localhost:8000/api/tree",
  "curl_dated_tree": "curl -X POST -H \"X-API-Key: $API_KEY\" -d \"common_names=$COMMON_NAMES&scientific_names=$SCIENTIFIC_NAMES\" http://localhost:8000/api/dated-tree"
}
EOF
)
    
    # Save to file
    echo "$OUTPUT_JSON" | jq . > "$OUTPUT_FILE"
    
    echo "✅ Successfully generated $OUTPUT_FILE"
    echo "   Species count: $COUNT"
    echo "   Common names: $COMMON_NAMES"
    echo "   Scientific names: $SCIENTIFIC_NAMES"
    echo ""
    echo "📋 Ready-to-use curl commands are included in the JSON file"
    echo "🌐 View file: cat $OUTPUT_FILE"
    
else
    # Show error message
    ERROR_MSG=$(echo "$RESPONSE" | jq -r '.error // "Unknown error"')
    echo "❌ API Error: $ERROR_MSG"
    echo "$RESPONSE" > "$OUTPUT_FILE"
    exit 1
fi