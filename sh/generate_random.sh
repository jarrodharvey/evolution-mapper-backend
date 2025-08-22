#!/bin/bash

# Generate random phylogenetic tree HTML
# Usage: ./generate_random.sh [count] [output_file]
# 
# Arguments:
#   count: Number of species (2-7, default 7)
#   output_file: Output HTML file (default colors.html)

# Set defaults
COUNT=${1:-7}
OUTPUT_FILE=${2:-colors.html}

# Validate count parameter
if [[ $COUNT -lt 2 || $COUNT -gt 7 ]]; then
    echo "Error: Count must be between 2 and 7" >&2
    exit 1
fi

echo "Generating random tree with $COUNT species..."
echo "Output will be saved to: $OUTPUT_FILE"

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

# Generate the tree with improved JSON handling
RESPONSE=$(curl -s -H "X-API-Key: $API_KEY" "http://localhost:8000/api/random-tree?count=$COUNT")

# Always show selected species for debugging
SELECTED_SPECIES=$(echo "$RESPONSE" | jq -r '.selected_species[0] // .selected_species // []')
if [[ "$SELECTED_SPECIES" != "[]" && "$SELECTED_SPECIES" != "null" ]]; then
    echo "🧬 Selected species: $SELECTED_SPECIES"
fi

# Check if the API call was successful
if echo "$RESPONSE" | jq -r '.success[0]' | grep -q "true"; then
    # Extract HTML content - handle both array and object formats
    echo "$RESPONSE" | jq -r 'if (.html | type) == "array" then .html[0] else .html end' > "$OUTPUT_FILE"
else
    # Show error message
    ERROR_MSG=$(echo "$RESPONSE" | jq -r '.error[0] // .error // "Unknown error"')
    echo "❌ API Error: $ERROR_MSG"
    echo "$RESPONSE" > "$OUTPUT_FILE"
    exit 1
fi

# Check if generation was successful
if [[ -s "$OUTPUT_FILE" ]] && [[ "$(head -1 "$OUTPUT_FILE")" == "<!DOCTYPE html>" ]]; then
    echo "✅ Successfully generated $OUTPUT_FILE"
    echo "   File size: $(wc -c < "$OUTPUT_FILE") bytes"
    open "$OUTPUT_FILE"
else
    echo "❌ Failed to generate valid HTML file"
    echo "   Content preview:"
    head -3 "$OUTPUT_FILE" 2>/dev/null || echo "   (file empty or missing)"
    exit 1
fi