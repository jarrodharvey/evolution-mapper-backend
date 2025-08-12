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

# Generate the tree with improved JSON handling
curl -v -H "X-API-Key: demo-key-12345" \
    "http://localhost:8000/api/random-tree?count=$COUNT" \
    | jq -r 'if .html then .html[0] else "null" end' > "$OUTPUT_FILE"

# Check if generation was successful
if [[ -s "$OUTPUT_FILE" ]] && [[ "$(head -1 "$OUTPUT_FILE")" == "<!DOCTYPE html>" ]]; then
    echo "✅ Successfully generated $OUTPUT_FILE"
    echo "   File size: $(wc -c < "$OUTPUT_FILE") bytes"
    echo "   Open in browser: file://$(pwd)/$OUTPUT_FILE"
else
    echo "❌ Failed to generate valid HTML file"
    echo "   Content preview:"
    head -3 "$OUTPUT_FILE" 2>/dev/null || echo "   (file empty or missing)"
    exit 1
fi