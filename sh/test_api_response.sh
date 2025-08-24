#!/bin/bash

# Test API response script
# Usage: ./test_api_response.sh [output_file.html]

# API configuration - load from .Renviron
if [[ -f .Renviron ]]; then
    # Extract first API key from EVOLUTION_API_KEYS
    API_KEY=$(grep "^EVOLUTION_API_KEYS=" .Renviron | cut -d'=' -f2 | cut -d',' -f1)
else
    echo "❌ .Renviron file not found. Please create it with EVOLUTION_API_KEYS configuration."
    exit 1
fi

if [[ -z "$API_KEY" ]]; then
    echo "❌ No API key found in .Renviron. Please check EVOLUTION_API_KEYS configuration."
    exit 1
fi
API_URL="http://localhost:8000/api/full-tree-dated"

# Test species that should show incorrect missing species count
COMMON_NAMES="Panama porkfish,Black-tipped silver-biddy,Scaly anteater"
SCIENTIFIC_NAMES="Anisotremus taeniatus,Gerres oyena,Pholidota"

# Output file
OUTPUT_HTML=${1:-"test_tree_output.html"}

echo "=== Testing API Response ==="
echo "Common names: $COMMON_NAMES"
echo "Scientific names: $SCIENTIFIC_NAMES"
echo "Output HTML file: $OUTPUT_HTML"
echo ""

# Create temporary file for full response
TEMP_RESPONSE=$(mktemp)

echo "🔄 Making API request..."
curl -s -X POST \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "common_names=$COMMON_NAMES&scientific_names=$SCIENTIFIC_NAMES" \
    "$API_URL" > "$TEMP_RESPONSE"

if [[ $? -ne 0 ]]; then
    echo "❌ API request failed"
    rm -f "$TEMP_RESPONSE"
    exit 1
fi

echo "📊 JSON Response (without HTML):"
echo "=================================="
cat "$TEMP_RESPONSE" | jq 'del(.html)'
echo ""

echo "💾 Extracting HTML to $OUTPUT_HTML..."
cat "$TEMP_RESPONSE" | jq -r 'if (.html | type) == "array" then .html[0] else .html end' > "$OUTPUT_HTML"

if [[ $? -eq 0 && -s "$OUTPUT_HTML" ]]; then
    echo "✅ HTML saved successfully!"
    echo "   📁 File: $OUTPUT_HTML"
    echo "   📏 Size: $(wc -c < "$OUTPUT_HTML") bytes"
    
    echo "🚀 Opening HTML file..."
    open "$OUTPUT_HTML"
else
    echo "❌ Failed to save HTML file"
    exit 1
fi

# Cleanup
rm -f "$TEMP_RESPONSE"

echo ""
echo "🔍 Key findings from the response:"
echo "- species_with_ages: Contains species that HAVE age data"
echo "- species_without_ages: Should contain species that LACK age data (but appears buggy)"
echo "- missing_common_names: Should contain missing species (but appears buggy)"
echo ""
echo "The bug appears to be in the backend API logic for determining which species lack age data."