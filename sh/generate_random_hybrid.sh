#!/bin/bash

# Kill any existing phylocom processes that may be hanging
pkill -f "phylocom bladj" 2>/dev/null || true

# Generate random hybrid phylogenetic tree HTML using /api/full-tree-dated
# Combines ROTL topology with DateLife ages where available
# Usage: ./sh/generate_random_hybrid.sh [count] [output_file]
#
# Arguments:
#   count: Number of species (3-20, default random between 4-12)
#   output_file: Output HTML file (default sh/random_hybrid_tree.html)

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

# Set defaults - random count between 4-12 if not specified
if [ -z "$1" ]; then
    COUNT=$((4 + RANDOM % 9))  # Random number between 4 and 12
else
    COUNT=$1
fi
OUTPUT_FILE=${2:-"sh/random_hybrid_tree.html"}

# Validate count parameter
if [[ $COUNT -lt 3 || $COUNT -gt 20 ]]; then
    echo "Error: Count must be between 3 and 20" >&2
    exit 1
fi

echo "=== Hybrid Tree Generation Test ==="
echo "Generating random hybrid tree with $COUNT species..."
echo "Output will be saved to: $OUTPUT_FILE"

# Check if server is running
echo ""
echo "🔍 Checking server status..."
if curl -s --connect-timeout 5 --max-time 10 "http://localhost:8000/api/health" > /dev/null 2>&1; then
    SERVER_STATUS=$(curl -s --connect-timeout 5 --max-time 10 "http://localhost:8000/api/health" | jq -r '.status[0] // .status // "unknown"' 2>/dev/null)
    if [[ "$SERVER_STATUS" == "ok" ]]; then
        echo "✅ Server is running and healthy"
    else
        echo "⚠️ Server responded but status unclear: $SERVER_STATUS"
        echo "❌ Health check failed - exiting"
        exit 1
    fi
else
    echo "❌ Server is not responding on http://localhost:8000"
    echo ""
    echo "💡 To start the server, run:"
    echo "   R --no-restore --no-save -e \"library(plumber); pr <- plumb('plumber.R'); pr\\\$run(port = 8000, host = '127.0.0.1')\""
    echo ""
    echo "   Or use the background version:"
    echo "   R --no-restore --no-save -e \"library(plumber); pr <- plumb('plumber.R'); pr\\\$run(port = 8000, host = '127.0.0.1')\" &"
    exit 1
fi

# Step 1: Select random species directly from database
echo ""
echo "Step 1: Selecting $COUNT random species from database..."

# Use SQLite CLI to directly query the species database
SQLITE_OUTPUT=$(sqlite3 data/species.sqlite "
  SELECT common || '|' || scientific 
  FROM species 
  WHERE common IS NOT NULL 
    AND scientific IS NOT NULL 
    AND common != '' 
    AND scientific != ''
  ORDER BY RANDOM() 
  LIMIT $COUNT;
")

# Check if database query was successful
if [[ -z "$SQLITE_OUTPUT" ]]; then
    echo "❌ Error querying species database or no species found"
    exit 1
fi

# Parse the pipe-delimited output
COMMON_ARRAY=()
SCIENTIFIC_ARRAY=()

echo "✅ Selected species from database:"
i=0
while IFS='|' read -r COMMON_NAME SCIENTIFIC_NAME; do
    if [[ -n "$COMMON_NAME" && -n "$SCIENTIFIC_NAME" ]]; then
        echo "   🧬 $COMMON_NAME ($SCIENTIFIC_NAME)"
        
        # Add to arrays
        COMMON_ARRAY+=("$COMMON_NAME")
        SCIENTIFIC_ARRAY+=("$SCIENTIFIC_NAME")
        ((i++))
    fi
done <<< "$SQLITE_OUTPUT"

# Convert arrays to JSON format
COMMON_LIST=$(printf '%s\n' "${COMMON_ARRAY[@]}" | jq -R . | jq -s .)
SCIENTIFIC_LIST=$(printf '%s\n' "${SCIENTIFIC_ARRAY[@]}" | jq -R . | jq -s .)

if [[ -z "$SCIENTIFIC_LIST" ]]; then
    echo "❌ No valid species pairs found in database"
    exit 1
fi

# Step 2: Generate hybrid tree
echo ""
echo "Step 2: Generating hybrid tree with /api/full-tree-dated..."
echo "Common names: $COMMON_LIST"
echo "Scientific names: $SCIENTIFIC_LIST"
echo ""
echo "🔄 Processing hybrid tree generation (this may take 30-90 seconds for $COUNT species)..."
echo "   📊 Stage 1: Querying DateLife chronogram database..."
echo "   🌳 Stage 2: Building ROTL phylogenetic tree topology..."
echo "   🧬 Stage 3: Mapping ancestor ages to tree nodes..."
echo "   🎨 Stage 4: Generating interactive HTML visualization..."
echo ""

# Create a temporary file for the response
TEMP_RESPONSE=$(mktemp)

echo "🚀 Starting API request..."
echo -n "   Progress: "

# Start curl in background and write to temp file
(curl -s -X POST \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "common_names=$COMMON_LIST&scientific_names=$SCIENTIFIC_LIST" \
    "http://localhost:8000/api/full-tree-dated" > "$TEMP_RESPONSE") &

CURL_PID=$!

# Show animated progress while waiting
chars="⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
count=0
while kill -0 $CURL_PID 2>/dev/null; do
    printf "\b${chars:$((count % ${#chars})):1}"
    sleep 0.3
    ((count++))
    
    # Show elapsed time every 10 iterations (3 seconds)
    if (( count % 10 == 0 )); then
        elapsed=$((count / 3))
        printf "\r   Progress: ${chars:$((count % ${#chars})):1} (${elapsed}s elapsed) "
    fi
done

# Wait for completion and get exit code
wait $CURL_PID
CURL_EXIT_CODE=$?
printf "\r   Progress: ✅ Complete!                    \n"

if [[ $CURL_EXIT_CODE -ne 0 ]]; then
    echo "❌ Network request failed (exit code: $CURL_EXIT_CODE)"
    rm -f "$TEMP_RESPONSE"
    exit 1
fi

# Read the response
HYBRID_RESPONSE=$(cat "$TEMP_RESPONSE")
rm -f "$TEMP_RESPONSE"

echo "📡 Received response from API, processing results..."

# Check if hybrid tree generation was successful
if echo "$HYBRID_RESPONSE" | jq -r '.success' | grep -q "true"; then
    echo "✅ Hybrid tree generated successfully!"
    
    # Extract HTML content (handle array format)
    echo "$HYBRID_RESPONSE" | jq -r 'if (.html | type) == "array" then .html[0] else .html end' > "$OUTPUT_FILE"
    
    if [[ $? -eq 0 && -s "$OUTPUT_FILE" ]]; then
        echo "💾 HTML extraction successful!"
        echo "   📁 File: $OUTPUT_FILE"
        echo "   📏 Size: $(wc -c < "$OUTPUT_FILE") bytes"
        
        # Verify it's valid HTML
        if head -1 "$OUTPUT_FILE" | grep -q "<!DOCTYPE html>"; then
            echo "   ✅ Valid HTML document confirmed"
        else
            echo "   ⚠️  Warning: File may not be valid HTML"
        fi
        
        # Extract and display tree information (handle array format)
        TREE_TYPE=$(echo "$HYBRID_RESPONSE" | jq -r 'if (.tree_type | type) == "array" then .tree_type[0] else (.tree_type // "unknown") end')
        DATA_SOURCE=$(echo "$HYBRID_RESPONSE" | jq -r 'if (.data_source | type) == "array" then .data_source[0] else (.data_source // "unknown") end')
        SPECIES_COUNT=$(echo "$HYBRID_RESPONSE" | jq -r 'if (.species_count | type) == "array" then .species_count[0] else (.species_count // 0) end')
        DATELIFE_COVERAGE=$(echo "$HYBRID_RESPONSE" | jq -r 'if (.datelife_coverage | type) == "array" then .datelife_coverage[0] else (.datelife_coverage // 0) end')
        
        echo ""
        echo "=== Hybrid Tree Results ==="
        echo "🎯 Tree type: $TREE_TYPE"
        echo "📚 Data source: $DATA_SOURCE"
        echo "🧬 Total species: $SPECIES_COUNT"
        echo "⏰ DateLife coverage: $DATELIFE_COVERAGE species"
        
        # Calculate coverage percentage
        if [[ $SPECIES_COUNT -gt 0 ]]; then
            COVERAGE_PCT=$((DATELIFE_COVERAGE * 100 / SPECIES_COUNT))
            echo "📊 Age coverage: ${COVERAGE_PCT}% ($DATELIFE_COVERAGE/$SPECIES_COUNT)"
        fi
        
        # Show which species have/don't have age data (if available)
        SPECIES_WITH_AGES=$(echo "$HYBRID_RESPONSE" | jq -r '.species_with_ages // empty | join(", ")')
        SPECIES_WITHOUT_AGES=$(echo "$HYBRID_RESPONSE" | jq -r '.species_without_ages // empty | join(", ")')
        
        if [[ -n "$SPECIES_WITH_AGES" ]]; then
            echo "✅ Species with age data: $SPECIES_WITH_AGES"
        fi
        if [[ -n "$SPECIES_WITHOUT_AGES" ]]; then
            echo "❌ Species without age data: $SPECIES_WITHOUT_AGES"
        fi
        
        echo ""
        echo "🚀 Opening tree visualization..."
        open "$OUTPUT_FILE"
        
        echo ""
        echo "🔧 For troubleshooting, compare with other endpoints:"
        echo ""
        echo "📋 Topology-only tree (/api/tree) - save and open:"
        echo "curl -X POST -H \"X-API-Key: $API_KEY\" \\"
        echo "  -H \"Content-Type: application/x-www-form-urlencoded\" \\"
        echo "  -d \"common_names=$COMMON_LIST&scientific_names=$SCIENTIFIC_LIST\" \\"
        echo "  \"http://localhost:8000/api/tree\" | jq -r 'if (.html | type) == \"array\" then .html[0] else .html end' > sh/comparison_topology_tree.html && open sh/comparison_topology_tree.html"
        echo ""
        echo "📋 DateLife-only tree (/api/dated-tree) - save and open:"
        echo "curl -X POST -H \"X-API-Key: $API_KEY\" \\"
        echo "  -H \"Content-Type: application/x-www-form-urlencoded\" \\"
        echo "  -d \"common_names=$COMMON_LIST&scientific_names=$SCIENTIFIC_LIST&allow_partial_response=true\" \\"
        echo "  \"http://localhost:8000/api/dated-tree\" | jq -r 'if (.html | type) == \"array\" then .html[0] else .html end' > sh/comparison_datelife_tree.html && open sh/comparison_datelife_tree.html"
        echo ""
        echo "💡 These commands will:"
        echo "   • Extract HTML from JSON response"  
        echo "   • Save to comparison files in sh/ directory"
        echo "   • Automatically open in your browser"
        echo ""
        echo "📊 Files for comparison:"
        echo "   🌳 Hybrid (current):     sh/random_hybrid_tree.html"
        echo "   🔗 Topology-only:        sh/comparison_topology_tree.html"  
        echo "   ⏰ DateLife-only:        sh/comparison_datelife_tree.html"
        
    else
        echo "❌ Error saving HTML file"
        exit 1
    fi
else
    echo "❌ Hybrid tree generation failed"
    ERROR_MSG=$(echo "$HYBRID_RESPONSE" | jq -r '.error // "Unknown error"')
    echo "📝 Error: $ERROR_MSG"
    echo "📋 Full response:"
    echo "$HYBRID_RESPONSE" | jq .
    exit 1
fi