#!/bin/bash

# Kill any existing phylocom processes that may be hanging
pkill -f "phylocom bladj" 2>/dev/null || true

# Kill any existing tail processes on the log file to prevent duplication
pkill -f "tail -f logs/api.log" 2>/dev/null || true

# Generate random hybrid phylogenetic tree HTML using /api/full-tree-dated
# Combines ROTL topology with DateLife ages where available
# Usage: ./sh/generate_random_hybrid.sh [count] [output_file] [--simple] [--progress]
#
# Arguments:
#   count: Number of species (3-20, default random between 4-12)
#   output_file: Output HTML file (default sh/random_hybrid_tree.html)
#   --simple: Use predefined simple species set (chicken, human, chimpanzee)
#   --progress: Use progress tracking instead of log streaming

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

# Parse command line arguments
USE_SIMPLE=false
USE_PROGRESS=false
COUNT=""
OUTPUT_FILE=""

# Parse arguments
for arg in "$@"; do
    case $arg in
        --simple)
            USE_SIMPLE=true
            ;;
        --progress)
            USE_PROGRESS=true
            ;;
        *)
            if [[ -z "$COUNT" && "$arg" =~ ^[0-9]+$ ]]; then
                COUNT=$arg
            elif [[ -z "$OUTPUT_FILE" ]]; then
                OUTPUT_FILE=$arg
            fi
            ;;
    esac
done

# Set defaults
if [[ "$USE_SIMPLE" == true ]]; then
    COUNT=3  # Fixed count for simple mode
else
    # Set defaults - random count between 4-12 if not specified
    if [ -z "$COUNT" ]; then
        COUNT=$((4 + RANDOM % 9))  # Random number between 4 and 12
    fi
    
    # Validate count parameter
    if [[ $COUNT -lt 3 || $COUNT -gt 20 ]]; then
        echo "Error: Count must be between 3 and 20" >&2
        exit 1
    fi
fi

OUTPUT_FILE=${OUTPUT_FILE:-"sh/random_hybrid_tree.html"}

echo "=== Hybrid Tree Generation Test ==="
if [[ "$USE_SIMPLE" == true ]]; then
    echo "Generating simple hybrid tree with predefined species set..."
else
    echo "Generating random hybrid tree with $COUNT species..."
fi
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

# Get progress token if --progress flag is used
PROGRESS_TOKEN=""
if [[ "$USE_PROGRESS" == true ]]; then
    echo ""
    echo "🎯 Getting progress token for real-time monitoring..."
    
    PROGRESS_RESPONSE=$(curl -s -H "X-API-Key: $API_KEY" "http://localhost:8000/api/get_progress_token")
    
    if echo "$PROGRESS_RESPONSE" | jq -r '.success' | grep -q "true"; then
        PROGRESS_TOKEN=$(echo "$PROGRESS_RESPONSE" | jq -r 'if (.token | type) == "array" then .token[0] else .token end')
        echo "✅ Progress token obtained: $PROGRESS_TOKEN"
        
        # Check if any cleanup occurred
        CLEANUP_COUNT=$(echo "$PROGRESS_RESPONSE" | jq -r '.cleanup_info.files_cleaned // 0')
        if [[ "$CLEANUP_COUNT" != "0" ]]; then
            echo "🧹 Cleaned up $CLEANUP_COUNT old progress files"
        fi
    else
        echo "❌ Failed to get progress token"
        ERROR_MSG=$(echo "$PROGRESS_RESPONSE" | jq -r '.error // "Unknown error"')
        echo "📝 Error: $ERROR_MSG"
        exit 1
    fi
fi

# Step 1: Select species (random or simple)
echo ""
if [[ "$USE_SIMPLE" == true ]]; then
    echo "Step 1: Using predefined simple species set..."
    
    # Use predefined simple species set
    COMMON_ARRAY=("Chicken" "Human" "Chimpanzee")
    SCIENTIFIC_ARRAY=("Gallus gallus" "Homo sapiens" "Pan troglodytes")
    
    echo "✅ Using predefined simple species:"
    for i in "${!COMMON_ARRAY[@]}"; do
        echo "   🧬 ${COMMON_ARRAY[$i]} (${SCIENTIFIC_ARRAY[$i]})"
    done
else
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
fi

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
if [[ "$USE_SIMPLE" == true ]]; then
    echo "🔄 Processing hybrid tree generation for simple species set (this may take 30-60 seconds)..."
else
    echo "🔄 Processing hybrid tree generation (this may take 30-90 seconds for $COUNT species)..."
fi
echo "   📊 Stage 1: Querying DateLife chronogram database..."
echo "   🌳 Stage 2: Building ROTL phylogenetic tree topology..."
echo "   🧬 Stage 3: Mapping ancestor ages to tree nodes..."
echo "   🎨 Stage 4: Generating interactive HTML visualization..."
echo ""

# Create a temporary file for the response
TEMP_RESPONSE=$(mktemp)

if [[ "$USE_PROGRESS" == true ]]; then
    echo "🚀 Starting API request with progress monitoring..."
else
    echo "🚀 Starting API request and streaming logs..."
fi

# Build API request data
API_DATA="common_names=$COMMON_LIST&scientific_names=$SCIENTIFIC_LIST"
if [[ -n "$PROGRESS_TOKEN" ]]; then
    API_DATA="${API_DATA}&progress_token=$PROGRESS_TOKEN"
    # Add throttle for better progress monitoring visibility
    API_DATA="${API_DATA}&throttle_secs=3"
fi

# Start curl in background and write to temp file
(curl -s -X POST \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "$API_DATA" \
    "http://localhost:8000/api/full-tree-dated" > "$TEMP_RESPONSE") &

CURL_PID=$!

# Monitor progress or stream logs
if [[ "$USE_PROGRESS" == true ]]; then
    echo "📊 Monitoring progress in real-time using /api/progress..."
    echo "🎯 Progress token: $PROGRESS_TOKEN"
    echo ""
    
    # Progress monitoring loop that exits when complete (now works thanks to multithreading!)
    (while true; do
        echo "=== Latest Progress Step $(date '+%H:%M:%S') ==="
        RESPONSE=$(curl -s -H "X-API-Key: $API_KEY" "http://localhost:8000/api/progress?progress_token=$PROGRESS_TOKEN")
        
        if [[ -n "$RESPONSE" ]] && echo "$RESPONSE" | jq -e '.steps' > /dev/null 2>&1; then
            if echo "$RESPONSE" | jq -e '.steps | length > 0' > /dev/null 2>&1; then
                LATEST_STEP=$(echo "$RESPONSE" | jq '.steps[-1]')
                echo "$LATEST_STEP"
                
                # Check if the request is completed
                if echo "$LATEST_STEP" | jq -e '.step == "request_completed"' > /dev/null 2>&1; then
                    echo ""
                    echo "✅ Progress monitoring complete!"
                    break
                fi
            else
                echo '{"step": "waiting", "status": "no_steps_yet"}'
            fi
        else
            echo "$RESPONSE" | jq 'if .token then {"step": "initializing", "status": .status, "token": .token} else . end'
        fi
        echo ""
        sleep 1
    done) &
    WATCH_PID=$!
else
    # Stream server logs while waiting with color coding
    (tail -f logs/api.log | while IFS= read -r line; do
        # Color coding for different log levels and patterns
        if [[ "$line" =~ ERROR|Error|error ]]; then
            echo -e "\033[31m$line\033[0m"  # Red for errors
        elif [[ "$line" =~ WARN|Warning|warning ]]; then
            echo -e "\033[33m$line\033[0m"  # Yellow for warnings
        elif [[ "$line" =~ INFO|Info|info|Starting|Finished|Success|Complete ]]; then
            echo -e "\033[32m$line\033[0m"  # Green for info/success
        elif [[ "$line" =~ DEBUG|Debug|debug ]]; then
            echo -e "\033[36m$line\033[0m"  # Cyan for debug
        elif [[ "$line" =~ "API call"|"Endpoint"|"Request"|"Response" ]]; then
            echo -e "\033[35m$line\033[0m"  # Magenta for API-related
        elif [[ "$line" =~ "DateLife"|"ROTL"|"Wikipedia"|"PhyloPic" ]]; then
            echo -e "\033[34m$line\033[0m"  # Blue for external services
        else
            echo "$line"  # Default color for other lines
        fi
    done) &
    TAIL_PID=$!
fi

# Wait for curl to complete
wait $CURL_PID
CURL_EXIT_CODE=$?

# Stop monitoring/log streaming
if [[ "$USE_PROGRESS" == true ]]; then
    kill $WATCH_PID 2>/dev/null
    echo "📊 Final progress check..."
else
    kill $TAIL_PID 2>/dev/null
fi

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