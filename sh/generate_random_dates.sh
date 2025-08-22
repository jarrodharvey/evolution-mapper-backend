#!/bin/bash

# Pure DateLife testing script - no fallback to rotl
# Usage: ./sh/generate_random_dates.sh [count] [output_file]

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

# Default values - random count between 5 and 17 if not specified
if [ -z "$1" ]; then
    COUNT=$((5 + RANDOM % 13))  # Random number between 5 and 17
else
    COUNT=$1
fi
OUTPUT_FILE=${2:-"claude/random_dated_tree.html"}

# Predefined list of species more likely to have DateLife coverage
# Based on commonly studied species in molecular clock studies
# Format: "scientific_name:common_name"
SPECIES_POOL=(
    "Homo sapiens:Human"
    "Canis lupus:Dog" 
    "Felis catus:Cat"
    "Mus musculus:House mouse"
    "Rattus norvegicus:Brown rat"
    "Pan troglodytes:Chimpanzee"
    "Macaca mulatta:Rhesus macaque"
    "Bos taurus:Cow"
    "Sus scrofa:Pig"
    "Gallus gallus:Chicken"
    "Danio rerio:Zebrafish"
    "Drosophila melanogaster:Fruit fly"
    "Caenorhabditis elegans:Roundworm"
    "Saccharomyces cerevisiae:Baker's yeast"
    "Arabidopsis thaliana:Thale cress"
    "Oryza sativa:Asian rice"
    "Escherichia coli:E. coli"
    "Bacillus subtilis:Hay bacillus"
)

echo "=== DateLife Functionality Test ==="
echo "Selecting $COUNT species from DateLife-optimized pool..."

# Validate count
if [ "$COUNT" -lt 2 ]; then
    echo "Error: Minimum 2 species required"
    exit 1
fi

if [ "$COUNT" -gt ${#SPECIES_POOL[@]} ]; then
    echo "Error: Requested $COUNT species but only ${#SPECIES_POOL[@]} available in pool"
    exit 1
fi

# Randomly select species from the pool
SELECTED_PAIRS=()
TEMP_POOL=("${SPECIES_POOL[@]}")

for i in $(seq 1 $COUNT); do
    # Get random index
    POOL_SIZE=${#TEMP_POOL[@]}
    RANDOM_INDEX=$((RANDOM % POOL_SIZE))
    
    # Add selected species pair
    SELECTED_PAIRS+=("${TEMP_POOL[$RANDOM_INDEX]}")
    
    # Remove selected species from temp pool to avoid duplicates
    unset TEMP_POOL[$RANDOM_INDEX]
    TEMP_POOL=("${TEMP_POOL[@]}")  # Reindex array
done

# Create separate lists for scientific and common names
SCIENTIFIC_LIST=""
COMMON_LIST=""
for i in "${!SELECTED_PAIRS[@]}"; do
    # Split the pair by colon
    PAIR="${SELECTED_PAIRS[$i]}"
    SCIENTIFIC_NAME="${PAIR%%:*}"
    COMMON_NAME="${PAIR##*:}"
    
    if [ $i -eq 0 ]; then
        SCIENTIFIC_LIST="$SCIENTIFIC_NAME"
        COMMON_LIST="$COMMON_NAME"
    else
        SCIENTIFIC_LIST="$SCIENTIFIC_LIST,$SCIENTIFIC_NAME"
        COMMON_LIST="$COMMON_LIST,$COMMON_NAME"
    fi
done

echo "Selected species (Scientific): $SCIENTIFIC_LIST"
echo "Selected species (Common): $COMMON_LIST"

# Test DateLife endpoint with paired names and partial response allowed
echo ""
echo "Testing /api/dated-tree endpoint (allow_partial_response=true)..."
DATELIFE_RESPONSE=$(curl -s -X POST \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "common_names=$COMMON_LIST&scientific_names=$SCIENTIFIC_LIST&allow_partial_response=true" \
    "http://localhost:8000/api/dated-tree")

# Analyze DateLife response (handle both array and simple formats)
if echo "$DATELIFE_RESPONSE" | grep -q '"success":\[*true' || echo "$DATELIFE_RESPONSE" | grep -q '"success":true'; then
    echo "✅ DateLife SUCCESS!"
    
    # Extract HTML from JSON response (handle array format)
    echo "$DATELIFE_RESPONSE" | jq -r 'if (.html | type) == "array" then .html[0] else .html end' > "$OUTPUT_FILE"
    
    if [ $? -eq 0 ]; then
        echo "✅ Dated tree HTML saved to: $OUTPUT_FILE"
        
        # Extract and display tree information
        CHRONOGRAMS=$(echo "$DATELIFE_RESPONSE" | jq -r '.datelife_info.chronograms_used // "unknown"')
        ROOT_AGE=$(echo "$DATELIFE_RESPONSE" | jq -r '.datelife_info.root_age_mya // "unknown"')
        COVERED_SPECIES=$(echo "$DATELIFE_RESPONSE" | jq -r '.datelife_info.covered_species | join(", ") // "unknown"')
        DATA_SOURCE=$(echo "$DATELIFE_RESPONSE" | jq -r '.datelife_info.data_source // "unknown"')
        COVERAGE=$(echo "$DATELIFE_RESPONSE" | jq -r '.coverage // "unknown"')
        MISSING_SPECIES=$(echo "$DATELIFE_RESPONSE" | jq -r '.missing_species | join(", ") // "none"')
        COVERAGE_NOTE=$(echo "$DATELIFE_RESPONSE" | jq -r '.datelife_info.coverage_note // ""')
        
        echo ""
        echo "=== DateLife Results ==="
        if [ "$COVERAGE" = "complete" ]; then
            echo "✅ Status: COMPLETE COVERAGE"
        elif [ "$COVERAGE" = "partial" ]; then
            echo "✅ Status: PARTIAL COVERAGE (allowed)"
            echo "❌ Missing species: $MISSING_SPECIES"
            if [ ! -z "$COVERAGE_NOTE" ]; then
                echo "📋 Coverage: $COVERAGE_NOTE"
            fi
        else
            echo "✅ Status: WORKING"
        fi
        echo "📊 Chronograms used: $CHRONOGRAMS"
        echo "⏰ Root age: $ROOT_AGE Mya"
        echo "🧬 Covered species: $COVERED_SPECIES"
        echo "📚 Data source: $DATA_SOURCE"
        echo "🎯 Tree type: Dated chronogram with ancestral ages"
        echo ""
        echo "🌐 Open file: open $OUTPUT_FILE"
        
        exit 0
    else
        echo "❌ Error extracting HTML from DateLife response"
        exit 1
    fi

elif echo "$DATELIFE_RESPONSE" | grep -q '"coverage":\[*"partial"' || echo "$DATELIFE_RESPONSE" | grep -q '"coverage":"partial"'; then
    # This should not happen anymore since we use allow_partial_response=true
    # But handle it gracefully just in case
    echo "⚠ DateLife PARTIAL COVERAGE (unexpected - should have generated tree)"
    
    # Extract coverage information (handle array formats)
    MISSING_SPECIES=$(echo "$DATELIFE_RESPONSE" | jq -r 'if (.missing_species | type) == "array" then (.missing_species | join(", ")) else (.missing_species // "unknown") end')
    COVERED_SPECIES=$(echo "$DATELIFE_RESPONSE" | jq -r 'if (.covered_species | type) == "array" then (.covered_species | join(", ")) else (.covered_species // "unknown") end')
    ERROR_MSG=$(echo "$DATELIFE_RESPONSE" | jq -r 'if (.error | type) == "array" then .error[0] else (.error // "unknown") end')
    
    echo ""
    echo "=== DateLife Results ==="
    echo "⚠ Status: UNEXPECTED PARTIAL COVERAGE ERROR"
    echo "❌ Missing species: $MISSING_SPECIES"
    echo "✅ Covered species: $COVERED_SPECIES"
    echo "📝 Error: $ERROR_MSG"
    echo ""
    echo "🔧 This may indicate an issue with allow_partial_response parameter"
    
    exit 2

elif echo "$DATELIFE_RESPONSE" | grep -q '"coverage":\[*"none"' || echo "$DATELIFE_RESPONSE" | grep -q '"coverage":"none"'; then
    echo "❌ DateLife NO COVERAGE"
    
    ERROR_MSG=$(echo "$DATELIFE_RESPONSE" | jq -r '.error // "unknown"')
    
    echo ""
    echo "=== DateLife Results ==="
    echo "❌ Status: NO COVERAGE"
    echo "📝 Error: $ERROR_MSG"
    echo "🧬 Input species (Scientific): $SCIENTIFIC_LIST"
    echo "🧬 Input species (Common): $COMMON_LIST"
    echo ""
    echo "🎯 None of the selected species have chronogram data"
    echo "💡 DateLife coverage is limited - try different species"
    
    exit 3

else
    echo "❌ DateLife REQUEST FAILED"
    
    echo ""
    echo "=== DateLife Results ==="
    echo "❌ Status: ENDPOINT ERROR"
    echo "📝 Raw response: $DATELIFE_RESPONSE"
    echo ""
    echo "🎯 Check if server is running and API key is valid"
    
    exit 4
fi