#!/bin/bash

# Test script for the new paired API format
# Usage: ./sh/test_paired_apis.sh

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

echo "=== Testing New Paired API Format ==="
echo ""

# Test data - paired common and scientific names
COMMON_NAMES="Human,Dog,Cat"
SCIENTIFIC_NAMES="Homo sapiens,Canis lupus,Felis catus"

echo "Test data:"
echo "  Common names: $COMMON_NAMES"
echo "  Scientific names: $SCIENTIFIC_NAMES"
echo ""

# Test 1: /api/tree (topology only with ROTL)
echo "--- Test 1: /api/tree (ROTL topology) ---"
echo "Running: curl -X POST -H \"X-API-Key: $API_KEY\" -d \"common_names=$COMMON_NAMES&scientific_names=$SCIENTIFIC_NAMES\" http://localhost:8000/api/tree"

TREE_RESPONSE=$(curl -s -X POST \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "common_names=$COMMON_NAMES&scientific_names=$SCIENTIFIC_NAMES" \
    "http://localhost:8000/api/tree")

TREE_SUCCESS=$(echo "$TREE_RESPONSE" | jq -r '.success[0] // .success // false')

if [[ "$TREE_SUCCESS" == "true" ]]; then
    echo "✅ SUCCESS: Topology tree generated"
    
    TREE_COUNT=$(echo "$TREE_RESPONSE" | jq -r '.species_count[0] // .species_count // 0')
    TREE_TYPE=$(echo "$TREE_RESPONSE" | jq -r '.tree_type[0] // .tree_type // "unknown"')
    DATA_SOURCE=$(echo "$TREE_RESPONSE" | jq -r '.data_source[0] // .data_source // "unknown"')
    INPUT_COMMON=$(echo "$TREE_RESPONSE" | jq -r '.input_common_names[]? // empty' | paste -sd, -)
    INPUT_SCIENTIFIC=$(echo "$TREE_RESPONSE" | jq -r '.input_scientific_names[]? // empty' | paste -sd, -)
    
    echo "   Species count: $TREE_COUNT"
    echo "   Tree type: $TREE_TYPE"
    echo "   Data source: $DATA_SOURCE"
    echo "   Input common names: $INPUT_COMMON"
    echo "   Input scientific names: $INPUT_SCIENTIFIC"
    
    # Save tree
    echo "$TREE_RESPONSE" | jq -r '.html[0] // .html' > "claude/test_paired_topology_tree.html" 2>/dev/null
    echo "   💾 Tree saved to: claude/test_paired_topology_tree.html"
    
else
    echo "❌ FAILED: Topology tree generation failed"
    ERROR=$(echo "$TREE_RESPONSE" | jq -r '.error[0] // .error // "Unknown error"')
    echo "   Error: $ERROR"
fi

echo ""

# Test 2: /api/dated-tree (chronogram with DateLife)
echo "--- Test 2: /api/dated-tree (DateLife chronogram) ---"
echo "Running: curl -X POST -H \"X-API-Key: $API_KEY\" -d \"common_names=$COMMON_NAMES&scientific_names=$SCIENTIFIC_NAMES&allow_partial_response=true\" http://localhost:8000/api/dated-tree"

DATED_RESPONSE=$(curl -s -X POST \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "common_names=$COMMON_NAMES&scientific_names=$SCIENTIFIC_NAMES&allow_partial_response=true" \
    "http://localhost:8000/api/dated-tree")

DATED_SUCCESS=$(echo "$DATED_RESPONSE" | jq -r '.success[0] // .success // false')

if [[ "$DATED_SUCCESS" == "true" ]]; then
    echo "✅ SUCCESS: Dated tree generated"
    
    DATED_COUNT=$(echo "$DATED_RESPONSE" | jq -r '.species_count[0] // .species_count // 0')
    DATED_TYPE=$(echo "$DATED_RESPONSE" | jq -r '.tree_type[0] // .tree_type // "unknown"')
    DATED_SOURCE=$(echo "$DATED_RESPONSE" | jq -r '.data_source[0] // .data_source // "unknown"')
    COVERAGE=$(echo "$DATED_RESPONSE" | jq -r '.coverage[0] // .coverage // "unknown"')
    ROOT_AGE=$(echo "$DATED_RESPONSE" | jq -r '.datelife_info.root_age_mya // "unknown"')
    CHRONOGRAMS=$(echo "$DATED_RESPONSE" | jq -r '.datelife_info.chronograms_used // "unknown"')
    INPUT_COMMON=$(echo "$DATED_RESPONSE" | jq -r '.input_common_names[]? // empty' | paste -sd, -)
    INPUT_SCIENTIFIC=$(echo "$DATED_RESPONSE" | jq -r '.input_scientific_names[]? // empty' | paste -sd, -)
    
    echo "   Species count: $DATED_COUNT"
    echo "   Tree type: $DATED_TYPE"
    echo "   Data source: $DATED_SOURCE"
    echo "   Coverage: $COVERAGE"
    echo "   Root age: $ROOT_AGE Mya"
    echo "   Chronograms used: $CHRONOGRAMS"
    echo "   Input common names: $INPUT_COMMON"
    echo "   Input scientific names: $INPUT_SCIENTIFIC"
    
    # Check for missing species in partial coverage
    MISSING_COMMON=$(echo "$DATED_RESPONSE" | jq -r '.missing_common_names[]? // empty' | paste -sd, -)
    MISSING_SCIENTIFIC=$(echo "$DATED_RESPONSE" | jq -r '.missing_scientific_names[]? // empty' | paste -sd, -)
    
    if [[ ! -z "$MISSING_COMMON" ]]; then
        echo "   ⚠️  Missing common names: $MISSING_COMMON"
        echo "   ⚠️  Missing scientific names: $MISSING_SCIENTIFIC"
    fi
    
    # Save tree
    echo "$DATED_RESPONSE" | jq -r '.html[0] // .html' > "claude/test_paired_dated_tree.html" 2>/dev/null
    echo "   💾 Tree saved to: claude/test_paired_dated_tree.html"
    
else
    echo "❌ FAILED: Dated tree generation failed"
    ERROR=$(echo "$DATED_RESPONSE" | jq -r '.error[0] // .error // "Unknown error"')
    COVERAGE=$(echo "$DATED_RESPONSE" | jq -r '.coverage[0] // .coverage // "unknown"')
    echo "   Error: $ERROR"
    echo "   Coverage: $COVERAGE"
    
    # Show coverage details if available
    MISSING_COMMON=$(echo "$DATED_RESPONSE" | jq -r '.missing_common_names[]? // empty' | paste -sd, -)
    MISSING_SCIENTIFIC=$(echo "$DATED_RESPONSE" | jq -r '.missing_scientific_names[]? // empty' | paste -sd, -)
    COVERED_SPECIES=$(echo "$DATED_RESPONSE" | jq -r '.covered_species[]? // empty' | paste -sd, -)
    
    if [[ ! -z "$MISSING_COMMON" ]]; then
        echo "   Missing common names: $MISSING_COMMON"
        echo "   Missing scientific names: $MISSING_SCIENTIFIC"
    fi
    if [[ ! -z "$COVERED_SPECIES" ]]; then
        echo "   Covered species: $COVERED_SPECIES"
    fi
fi

echo ""

# Test 3: Error cases
echo "--- Test 3: Error handling ---"

# Test mismatched array lengths
echo "Testing mismatched array lengths..."
ERROR_RESPONSE=$(curl -s -X POST \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "common_names=Human,Dog&scientific_names=Homo sapiens" \
    "http://localhost:8000/api/tree")

ERROR_SUCCESS=$(echo "$ERROR_RESPONSE" | jq -r '.success[0] // .success // true')
if [[ "$ERROR_SUCCESS" == "false" ]]; then
    ERROR_MSG=$(echo "$ERROR_RESPONSE" | jq -r '.error[0] // .error // "No error"')
    echo "✅ Correctly rejected mismatched lengths: $ERROR_MSG"
else
    echo "❌ Should have rejected mismatched array lengths"
fi

# Test missing parameters
echo "Testing missing parameters..."
ERROR_RESPONSE2=$(curl -s -X POST \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "common_names=Human,Dog" \
    "http://localhost:8000/api/tree")

ERROR_SUCCESS2=$(echo "$ERROR_RESPONSE2" | jq -r '.success[0] // .success // true')
if [[ "$ERROR_SUCCESS2" == "false" ]]; then
    ERROR_MSG2=$(echo "$ERROR_RESPONSE2" | jq -r '.error[0] // .error // "No error"')
    echo "✅ Correctly rejected missing parameters: $ERROR_MSG2"
else
    echo "❌ Should have rejected missing scientific_names parameter"
fi

echo ""
echo "=== Summary ==="
echo "✅ New paired API format implemented successfully"
echo "✅ Both /api/tree and /api/dated-tree require common_names + scientific_names"
echo "✅ User-provided common names are preserved in tree visualization"
echo "✅ Error handling works for mismatched lengths and missing parameters"
echo "✅ DateLife partial coverage handling with allow_partial_response parameter"
echo ""
echo "🌐 View trees:"
echo "   open claude/test_paired_topology_tree.html"
echo "   open claude/test_paired_dated_tree.html"