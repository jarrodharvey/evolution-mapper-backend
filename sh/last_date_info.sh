#!/bin/bash

# Script to extract pairwise ages from the most recent API request
# and map them to common names

# Check for --droplet flag
USE_DROPLET=false
if [[ "$1" == "--droplet" ]]; then
    USE_DROPLET=true
    echo "🌐 Querying logs from DigitalOcean droplet..."
    echo
fi

if [[ "$USE_DROPLET" == "true" ]]; then
    # Source .Renviron to get DO_DROPLET_DOMAIN
    if [[ -f ".Renviron" ]]; then
        export $(grep -v '^#' .Renviron | xargs)
    fi
    
    if [[ -z "$DO_DROPLET_DOMAIN" ]]; then
        echo "Error: DO_DROPLET_DOMAIN not found in .Renviron"
        exit 1
    fi
    
    # Create temporary file for remote log content
    REMOTE_LOG=$(mktemp)
    LOG_FILE="$REMOTE_LOG"
    
    # Copy remote log file via SSH
    if ! scp "root@$DO_DROPLET_DOMAIN:/var/plumber/evolution-mapper/logs/api.log" "$REMOTE_LOG" 2>/dev/null; then
        echo "Error: Could not retrieve log file from droplet $DO_DROPLET_DOMAIN"
        echo "Make sure SSH access is configured and the service is running"
        rm -f "$REMOTE_LOG"
        exit 1
    fi
    
    # Set cleanup for remote log file
    trap "rm -f $REMOTE_LOG" EXIT
else
    LOG_FILE="logs/api.log"
    
    # Check if local log file exists
    if [[ ! -f "$LOG_FILE" ]]; then
        echo "Error: Log file $LOG_FILE not found"
        echo "Use --droplet flag to query remote logs"
        exit 1
    fi
fi

# Extract the most recent request ID
LATEST_REQUEST=$(grep -o "req_[0-9]*_[0-9]*_[0-9]*" "$LOG_FILE" | tail -1)

if [[ -z "$LATEST_REQUEST" ]]; then
    echo "Error: No request ID found in logs"
    exit 1
fi

echo "=== Latest Request: $LATEST_REQUEST ==="
echo

# Extract the common and scientific name mappings for this request
COMMON_NAMES_LINE=$(grep "\[ $LATEST_REQUEST \] Common names:" "$LOG_FILE" | tail -1)
SCIENTIFIC_NAMES_LINE=$(grep "\[ $LATEST_REQUEST \] Scientific names:" "$LOG_FILE" | tail -1)

if [[ -z "$COMMON_NAMES_LINE" || -z "$SCIENTIFIC_NAMES_LINE" ]]; then
    echo "Error: Could not find species name mappings for request $LATEST_REQUEST"
    exit 1
fi

# Parse the names (remove the log prefix and extract just the names)
COMMON_NAMES=$(echo "$COMMON_NAMES_LINE" | sed 's/.*Common names: //')
SCIENTIFIC_NAMES=$(echo "$SCIENTIFIC_NAMES_LINE" | sed 's/.*Scientific names: //')

echo "Common names: $COMMON_NAMES"
echo "Scientific names: $SCIENTIFIC_NAMES"
echo

# Create temporary files for name mapping
TEMP_COMMON=$(mktemp)
TEMP_SCIENTIFIC=$(mktemp)

# Convert comma-separated lists to line-separated for easier processing
echo "$COMMON_NAMES" | tr ',' '\n' | sed 's/^[ \t]*//;s/[ \t]*$//' > "$TEMP_COMMON"
echo "$SCIENTIFIC_NAMES" | tr ',' '\n' | sed 's/^[ \t]*//;s/[ \t]*$//' > "$TEMP_SCIENTIFIC"

# Function to map scientific name to common name
map_scientific_to_common() {
    local sci_name="$1"
    local sci_clean=$(echo "$sci_name" | sed 's/_/ /g')
    
    # Find the line number of this scientific name
    local line_num=$(grep -n "^${sci_clean}$" "$TEMP_SCIENTIFIC" | cut -d: -f1)
    
    if [[ -n "$line_num" ]]; then
        # Get the corresponding common name
        sed -n "${line_num}p" "$TEMP_COMMON"
    else
        # Fallback: return the cleaned scientific name with indicator
        echo "$sci_clean [scientific name]"
    fi
}

# Cleanup function
cleanup() {
    rm -f "$TEMP_COMMON" "$TEMP_SCIENTIFIC"
}
trap cleanup EXIT


echo "=== Pairwise Ages Found ==="
echo

# Extract pairwise ages for this request
grep "\[ $LATEST_REQUEST \] Found age:" "$LOG_FILE" | while read -r line; do
    # Extract the pairwise age information
    # Format: "Found age: Species1 — Species2 MRCA = X.X Mya"
    age_info=$(echo "$line" | sed 's/.*Found age: //')
    
    # Extract species names and age
    species1=$(echo "$age_info" | sed 's/ — .*//' | sed 's/_/ /g')
    species2=$(echo "$age_info" | sed 's/.* — //' | sed 's/ MRCA.*//' | sed 's/_/ /g')
    age=$(echo "$age_info" | grep -o '[0-9]*\.[0-9]* Mya')
    
    # Map to common names
    common1=$(map_scientific_to_common "$species1")
    common2=$(map_scientific_to_common "$species2")
    
    echo "$common1 — $common2 MRCA = $age"
done

# Summary
echo
echo "=== Summary ==="
TOTAL_AGES=$(grep "\[ $LATEST_REQUEST \] Found age:" "$LOG_FILE" | wc -l)
echo "Total pairwise ages found:        $TOTAL_AGES"

# Show coverage information if available
COVERAGE_LINE=$(grep "\[ $LATEST_REQUEST \] .*Species with ages:" "$LOG_FILE" | tail -1)
if [[ -n "$COVERAGE_LINE" ]]; then
    echo "Coverage: $(echo "$COVERAGE_LINE" | grep -o '[0-9]* / [0-9]*')"
fi

echo
echo "=== Age Assignment Method ==="

# Check which method was used for age assignment
# First check for chronos success
CHRONOS_METHOD=$(grep "\[ $LATEST_REQUEST \] Modern chronos approach successful" "$LOG_FILE")
FALLBACK_METHOD=$(grep "\[ $LATEST_REQUEST \] Attempting direct pairwise age mapping fallback" "$LOG_FILE")

if [[ -n "$CHRONOS_METHOD" ]]; then
    METHOD="chronos"
elif [[ -n "$FALLBACK_METHOD" ]]; then
    METHOD="direct_pairwise_fallback"
else
    METHOD="unknown"
fi

case "$METHOD" in
        "chronos")
            echo "🧬 CHRONOS METHOD: Using ape::chronos molecular clock optimization"
            echo "   → Ages are optimized by chronos based on calibration points"
            echo "   → May differ from raw median values due to clock model constraints"
            ;;
        "direct_pairwise_fallback")
            echo "📊 PAIRWISE MEDIAN METHOD: Using direct median of study ages"
            echo "   → Ages are exact medians of DateLife chronogram values"
            echo "   → Used when chronos fails or produces unreasonable results"
            ;;
        *)
            echo "❓ Unknown method: $METHOD"
            echo "⚠️  Age assignment method not found in logs"
            ;;
    esac

# Show chronos calibration vs final ages if chronos was used
CHRONOS_SUCCESS=$(grep "\[ $LATEST_REQUEST \] Modern chronos approach successful" "$LOG_FILE")
if [[ -n "$CHRONOS_SUCCESS" ]]; then
    echo
    echo "=== Chronos Calibration vs Final Ages ==="
    
    # Extract calibration points with common names
    grep "\[ $LATEST_REQUEST \] Calibration: Node" "$LOG_FILE" | while read -r line; do
        # Format: "Calibration: Node X for Species1 — Species2 = Y.Y Mya (min - max)"
        calibration=$(echo "$line" | sed 's/.*Calibration: //')
        
        # Extract species names from calibration
        species_part=$(echo "$calibration" | sed 's/Node [0-9]* for //' | sed 's/ = [0-9.]*.*//')
        species1=$(echo "$species_part" | sed 's/ — .*//')
        species2=$(echo "$species_part" | sed 's/.* — //')
        
        # Map to common names
        common1=$(map_scientific_to_common "$species1")
        common2=$(map_scientific_to_common "$species2")
        
        # Replace scientific names with common names in the calibration string
        calibration_with_common=$(echo "$calibration" | sed "s/$species1/$common1/g" | sed "s/$species2/$common2/g")
        
        echo "📝 Input calibration: $calibration_with_common"
    done
    
    # Extract final node ages with common names 
    grep "\[ $LATEST_REQUEST \] Node age: MRCA" "$LOG_FILE" | while read -r line; do
        # Format: "Node age: MRCA of Species1 and Species2 = Y.Y Mya (Node X)"
        final_age=$(echo "$line" | sed 's/.*Node age: //')
        
        # Extract species names from final age
        species_part=$(echo "$final_age" | sed 's/MRCA of //' | sed 's/ = [0-9.]*.*//')
        species1=$(echo "$species_part" | sed 's/ and .*//' | sed 's/_/ /g')
        species2=$(echo "$species_part" | sed 's/.* and //' | sed 's/_/ /g')
        
        # Map to common names
        common1=$(map_scientific_to_common "$species1")
        common2=$(map_scientific_to_common "$species2")
        
        # Replace scientific names with common names in the final age string
        final_age_with_common=$(echo "$final_age" | sed "s/${species1// /_}/$common1/g" | sed "s/${species2// /_}/$common2/g")
        
        echo "🎯 Final chronos age: $final_age_with_common"
    done
    
    echo
    echo "=== Root Age Assessment ==="
    
    # Check for calibration quality assessment
    QUALITY_CHECK=$(grep "\[ $LATEST_REQUEST \] Assessing calibration quality for root age reliability" "$LOG_FILE")
    QUALITY_PASS=$(grep "\[ $LATEST_REQUEST \] QUALITY PASS:" "$LOG_FILE")
    QUALITY_FAIL=$(grep "\[ $LATEST_REQUEST \] QUALITY FAIL:" "$LOG_FILE")
    
    if [[ -n "$QUALITY_CHECK" ]]; then
        echo "🔬 Calibration quality assessment performed for root age"
        
        if [[ -n "$QUALITY_PASS" ]]; then
            # Quality passed - show the root age
            ROOT_AGE=$(grep "\[ $LATEST_REQUEST \] Root age from chronos:" "$LOG_FILE" | tail -1)
            if [[ -n "$ROOT_AGE" ]]; then
                root_info=$(echo "$ROOT_AGE" | sed 's/.*Root age from chronos: //')
                echo "✅ Quality assessment: PASSED"
                echo "🌳 Chronos root age: $root_info"
                
                # Show the quality reason
                quality_reason=$(echo "$QUALITY_PASS" | sed 's/.*QUALITY PASS: //')
                echo "   → Reason: $quality_reason"
            fi
        elif [[ -n "$QUALITY_FAIL" ]]; then
            # Quality failed - explain why root age was excluded
            echo "❌ Quality assessment: FAILED"
            echo "🚫 Chronos root age excluded from tree visualization"
            
            # Show the quality failure reason
            quality_reason=$(echo "$QUALITY_FAIL" | sed 's/.*QUALITY FAIL: //')
            echo "   → Reason: $quality_reason"
            
            # Show calibration coverage statistics
            CALIBRATION_COVERAGE=$(grep "\[ $LATEST_REQUEST \] Deep lineage coverage:" "$LOG_FILE")
            if [[ -n "$CALIBRATION_COVERAGE" ]]; then
                coverage_info=$(echo "$CALIBRATION_COVERAGE" | sed 's/.*Deep lineage coverage: //')
                echo "   → Calibration coverage: $coverage_info"
            fi
            
            # Show additional quality details if available
            QUALITY_DETAIL=$(grep "\[ $LATEST_REQUEST \] Excluding chronos root age - insufficient calibration quality:" "$LOG_FILE")
            if [[ -n "$QUALITY_DETAIL" ]]; then
                detail_info=$(echo "$QUALITY_DETAIL" | sed 's/.*insufficient calibration quality: //')
                echo "   → Detail: $detail_info"
            fi
            
            # Show the calculated root age that was excluded
            ROOT_AGE=$(grep "\[ $LATEST_REQUEST \] Root age from chronos:" "$LOG_FILE" | tail -1)
            if [[ -n "$ROOT_AGE" ]]; then
                root_info=$(echo "$ROOT_AGE" | sed 's/.*Root age from chronos: //')
                echo "   → Calculated age (excluded): $root_info"
            fi
        fi
    else
        # Legacy behavior - just show root age if available
        ROOT_AGE=$(grep "\[ $LATEST_REQUEST \] Root age from chronos:" "$LOG_FILE" | tail -1)
        if [[ -n "$ROOT_AGE" ]]; then
            root_info=$(echo "$ROOT_AGE" | sed 's/.*Root age from chronos: //')
            echo "🌳 Chronos root age: $root_info"
            echo "   → (No quality assessment found in logs)"
        else
            echo "🌳 No chronos root age found"
        fi
    fi
fi