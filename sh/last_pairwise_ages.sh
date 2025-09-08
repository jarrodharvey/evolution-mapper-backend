#!/bin/bash

# Script to extract pairwise ages from the most recent API request
# and map them to common names

LOG_FILE="logs/api.log"

# Check if log file exists
if [[ ! -f "$LOG_FILE" ]]; then
    echo "Error: Log file $LOG_FILE not found"
    exit 1
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

# Function to map scientific name to common name
map_scientific_to_common() {
    local sci_name="$1"
    local sci_clean=$(echo "$sci_name" | sed 's/_/ /g')
    
    case "$sci_clean" in
        "Characidium fasciatum") echo "Banded characidium" ;;
        "Crenimugil crenilabis") echo "Warty-lip mullet" ;;
        "Plectropomus leopardus") echo "Bluedotted coraltrout" ;;
        "Pholidota (order in Opisthokonta)") echo "Scaly anteater" ;;
        "Doryichthys boaja") echo "Long-snouted pipefish" ;;
        "Goggia rupicola") echo "Namaqua dwarf leaf-toed gecko" ;;
        "Hypochaeris radicata") echo "Flatweed" ;;
        *) echo "$sci_clean" ;;
    esac
}


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
echo "Total pairwise ages found: $TOTAL_AGES"


# Show coverage information if available
COVERAGE_LINE=$(grep "\[ $LATEST_REQUEST \] .*Species with ages:" "$LOG_FILE" | tail -1)
if [[ -n "$COVERAGE_LINE" ]]; then
    echo "Coverage: $(echo "$COVERAGE_LINE" | grep -o '[0-9]* / [0-9]*')"
fi