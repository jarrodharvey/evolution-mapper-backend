#!/bin/bash

# Run both topology and dated tree generation scripts
# Usage: ./sh/random_both.sh [count] [topology_output] [dated_output]

# Set defaults
COUNT=${1:-5}
TOPOLOGY_OUTPUT=${2:-"sh/random_topology_tree.html"}
DATED_OUTPUT=${3:-"sh/random_dated_tree.html"}

echo "=== Running Both Tree Generation Scripts ==="
echo "Species count: $COUNT"
echo "Topology output: $TOPOLOGY_OUTPUT"
echo "Dated output: $DATED_OUTPUT"
echo ""

# Run topology tree generation
echo "--- Running Topology Tree Generation ---"
./sh/generate_random.sh "$COUNT" "$TOPOLOGY_OUTPUT"
TOPOLOGY_EXIT=$?

echo ""

# Run dated tree generation
echo "--- Running Dated Tree Generation ---"
./sh/generate_random_dates.sh "$COUNT" "$DATED_OUTPUT"
DATED_EXIT=$?

echo ""
echo "=== Summary ==="
if [ $TOPOLOGY_EXIT -eq 0 ]; then
    echo " Topology tree: SUCCESS"
else
    echo "L Topology tree: FAILED"
fi

if [ $DATED_EXIT -eq 0 ]; then
    echo " Dated tree: SUCCESS"
elif [ $DATED_EXIT -eq 2 ]; then
    echo "   Dated tree: PARTIAL COVERAGE"
elif [ $DATED_EXIT -eq 3 ]; then
    echo "L Dated tree: NO COVERAGE"
else
    echo "L Dated tree: FAILED"
fi

echo ""
echo "Both scripts completed. HTML files have been opened automatically."