#!/bin/bash

# Clear Cache Script
# Removes all cached data from the evolution-mapper backend

echo "=== CLEARING EVOLUTION MAPPER CACHE ==="
echo

# Check if cache directory exists
if [ ! -d "cache" ]; then
    echo "❌ Cache directory not found"
    exit 1
fi

echo "📁 Cache directory found"
echo

# Clear each cache subdirectory
cache_dirs=(
    "info_panels"
    "wikipedia"
    "phylopic"
    "wikipedia_images"
    "unsplash_images"
    "chatgpt_summaries"
)

total_removed=0

for dir in "${cache_dirs[@]}"; do
    cache_path="cache/$dir"

    if [ -d "$cache_path" ]; then
        file_count=$(find "$cache_path" -name "*.rds" -o -name "*.html" -o -name "*.json" | wc -l)

        if [ "$file_count" -gt 0 ]; then
            echo "🗑️  Clearing $dir cache ($file_count files)..."
            rm -f "$cache_path"/*.rds "$cache_path"/*.html "$cache_path"/*.json 2>/dev/null
            total_removed=$((total_removed + file_count))
        else
            echo "✅ $dir cache already empty"
        fi
    else
        echo "⚠️  $dir directory not found"
    fi
done

echo
echo "=== CACHE CLEARING COMPLETE ==="
echo "📊 Total files removed: $total_removed"
echo

# Show current cache status
echo "📋 Current cache status:"
for dir in "${cache_dirs[@]}"; do
    cache_path="cache/$dir"
    if [ -d "$cache_path" ]; then
        remaining=$(find "$cache_path" -name "*.rds" -o -name "*.html" -o -name "*.json" | wc -l)
        echo "   $dir: $remaining files"
    fi
done

echo
echo "✨ Cache cleared successfully!"