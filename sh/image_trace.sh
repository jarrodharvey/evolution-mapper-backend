#!/bin/bash

# Image Trace Report - ChatGPT Common Names to Unsplash Image Pipeline Analysis
# Traces the complete workflow: ChatGPT → Unsplash search → Topic filtering → Image selection
#
# Usage: ./sh/image_trace.sh [--tax="taxonomic_group_name"]
# Example: ./sh/image_trace.sh --tax="Amniota"

# Parse command line arguments
target_taxonomic_group=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --tax=*)
            target_taxonomic_group="${1#*=}"
            shift
            ;;
        *)
            echo "Unknown option $1"
            echo "Usage: $0 [--tax=\"taxonomic_group_name\"]"
            exit 1
            ;;
    esac
done

echo "=== IMAGE TRACE REPORT ==="
if [[ -n "$target_taxonomic_group" ]]; then
    echo "🎯 Filtering results for taxonomic group: $target_taxonomic_group"
fi
echo ""
echo "📊 Parsing ChatGPT → Unsplash → Topic Filtering workflow from logs/"
echo ""

# Check if logs exist
if [[ ! -f "logs/chatgpt.log" ]]; then
    echo "❌ logs/chatgpt.log not found"
    exit 1
fi

if [[ ! -f "logs/api.log" ]]; then
    echo "❌ logs/api.log not found"
    exit 1
fi

# Create temporary files for processing
temp_dir=$(mktemp -d)
trap "rm -rf $temp_dir" EXIT

echo "🔍 ChatGPT Common Name Generation Results:"
echo "============================================="
echo ""

# Extract unique taxonomic groups in order of first appearance
# Extract all taxonomic groups first
grep "\\[CHATGPT-COMMON_NAME\\]" logs/chatgpt.log | grep -o 'for [A-Za-z_][A-Za-z0-9_]*' | cut -d' ' -f2 | awk '!seen[$0]++' > "$temp_dir/taxonomic_groups.txt"

group_count=0

while IFS= read -r taxonomic_group; do
    if [[ -z "$taxonomic_group" ]]; then
        continue
    fi

    # Additional case-insensitive filtering if target is specified (using tr for portability)
    if [[ -n "$target_taxonomic_group" ]]; then
        taxonomic_group_lower=$(echo "$taxonomic_group" | tr '[:upper:]' '[:lower:]')
        target_lower=$(echo "$target_taxonomic_group" | tr '[:upper:]' '[:lower:]')
        if [[ "$taxonomic_group_lower" != "$target_lower" ]]; then
            continue
        fi
    fi

    echo "🏷️  TAXONOMIC GROUP: $taxonomic_group"
    echo "----------------------------------------"
    group_count=$((group_count + 1))

    # Find the most recent prompt and response for this taxonomic group
    latest_prompt=""
    latest_response=""
    latest_timestamp=""

    # Get the latest prompt
    latest_prompt=$(grep "\\[CHATGPT-COMMON_NAME\\].*Prompt for $taxonomic_group" logs/chatgpt.log | tail -1 | sed -n 's/.*Prompt for [^ ]* : \(.*\)/\1/p')

    # Get the latest response
    latest_response=$(grep "\\[CHATGPT-COMMON_NAME\\].*Raw response for $taxonomic_group" logs/chatgpt.log | tail -1 | sed -n 's/.*Raw response for [^ ]* : \(.*\)/\1/p')

    # Extract timestamp from the most recent workflow for this taxonomic group
    if [[ -n "$latest_response" ]]; then
        latest_timestamp=$(grep "Using common name for search: $latest_response" logs/api.log | tail -1 | sed -n 's/INFO \[\([^]]*\)\].*/\1/p')
    fi

    # Display the most recent prompt and response
    if [[ -n "$latest_timestamp" ]]; then
        echo "  🕒 Most Recent Workflow: $latest_timestamp"
    fi
    if [[ -n "$latest_prompt" ]]; then
        echo "  ❓ Prompt: $latest_prompt"
    fi
    if [[ -n "$latest_response" ]]; then
        echo "  🔤 Raw response: $latest_response"
    fi

    # Check for validation failures and retries
    retry_count=$(grep -c "CHATGPT-RETRY.*$taxonomic_group" logs/chatgpt.log 2>/dev/null || echo "0")
    if [[ "$retry_count" -gt 0 ]]; then
        echo "  🔄 Retries: $retry_count (validation failed, auto-retry triggered)"
    fi

    # Look for Unsplash search results in api.log
    if [[ -n "$latest_response" ]]; then
        # Search for Unsplash API calls with this common name - need to search around the time of this search
        # Find the context where this search term was used, then get the results
        search_context=$(grep -n "Using common name for search: $latest_response" logs/api.log | tail -1 | cut -d: -f1)
        if [[ -n "$search_context" ]]; then
            # Get results from a few lines after the search context
            unsplash_results=$(sed -n "${search_context},$(($search_context + 10))p" logs/api.log | grep "Found.*total results" | head -1 | sed -n 's/.*Found \([0-9]*\) total results.*/\1/p')
            unsplash_filtered=$(sed -n "${search_context},$(($search_context + 10))p" logs/api.log | grep "Found.*total results" | head -1 | sed -n 's/.*Found [0-9]* total results, \([0-9]*\) match topic filters.*/\1/p')
        else
            unsplash_results=""
            unsplash_filtered=""
        fi

        if [[ -n "$unsplash_results" && "$unsplash_results" != "" ]]; then
            echo "  🖼️  Unsplash Search: $unsplash_results results returned"

            # Extract topics found by Unsplash for this search
            if [[ -n "$search_context" ]]; then
                topics_line=$(sed -n "${search_context},$(($search_context + 10))p" logs/api.log | grep "Unsplash topics found for.*$latest_response" | head -1)
                if [[ -n "$topics_line" ]]; then
                    topics=$(echo "$topics_line" | sed -n 's/.*: \(.*\)/\1/p')
                    echo "  🏷️  All Topics Found: $topics"
                fi
            fi

            if [[ -n "$unsplash_filtered" && "$unsplash_filtered" != "" ]]; then
                echo "  🎯 Topic Filtering: $unsplash_filtered nature/wildlife matches"
                if [[ "$unsplash_results" -gt 0 ]]; then
                    filter_ratio=$(echo "scale=1; $unsplash_filtered * 100 / $unsplash_results" | bc -l 2>/dev/null || echo "N/A")
                    if [[ "$filter_ratio" != "N/A" ]]; then
                        echo "  📊 Filter Success Rate: ${filter_ratio}%"
                    fi
                fi
            fi

            # Check for Pixabay fallback (NEW)
            pixabay_fallback=$(grep -c "Using Pixabay fallback.*$latest_response" logs/api.log 2>/dev/null || echo "0")
            phylopic_fallback=$(grep -c "PhyloPic.*fallback.*$latest_response" logs/api.log 2>/dev/null || echo "0")

            if [[ "$pixabay_fallback" -gt 0 ]]; then
                echo "  🎨 Result: Pixabay image fallback (Unsplash failed)"
            elif [[ "$phylopic_fallback" -gt 0 ]]; then
                echo "  🦕 Result: PhyloPic fallback (Unsplash and Pixabay failed)"
            else
                echo "  📸 Result: Unsplash image selected"
            fi
        else
            echo "  ⚠️  No Unsplash search data found"
        fi
    fi

    # Check for image fallbacks even when Unsplash search data is missing
    if [[ -n "$latest_response" ]]; then
        # Check for direct Pixabay or PhyloPic usage (when Unsplash completely failed or wasn't attempted)
        pixabay_direct=$(grep -c "Pixabay.*success.*$latest_response" logs/api.log 2>/dev/null || echo "0")
        phylopic_direct=$(grep -c "PhyloPic.*success.*$latest_response" logs/api.log 2>/dev/null || echo "0")
        pixabay_fallback_alt=$(grep -c "Using Pixabay.*for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
        phylopic_fallback_alt=$(grep -c "Using PhyloPic.*for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")

        if [[ "$pixabay_direct" -gt 0 || "$pixabay_fallback_alt" -gt 0 ]]; then
            echo "  🎨 Result: Pixabay image used (direct fallback)"
        elif [[ "$phylopic_direct" -gt 0 || "$phylopic_fallback_alt" -gt 0 ]]; then
            echo "  🦕 Result: PhyloPic used (final fallback)"
        fi
    fi

    # Get tokens used for this group
    tokens_used=$(grep "Final result for $taxonomic_group.*tokens:" logs/chatgpt.log | tail -1 | sed -n 's/.*tokens: \([0-9]*\).*/\1/p')
    if [[ -n "$tokens_used" ]]; then
        echo "  🔧 Tokens used: $tokens_used"
    fi
    echo ""
done < "$temp_dir/taxonomic_groups.txt"

# Summary statistics
echo ""
echo "============================================="
echo "📈 SUMMARY STATISTICS:"

total_summary=$(grep -c "\\[CHATGPT-SUMMARY\\]" logs/chatgpt.log 2>/dev/null || echo "0")
total_common_name=$(grep -c "\\[CHATGPT-COMMON_NAME\\]" logs/chatgpt.log 2>/dev/null || echo "0")
total_selection=$(grep -c "\\[CHATGPT-SELECTION\\]" logs/chatgpt.log 2>/dev/null || echo "0")
total_tokens=$(grep "tokens:" logs/chatgpt.log 2>/dev/null | sed -n 's/.*tokens: \([0-9]*\).*/\1/p' | awk '{sum+=$1} END {print sum+0}')
total_retries=$(grep -c "CHATGPT-RETRY" logs/chatgpt.log 2>/dev/null || echo "0")
total_unsplash=$(grep -c "Unsplash search" logs/api.log 2>/dev/null || echo "0")
total_pixabay=$(grep -c "Pixabay.*fallback" logs/api.log 2>/dev/null || echo "0")
total_phylopic=$(grep -c "PhyloPic.*fallback" logs/api.log 2>/dev/null || echo "0")

echo "  Taxonomic groups processed: $group_count"
echo "  Summary operations: $total_summary"
echo "  Common name operations: $total_common_name"
echo "  Selection operations: $total_selection"
echo "  Total tokens used: $total_tokens"
echo "  Validation retries: $total_retries"
echo "  Unsplash searches: $total_unsplash"
echo "  Pixabay searches: $total_pixabay"
echo "  PhyloPic fallbacks: $total_phylopic"

# Image source breakdown
if [[ "$total_unsplash" -gt 0 ]] || [[ "$total_pixabay" -gt 0 ]] || [[ "$total_phylopic" -gt 0 ]]; then
    echo ""
    echo "🖼️  IMAGE SOURCE BREAKDOWN:"
    total_images=$((total_unsplash + total_pixabay + total_phylopic))

    if [[ "$total_unsplash" -gt 0 ]]; then
        if [[ "$total_images" -gt 0 ]]; then
            unsplash_percent=$(echo "scale=1; $total_unsplash * 100 / $total_images" | bc -l 2>/dev/null || echo "N/A")
            echo "  📸 Unsplash photos: $total_unsplash (${unsplash_percent}%)"
        else
            echo "  📸 Unsplash photos: $total_unsplash"
        fi
    fi
    if [[ "$total_pixabay" -gt 0 ]]; then
        if [[ "$total_images" -gt 0 ]]; then
            pixabay_percent=$(echo "scale=1; $total_pixabay * 100 / $total_images" | bc -l 2>/dev/null || echo "N/A")
            echo "  🎨 Pixabay photos: $total_pixabay (${pixabay_percent}%)"
        else
            echo "  🎨 Pixabay photos: $total_pixabay"
        fi
    fi
    if [[ "$total_phylopic" -gt 0 ]]; then
        if [[ "$total_images" -gt 0 ]]; then
            phylopic_percent=$(echo "scale=1; $total_phylopic * 100 / $total_images" | bc -l 2>/dev/null || echo "N/A")
            echo "  🦕 PhyloPic silhouettes: $total_phylopic (${phylopic_percent}%)"
        else
            echo "  🦕 PhyloPic silhouettes: $total_phylopic"
        fi
    fi
fi

# Topic filtering effectiveness
topic_success=$(grep "topic filtering.*found [1-9]" logs/api.log | wc -l | tr -d ' ')
topic_failures=$(grep "topic filtering.*found 0" logs/api.log | wc -l | tr -d ' ')
if [[ "$topic_success" -gt 0 ]] || [[ "$topic_failures" -gt 0 ]]; then
    echo ""
    echo "🎯 TOPIC FILTERING EFFECTIVENESS:"
    total_filtering=$((topic_success + topic_failures))
    if [[ "$total_filtering" -gt 0 ]]; then
        success_rate=$(echo "scale=1; $topic_success * 100 / $total_filtering" | bc -l 2>/dev/null || echo "N/A")
        echo "  ✅ Successful filters: $topic_success"
        echo "  ❌ Failed filters: $topic_failures"
        echo "  📊 Filter success rate: ${success_rate}%"
    fi
fi

# Topic analysis across all searches
echo ""
echo "🏷️  COMPREHENSIVE TOPIC ANALYSIS:"
echo "=================================="

# Extract all unique topics found across Unsplash searches (filtered by target group if specified)
if [[ -n "$target_taxonomic_group" ]]; then
    # Get topics only for the target taxonomic group by finding its search terms
    target_search_terms=$(grep "\\[CHATGPT-COMMON_NAME\\].*Raw response for.*${target_taxonomic_group}.*:" logs/chatgpt.log | sed -n 's/.*Raw response for [^ ]* : \(.*\)/\1/p')
    all_search_topics=""
    while IFS= read -r search_term; do
        if [[ -n "$search_term" ]]; then
            topics=$(grep "Unsplash topics found for.*${search_term}.*:" logs/api.log 2>/dev/null | sed -n 's/.*: \(.*\)/\1/p' | tr ',' '\n' | sed 's/^ *//;s/ *$//')
            all_search_topics="$all_search_topics"$'\n'"$topics"
        fi
    done <<< "$target_search_terms"
    all_search_topics=$(echo "$all_search_topics" | sort -u)
else
    # Get topics from all searches
    all_search_topics=$(grep "Unsplash topics found for.*:" logs/api.log 2>/dev/null | sed -n 's/.*: \(.*\)/\1/p' | tr ',' '\n' | sed 's/^ *//;s/ *$//' | sort -u)
fi

if [[ -n "$all_search_topics" ]]; then
    echo "All topics discovered across Unsplash searches:"
    echo "$all_search_topics" | while IFS= read -r topic; do
        if [[ -n "$topic" ]]; then
            echo "  • $topic"
        fi
    done

    # Show current acceptable topics for comparison
    echo ""
    echo "Current acceptable topic filter:"
    echo "  animals, nature, wildlife, birds, marine-life, insects, plants,"
    echo "  forest, ocean, freshwater, mountains, savanna, macro, zoology,"
    echo "  botany, ecology, aquatic-life, wild-animals"

    # Suggest potentially missing topics
    echo ""
    echo "Potentially valuable topics not in current filter:"
    current_topics="animals nature wildlife birds marine-life insects plants forest ocean freshwater mountains savanna macro zoology botany ecology aquatic-life wild-animals"
    echo "$all_search_topics" | while IFS= read -r topic; do
        if [[ -n "$topic" && ! "$current_topics" =~ (^|[[:space:]])"$topic"($|[[:space:]]) ]]; then
            echo "  • $topic"
        fi
    done
else
    echo "No topic data found in api.log"
fi

echo ""
echo "✨ Image trace report generated successfully!"