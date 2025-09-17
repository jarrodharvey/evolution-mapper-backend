#!/bin/bash

# Image Trace Report - Complete Image Pipeline Analysis
# Traces the complete workflow: Override → Wikimedia → Unsplash → Pixabay → PhyloPic
# Includes ChatGPT common name generation and topic filtering analysis
#
# Usage: ./sh/image_trace.sh [taxonomic_group_name]
# Example: ./sh/image_trace.sh "Amniota"

# Parse command line arguments
target_taxonomic_group=""
if [[ $# -gt 0 ]]; then
    target_taxonomic_group="$1"
fi

echo "=== IMAGE TRACE REPORT ==="
if [[ -n "$target_taxonomic_group" ]]; then
    echo "🎯 Filtering results for taxonomic group: $target_taxonomic_group"
fi
echo ""
echo "📊 Parsing Override → Wikimedia → Unsplash → Pixabay → PhyloPic workflow from logs/"
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
    retry_count=$(echo "$retry_count" | head -1 | tr -d '\n')
    if [[ "$retry_count" -gt "0" ]]; then
        echo "  🔄 Retries: $retry_count (validation failed, auto-retry triggered)"
    fi

    # Analyze complete image pipeline: Override → Wikimedia → Unsplash → Pixabay → PhyloPic
    echo "  🖼️  IMAGE PIPELINE ANALYSIS:"

    # Check for image override (highest priority)
    override_found=$(grep -c "override image available for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
    override_found=$(echo "$override_found" | head -1 | tr -d '\n')
    if [[ "$override_found" -gt "0" ]]; then
        echo "  🎯 Result: Override image used (highest priority)"
    else
        # Check for Wikimedia search and results (second priority)
        wikimedia_search=$(grep -c "Fetching Wikimedia image for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
        wikimedia_search=$(echo "$wikimedia_search" | head -1 | tr -d '\n')
        wikimedia_success=$(grep -c "Wikimedia image success for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
        wikimedia_success=$(echo "$wikimedia_success" | head -1 | tr -d '\n')
        wikimedia_failed=$(grep -c "Wikimedia image failed for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
        wikimedia_failed=$(echo "$wikimedia_failed" | head -1 | tr -d '\n')

        if [[ "$wikimedia_search" -gt "0" ]]; then
            if [[ "$wikimedia_success" -gt "0" ]]; then
                # Extract the ChatGPT species name for this taxonomic group
                chatgpt_species_name=""
                if [[ -n "$latest_response" ]]; then
                    chatgpt_species_name="$latest_response"
                fi

                # Extract Wikipedia article information using the species name (case-insensitive)
                wikimedia_entity_info=""
                if [[ -n "$chatgpt_species_name" ]]; then
                    wikimedia_entity_info=$(grep -i "\\[WIKIMEDIA\\] Entity:.*$chatgpt_species_name" logs/api.log 2>/dev/null | tail -1 | sed -n 's/.*Entity: \(.*\)/\1/p')
                fi

                if [[ -n "$wikimedia_entity_info" ]]; then
                    # Parse entity ID and label from format like "Q147128 - edible dormouse"
                    entity_id=$(echo "$wikimedia_entity_info" | cut -d' ' -f1)
                    entity_label=$(echo "$wikimedia_entity_info" | cut -d'-' -f2- | sed 's/^ *//')
                    if [[ -n "$entity_label" ]]; then
                        echo "  🌐 Result: Wikimedia image used from Wikipedia article: \"$entity_label\" ($entity_id)"
                    else
                        echo "  🌐 Result: Wikimedia image used (Wikipedia Commons) - Entity: $entity_id"
                    fi
                else
                    echo "  🌐 Result: Wikimedia image used (Wikipedia Commons)"
                fi
            else
                echo "  🌐 Wikimedia search attempted but failed"

                # Continue to Unsplash analysis
                if [[ -n "$latest_response" ]]; then
                    # Search for Unsplash API calls with this common name
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
                        echo "  📸 Unsplash Search: $unsplash_results results returned"

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
                            if [[ "$unsplash_results" -gt "0" ]]; then
                                filter_ratio=$(echo "scale=1; $unsplash_filtered * 100 / $unsplash_results" | bc -l 2>/dev/null || echo "N/A")
                                if [[ "$filter_ratio" != "N/A" ]]; then
                                    echo "  📊 Filter Success Rate: ${filter_ratio}%"
                                fi
                            fi
                        fi

                        # Check for further fallbacks
                        pixabay_success=$(grep -c "Pixabay image success for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
                        pixabay_success=$(echo "$pixabay_success" | head -1 | tr -d '\n')
                        phylopic_success=$(grep -c "PhyloPic.*success.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
                        phylopic_success=$(echo "$phylopic_success" | head -1 | tr -d '\n')

                        if [[ "$pixabay_success" -gt "0" ]]; then
                            echo "  🎨 Result: Pixabay image used (tertiary fallback)"
                        elif [[ "$phylopic_success" -gt "0" ]]; then
                            echo "  🦕 Result: PhyloPic used (final fallback)"
                        else
                            echo "  📸 Result: Unsplash image selected"
                        fi
                    else
                        # No Unsplash data, check other sources
                        pixabay_success=$(grep -c "Pixabay image success for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
                        phylopic_success=$(grep -c "PhyloPic.*success.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")

                        if [[ "$pixabay_success" -gt "0" ]]; then
                            echo "  🎨 Result: Pixabay image used (Unsplash skipped)"
                        elif [[ "$phylopic_success" -gt "0" ]]; then
                            echo "  🦕 Result: PhyloPic used (final fallback)"
                        else
                            echo "  ⚠️  No image source data found"
                        fi
                    fi
                fi
            fi
        else
            # No Wikimedia search, go directly to other sources analysis
            if [[ -n "$latest_response" ]]; then
                # Search for Unsplash API calls with this common name
                search_context=$(grep -n "Using common name for search: $latest_response" logs/api.log | tail -1 | cut -d: -f1)
                if [[ -n "$search_context" ]]; then
                    unsplash_results=$(sed -n "${search_context},$(($search_context + 10))p" logs/api.log | grep "Found.*total results" | head -1 | sed -n 's/.*Found \([0-9]*\) total results.*/\1/p')
                    unsplash_filtered=$(sed -n "${search_context},$(($search_context + 10))p" logs/api.log | grep "Found.*total results" | head -1 | sed -n 's/.*Found [0-9]* total results, \([0-9]*\) match topic filters.*/\1/p')

                    if [[ -n "$unsplash_results" && "$unsplash_results" != "" ]]; then
                        echo "  📸 Unsplash Search: $unsplash_results results returned"

                        if [[ -n "$unsplash_filtered" && "$unsplash_filtered" != "" ]]; then
                            echo "  🎯 Topic Filtering: $unsplash_filtered nature/wildlife matches"
                            if [[ "$unsplash_results" -gt "0" ]]; then
                                filter_ratio=$(echo "scale=1; $unsplash_filtered * 100 / $unsplash_results" | bc -l 2>/dev/null || echo "N/A")
                                if [[ "$filter_ratio" != "N/A" ]]; then
                                    echo "  📊 Filter Success Rate: ${filter_ratio}%"
                                fi
                            fi
                        fi
                        echo "  📸 Result: Unsplash image selected"
                    fi
                fi
            fi
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
total_summary=$(echo "$total_summary" | head -1 | tr -d '\n')
total_common_name=$(grep -c "\\[CHATGPT-COMMON_NAME\\]" logs/chatgpt.log 2>/dev/null || echo "0")
total_common_name=$(echo "$total_common_name" | head -1 | tr -d '\n')
total_selection=$(grep -c "\\[CHATGPT-SELECTION\\]" logs/chatgpt.log 2>/dev/null || echo "0")
total_selection=$(echo "$total_selection" | head -1 | tr -d '\n')
total_tokens=$(grep "tokens:" logs/chatgpt.log 2>/dev/null | sed -n 's/.*tokens: \([0-9]*\).*/\1/p' | awk '{sum+=$1} END {print sum+0}')
total_tokens=$(echo "$total_tokens" | head -1 | tr -d '\n')
total_retries=$(grep -c "CHATGPT-RETRY" logs/chatgpt.log 2>/dev/null || echo "0")
total_retries=$(echo "$total_retries" | head -1 | tr -d '\n')
total_overrides=$(grep -c "override image available" logs/api.log 2>/dev/null || echo "0")
total_overrides=$(echo "$total_overrides" | head -1 | tr -d '\n')
total_wikimedia=$(grep -c "Wikimedia image success" logs/api.log 2>/dev/null || echo "0")
total_wikimedia=$(echo "$total_wikimedia" | head -1 | tr -d '\n')
total_unsplash=$(grep -c "Unsplash.*success" logs/api.log 2>/dev/null || echo "0")
total_unsplash=$(echo "$total_unsplash" | head -1 | tr -d '\n')
total_pixabay=$(grep -c "Pixabay.*success" logs/api.log 2>/dev/null || echo "0")
total_pixabay=$(echo "$total_pixabay" | head -1 | tr -d '\n')
total_phylopic=$(grep -c "PhyloPic.*success" logs/api.log 2>/dev/null || echo "0")
total_phylopic=$(echo "$total_phylopic" | head -1 | tr -d '\n')

echo "  Taxonomic groups processed: $group_count"
echo "  Summary operations: $total_summary"
echo "  Common name operations: $total_common_name"
echo "  Selection operations: $total_selection"
echo "  Total tokens used: $total_tokens"
echo "  Validation retries: $total_retries"
echo "  Override images used: $total_overrides"
echo "  Wikimedia images used: $total_wikimedia"
echo "  Unsplash images used: $total_unsplash"
echo "  Pixabay images used: $total_pixabay"
echo "  PhyloPic silhouettes used: $total_phylopic"

# Image source breakdown
total_images=$((total_overrides + total_wikimedia + total_unsplash + total_pixabay + total_phylopic))
if [[ "$total_images" -gt "0" ]]; then
    echo ""
    echo "🖼️  IMAGE SOURCE BREAKDOWN:"

    if [[ "$total_overrides" -gt "0" ]]; then
        override_percent=$(echo "scale=1; $total_overrides * 100 / $total_images" | bc -l 2>/dev/null || echo "N/A")
        echo "  🎯 Override images: $total_overrides (${override_percent}%)"
    fi
    if [[ "$total_wikimedia" -gt "0" ]]; then
        wikimedia_percent=$(echo "scale=1; $total_wikimedia * 100 / $total_images" | bc -l 2>/dev/null || echo "N/A")
        echo "  🌐 Wikimedia images: $total_wikimedia (${wikimedia_percent}%)"
    fi
    if [[ "$total_unsplash" -gt "0" ]]; then
        unsplash_percent=$(echo "scale=1; $total_unsplash * 100 / $total_images" | bc -l 2>/dev/null || echo "N/A")
        echo "  📸 Unsplash photos: $total_unsplash (${unsplash_percent}%)"
    fi
    if [[ "$total_pixabay" -gt "0" ]]; then
        pixabay_percent=$(echo "scale=1; $total_pixabay * 100 / $total_images" | bc -l 2>/dev/null || echo "N/A")
        echo "  🎨 Pixabay photos: $total_pixabay (${pixabay_percent}%)"
    fi
    if [[ "$total_phylopic" -gt "0" ]]; then
        phylopic_percent=$(echo "scale=1; $total_phylopic * 100 / $total_images" | bc -l 2>/dev/null || echo "N/A")
        echo "  🦕 PhyloPic silhouettes: $total_phylopic (${phylopic_percent}%)"
    fi
fi

# Topic filtering effectiveness
topic_success=$(grep "topic filtering.*found [1-9]" logs/api.log | wc -l | tr -d ' ')
topic_success=$(echo "$topic_success" | head -1 | tr -d '\n')
topic_failures=$(grep "topic filtering.*found 0" logs/api.log | wc -l | tr -d ' ')
topic_failures=$(echo "$topic_failures" | head -1 | tr -d '\n')
if [[ "$topic_success" -gt "0" ]] || [[ "$topic_failures" -gt "0" ]]; then
    echo ""
    echo "🎯 TOPIC FILTERING EFFECTIVENESS:"
    total_filtering=$((topic_success + topic_failures))
    if [[ "$total_filtering" -gt "0" ]]; then
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
    echo "Current acceptable topic filter (applies to Unsplash only):"
    echo "  animals, nature, wildlife, birds, marine-life, insects, plants,"
    echo "  forest, ocean, freshwater, mountains, savanna, macro, zoology,"
    echo "  botany, ecology, aquatic-life, wild-animals"
    echo ""
    echo "Note: Override images and Wikimedia images bypass topic filtering"

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