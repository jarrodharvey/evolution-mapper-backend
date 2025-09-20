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
# Look for the more recent pattern in API logs (both success and failure)
{
    grep "ChatGPT species name success for:" logs/api.log | sed -E 's/.*ChatGPT species name success for: ([^ ]*) .*/\1/'
    grep "ChatGPT species name failed for:" logs/api.log | sed -E 's/.*ChatGPT species name failed for: ([^ ]*) .*/\1/'
} | sort -u > "$temp_dir/taxonomic_groups.txt"

# If no results from new format, try old format
if [[ ! -s "$temp_dir/taxonomic_groups.txt" ]]; then
    grep "\\[CHATGPT-COMMON_NAME\\]" logs/chatgpt.log 2>/dev/null | grep -o 'for [A-Za-z_][A-Za-z0-9_]*' | cut -d' ' -f2 | sort -u > "$temp_dir/taxonomic_groups.txt"
fi

group_count=0

while IFS= read -r taxonomic_group || [[ -n "$taxonomic_group" ]]; do
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

    # Check for ChatGPT success first
    latest_response=$(grep "ChatGPT species name success for: $taxonomic_group" logs/api.log | tail -1 | sed -E 's/.*-> ([^(]*) \(.*/\1/' | sed 's/^ *//;s/ *$//')
    chatgpt_failed=""

    # Get timestamp from the processing log
    if [[ -n "$latest_response" ]]; then
        latest_timestamp=$(grep "ChatGPT species name success for: $taxonomic_group" logs/api.log | tail -1 | sed -n 's/INFO \[\([^]]*\)\].*/\1/p')
    else
        # Check for ChatGPT failure
        chatgpt_failed=$(grep "ChatGPT species name failed for: $taxonomic_group" logs/api.log | tail -1)
        if [[ -n "$chatgpt_failed" ]]; then
            latest_timestamp=$(echo "$chatgpt_failed" | sed -n 's/INFO \[\([^]]*\)\].*/\1/p')
            # If ChatGPT failed, the system falls back to using taxonomic name
            latest_response="(ChatGPT failed - using taxonomic name)"
        fi
    fi

    # Try old format if no results from new format
    if [[ -z "$latest_response" && -z "$chatgpt_failed" ]]; then
        latest_prompt=$(grep "\\[CHATGPT-COMMON_NAME\\].*Prompt for $taxonomic_group" logs/chatgpt.log 2>/dev/null | tail -1 | sed -n 's/.*Prompt for [^ ]* : \(.*\)/\1/p')
        latest_response=$(grep "\\[CHATGPT-COMMON_NAME\\].*Raw response for $taxonomic_group" logs/chatgpt.log 2>/dev/null | tail -1 | sed -n 's/.*Raw response for [^ ]* : \(.*\)/\1/p')
        if [[ -n "$latest_response" ]]; then
            latest_timestamp=$(grep "Using common name for search: $latest_response" logs/api.log 2>/dev/null | tail -1 | sed -n 's/INFO \[\([^]]*\)\].*/\1/p')
        fi
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

    # Check for image override (highest priority) - this still uses taxonomic group name
    override_found=$(grep -c "override image available.*$taxonomic_group\|No override image for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
    override_found=$(echo "$override_found" | head -1 | tr -d '\n')
    override_success=$(grep -c "Override image found for.*$taxonomic_group" logs/api.log 2>/dev/null || echo "0")
    override_success=$(echo "$override_success" | head -1 | tr -d '\n')

    if [[ "$override_success" -gt "0" ]]; then
        echo "  🎯 Result: Override image used (highest priority)"
    else
        # Show override was checked but not found
        if [[ "$override_found" -gt "0" ]]; then
            echo "  🎯 Override: Not found"
        fi

        # For subsequent searches, we need to look for the ChatGPT species name
        search_species_name="$latest_response"

        # Trace the complete image pipeline for this taxonomic group
        # Look for the processing logs in chronological order

        echo "  📋 Step-by-step analysis:"

        # Step 1: ChatGPT species name generation
        if [[ -n "$chatgpt_failed" ]]; then
            # Extract failure reason from the log
            failure_reason=$(echo "$chatgpt_failed" | sed -E 's/.*ChatGPT species name failed for: [^-]* - (.*)/\1/' | head -1)
            if [[ -n "$failure_reason" && "$failure_reason" != "$chatgpt_failed" ]]; then
                if [[ "$failure_reason" == *"glue"* ]]; then
                    echo "    ❌ ChatGPT: FAILED - Logging error (ChatGPT response contained special characters)"
                    echo "        💡 Note: Check logs for sanitized version of the response"
                    # Try to find the sanitized response in logs
                    sanitized_response=$(grep "\\[CHATGPT-COMMON_NAME\\] Raw response for $taxonomic_group (sanitized):" logs/chatgpt.log | tail -1 | sed 's/.*: //')
                    if [[ -n "$sanitized_response" ]]; then
                        echo "        📝 Sanitized response: \"$sanitized_response\""
                    fi
                else
                    echo "    ❌ ChatGPT: FAILED - $failure_reason"
                fi
            else
                echo "    ❌ ChatGPT: FAILED (falling back to taxonomic name)"
            fi
            search_species_name="$taxonomic_group"  # Use taxonomic name as fallback
        elif [[ -n "$search_species_name" && "$search_species_name" != "(ChatGPT failed - using taxonomic name)" ]]; then
            echo "    ✅ ChatGPT: $taxonomic_group → \"$search_species_name\""
        else
            echo "    ❌ ChatGPT: Failed to generate species name"
            echo "  ⚠️  Cannot trace image pipeline without species name"
            return
        fi

        # Step 2: Check each image source in order using more precise log analysis
        # Look for processing patterns that are more accurate to actual logs

        # Wikimedia processing (searches using species name from ChatGPT, or scientific name from database lookup)
        # Filter by timestamp to get results from the same request as the ChatGPT response
        if [[ -n "$latest_timestamp" ]]; then
            # Extract date for filtering (YYYY-MM-DD format)
            filter_date=$(echo "$latest_timestamp" | cut -d' ' -f1)
            wikimedia_fetch_common=$(grep "$filter_date" logs/api.log | grep "Fetching Wikimedia image for: $search_species_name")
            wikimedia_fetch_scientific=$(grep "$filter_date" logs/api.log | grep "using scientific name from database lookup of $search_species_name")
            wikimedia_success=$(grep "$filter_date" logs/api.log | grep "Wikimedia image success for: $taxonomic_group")
            wikimedia_failed=$(grep "$filter_date" logs/api.log | grep "Wikimedia image failed for: $taxonomic_group")
            wikimedia_skipped_db=$(grep "$filter_date" logs/api.log | grep "Skipping Wikimedia - no scientific name found in database for: $search_species_name")
        else
            # Fallback to unfiltered search if no timestamp available
            wikimedia_fetch_common=$(grep "Fetching Wikimedia image for: $search_species_name" logs/api.log)
            wikimedia_fetch_scientific=$(grep "using scientific name from database lookup of $search_species_name" logs/api.log)
            wikimedia_success=$(grep "Wikimedia image success for: $taxonomic_group" logs/api.log)
            wikimedia_failed=$(grep "Wikimedia image failed for: $taxonomic_group" logs/api.log)
            wikimedia_skipped_db=$(grep "Skipping Wikimedia - no scientific name found in database for: $search_species_name" logs/api.log)
        fi

        if [[ -n "$wikimedia_skipped_db" ]]; then
            echo "    ⏭️  Wikimedia: Skipped - no scientific name found in database for \"$search_species_name\""
        elif [[ -n "$wikimedia_fetch_common" ]] || [[ -n "$wikimedia_fetch_scientific" ]] || [[ -n "$wikimedia_failed" ]]; then
            if [[ -n "$wikimedia_success" ]]; then
                if [[ -n "$wikimedia_fetch_scientific" ]]; then
                    # Extract the scientific name used from the log
                    scientific_name=$(echo "$wikimedia_fetch_scientific" | head -1 | sed -E 's/.*Fetching Wikimedia image for: ([^(]*) \(using scientific name from database lookup.*/\1/' | xargs)
                    echo "    ✅ Wikimedia: SUCCESS using scientific name \"$scientific_name\" from database lookup of \"$search_species_name\""
                else
                    # Extract Wikipedia article info from entity search
                    wikimedia_entity_info=$(grep "\\[WIKIMEDIA\\] Top result:" logs/api.log | grep -A2 -B2 "$search_species_name" | tail -1 | sed 's/.*Top result: //')
                    if [[ -n "$wikimedia_entity_info" && "$wikimedia_entity_info" != "NA - NA - NA" ]]; then
                        echo "    ✅ Wikimedia: SUCCESS → Wikipedia entity: $wikimedia_entity_info"
                    else
                        echo "    ✅ Wikimedia: SUCCESS → Wikipedia Commons"
                    fi
                fi
            elif [[ -n "$wikimedia_failed" ]]; then
                failure_reason=$(echo "$wikimedia_failed" | sed -E 's/.*Wikimedia image failed for: [^-]* - (.*)/\1/')
                echo "    ❌ Wikimedia: FAILED for \"$search_species_name\" - $failure_reason"
            else
                echo "    ❓ Wikimedia: Search attempted (result unclear)"
            fi
        else
            echo "    ⏭️  Wikimedia: Not attempted (possibly skipped)"
        fi

        # Unsplash processing (searches using species name from ChatGPT)
        # Filter by timestamp to get results from the same request
        if [[ -n "$latest_timestamp" ]]; then
            unsplash_fetch=$(grep "$filter_date" logs/api.log | grep "Fetching Unsplash image for: $search_species_name")
            unsplash_success=$(grep "$filter_date" logs/api.log | grep "Unsplash image success for: $taxonomic_group")
            unsplash_failed=$(grep "$filter_date" logs/api.log | grep "Unsplash image failed for: $taxonomic_group")
            unsplash_skipped=$(grep "$filter_date" logs/api.log | grep "Skipping Unsplash.*$taxonomic_group")
            unsplash_topics=$(grep "$filter_date" logs/api.log | grep "Unsplash topics found for $search_species_name :" | tail -1 | sed -n 's/.*: \(.*\)/\1/p')
            unsplash_no_topics=$(grep "$filter_date" logs/api.log | grep "No topic_submissions found in Unsplash results for $search_species_name")
        else
            unsplash_fetch=$(grep "Fetching Unsplash image for: $search_species_name" logs/api.log)
            unsplash_success=$(grep "Unsplash image success for: $taxonomic_group" logs/api.log)
            unsplash_failed=$(grep "Unsplash image failed for: $taxonomic_group" logs/api.log)
            unsplash_skipped=$(grep "Skipping Unsplash.*$taxonomic_group" logs/api.log)
            unsplash_topics=$(grep "Unsplash topics found for $search_species_name :" logs/api.log | tail -1 | sed -n 's/.*: \(.*\)/\1/p')
            unsplash_no_topics=$(grep "No topic_submissions found in Unsplash results for $search_species_name" logs/api.log)
        fi
        if [[ -z "$unsplash_topics" ]]; then
            # If no direct match, look for topics in the same request context
            request_context=$(grep -B 5 -A 5 "$taxonomic_group" logs/api.log | grep "Unsplash topics found for.*:" | tail -1 | sed -n 's/.*: \(.*\)/\1/p')
            if [[ -n "$request_context" ]]; then
                unsplash_topics="$request_context"
            fi
        fi

        if [[ -n "$unsplash_skipped" ]]; then
            echo "    ⏭️  Unsplash: Skipped (higher priority source succeeded)"
        elif [[ -n "$wikimedia_skipped_db" && -n "$unsplash_fetch" ]]; then
            if [[ -n "$unsplash_success" ]]; then
                echo "    ✅ Unsplash: SUCCESS for \"$search_species_name\" (fallback from failed database lookup)"
                if [[ -n "$unsplash_topics" ]]; then
                    echo "         📋 Topics found: $unsplash_topics"
                fi
            elif [[ -n "$unsplash_failed" ]]; then
                if [[ -n "$unsplash_no_topics" ]]; then
                    echo "    ❌ Unsplash: FAILED for \"$search_species_name\" (fallback from failed database lookup) - No topic_submissions found"
                elif [[ -n "$unsplash_topics" ]]; then
                    echo "    ❌ Unsplash: FAILED for \"$search_species_name\" (fallback from failed database lookup) - No photos matched topic filters"
                    echo "         📋 Topics found: $unsplash_topics"
                else
                    echo "    ❌ Unsplash: FAILED for \"$search_species_name\" (fallback from failed database lookup)"
                fi
            else
                echo "    ❓ Unsplash: Search attempted (fallback from failed database lookup)"
                if [[ -n "$unsplash_topics" ]]; then
                    echo "         📋 Topics found: $unsplash_topics"
                fi
            fi
        elif [[ -n "$unsplash_fetch" ]] || [[ -n "$unsplash_success" ]] || [[ -n "$unsplash_failed" ]]; then
            if [[ -n "$unsplash_success" ]]; then
                echo "    ✅ Unsplash: SUCCESS for \"$search_species_name\""
                if [[ -n "$unsplash_topics" ]]; then
                    echo "         📋 Topics found: $unsplash_topics"
                fi
            elif [[ -n "$unsplash_failed" ]]; then
                if [[ -n "$unsplash_no_topics" ]]; then
                    echo "    ❌ Unsplash: FAILED for \"$search_species_name\" - No topic_submissions found (photos have no biological topics)"
                elif [[ -n "$unsplash_topics" ]]; then
                    echo "    ❌ Unsplash: FAILED for \"$search_species_name\" - No photos matched topic filters"
                    echo "         📋 Topics found: $unsplash_topics"
                    echo "         📋 Acceptable topics: animals, nature, wildlife, birds, marine-life, insects, plants, forest, ocean, freshwater, mountains, savanna, macro, zoology, botany, ecology, aquatic-life, wild-animals"
                else
                    echo "    ❌ Unsplash: FAILED for \"$search_species_name\" - Topic filtering failed"
                fi
            else
                echo "    ❓ Unsplash: Search attempted (result unclear)"
                if [[ -n "$unsplash_topics" ]]; then
                    echo "         📋 Topics found: $unsplash_topics"
                fi
            fi
        else
            echo "    ⏭️  Unsplash: Not attempted"
        fi

        # Pixabay processing (searches using species name from ChatGPT)
        if [[ -n "$latest_timestamp" ]]; then
            pixabay_fetch=$(grep "$filter_date" logs/api.log | grep "Fetching Pixabay image for: $search_species_name")
            pixabay_success=$(grep "$filter_date" logs/api.log | grep "Pixabay image success for: $taxonomic_group")
            pixabay_failed=$(grep "$filter_date" logs/api.log | grep "Pixabay image failed for: $taxonomic_group")
            pixabay_skipped=$(grep "$filter_date" logs/api.log | grep "Skipping Pixabay.*$taxonomic_group")
        else
            pixabay_fetch=$(grep "Fetching Pixabay image for: $search_species_name" logs/api.log)
            pixabay_success=$(grep "Pixabay image success for: $taxonomic_group" logs/api.log)
            pixabay_failed=$(grep "Pixabay image failed for: $taxonomic_group" logs/api.log)
            pixabay_skipped=$(grep "Skipping Pixabay.*$taxonomic_group" logs/api.log)
        fi

        if [[ -n "$pixabay_skipped" ]]; then
            echo "    ⏭️  Pixabay: Skipped (higher priority source succeeded)"
        elif [[ -n "$pixabay_fetch" ]] || [[ -n "$pixabay_success" ]] || [[ -n "$pixabay_failed" ]]; then
            if [[ -n "$pixabay_success" ]]; then
                echo "    ✅ Pixabay: SUCCESS for \"$search_species_name\""
            elif [[ -n "$pixabay_failed" ]]; then
                echo "    ❌ Pixabay: FAILED for \"$search_species_name\""
            else
                echo "    ❓ Pixabay: Search attempted (result unclear)"
            fi
        else
            echo "    ⏭️  Pixabay: Not attempted"
        fi

        # PhyloPic processing (uses original taxonomic name, not species name)
        if [[ -n "$latest_timestamp" ]]; then
            phylopic_fetch=$(grep "$filter_date" logs/api.log | grep "Fetching PhyloPic data for: $taxonomic_group")
            phylopic_success=$(grep "$filter_date" logs/api.log | grep "PhyloPic.*success.*for: $taxonomic_group")
            phylopic_failed=$(grep "$filter_date" logs/api.log | grep "PhyloPic.*failed.*for: $taxonomic_group")
            phylopic_skipped=$(grep "$filter_date" logs/api.log | grep "Skipping PhyloPic.*$taxonomic_group")
        else
            phylopic_fetch=$(grep "Fetching PhyloPic data for: $taxonomic_group" logs/api.log)
            phylopic_success=$(grep "PhyloPic.*success.*for: $taxonomic_group" logs/api.log)
            phylopic_failed=$(grep "PhyloPic.*failed.*for: $taxonomic_group" logs/api.log)
            phylopic_skipped=$(grep "Skipping PhyloPic.*$taxonomic_group" logs/api.log)
        fi

        if [[ -n "$phylopic_skipped" ]]; then
            echo "    ⏭️  PhyloPic: Skipped (other image source succeeded)"
        elif [[ -n "$phylopic_fetch" ]] || [[ -n "$phylopic_success" ]] || [[ -n "$phylopic_failed" ]]; then
            if [[ -n "$phylopic_success" ]]; then
                echo "    ✅ PhyloPic: SUCCESS for \"$taxonomic_group\""
            elif [[ -n "$phylopic_failed" ]]; then
                echo "    ❌ PhyloPic: FAILED for \"$taxonomic_group\""
            else
                echo "    ❓ PhyloPic: Search attempted (result unclear)"
            fi
        else
            echo "    ⏭️  PhyloPic: Not attempted"
        fi

        # Determine final result
        echo ""
        echo "  🏆 FINAL RESULT:"
        if [[ "$override_success" -gt "0" ]]; then
            echo "    🎯 Override image used"
        elif [[ -n "$wikimedia_success" ]]; then
            echo "    🌐 Wikimedia image used"
        elif [[ -n "$unsplash_success" ]]; then
            echo "    📸 Unsplash image used"
        elif [[ -n "$pixabay_success" ]]; then
            echo "    🎨 Pixabay image used"
        elif [[ -n "$phylopic_success" ]]; then
            echo "    🦕 PhyloPic silhouette used"
        else
            echo "    ❌ No image source succeeded"
        fi

    fi

    # Get tokens used for this group
    tokens_used=$(grep "Final result for $taxonomic_group.*tokens:" logs/chatgpt.log | tail -1 | sed -n 's/.*tokens: \([0-9]*\) ).*/\1/p')
    if [[ -n "$tokens_used" && "$tokens_used" != "" ]]; then
        echo "  🔧 Tokens used: $tokens_used"
    fi

    echo ""
    echo ""
done < "$temp_dir/taxonomic_groups.txt"

# Only show summary statistics if no specific taxonomic group filter is applied
if [[ -z "$target_taxonomic_group" ]]; then
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

echo "  Taxonomic groups processed: $(echo "$taxonomic_groups" | wc -w | tr -d ' ')"
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

echo ""
echo "🖼️  IMAGE SOURCE BREAKDOWN:"
total_images=$((total_overrides + total_wikimedia + total_unsplash + total_pixabay + total_phylopic))
if [[ "$total_images" -gt "0" ]]; then
    override_pct=$(echo "scale=1; $total_overrides * 100 / $total_images" | bc -l 2>/dev/null || echo "0")
    wikimedia_pct=$(echo "scale=1; $total_wikimedia * 100 / $total_images" | bc -l 2>/dev/null || echo "0")
    unsplash_pct=$(echo "scale=1; $total_unsplash * 100 / $total_images" | bc -l 2>/dev/null || echo "0")
    pixabay_pct=$(echo "scale=1; $total_pixabay * 100 / $total_images" | bc -l 2>/dev/null || echo "0")
    phylopic_pct=$(echo "scale=1; $total_phylopic * 100 / $total_images" | bc -l 2>/dev/null || echo "0")

    echo "  🎯 Override images: $total_overrides (${override_pct}%)"
    echo "  🌐 Wikimedia images: $total_wikimedia (${wikimedia_pct}%)"
    echo "  📸 Unsplash photos: $total_unsplash (${unsplash_pct}%)"
    echo "  🎨 Pixabay photos: $total_pixabay (${pixabay_pct}%)"
    echo "  🦕 PhyloPic silhouettes: $total_phylopic (${phylopic_pct}%)"
else
    echo "  No image data found"
fi
fi

# Only show topic analysis if no specific taxonomic group filter is applied
if [[ -z "$target_taxonomic_group" ]]; then
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
fi

echo ""
echo "✨ Image trace report generated successfully!"
