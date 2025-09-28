# Required libraries
library(collapsibleTree)
library(htmlwidgets)

#' Transform hybrid network data to info panel format
#' @param network_data Hybrid network data with AgeInfo and HasAge fields
#' @return Network data compatible with info panel system
transform_hybrid_to_info_panel_format <- function(network_data) {
  # Transform hybrid data structure to info panel format
  # Convert from: from/to, AgeInfo, HasAge
  # Convert to: to/Child, Age, AgeValid, AgeSource, ValidationNotes

  info_panel_data <- data.frame(
    to = network_data$to,
    NodeType = network_data$NodeType,
    stringsAsFactors = FALSE
  )

  # Extract age from AgeInfo and convert to numeric
  info_panel_data$Age <- sapply(network_data$AgeInfo, function(age_info) {
    if (is.na(age_info) || age_info == "age unavailable") {
      return(NA_real_)
    }
    # Extract numeric value from strings like "65.2 Mya" or "~65.2 Mya"
    age_match <- regmatches(age_info, regexpr("\\d+\\.?\\d*", age_info))
    if (length(age_match) > 0) {
      return(as.numeric(age_match[1]))
    }
    return(NA_real_)
  })

  # Set validation fields based on whether we actually extracted a numeric age
  # This fixes the bug where nodes with age data in their labels were marked as invalid
  info_panel_data$AgeValid <- !is.na(info_panel_data$Age) & is.numeric(info_panel_data$Age)
  info_panel_data$AgeSource <- ifelse(!is.na(info_panel_data$Age) & is.numeric(info_panel_data$Age),
                                      "DateLife chronogram database (hybrid tree)",
                                      "Age data unavailable")
  info_panel_data$ValidationNotes <- ifelse(!is.na(info_panel_data$Age) & is.numeric(info_panel_data$Age),
                                            NA_character_,
                                            NA_character_)

  return(info_panel_data)
}

#' Create CollapsibleTree visualization for hybrid tree
#' @param network_data Network data frame with age information
#' @param request_id Optional request ID for logging correlation
#' @return HTML string for CollapsibleTree
create_hybrid_tree_visualization <- function(network_data, request_id = NULL, progress_token = NULL, expansion_speed = 750) {
  if (is.null(request_id)) {
    request_id <- "viz_create"
  }

  # Helper function to update progress if token provided
  update_progress_internal <- function(step_name, status = "completed", additional_data = NULL) {
    if (!is.null(progress_token) && progress_token != "") {
      update_progress(progress_token, step_name, status, additional_data)
    }
  }

  api_log_info(paste("[", request_id, "] Starting hybrid tree visualization creation...", sep=""))

  # Step 5.1: Calculate dynamic link lengths
  update_progress_internal("calculating_link_lengths", "in_progress",
                         list(step = "Calculating dynamic link lengths for tree layout"))
  link_length <- calculate_dynamic_link_length_hybrid(network_data)
  update_progress_internal("calculating_link_lengths", "completed")

  # Step 5.2: Prepare tree structure data
  update_progress_internal("preparing_tree_structure", "in_progress",
                         list(step = "Preparing tree structure and color mapping"))
  tree_data <- data.frame(
    Parent = network_data$from,
    Child = network_data$to,
    NodeType = network_data$NodeType,
    HasAge = network_data$HasAge,
    stringsAsFactors = FALSE
  )

  # Add color mapping using new adaptive ancestral node coloring system
  tree_data$Color <- character(nrow(tree_data))

  # Get ancestral node colors using new system
  ancestral_colors <- get_ancestral_node_color(network_data)

  # Assign colors to each row
  ancestral_idx <- 1
  for (i in 1:nrow(tree_data)) {
    node_type <- tree_data$NodeType[i]

    if (node_type %in% c("taxonomic", "ancestor")) {
      # Use new ancestral coloring system
      tree_data$Color[i] <- ancestral_colors[ancestral_idx]
      ancestral_idx <- ancestral_idx + 1
    } else {
      # Use original system for root and species
      has_age <- tree_data$HasAge[i]
      tree_data$Color[i] <- get_node_color(node_type, has_age)
    }
  }
  update_progress_internal("preparing_tree_structure", "completed")

  # Step 5.3: Create base tree widget
  update_progress_internal("creating_base_widget", "in_progress",
                         list(step = "Creating collapsibleTree widget with interactions"))
  tree_widget <- collapsibleTreeNetwork(
    tree_data,
    attribute = "NodeType",
    fill = "Color",
    fontSize = 12,
    linkLength = link_length,
    nodeSize = "leafCount",
    width = 1000,
    height = 800,
    zoomable = TRUE,
    collapsed = TRUE  # Start collapsed, will expand all with custom speed
  )
  update_progress_internal("creating_base_widget", "completed")

  # Step 5.4: Apply JavaScript enhancements for expansion behavior
  if (expansion_speed != 750) {
    update_progress_internal("applying_js_enhancements", "in_progress",
                           list(step = "Applying JavaScript enhancements for tree expansion"))
    tree_widget <- tree_widget %>%
      htmlwidgets::onRender(paste0("
        function(el, x) {
          // Override D3 transition duration for all transitions
          var originalTransition = d3.selection.prototype.transition;
          d3.selection.prototype.transition = function() {
            var transition = originalTransition.apply(this, arguments);
            var originalDuration = transition.duration;
            transition.duration = function(d) {
              if (arguments.length === 0) return originalDuration.apply(this, arguments);
              return originalDuration.call(this, ", expansion_speed, ");
            };
            return transition;
          };

          // Sequential expansion function - each node waits for previous to complete
          function expandAll() {
            var nodes = d3.select(el).selectAll('g.node').filter(function(d) {
              return d._children;
            });

            if (nodes.size() > 0) {
              // Group nodes by their depth level
              var nodesByDepth = new Map();
              nodes.each(function(d) {
                if (!nodesByDepth.has(d.depth)) {
                  nodesByDepth.set(d.depth, []);
                }
                nodesByDepth.get(d.depth).push({ element: this, data: d });
              });

              // Sort nodes within each level by their vertical position (y coordinate)
              nodesByDepth.forEach(function(levelNodes) {
                levelNodes.sort(function(a, b) {
                  return a.data.y - b.data.y; // Top to bottom ordering
                });
              });

              // Get sorted depth levels and flatten into single array
              var sortedDepths = Array.from(nodesByDepth.keys()).sort(function(a, b) {
                return a - b;
              });

              var allNodes = [];
              sortedDepths.forEach(function(depth) {
                var levelNodes = nodesByDepth.get(depth);
                allNodes = allNodes.concat(levelNodes);
              });

              // Expand nodes one by one, waiting for each to complete
              function expandNext(index) {
                if (index >= allNodes.length) {
                  // All nodes in this batch are expanded, check for more
                  setTimeout(expandAll, 100);
                  return;
                }

                var nodeInfo = allNodes[index];
                var clickEvent = new MouseEvent('click', {
                  bubbles: true,
                  cancelable: true,
                  view: window
                });
                nodeInfo.element.dispatchEvent(clickEvent);

                // Wait for this node's animation to complete before expanding next
                setTimeout(function() {
                  expandNext(index + 1);
                }, ", expansion_speed, " + 50);
              }

              // Start the sequential expansion
              expandNext(0);
            } else {
              // All nodes expanded! Restore default 750ms speed for future user interactions
              d3.selection.prototype.transition = originalTransition;
            }
          }

          // Wait for tree to render, then expand all nodes
          setTimeout(function() {
            expandAll();
          }, 500);
        }
      "))
    update_progress_internal("applying_js_enhancements", "completed")
  }

  # Step 5.5: Transform network data for info panels
  update_progress_internal("transforming_network_data", "in_progress",
                         list(step = "Transforming network data to info panel format"))
  info_panel_network_data <- transform_hybrid_to_info_panel_format(network_data)
  update_progress_internal("transforming_network_data", "completed")

  # Step 5.6: Generate info panel data (Wikipedia & PhyloPic content)
  update_progress_internal("generating_info_panels", "in_progress",
                         list(step = "Generating info panels with Wikipedia and PhyloPic data"))
  tree_data <- add_info_panel_data(tree_data, info_panel_network_data, request_id, progress_token)
  update_progress_internal("generating_info_panels", "completed")

  # Step 5.7: Generate final enhanced HTML
  update_progress_internal("generating_final_html", "in_progress",
                         list(step = "Generating final enhanced HTML with interactive features"))
  tree_html <- create_enhanced_tree_html(tree_data, info_panel_network_data, tree_widget, request_id, progress_token, expansion_speed)
  update_progress_internal("generating_final_html", "completed")

  api_log_info(paste("[", request_id, "] Hybrid tree visualization creation completed", sep=""))
  return(tree_html)
}