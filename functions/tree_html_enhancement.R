# Tree HTML Enhancement Functions
# Shared functions for adding info panel system to phylogenetic tree HTML
# Eliminates code duplication across tree generation functions

library(jsonlite)
source("functions/info_panel_system.R")

#' Add info panel data to tree data frame
#' @param tree_data Data frame with Parent, Child columns for collapsibleTreeNetwork
#' @param network_data Network data with node information for info panels
#' @param request_id Optional request ID for logging correlation
#' @return Enhanced tree_data with InfoPanel column
add_info_panel_data <- function(tree_data, network_data, request_id = NULL) {
  # Create info panel data for ancestor nodes using parallel processing
  info_panels <- create_info_panel_data_parallel(network_data, request_id = request_id)
  
  # Add InfoPanel column to tree data
  tree_data$InfoPanel <- info_panels
  
  return(tree_data)
}

#' Create JavaScript object mapping node names to info panel HTML
#' @param network_data Network data with node information
#' @param request_id Optional request ID for logging correlation
#' @return JSON string for JavaScript
create_info_panel_data_js <- function(network_data, request_id = NULL) {
  
  # Create info panel data for all nodes using parallel processing
  info_panels <- create_info_panel_data_parallel(network_data, request_id = request_id)
  
  # Create mapping of node names to info panel HTML
  node_names <- if ("Child" %in% names(network_data)) {
    network_data$Child
  } else if ("to" %in% names(network_data)) {
    network_data$to
  } else {
    paste("Node", 1:nrow(network_data))
  }
  
  # Create named list
  panel_mapping <- setNames(as.list(info_panels), node_names)
  
  # Use jsonlite with proper escaping
  # First convert HTML to base64 to avoid all escaping issues
  encoded_mapping <- list()
  for (i in 1:length(panel_mapping)) {
    node_name <- names(panel_mapping)[i]
    panel_html <- panel_mapping[[i]]
    
    if (panel_html != "") {
      # Encode HTML as base64 to avoid any escaping issues
      encoded_html <- base64enc::base64encode(charToRaw(panel_html))
      encoded_mapping[[node_name]] <- encoded_html
    } else {
      encoded_mapping[[node_name]] <- ""
    }
  }
  
  # Convert to JSON safely
  json_data <- jsonlite::toJSON(encoded_mapping, auto_unbox = TRUE)
  
  return(json_data)
}

#' Create JavaScript object mapping using cached info panel data (no duplicate API calls)
#' @param network_data Network data with node information
#' @param cached_info_panels Pre-generated info panel data from create_info_panel_data_parallel()
#' @return JSON string for JavaScript
create_info_panel_data_js_with_cache <- function(network_data, cached_info_panels) {
  
  # Create mapping of node names to cached info panel HTML (no API calls)
  node_names <- if ("Child" %in% names(network_data)) {
    network_data$Child
  } else if ("to" %in% names(network_data)) {
    network_data$to
  } else {
    paste("Node", 1:nrow(network_data))
  }
  
  # Create named list using cached data
  panel_mapping <- setNames(as.list(cached_info_panels), node_names)
  
  # Use jsonlite with proper escaping
  # First convert HTML to base64 to avoid all escaping issues
  encoded_mapping <- list()
  for (i in 1:length(panel_mapping)) {
    node_name <- names(panel_mapping)[i]
    panel_html <- panel_mapping[[i]]
    
    if (panel_html != "") {
      # Encode HTML as base64 to avoid any escaping issues
      encoded_html <- base64enc::base64encode(charToRaw(panel_html))
      encoded_mapping[[node_name]] <- encoded_html
    } else {
      encoded_mapping[[node_name]] <- ""
    }
  }
  
  # Convert to JSON safely
  json_data <- jsonlite::toJSON(encoded_mapping, auto_unbox = TRUE)
  
  return(json_data)
}

#' Enhance tree HTML with info panel system
#' @param tree_html HTML string from collapsibleTreeNetwork
#' @param network_data Network data with node information
#' @return Enhanced HTML with CSS and JavaScript for info panel system
enhance_tree_html_with_info_panels <- function(tree_html, network_data) {
  
  # Add layout CSS and info panel CSS
  layout_css <- "<style>body { margin: 0 !important; padding: 0 !important; overflow: hidden !important; } html { margin: 0 !important; padding: 0 !important; } #htmlwidget_container { margin: 0 !important; padding: 0 !important; }</style>"
  
  # Generate info panel CSS with the text wrapping improvements
  info_panel_css <- paste0("<style>", generate_info_panel_css(), "</style>")
  
  # Combine both CSS sections
  combined_css <- paste0(layout_css, info_panel_css)
  tree_html <- gsub("</head>", paste0(combined_css, "</head>"), tree_html)
  
  # Create JavaScript data structure with info panel mappings
  info_panel_data_js <- create_info_panel_data_js(network_data)
  
  # Add info panel system to the tree with proper event delegation
  info_panel_script <- paste0('<script>
  // Info panel data mapping (base64 encoded to avoid escaping issues)
  var infoPanelDataEncoded = ', info_panel_data_js, ';
  
  // Decode base64 data with proper UTF-8 handling
  var infoPanelData = {};
  for (var key in infoPanelDataEncoded) {
    if (infoPanelDataEncoded[key] && infoPanelDataEncoded[key] !== "") {
      try {
        // Use proper UTF-8 decoding instead of basic atob()
        var binaryString = atob(infoPanelDataEncoded[key]);
        var bytes = new Uint8Array(binaryString.length);
        for (var i = 0; i < binaryString.length; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }
        infoPanelData[key] = new TextDecoder("utf-8").decode(bytes);
      } catch (e) {
        console.error("Failed to decode panel data for", key, ":", e);
        infoPanelData[key] = "Error: Could not decode content.";
      }
    } else {
      infoPanelData[key] = "";
    }
  }
  
  // Info panel integration with collapsibleTree using event delegation
  setTimeout(function() {
    console.log("=== INFO PANEL DEBUG START ===");
    console.log("Info panel data keys:", Object.keys(infoPanelData));
    console.log("Available nodes:", d3.selectAll(".collapsibleTree .node").size());
    
    // Remove existing tooltips
    d3.selectAll(".tooltip").remove();
    
    // Disable original tooltip functionality  
    d3.selectAll(".collapsibleTree .node").on("mouseover.tooltip", null).on("mouseout.tooltip", null);
    
    // Function to add info icons to nodes (called initially and after expansions)
    function addInfoIcons() {
      d3.selectAll(".collapsibleTree .node").each(function() {
        var nodeElement = this;
        var node = d3.select(nodeElement);
        
        // Skip if already has info icon
        if (node.select(".text-with-info").size() > 0) {
          return;
        }
        
        var nodeData = node.datum();
        var nodeName = nodeData && nodeData.data ? nodeData.data.name : null;
        
        console.log("Processing node:", nodeName, "Full nodeData:", nodeData);
        
        // Check if we have info panel data for this node
        var infoPanelHtml = infoPanelData[nodeName];
        if (infoPanelHtml && infoPanelHtml.trim() !== "") {
          console.log("Adding info icon to:", nodeName);
          
          // Find the text element
          var textElement = node.select("text");
          if (!textElement.empty()) {
            try {
              // Position (i) symbol ABOVE the node text, not to the right
              var iconY = -20; // Position above the node
              
              // Add ONLY the (i) text symbol - NO white circle background
              var infoIcon = node.append("text")
                .attr("class", "info-icon-text")
                .attr("x", 0) // Center horizontally with the node
                .attr("y", iconY)
                .attr("dy", "0.35em")
                .attr("text-anchor", "middle")
                .attr("fill", "#3498db") // Blue color for visibility
                .attr("font-size", "14px") // Slightly larger for better visibility
                .attr("font-weight", "bold")
                .style("cursor", "pointer")
                .text("ⓘ");
                
              // Store panel data on the icon for click handling
              infoIcon.datum({panelHtml: infoPanelHtml, nodeName: nodeName});
              
              console.log("Successfully added icon to:", nodeName);
            } catch (e) {
              console.error("Error adding icon to", nodeName, ":", e);
            }
          }
        } else {
          console.log("No info panel data for:", nodeName);
        }
      });
    }
    
    // Add info icons initially
    addInfoIcons();
    
    // Set up direct click handlers for each info icon (better than delegation)
    function setupIconClickHandlers() {
      d3.selectAll(".info-icon-text").on("click", function() {
        console.log("Direct info icon click handler triggered");
        d3.event.stopPropagation(); // Prevent event bubbling to node
        d3.event.preventDefault(); // Prevent any default behaviors
        var iconData = d3.select(this).datum();
        console.log("Info icon clicked for:", iconData.nodeName);
        showInfoPanel(this, iconData.panelHtml);
      });
    }
    
    // Setup initial click handlers
    setupIconClickHandlers();
    
    // Set up mutation observer to detect new nodes after expansion
    if (typeof MutationObserver !== "undefined") {
      var observer = new MutationObserver(function(mutations) {
        var hasNewNodes = false;
        mutations.forEach(function(mutation) {
          if (mutation.addedNodes.length > 0) {
            for (var i = 0; i < mutation.addedNodes.length; i++) {
              if (mutation.addedNodes[i].classList && 
                  mutation.addedNodes[i].classList.contains("node")) {
                hasNewNodes = true;
                break;
              }
            }
          }
        });
        
        if (hasNewNodes) {
          console.log("New nodes detected, adding info icons...");
          setTimeout(function() {
            addInfoIcons();
            setupIconClickHandlers(); // Re-setup click handlers for new icons
          }, 100); // Small delay to ensure nodes are fully rendered
        }
      });
      
      var treeContainer = document.querySelector(".collapsibleTree");
      if (treeContainer) {
        observer.observe(treeContainer, {
          childList: true,
          subtree: true
        });
      }
    }
    
    console.log("Info panel system integrated with dynamic expansion support");
    console.log("=== INFO PANEL DEBUG END ===");
  }, 1000);
  
  // Function to show info panel (simplified for now)
  function showInfoPanel(iconElement, panelHtml) {
    // Remove any existing panels
    d3.selectAll(".active-info-panel").remove();
    
    // Create panel
    var panel = d3.select("body")
      .append("div")
      .attr("class", "active-info-panel")
      .style("position", "absolute")
      .style("background", "white")
      .style("border", "2px solid #3498db")
      .style("border-radius", "8px")
      .style("padding", "16px")
      .style("box-shadow", "0 4px 15px rgba(0,0,0,0.15)")
      .style("z-index", "1000")
      .style("max-width", "320px")
      .html(panelHtml);
      
    // Position panel near the icon
    var iconRect = iconElement.getBoundingClientRect();
    panel
      .style("left", (iconRect.right + 10) + "px")
      .style("top", iconRect.top + "px");
      
    // Close panel when clicking outside
    setTimeout(function() {
      d3.select("body").on("click.closePanel", function() {
        if (!d3.event.target.closest(".active-info-panel") && 
            !d3.select(d3.event.target).classed("info-icon-text")) {
          d3.selectAll(".active-info-panel").remove();
          d3.select("body").on("click.closePanel", null);
        }
      });
    }, 10);
  }
  </script>')
  
  # Add only our new info panel script (remove old conflicting system)
  tree_html <- gsub("</body>", paste0(info_panel_script, "</body>"), tree_html)
  
  return(tree_html)
}

#' Enhance tree HTML with info panel system using cached data (no duplicate API calls)
#' @param tree_html HTML string from collapsibleTreeNetwork
#' @param network_data Network data with node information
#' @param cached_info_panels Pre-generated info panel data from create_info_panel_data_parallel()
#' @return Enhanced HTML with CSS and JavaScript for info panel system
enhance_tree_html_with_info_panels_cached <- function(tree_html, network_data, cached_info_panels) {
  
  # Add layout CSS and info panel CSS
  layout_css <- "<style>body { margin: 0 !important; padding: 0 !important; overflow: hidden !important; } html { margin: 0 !important; padding: 0 !important; } #htmlwidget_container { margin: 0 !important; padding: 0 !important; }</style>"
  
  # Generate info panel CSS with the text wrapping improvements
  info_panel_css <- paste0("<style>", generate_info_panel_css(), "</style>")
  
  # Combine both CSS sections
  combined_css <- paste0(layout_css, info_panel_css)
  tree_html <- gsub("</head>", paste0(combined_css, "</head>"), tree_html)
  
  # Create JavaScript data structure using cached info panel data (no API calls)
  info_panel_data_js <- create_info_panel_data_js_with_cache(network_data, cached_info_panels)
  
  # Add info panel system to the tree with proper event delegation
  info_panel_script <- paste0('<script>
  // Info panel data mapping (base64 encoded to avoid escaping issues)
  var infoPanelDataEncoded = ', info_panel_data_js, ';
  
  // Decode base64 data with proper UTF-8 handling
  var infoPanelData = {};
  for (var key in infoPanelDataEncoded) {
    if (infoPanelDataEncoded[key] && infoPanelDataEncoded[key] !== "") {
      try {
        // Use proper UTF-8 decoding instead of basic atob()
        var binaryString = atob(infoPanelDataEncoded[key]);
        var bytes = new Uint8Array(binaryString.length);
        for (var i = 0; i < binaryString.length; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }
        infoPanelData[key] = new TextDecoder("utf-8").decode(bytes);
      } catch (e) {
        console.error("Failed to decode panel data for", key, ":", e);
        infoPanelData[key] = "Error: Could not decode content.";
      }
    } else {
      infoPanelData[key] = "";
    }
  }
  
  // Info panel integration with collapsibleTree using event delegation
  setTimeout(function() {
    console.log("=== INFO PANEL DEBUG START ===");
    console.log("Info panel data keys:", Object.keys(infoPanelData));
    console.log("Available nodes:", d3.selectAll(".collapsibleTree .node").size());
    
    // Remove existing tooltips
    d3.selectAll(".tooltip").remove();
    
    // Disable original tooltip functionality  
    d3.selectAll(".collapsibleTree .node").on("mouseover.tooltip", null).on("mouseout.tooltip", null);
    
    // Function to add info icons to nodes (called initially and after expansions)
    function addInfoIcons() {
      d3.selectAll(".collapsibleTree .node").each(function() {
        var nodeElement = this;
        var node = d3.select(nodeElement);
        
        // Skip if already has info icon
        if (node.select(".text-with-info").size() > 0) {
          return;
        }
        
        var nodeData = node.datum();
        var nodeName = nodeData && nodeData.data ? nodeData.data.name : null;
        
        console.log("Processing node:", nodeName, "Full nodeData:", nodeData);
        
        // Check if we have info panel data for this node
        var infoPanelHtml = infoPanelData[nodeName];
        if (infoPanelHtml && infoPanelHtml.trim() !== "") {
          console.log("Adding info icon to:", nodeName);
          
          // Find the text element
          var textElement = node.select("text");
          if (!textElement.empty()) {
            try {
              // Position (i) symbol ABOVE the node text, not to the right
              var iconY = -20; // Position above the node
              
              // Add ONLY the (i) text symbol - NO white circle background
              var infoIcon = node.append("text")
                .attr("class", "info-icon-text")
                .attr("x", 0) // Center horizontally with the node
                .attr("y", iconY)
                .attr("dy", "0.35em")
                .attr("text-anchor", "middle")
                .attr("fill", "#3498db") // Blue color for visibility
                .attr("font-size", "14px") // Slightly larger for better visibility
                .attr("font-weight", "bold")
                .style("cursor", "pointer")
                .text("ⓘ");
                
              // Store panel data on the icon for click handling
              infoIcon.datum({panelHtml: infoPanelHtml, nodeName: nodeName});
              
              console.log("Successfully added icon to:", nodeName);
            } catch (e) {
              console.error("Error adding icon to", nodeName, ":", e);
            }
          }
        } else {
          console.log("No info panel data for:", nodeName);
        }
      });
    }
    
    // Add info icons initially
    addInfoIcons();
    
    // Set up direct click handlers for each info icon (better than delegation)
    function setupIconClickHandlers() {
      d3.selectAll(".info-icon-text").on("click", function() {
        console.log("Direct info icon click handler triggered");
        d3.event.stopPropagation(); // Prevent event bubbling to node
        d3.event.preventDefault(); // Prevent any default behaviors
        var iconData = d3.select(this).datum();
        console.log("Info icon clicked for:", iconData.nodeName);
        showInfoPanel(this, iconData.panelHtml);
      });
    }
    
    // Setup initial click handlers
    setupIconClickHandlers();
    
    // Set up mutation observer to detect new nodes after expansion
    if (typeof MutationObserver !== "undefined") {
      var observer = new MutationObserver(function(mutations) {
        var hasNewNodes = false;
        mutations.forEach(function(mutation) {
          if (mutation.addedNodes.length > 0) {
            for (var i = 0; i < mutation.addedNodes.length; i++) {
              if (mutation.addedNodes[i].classList && 
                  mutation.addedNodes[i].classList.contains("node")) {
                hasNewNodes = true;
                break;
              }
            }
          }
        });
        
        if (hasNewNodes) {
          console.log("New nodes detected, adding info icons...");
          setTimeout(function() {
            addInfoIcons();
            setupIconClickHandlers(); // Re-setup click handlers for new icons
          }, 100); // Small delay to ensure nodes are fully rendered
        }
      });
      
      var treeContainer = document.querySelector(".collapsibleTree");
      if (treeContainer) {
        observer.observe(treeContainer, {
          childList: true,
          subtree: true
        });
      }
    }
    
    console.log("Info panel system integrated with dynamic expansion support");
    console.log("=== INFO PANEL DEBUG END ===");
  }, 1000);
  
  // Function to show info panel (simplified for now)
  function showInfoPanel(iconElement, panelHtml) {
    // Remove any existing panels
    d3.selectAll(".active-info-panel").remove();
    
    // Create panel
    var panel = d3.select("body")
      .append("div")
      .attr("class", "active-info-panel")
      .style("position", "absolute")
      .style("background", "white")
      .style("border", "2px solid #3498db")
      .style("border-radius", "8px")
      .style("padding", "16px")
      .style("box-shadow", "0 4px 15px rgba(0,0,0,0.15)")
      .style("z-index", "1000")
      .style("max-width", "320px")
      .html(panelHtml);
      
    // Position panel near the icon
    var iconRect = iconElement.getBoundingClientRect();
    panel
      .style("left", (iconRect.right + 10) + "px")
      .style("top", iconRect.top + "px");
      
    // Close panel when clicking outside
    setTimeout(function() {
      d3.select("body").on("click.closePanel", function() {
        if (!d3.event.target.closest(".active-info-panel") && 
            !d3.select(d3.event.target).classed("info-icon-text")) {
          d3.selectAll(".active-info-panel").remove();
          d3.select("body").on("click.closePanel", null);
        }
      });
    }, 10);
  }
  </script>')
  
  # Add only our new info panel script (remove old conflicting system)
  tree_html <- gsub("</body>", paste0(info_panel_script, "</body>"), tree_html)
  
  return(tree_html)
}

#' Complete workflow to enhance collapsibleTreeNetwork with info panels
#' @param tree_data Data frame with Parent, Child columns (should already have InfoPanel data)
#' @param network_data Network data with node information (for debugging)
#' @param tree_widget collapsibleTreeNetwork widget created with InfoPanel data
#' @return Enhanced HTML with info panel system
create_enhanced_tree_html <- function(tree_data, network_data, tree_widget, request_id = NULL) {
  
  # Convert to HTML using temporary file approach
  temp_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(tree_widget, temp_file, selfcontained = TRUE)
  tree_html <- paste(readLines(temp_file), collapse = "\n")
  unlink(temp_file)  # Clean up temp file
  
  # Generate cached info panel data ONCE to avoid duplicate API calls
  cached_info_panels <- create_info_panel_data_parallel(network_data, request_id = request_id)
  
  # Enhance HTML with info panel system using cached data
  enhanced_html <- enhance_tree_html_with_info_panels_cached(tree_html, network_data, cached_info_panels)
  
  return(enhanced_html)
}

cat("Tree HTML enhancement functions loaded successfully\n")