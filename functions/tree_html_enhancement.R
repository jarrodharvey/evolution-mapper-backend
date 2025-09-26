# Tree HTML Enhancement Functions
# Shared functions for adding info panel system to phylogenetic tree HTML
# Eliminates code duplication across tree generation functions

library(jsonlite)
source("functions/info_panel_system.R")
source("functions/phylopic_silhouettes.R")

#' Add info panel data to tree data frame
#' @param tree_data Data frame with Parent, Child columns for collapsibleTreeNetwork
#' @param network_data Network data with node information for info panels
#' @param request_id Optional request ID for logging correlation
#' @param progress_token Optional progress token for tracking external API calls
#' @return Enhanced tree_data with InfoPanel column
add_info_panel_data <- function(tree_data, network_data, request_id = NULL, progress_token = NULL) {
  # Create info panel data for ancestor nodes using sequential processing (prevents API hangs)
  info_panels <- create_info_panel_data_sequential(network_data, request_id = request_id, progress_token = progress_token)
  
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
  
  # Calculate optimal dimensions for info panels (based on typical Wikipedia content)
  # Average Wikipedia intro: ~300 characters
  typical_dimensions <- extract_content_dimensions("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor.")

  # Generate info panel CSS with pre-calculated dimensions
  info_panel_css <- paste0("<style>", generate_info_panel_css(typical_dimensions$panel_width, typical_dimensions$text_column_width), "</style>")
  
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
        
        // Add ID to Common ancestor node
        if (nodeName === "Common ancestor") {
          node.attr("id", "common_ancestor_node");
        }
        
        // Check if we have info panel data for this node
        var infoPanelHtml = infoPanelData[nodeName];
        if (infoPanelHtml && infoPanelHtml.trim() !== "") {
          console.log("Adding info icon to:", nodeName);
          
          // Find the text element
          var textElement = node.select("text");
          if (!textElement.empty()) {
            try {
              // Position (i) symbol ABOVE the text label, not above the node center
              // Get the text element position and dimensions
              var textX = parseFloat(textElement.attr("x")) || 0;
              var textY = parseFloat(textElement.attr("y")) || 0;
              
              // Check if this node will have a PhyloPic (taxonomic node)
              var hasPhyloPic = phylopicNodeData && phylopicNodeData[nodeName.replace(/\\s*\\([^)]*\\)\\s*$/, "")];
              var iconOffsetY = hasPhyloPic ? -35 : -15; // Offset above text (smaller values since we are positioning relative to text)
              
              // Add ONLY the (i) text symbol - NO white circle background
              var infoIcon = node.append("text")
                .attr("class", "info-icon-text")
                .attr("x", textX - 25) // Position to the left of the text to avoid PhyloPic overlap
                .attr("y", textY + iconOffsetY) // Position above the text label
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
      .style("width", "fit-content")
      .html(panelHtml);

    // Ensure close button exists even if panelHtml lacked one
    var closeButton = panel.select(".close-panel");
    if (closeButton.empty()) {
      closeButton = panel.append("button")
        .attr("class", "close-panel")
        .attr("type", "button")
        .html("&times;");
    }

    closeButton.on("click", function() {
      closeInfoPanel(this);
    });
      
    // Position panel at the center of the SVG container to avoid cutoffs
    var svgElement = document.querySelector("svg");
    if (svgElement) {
      var svgRect = svgElement.getBoundingClientRect();
      var svgCenterX = svgRect.left + (svgRect.width / 2);
      var svgCenterY = svgRect.top + (svgRect.height / 2);

      panel
        .style("position", "fixed")
        .style("left", svgCenterX + "px")
        .style("top", svgCenterY + "px")
        .style("transform", "translate(-50%, -50%)")
        .style("z-index", "1000");
    } else {
      // Fallback to old positioning if no SVG found
      var iconRect = iconElement.getBoundingClientRect();
      panel
        .style("left", (iconRect.right + 10) + "px")
        .style("top", iconRect.top + "px");
    }
      
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

  // Function to close info panel via close button
  function closeInfoPanel(button) {
    console.log("Close button clicked");
    // Find the parent info panel and remove it
    var panel = d3.select(button.parentNode);
    if (!panel.empty()) {
      panel.remove();
      // Clean up click-outside handler
      d3.select("body").on("click.closePanel", null);
      console.log("Info panel closed via close button");
    }
  }
  </script>')

  # NOTE: Duration modification is now handled via htmlwidgets::onRender() in hybrid_tree_generation.R
  # This is more reliable than regex replacement as it works at the JavaScript runtime level

  # Add info panel script
  tree_html <- gsub("</body>", paste0(info_panel_script, "</body>"), tree_html)
  
  return(tree_html)
}

#' Enhance tree HTML with info panel system and PhyloPic node replacement using cached data
#' @param tree_html HTML string from collapsibleTreeNetwork
#' @param network_data Network data with node information
#' @param cached_info_panels Pre-generated info panel data from create_info_panel_data_parallel()
#' @param expansion_speed Duration in milliseconds for tree expansion animations (default: 750)
#' @param enable_phylopic_nodes Enable PhyloPic replacement for taxonomic nodes (default: TRUE)
#' @return Enhanced HTML with CSS and JavaScript for info panel system and PhyloPic nodes
enhance_tree_html_with_info_panels_cached <- function(tree_html, network_data, cached_info_panels, expansion_speed = 750, enable_phylopic_nodes = TRUE) {
  
  # Add layout CSS and info panel CSS
  layout_css <- "<style>body { margin: 0 !important; padding: 0 !important; overflow: hidden !important; } html { margin: 0 !important; padding: 0 !important; } #htmlwidget_container { margin: 0 !important; padding: 0 !important; }</style>"
  
  # Calculate optimal dimensions for info panels (based on typical Wikipedia content)
  # Average Wikipedia intro: ~300 characters
  typical_dimensions <- extract_content_dimensions("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor.")

  # Generate info panel CSS with pre-calculated dimensions
  info_panel_css <- paste0("<style>", generate_info_panel_css(typical_dimensions$panel_width, typical_dimensions$text_column_width), "</style>")
  
  # Combine both CSS sections
  combined_css <- paste0(layout_css, info_panel_css)
  tree_html <- gsub("</head>", paste0(combined_css, "</head>"), tree_html)
  
  # Create JavaScript data structure using cached info panel data (no API calls)
  info_panel_data_js <- create_info_panel_data_js_with_cache(network_data, cached_info_panels)
  
  # Generate PhyloPic node replacement data if enabled
  phylopic_node_data_js <- "{}"  # Default empty object
  if (enable_phylopic_nodes) {
    tryCatch({
      phylopic_node_data_js <- create_phylopic_node_replacement_data(network_data)
    }, error = function(e) {
      api_log_warn(paste("Failed to create PhyloPic node replacement data:", e$message))
      phylopic_node_data_js <- "{}"  # Fall back to empty object
    })
  }
  
  # Add info panel system to the tree with proper event delegation
  info_panel_script <- paste0('<script>
  // Info panel data mapping (base64 encoded to avoid escaping issues)
  var infoPanelDataEncoded = ', info_panel_data_js, ';
  
  // PhyloPic node replacement data
  var phylopicNodeData = ', phylopic_node_data_js, ';
  
  // Utility function to convert RGB color to CSS filter for recoloring black silhouettes
  function colorToFilterCSS(color) {
    if (!color || color === "none" || color === "") return null;

    try {
      // Handle different color formats (rgb, rgba, hex)
      var rgb = null;

      if (color.startsWith("rgb(")) {
        // Parse rgb(r, g, b) format
        var match = color.match(/rgb\\(([^)]+)\\)/);
        if (match) {
          var values = match[1].split(",").map(v => parseInt(v.trim()));
          rgb = { r: values[0], g: values[1], b: values[2] };
        }
      } else if (color.startsWith("rgba(")) {
        // Parse rgba(r, g, b, a) format
        var match = color.match(/rgba\\(([^)]+)\\)/);
        if (match) {
          var values = match[1].split(",").map(v => parseFloat(v.trim()));
          rgb = { r: values[0], g: values[1], b: values[2] };
        }
      } else if (color.startsWith("#")) {
        // Parse hex format
        var hex = color.replace("#", "");
        if (hex.length === 3) {
          hex = hex[0] + hex[0] + hex[1] + hex[1] + hex[2] + hex[2];
        }
        rgb = {
          r: parseInt(hex.substr(0, 2), 16),
          g: parseInt(hex.substr(2, 2), 16),
          b: parseInt(hex.substr(4, 2), 16)
        };
      }

      if (!rgb) return null;

      // Convert RGB to hue-rotate and saturate values for CSS filter
      // This is a simplified approach - more complex color mapping could be implemented
      var hue = rgbToHue(rgb.r, rgb.g, rgb.b);
      var saturation = rgbToSaturation(rgb.r, rgb.g, rgb.b);
      var brightness = (rgb.r + rgb.g + rgb.b) / (3 * 255);

      // Create CSS filter that converts black to the target color
      var filter = "brightness(0) saturate(100%) invert(" + Math.round(brightness * 100) + "%) sepia(100%) saturate(" + Math.round(saturation * 500) + "%) hue-rotate(" + Math.round(hue) + "deg)";

      return filter;

    } catch (e) {
      console.error("Error converting color to filter:", e);
      return null;
    }
  }

  // Helper function to calculate hue from RGB
  function rgbToHue(r, g, b) {
    r /= 255; g /= 255; b /= 255;
    var max = Math.max(r, g, b), min = Math.min(r, g, b);
    var h, s, l = (max + min) / 2;

    if (max === min) {
      h = s = 0; // achromatic
    } else {
      var d = max - min;
      s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
      switch (max) {
        case r: h = (g - b) / d + (g < b ? 6 : 0); break;
        case g: h = (b - r) / d + 2; break;
        case b: h = (r - g) / d + 4; break;
      }
      h /= 6;
    }

    return h * 360;
  }

  // Helper function to calculate saturation from RGB
  function rgbToSaturation(r, g, b) {
    r /= 255; g /= 255; b /= 255;
    var max = Math.max(r, g, b), min = Math.min(r, g, b);
    var s = max === 0 ? 0 : (max - min) / max;
    return s;
  }

  // Function to replace taxonomic nodes with PhyloPic silhouettes
  function replacePhylopicNodes() {
    console.log("=== PHYLOPIC NODE REPLACEMENT START ===");
    console.log("Available PhyloPic data:", Object.keys(phylopicNodeData));
    
    // Find taxonomic nodes using same logic as info panel system (reuses successful node identification)
    var taxonomicNodes = [];
    var nodeSelection = d3.selectAll(".collapsibleTree .node");
    console.log("PhyloPic node selection size:", nodeSelection.size());
    nodeSelection.each(function() {
      var node = d3.select(this);
      var nodeData = node.datum();
      var nodeName = nodeData && nodeData.data ? nodeData.data.name : null;
      
      console.log("PhyloPic checking node:", nodeName, "NodeData exists:", !!nodeData);
      
      if (!nodeName) return;
      
      // Extract taxonomic name from age format like "Tetrapoda (123 Mya)"
      var taxonomicName = nodeName;
      var matches = nodeName.match(/^([^(]+)\\\\s*\\\\(/);
      if (matches && matches[1]) {
        taxonomicName = matches[1].trim();
      }
      
      console.log("  - Original name:", nodeName, "Extracted name:", taxonomicName);
      console.log("  - Has PhyloPic data:", !!(phylopicNodeData[taxonomicName] && phylopicNodeData[taxonomicName].data_url));
      
      // Only include nodes that have PhyloPic data available (same as info panel condition)
      if (phylopicNodeData[taxonomicName] && phylopicNodeData[taxonomicName].data_url) {
        var circle = node.select("circle");
        console.log("  - Has circle element:", !circle.empty());
        if (!circle.empty()) {
          console.log("  - ADDING TO REPLACEMENT LIST:", nodeName);
          taxonomicNodes.push({
            node: node,
            circle: circle,
            nodeName: nodeName,
            taxonomicName: taxonomicName
          });
        }
      }
    });
    console.log("Found", taxonomicNodes.length, "taxonomic nodes to potentially replace");
    
    taxonomicNodes.forEach(function(nodeInfo) {
      var node = nodeInfo.node;
      var circle = nodeInfo.circle;
      var nodeName = nodeInfo.nodeName;
      var taxonomicName = nodeInfo.taxonomicName;
      
      console.log("Processing taxonomic node:", nodeName, "-> extracted name:", taxonomicName);
      
      // Check if we have PhyloPic data for this taxonomic group
      var phylopicData = phylopicNodeData[taxonomicName];
      if (phylopicData && phylopicData.data_url) {
        console.log("Replacing node with PhyloPic:", taxonomicName);

        try {
          // Get circle properties for positioning and color inheritance
          var circleR = parseFloat(circle.attr("r")) || 4.5;
          var circleX = parseFloat(circle.attr("cx")) || 0;
          var circleY = parseFloat(circle.attr("cy")) || 0;

          // EXTRACT COLOR FROM ORIGINAL CIRCLE for inheritance
          var inheritedColor = null;
          try {
            var circleNode = circle.node();
            if (circleNode) {
              // Try multiple approaches to get the color
              console.log("=== COLOR EXTRACTION DEBUG for", taxonomicName, "===");

              // Method 1: Computed style
              var computedStyle = window.getComputedStyle(circleNode);
              var computedFill = computedStyle.fill;
              console.log("  Computed fill:", computedFill);

              // Method 2: Direct fill attribute
              var attrFill = circle.attr("fill");
              console.log("  Attribute fill:", attrFill);

              // Method 3: D3 style
              var d3Fill = circle.style("fill");
              console.log("  D3 style fill:", d3Fill);

              // Method 4: Direct style attribute
              var styleFill = circleNode.style.fill;
              console.log("  Direct style fill:", styleFill);

              // Choose the best available color
              if (computedFill && computedFill !== "none" && computedFill !== "" && computedFill !== "rgb(0, 0, 0)") {
                inheritedColor = computedFill;
                console.log("  Using computed style color");
              } else if (d3Fill && d3Fill !== "none" && d3Fill !== "" && d3Fill !== "rgb(0, 0, 0)") {
                inheritedColor = d3Fill;
                console.log("  Using D3 style color");
              } else if (attrFill && attrFill !== "none" && attrFill !== "" && attrFill !== "#000000") {
                inheritedColor = attrFill;
                console.log("  Using attribute color");
              } else if (styleFill && styleFill !== "none" && styleFill !== "" && styleFill !== "rgb(0, 0, 0)") {
                inheritedColor = styleFill;
                console.log("  Using direct style color");
              }

              console.log("  FINAL inherited color for", taxonomicName, ":", inheritedColor);
              console.log("=== END COLOR EXTRACTION DEBUG ===");
            }
          } catch (colorError) {
            console.warn("Failed to extract color for", taxonomicName, ":", colorError);
            inheritedColor = null;
          }

          // Calculate image size (slightly larger than original circle)
          var imageSize = Math.max(phylopicData.target_size || 35, circleR * 2.2);

          // Position PhyloPic slightly to the right of the original circle position
          // This avoids getBBox() calculation issues with CollapsibleTree
          var phylopicX = circleX - circleR; // Positioned at left edge of circle
          var phylopicY = circleY - imageSize / 2; // Centered on circle

          // Hide the original circle instead of removing it (preserve functionality)
          circle.style("opacity", 0);

          // Add PhyloPic image positioned to avoid text overlap
          var image = node.append("image")
            .attr("class", "phylopic-node-image")
            .attr("xlink:href", phylopicData.data_url)
            .attr("x", phylopicX)
            .attr("y", phylopicY)
            .attr("width", imageSize)
            .attr("height", imageSize)
            .style("cursor", "pointer")
            .style("opacity", 0.9)
            .attr("title", "PhyloPic silhouette for " + taxonomicName);

          // APPLY COLOR INHERITANCE if color was successfully extracted
          if (inheritedColor && inheritedColor !== "none" && inheritedColor !== "") {
            // Use SVG-based recoloring for better color accuracy
            try {
              // For PNG images, we need to use a different approach
              // Create a colored version by applying the color directly to the image
              var canvas = document.createElement("canvas");
              var ctx = canvas.getContext("2d");
              var img = new Image();

              img.onload = function() {
                canvas.width = img.width;
                canvas.height = img.height;

                // Draw the original image
                ctx.drawImage(img, 0, 0);

                // Apply color overlay using composite operation
                ctx.globalCompositeOperation = "source-in";
                ctx.fillStyle = inheritedColor;
                ctx.fillRect(0, 0, canvas.width, canvas.height);

                // Convert back to data URL and update the image
                var coloredDataURL = canvas.toDataURL();
                image.attr("xlink:href", coloredDataURL);

                console.log("Applied color inheritance to", taxonomicName, "using color:", inheritedColor);
              };

              // Set the source to trigger the onload
              img.src = phylopicData.data_url;

            } catch (canvasError) {
              console.warn("Canvas recoloring failed for", taxonomicName, ", trying CSS filter fallback:", canvasError);
              // Fallback to CSS filter approach
              var filterCSS = colorToFilterCSS(inheritedColor);
              if (filterCSS) {
                image.style("filter", filterCSS);
                console.log("Applied color inheritance filter fallback to", taxonomicName, ":", filterCSS);
              } else {
                console.warn("Both canvas and filter approaches failed for", taxonomicName, "- using default black");
              }
            }
          } else {
            console.warn("No inherited color available for", taxonomicName, "- using default black silhouette");
          }

          // Store original circle reference and color info for potential restoration
          image.node()._originalCircle = circle.node();
          image.node()._inheritedColor = inheritedColor;

          console.log("Successfully replaced", taxonomicName, "with PhyloPic silhouette");

        } catch (e) {
          console.error("Error replacing node", taxonomicName, "with PhyloPic:", e);
          // Ensure circle remains visible if replacement fails
          circle.style("opacity", 1);
        }
      } else {
        console.log("No PhyloPic data available for:", taxonomicName);
      }
    });
    
    console.log("=== PHYLOPIC NODE REPLACEMENT END ===");
  }
  
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
    console.log("PhyloPic node data keys:", Object.keys(phylopicNodeData));
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
        
        // Add ID to Common ancestor node
        if (nodeName === "Common ancestor") {
          node.attr("id", "common_ancestor_node");
        }
        
        // Check if we have info panel data for this node
        var infoPanelHtml = infoPanelData[nodeName];
        if (infoPanelHtml && infoPanelHtml.trim() !== "") {
          console.log("Adding info icon to:", nodeName);
          
          // Find the text element
          var textElement = node.select("text");
          if (!textElement.empty()) {
            try {
              // Position (i) symbol ABOVE the text label, not above the node center
              // Get the text element position and dimensions
              var textX = parseFloat(textElement.attr("x")) || 0;
              var textY = parseFloat(textElement.attr("y")) || 0;
              
              // Check if this node will have a PhyloPic (taxonomic node)
              var hasPhyloPic = phylopicNodeData && phylopicNodeData[nodeName.replace(/\\s*\\([^)]*\\)\\s*$/, "")];
              var iconOffsetY = hasPhyloPic ? -35 : -15; // Offset above text (smaller values since we are positioning relative to text)
              
              // Add ONLY the (i) text symbol - NO white circle background
              var infoIcon = node.append("text")
                .attr("class", "info-icon-text")
                .attr("x", textX - 25) // Position to the left of the text to avoid PhyloPic overlap
                .attr("y", textY + iconOffsetY) // Position above the text label
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
    
    // Replace taxonomic nodes with PhyloPic silhouettes
    if (Object.keys(phylopicNodeData).length > 0) {
      replacePhylopicNodes();
    }
    
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
            // Re-apply PhyloPic replacements to new nodes
            if (Object.keys(phylopicNodeData).length > 0) {
              replacePhylopicNodes();
            }
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
      .style("width", "fit-content")
      .html(panelHtml);

    // Ensure close button exists even if panelHtml lacked one
    var closeButton = panel.select(".close-panel");
    if (closeButton.empty()) {
      closeButton = panel.append("button")
        .attr("class", "close-panel")
        .attr("type", "button")
        .html("&times;");
    }

    closeButton.on("click", function() {
      closeInfoPanel(this);
    });
      
    // Position panel at the center of the SVG container to avoid cutoffs
    var svgElement = document.querySelector("svg");
    if (svgElement) {
      var svgRect = svgElement.getBoundingClientRect();
      var svgCenterX = svgRect.left + (svgRect.width / 2);
      var svgCenterY = svgRect.top + (svgRect.height / 2);

      panel
        .style("position", "fixed")
        .style("left", svgCenterX + "px")
        .style("top", svgCenterY + "px")
        .style("transform", "translate(-50%, -50%)")
        .style("z-index", "1000");
    } else {
      // Fallback to old positioning if no SVG found
      var iconRect = iconElement.getBoundingClientRect();
      panel
        .style("left", (iconRect.right + 10) + "px")
        .style("top", iconRect.top + "px");
    }
      
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

  // Function to close info panel via close button
  function closeInfoPanel(button) {
    console.log("Close button clicked");
    // Find the parent info panel and remove it
    var panel = d3.select(button.parentNode);
    if (!panel.empty()) {
      panel.remove();
      // Clean up click-outside handler
      d3.select("body").on("click.closePanel", null);
      console.log("Info panel closed via close button");
    }
  }
  </script>')

  # Add only our new info panel script (remove old conflicting system)
  tree_html <- gsub("</body>", paste0(info_panel_script, "</body>"), tree_html)
  
  return(tree_html)
}

#' Complete workflow to enhance collapsibleTreeNetwork with info panels and PhyloPic nodes
#' @param tree_data Data frame with Parent, Child columns (should already have InfoPanel data)
#' @param network_data Network data with node information (for debugging)
#' @param tree_widget collapsibleTreeNetwork widget created with InfoPanel data
#' @param request_id Optional request ID for logging correlation
#' @param progress_token Optional progress token for tracking external API calls
#' @param expansion_speed Duration in milliseconds for tree expansion animations (default: 750)
#' @param enable_phylopic_nodes Enable PhyloPic replacement for taxonomic nodes (default: TRUE)
#' @return Enhanced HTML with info panel system and PhyloPic nodes
create_enhanced_tree_html <- function(tree_data, network_data, tree_widget, request_id = NULL, progress_token = NULL, expansion_speed = 750, enable_phylopic_nodes = TRUE) {
  
  # Convert to HTML using temporary file approach
  temp_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(tree_widget, temp_file, selfcontained = TRUE)
  tree_html <- paste(readLines(temp_file), collapse = "\n")
  unlink(temp_file)  # Clean up temp file
  
  # Generate cached info panel data ONCE using sequential processing (prevents API hangs)
  cached_info_panels <- create_info_panel_data_sequential(network_data, request_id = request_id, progress_token = progress_token)
  
  # Enhance HTML with info panel system and PhyloPic nodes using cached data
  enhanced_html <- enhance_tree_html_with_info_panels_cached(tree_html, network_data, cached_info_panels, expansion_speed, enable_phylopic_nodes)
  
  return(enhanced_html)
}

cat("Tree HTML enhancement functions loaded successfully\n")
