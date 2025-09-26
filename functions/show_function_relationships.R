#' Generate Function Dependency Visualization
#'
#' Uses mvbutils::foodweb to create a visual representation of function
#' relationships in the evolution-mapper project
#'
#' @param output_file Character. Path where to save the HTML output
#' @param open_result Logical. Whether to open the result automatically
#' @return Invisible. Creates an interactive HTML file with the function dependency graph

show_function_relationships <- function(output_file = "foodweb_analysis.html", open_result = TRUE) {

  # Check if required packages are available, install if needed
  required_packages <- c("mvbutils", "htmlwidgets", "networkD3")

  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      message("Installing ", pkg, " package...")
      install.packages(pkg, repos = "https://cran.rstudio.com/")
      library(pkg, character.only = TRUE)
    }
  }

  # Store original working directory
  original_wd <- getwd()

  # Ensure we're in the project root
  if (!file.exists("plumber.R")) {
    # Try to navigate to project root
    if (file.exists("../plumber.R")) {
      setwd("..")
    } else {
      stop("Cannot find plumber.R. Please run from project root directory.")
    }
  }

  # Create a clean environment for our analysis
  analysis_env <- new.env()

  # Source all R files in the functions directory
  function_files <- list.files("functions", pattern = "\\.R$", full.names = TRUE)
  message("Loading functions from ", length(function_files), " files")

  # Source each function file into our analysis environment
  for (file in function_files) {
    message("Sourcing: ", basename(file))
    tryCatch({
      source(file, local = analysis_env)
    }, error = function(e) {
      warning("Could not source ", file, ": ", e$message)
    })
  }

  # Also source the main plumber.R file
  tryCatch({
    source("plumber.R", local = analysis_env)
    message("Sourcing: plumber.R")
  }, error = function(e) {
    warning("Could not source plumber.R: ", e$message)
  })

  # Get all functions in our analysis environment
  all_functions <- ls(analysis_env)[sapply(ls(analysis_env), function(x) is.function(get(x, envir = analysis_env)))]
  message("Found ", length(all_functions), " functions in environment")

  if (length(all_functions) == 0) {
    stop("No functions found to analyze!")
  }

  # Print the functions we found
  message("\nFunctions to analyze:")
  message(paste(all_functions, collapse = ", "))

  # Create the interactive foodweb visualization
  message("\nGenerating interactive function dependency graph...")

  # Get foodweb data structure
  tryCatch({
    # Generate foodweb object (but don't plot yet)
    fw_result <- foodweb(where = analysis_env, plotting = FALSE)

    # Convert foodweb matrix to network data for D3
    network_data <- convert_foodweb_to_network(fw_result, all_functions)

    # Create interactive network visualization
    network_viz <- networkD3::forceNetwork(
      Links = network_data$links,
      Nodes = network_data$nodes,
      Source = "source",
      Target = "target",
      NodeID = "name",
      Group = "group",
      Value = "value",
      opacity = 0.9,
      zoom = TRUE,
      fontSize = 14,
      fontFamily = "Arial, sans-serif",
      colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory10);"),
      charge = -300,
      linkDistance = 100,
      width = 1200,
      height = 800
    )

    # Save as HTML
    htmlwidgets::saveWidget(
      network_viz,
      file = normalizePath(output_file, mustWork = FALSE),
      selfcontained = TRUE,
      title = "Evolution Mapper - Function Dependencies"
    )

    message("Interactive foodweb plot created successfully!")

  }, error = function(e) {
    warning("Error creating interactive foodweb: ", e$message)
    message("Falling back to static SVG visualization...")

    # Fallback: create SVG version that can be embedded in HTML
    create_svg_foodweb(analysis_env, all_functions, output_file)
  })

  # Restore original working directory
  setwd(original_wd)

  message("Function dependency analysis saved to: ", output_file)

  # Open the result if requested
  if (open_result) {
    open_visualization(output_file)
  }

  invisible(output_file)
}

#' Convert foodweb matrix to network data for D3 visualization
#'
#' @param fw_result foodweb object from mvbutils::foodweb
#' @param function_names character vector of function names
#' @return list with nodes and links data frames
convert_foodweb_to_network <- function(fw_result, function_names) {

  if (is.null(fw_result$funmat)) {
    stop("Invalid foodweb result: no function matrix found")
  }

  # Get the function matrix
  funmat <- fw_result$funmat

  # Create nodes data frame
  nodes <- data.frame(
    name = function_names,
    group = 1,  # Could be enhanced to group by file or functionality
    stringsAsFactors = FALSE
  )

  # Create links data frame from the matrix
  links <- data.frame()

  for (i in 1:nrow(funmat)) {
    for (j in 1:ncol(funmat)) {
      if (funmat[i, j] > 0) {
        # Function i calls function j
        links <- rbind(links, data.frame(
          source = i - 1,  # D3 uses 0-based indexing
          target = j - 1,
          value = funmat[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(list(nodes = nodes, links = links))
}

#' Create SVG-based foodweb as fallback
#'
#' @param analysis_env environment containing functions
#' @param function_names character vector of function names
#' @param output_file path to save HTML file
create_svg_foodweb <- function(analysis_env, function_names, output_file) {

  tryCatch({
    # Create temporary SVG file
    svg_file <- tempfile(fileext = ".svg")
    svg(svg_file, width = 16, height = 12)

    # Generate foodweb with custom styling
    foodweb(
      where = analysis_env,
      cex = 0.8,
      charlim = 30,
      boxcolor = "#E8F4FD",
      textcolor = "#2C3E50",
      border = TRUE,
      expand.xbox = 1.2,
      lwd = 1.5
    )

    # Add title
    title(
      main = "Evolution Mapper - Function Dependency Graph",
      sub = paste("Generated:", Sys.time()),
      cex.main = 1.2,
      cex.sub = 0.8,
      col.main = "#2C3E50",
      col.sub = "#7F8C8D"
    )

    dev.off()

    # Read SVG content
    svg_content <- readLines(svg_file, warn = FALSE)
    svg_text <- paste(svg_content, collapse = "\n")

    # Create HTML wrapper with zoom/pan capabilities
    html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>Evolution Mapper - Function Dependencies</title>
    <style>
        body {
            margin: 0;
            padding: 20px;
            font-family: Arial, sans-serif;
            background-color: #f5f5f5;
        }
        .container {
            max-width: 100%%;
            margin: 0 auto;
            background: white;
            border-radius: 8px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            overflow: hidden;
        }
        .header {
            background: #2C3E50;
            color: white;
            padding: 20px;
            text-align: center;
        }
        .svg-container {
            padding: 20px;
            text-align: center;
            overflow: auto;
        }
        svg {
            max-width: 100%%;
            height: auto;
            cursor: move;
        }
        .instructions {
            padding: 20px;
            background: #ecf0f1;
            color: #2c3e50;
            text-align: center;
            font-size: 14px;
        }
    </style>
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Evolution Mapper - Function Dependencies</h1>
            <p>Interactive function relationship diagram</p>
        </div>
        <div class="svg-container" id="svg-container">
            %s
        </div>
        <div class="instructions">
            <strong>Instructions:</strong> Use mouse wheel to zoom, click and drag to pan around the diagram.
            Arrows show which functions call other functions.
        </div>
    </div>

    <script>
        // Add zoom and pan functionality
        const svg = d3.select("svg");
        const container = d3.select("#svg-container");

        const zoom = d3.zoom()
            .scaleExtent([0.1, 10])
            .on("zoom", function(event) {
                svg.attr("transform", event.transform);
            });

        svg.call(zoom);

        // Add zoom controls
        const controls = container.append("div")
            .style("position", "absolute")
            .style("top", "10px")
            .style("right", "10px")
            .style("z-index", "1000");

        controls.append("button")
            .text("Reset Zoom")
            .style("margin", "2px")
            .on("click", function() {
                svg.transition().duration(300).call(zoom.transform, d3.zoomIdentity);
            });

        controls.append("button")
            .text("Zoom In")
            .style("margin", "2px")
            .on("click", function() {
                svg.transition().duration(300).call(zoom.scaleBy, 1.5);
            });

        controls.append("button")
            .text("Zoom Out")
            .style("margin", "2px")
            .on("click", function() {
                svg.transition().duration(300).call(zoom.scaleBy, 0.67);
            });
    </script>
</body>
</html>
', svg_text)

    # Write HTML file
    writeLines(html_content, output_file)

    # Clean up temporary SVG
    unlink(svg_file)

    message("SVG-based foodweb created successfully!")

  }, error = function(e) {
    stop("Failed to create fallback visualization: ", e$message)
  })
}

#' Open visualization file with system default application
#'
#' @param file_path Character. Path to the file to open
open_visualization <- function(file_path) {

  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return(invisible(FALSE))
  }

  # Detect operating system and open accordingly
  system_info <- Sys.info()

  tryCatch({
    if (system_info["sysname"] == "Darwin") {
      # macOS
      system(paste("open", shQuote(file_path)))
    } else if (system_info["sysname"] == "Linux") {
      # Linux - try different commands in order of preference
      if (system("which xdg-open", ignore.stdout = TRUE) == 0) {
        system(paste("xdg-open", shQuote(file_path)))
      } else if (system("which display", ignore.stdout = TRUE) == 0) {
        system(paste("display", shQuote(file_path)))
      } else {
        message("Please open ", file_path, " manually")
        return(invisible(FALSE))
      }
    } else if (grepl("CYGWIN|MINGW|MSYS", system_info["sysname"])) {
      # Windows
      system(paste("start", shQuote(file_path)))
    } else {
      message("Unknown OS. Please open ", file_path, " manually")
      return(invisible(FALSE))
    }

    message("Opening visualization...")
    return(invisible(TRUE))

  }, error = function(e) {
    warning("Could not open file automatically: ", e$message)
    message("Please open ", file_path, " manually")
    return(invisible(FALSE))
  })
}

# If this script is run directly (not sourced), execute the function
if (!interactive() && identical(environment(), globalenv())) {
  show_function_relationships()
}