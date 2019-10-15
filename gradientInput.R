library(shiny)

# Youcan change these sizes to make the input look different
INPUT_SIZE <- 14
CONTAINER_SIZE <- 300
CONTAINER_HEIGHT <- 35

# To get the exact correct pixel, apply a small correction since the javascript
# library is missing the last 6 pixels
JS_DRAGGABLE_FIX_MARGIN <- 6
CONTAINER_SIZE_CORRECTED <- CONTAINER_SIZE - JS_DRAGGABLE_FIX_MARGIN

gradient_js <-
"
// When a colour bar is dragged, send shiny the position
$(document).on('dragstop', '.ui-draggable', function(e, ui) {
var left = ui.position.left;
Shiny.setInputValue(e.target.id + '_left', left, {priority: 'event'});
});

// When a colour bar is created, send shiny the position
$(document).on('dragcreate', '.ui-draggable', function(e, ui) {
var left = parseInt(e.target.style.left);
Shiny.setInputValue(e.target.id + '_left', left, {priority: 'event'});
});

// When clicking somewhere in the box, send shiny the position of the click
$(document).on('click', '.draggables-module', function(event) {
var module = $(this).closest('.draggables-module');
var ns = module.data('shiny_ns');
if ($(event.target).hasClass('draggables-module')) {
var left = event.offsetX + 1;
} else if ($(event.target).hasClass('draggables-container')) {
var left = event.offsetX;
} else {
return;
}

Shiny.setInputValue(ns + 'add_drag_col', left, {priority: 'event'});
});
"

gradient_css <- glue::glue(
"
.draggables-module {
margin-top: 10px;
margin-bottom: {{INPUT_SIZE}}px;
width: {{CONTAINER_SIZE}}px;
height: {{CONTAINER_HEIGHT}}px;
border: 1px solid #aaa;
cursor: crosshair;
}
.draggables-module .draggables-container {
width: {{CONTAINER_SIZE_CORRECTED}}px;
height: 100%;
}
.draggables-module .gradient-draggable-wrapper {
float: left;
width: 0;
height: 100%;
}
.draggables-module .gradient-draggable-delete {
position: absolute;
top: -10px;
font-size: 10px;
left: -1px;
line-height: 10px;
cursor: pointer;
color: #000;
opacity: 0.4;
}
.draggables-module .gradient-draggable-wrapper:hover .gradient-draggable-delete {
opacity: 1;
}
.draggables-module .gradient-draggable-bar {
background: #ccc;
width: 4px;
height: 100%;
background: rgba(0,0,0,0.5);
cursor: move;
}
.draggables-module .gradient-draggable-bar:hover {
background: rgba(0,0,0,0.8);
}
.draggables-module .gradient-draggable-wrapper .colourpicker {
left: -{{INPUT_SIZE/2 - 2}}px;
}
.draggables-module .gradient-draggable-wrapper .colourpicker-input-container {
background: transparent;
}
.draggables-module .gradient-draggable-wrapper input {
height: {{INPUT_SIZE}}px;
width: {{INPUT_SIZE}}px;
padding: 0;
cursor: pointer;
font-size: 0;
border-radius: {{INPUT_SIZE}}px;
border: 1px solid #555;
position: relative;
}
.draggables-module .col-expand .colourpicker input:focus,
.draggables-module .col-expand .colourpicker:hover input {
width: 100px;
height: auto;
border-radius: 4px;
font-size: 14px;
padding: 5px;
z-index: 2;
}
", .open = "{{", .close = "}}")

generate_random_col <- function() {
  red <- sprintf("%02x", sample(256, 1) - 1)
  green <- sprintf("%02x", sample(256, 1) - 1)
  blue <- sprintf("%02x", sample(256, 1) - 1)
  paste0("#", red, green, blue)
}
normalize_draggable_position <- function(pos) {
  value <- pos / (CONTAINER_SIZE_CORRECTED)
  value <- max(0, value)
  value <- min(1, value)
  value
}

draggable_colourinput <- function(id,
                                  col = generate_random_col(),
                                  position = NULL,
                                  allow_modify = TRUE,
                                  col_expand = TRUE) {
  ns <- getDefaultReactiveDomain()$ns

  if (is.null(position)) {
    left <- 0
  } else if (is.numeric(position)) {
    left <- position*(CONTAINER_SIZE_CORRECTED)
    left <- max(0, left)
    left <- min(CONTAINER_SIZE_CORRECTED, left)
  } else if (position == "random") {
    left <- sample(CONTAINER_SIZE - (JS_DRAGGABLE_FIX_MARGIN-1), 1) - 1
  } else {
    stop("Unknown position: ", position)
  }

  tag <- tags$div(
    id = ns(paste0(id, "-draggable")),
    class = paste0("gradient-draggable-wrapper",
                   if (col_expand) " col-expand" else ""),
    style = paste0("left:", left, "px"),
    if (allow_modify)
      actionLink(ns(paste0(id, "-delete")), icon("close"),
                 class = "gradient-draggable-delete"),
    tags$div(class = "gradient-draggable-bar"),
    colourpicker::colourInput(ns(id), NULL, col, showColour = "both")
  )
  if (allow_modify) {
    shinyjqui::jqui_draggable(
      tag,
      options = list(axis = "x", handle = ".gradient-draggable-bar",
                     containment = "parent")
    )
  } else {
    shinyjqui::jqui_draggable(
      tag,
      options = list(axis = "x", handle = NULL,
                     containment = "parent")
    )
  }
}
gradientInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    htmltools::singleton(tags$head(tags$script(gradient_js))),
    htmltools::singleton(tags$head(tags$style(gradient_css))),
    tags$div(
      id = ns("draggables-box"),
      class = "draggables-module",
      `data-shiny_ns` = ns(""),
      tags$div(id = ns("draggables-container"), class = "draggables-container")
    )
  )
}

#' @param init_num Number of colours to use initially. Ignored if `init_positions`
#'   is provided.
#' @param init_positions List of positions of colours to use initially (values 0-1).
#' @param allow_modify Whether or not the user can add, delete, and change
#'   positions of colours.
#' @param col_expand Whether or not the colour input can expand into a full
#'   colour picker text box that lets the user write colour names in English.
gradientInput <- function(input, output, session,
                     init_num = 2, init_positions = NULL,
                     allow_modify = TRUE, col_expand = FALSE) {
  ns <- session$ns

  col_inputs <- reactiveVal(NULL)

  add_input <- function(id, position = NULL) {
    colourinput <- draggable_colourinput(
      id,
      position = position, allow_modify = allow_modify, col_expand = col_expand
    )

    col_inputs(c(col_inputs(), id))

    insertUI(
      selector = paste0("#", ns("draggables-container")),
      where = "beforeEnd",
      ui = colourinput
    )

    observeEvent(input[[paste0(id, "-delete")]], {
      col_inputs(setdiff(col_inputs(), id))
      shinyjs::addClass(paste0(id, "-draggable"), "invisible")
    }, once = TRUE)


  }

  # Add two colours to begin with
  isolate({
    if (is.null(init_positions)) {
      init_positions <- seq(0, 1, length.out = init_num)
    }
    for (idx in seq(init_positions)) {
      add_input(paste0("col_init_", idx), position = init_positions[idx])
    }
  })

  observeEvent(input$add_drag_col, {
    if (!allow_modify) {
      return()
    }
    id <- paste0("col_direct_", uuid::UUIDgenerate())
    add_input(id, position = input$add_drag_col / CONTAINER_SIZE)
  })

  col_positions <- reactive({
    colours <- lapply(col_inputs(), function(col_input) {
      col <- input[[col_input]]
      pos <- input[[paste0(col_input, "-draggable_left")]]
      if (is.null(col) || is.null(pos)) {
        return()
      }
      list(
        col = col,
        position = normalize_draggable_position(pos)
      )
    })
    colours <- dplyr::bind_rows(colours)
    if (nrow(colours) == 0) {
      return(data.frame(col = character(0), position = character(0)))
    }

    colours <- colours[order(colours$position), ]

    colours
  })
  col_positions_slow <- debounce(col_positions, 150)

  observeEvent(col_positions_slow(), {
    if (nrow(col_positions_slow()) == 0) {
      return()
    }
    if (nrow(col_positions_slow()) == 1) {
      background_value <- col_positions_slow()$col
      js_call <- paste0('$("#', ns("draggables-box"), '").css("background", "', background_value, '");')
      shinyjs::runjs(js_call)
      return()
    }

    percentages <- apply(col_positions_slow(), 1, function(row) {
      paste0(row[["col"]], " ", round(100*as.numeric(row[["position"]])), "%")
    })
    percentages <- paste0("(left, ", paste(percentages, collapse = ", "), ")")
    prefixes <- c("linear-gradient",
                  "-o-linear-gradient",
                  "-ms-linear-gradient",
                  "-moz-linear-gradient",
                  "-webkit-linear-gradient")
    prefixes_full <- paste0(prefixes, percentages)
    css_calls <- paste0(".css('background', '", prefixes_full, "')")
    css_calls_string <- paste(css_calls, collapse = "")
    js_call <- paste0('$("#', ns("draggables-box"), '")', css_calls_string)
    shinyjs::runjs(js_call)
  })

  return(
    col_positions_slow
  )
}
