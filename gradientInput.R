library(shiny)

generate_random_col <- function() {
  red <- sprintf("%02x", sample(256, 1) - 1)
  green <- sprintf("%02x", sample(256, 1) - 1)
  blue <- sprintf("%02x", sample(256, 1) - 1)
  paste0("#", red, green, blue)
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
    if (position < 0 || position > 100) {
      stop("position must be beween 0 and 100", call. = FALSE)
    }
    left <- position
  } else if (position == "random") {
    left <- sample(1000, 1) / 10
  } else {
    stop("Unknown position: ", position)
  }

  tag <- tags$div(
    id = ns(paste0(id, "-draggable")),
    class = paste0("gradient-draggable-wrapper",
                   if (col_expand) " col-expand" else ""),
    style = paste0("left:", left, "%"),
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
gradientInputUI <- function(id, width = NULL, resource_path = ".") {
  ns <- NS(id)
  addResourcePath("gradientInputRes", resource_path)

  if (is.null(width)) {
    width <- 300
  }
  width <- validateCssUnit(width)

  tagList(
    shinyjs::useShinyjs(),
    htmltools::singleton(tags$head(
      tags$script(src = file.path("gradientInputRes", "gradientInput.js")),
      tags$link(href = file.path("gradientInputRes", "gradientInput.css"), rel = "stylesheet")
    )),
    tags$div(
      id = ns("draggables-box"),
      class = "draggables-module",
      style = paste0("width: ", width),
      `data-shiny_ns` = ns(""),
      tags$div(id = ns("draggables-container"), class = "draggables-container")
    )
  )
}

#' @param init_num Number of colours to use initially. Ignored if `init_positions`
#'   is provided.
#' @param init_positions List of positions of colours to use initially (values 0-100).
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

  shinyjs::runjs(paste0("GradientInputInitResize('", ns(""), "')"))

  # Add two colours to begin with
  isolate({
    if (is.null(init_positions)) {
      init_positions <- seq(0, 100, length.out = init_num)
    }
    for (idx in seq(init_positions)) {
      add_input(paste0("col_init_", idx), position = init_positions[idx])
    }
  })

  gradient_resize_slow <- debounce(reactive(input$gradient_resize), 150)
  observeEvent(gradient_resize_slow(), {
    if (nrow(col_positions_slow()) == 0) {
      return()
    }
    shinyjs::runjs(paste0("GradientInputReposition('", ns(""), "', ",
                          jsonlite::toJSON(col_positions_slow()), ")"))
  })

  observeEvent(input$add_drag_col, {
    if (!allow_modify) {
      return()
    }
    id <- paste0("col_direct_", uuid::UUIDgenerate())
    add_input(id, position = input$add_drag_col)
  })

  col_positions <- reactive({
    colours <- lapply(col_inputs(), function(col_input) {
      col <- input[[col_input]]
      pos <- input[[paste0(col_input, "-draggable_percent")]]
      if (is.null(col) || is.null(pos)) {
        return()
      }
      list(
        id = col_input,
        col = col,
        position = pos
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
      paste0(row[["col"]], " ", round(as.numeric(row[["position"]])), "%")
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

  retval <- reactive({
    col_positions_slow()[c("col", "position")]
  })

  return(
    retval
  )
}
