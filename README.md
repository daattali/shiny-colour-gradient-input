# Colour gradient input for R-Shiny

A colour gradient picker for Shiny, implemented as a shiny module. This can be used, for example, in ggplots with the `scale_color_gradientn()` function. This can most likely be improved upon by making it a proper input instead of a module--left as an exercise for the reader :)

![screenshot](https://i.imgur.com/KfA7htH.png)

The UI function is `gradientInputUI()` and has no parameters. The server function is `gradientInput()` and has 4 parameters:

- **init_num**: Number of colours to use initially. Ignored if `init_positions` is provided. (default: 2)
- **init_positions**: List of positions of colours to use initially (allowed values between 0 to 1).
- **allow_modify**: Whether or not the user can add, delete, and change positions of colours. (default: true)
- **col_expand**: Whether or not the colour input can expand into a full colour picker text box that lets the user write colour names in English. (default: false)

The return value is a dataframe with 2 columns: `position` (the left position, betweenn 0 to 1) and `col` (the colour hex string).

Note that the colours are initialized as random colours every time.

Example usage:

```
library(shiny)
source("gradientInput.R")

ui <- fluidPage(
  gradientInputUI("cols"),
  tableOutput("result")
)
server <- function(input, output, session) {
  result <- callModule(gradientInput, "cols", init_positions = c(0.1, 0.5, 0.7))
  output$result <- renderTable(result())
}
shinyApp(ui, server)
```

Required packages: `glue`, `colourpicker`, `shinyjs`, `shinyjqui`, `uuid`, `dplyr`
