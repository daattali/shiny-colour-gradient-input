# Colour gradient input for R-Shiny

A colour gradient picker for Shiny, implemented as a shiny module. This can be used, for example, in ggplots with the `scale_color_gradientn()` function. This can most likely be improved upon by making it a proper input instead of a module--left as an exercise for the reader :)

![screenshot](https://i.imgur.com/KfA7htH.png)

The UI function is `gradientInputUI()` and has two parameters:

- **width**: The width of the input (either in pixels or in percent) (default: 300px)
- **resource_path**: The path where the JavaScript and CSS files are located.

The server function is `gradientInput()` and has 4 parameters:

- **init_cols**: The initial state of the colour gradient. There are 4 different values that are accepted:
    1. If a single integer N is provided (eg. `5`), then the input is initialized with N colours. The colours are random and their positions are evenly distributed. If nothing is provided, the default behaviour is to initialize the input with 2 colours.
    2. If a vector of N integers is provided (each number must be between 0 and 100) (eg. `c(10, 40, 90)`), the input is initialized with N different colours and each number in the vector corresponds to the position of one colour. The colours are random.
    3. If a vector of N colours is provided (a colour can be any R colour name or a HEX string) (eg. `c("red", "blue", "#00FF00")`), the input is initialized with these N colours. The positions of the colours are distributed evenly.
    4. If a dataframe with 2 columns "col" (any R colour) and "position" (number between 0 and 100) and N rows is provided, the input is initialized with N colours. Each colour uses the "col" column as its initial colour and "position" as its initial position.
- **allow_modify**: Whether or not the user can add, delete, and change positions of colours. (default: true)
- **col_expand**: Whether or not the colour input can expand into a full colour picker text box that lets the user write colour names in English. (default: false)

The return value is a dataframe with 2 columns: `position` (the left position, between 0 and 100) and `col` (the colour hex string).


Example usage:

```
library(shiny)
source("gradientInput.R")

ui <- fluidPage(
  gradientInputUI("cols"),
  tableOutput("result")
)
server <- function(input, output, session) {
  result <- callModule(gradientInput, "cols", init_cols = c(10, 50, 70))
  output$result <- renderTable(result())
}
shinyApp(ui, server)
```

Required packages: `glue`, `colourpicker`, `shinyjs`, `shinyjqui`, `uuid`, `dplyr`
