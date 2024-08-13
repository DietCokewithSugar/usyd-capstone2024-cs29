library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Simple Shiny Greeting App"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Please enter your name:", "")
    ),
    mainPanel(
      h3(textOutput("greeting"))
    )
  )
)

# Server logical
server <- function(input, output) {
  output$greeting <- renderText({
    if (input$name != "") {
      paste("Hello,", input$name, "! Welcome to the Shiny world!")
    } else {
      "Please enter your name on the left."
    }
  })
}

# Run
shinyApp(ui = ui, server = server)