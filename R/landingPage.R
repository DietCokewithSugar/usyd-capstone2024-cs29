# landingPage.R
library(shiny)

landingPage_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        style = "display: flex; justify-content: center; align-items: flex-start; height: 85vh; margin-top: 15vh;",
        div(
          style = "background-color: white; padding: 20px; border-radius: 10px; width: 25%;",
          
          # Dropdown for Layout Options
          selectInput(
            ns("layout_option"),
            "Select a Layout Option:",
            choices = c("Stack", "Herringbone", "Basketweave", "Lattice"),
            selected = "Stack",  # Default value
            width = '100%'
          ),
          
          # Top Button (NEXT)
          div(
            actionButton(
              ns("next_button"),
              "NEXT",
              style = "width: 100%; background-color: #add8e6; color: black; margin-bottom: 10px;"  # Light blue color
            )
          ),
          
          # Bottom Button (Reset)
          div(
            actionButton(
              ns("reset"),
              "Reset",
              style = "width: 100%; background-color: #d3d3d3; color: black; margin-top: 10px;"  # Light gray color
            )
          )
        )
      )
    )
  )
}

landingPage_server <- function(id, switch_ui) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$reset, {
      # Reset the layout dropdown to its default value
      updateSelectInput(session, "layout_option", selected = "Stack")
    })
    
    observeEvent(input$next_button, {
      # Pass the selected layout option to the second page
      selected_values <- list(
        layout_option = input$layout_option
      )
      switch_ui("secondPage", selected_values)
    })
    
  })
}