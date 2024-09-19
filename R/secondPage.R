library(shiny)
source("R/obstacles.R")

secondPage_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        style = "display: flex; justify-content: center; align-items: flex-start; height: 85vh; margin-top: 15vh;",
        div(
          style = "background-color: white; padding: 20px; border-radius: 10px; width: 50%;",

          fluidRow(
            column(
              width = 12,
              h4("Selected Layout Option:"),
              textOutput(ns("selected_layout_option"))
            )
          ),
          
          numericInput(ns("wall_height"), "Wall Height:", value = 300, min = 100, max = 900, width = '100%'),
          numericInput(ns("wall_width"), "Wall Width:", value = 500, min = 100, max = 1200, width = '100%'),
          numericInput(ns("wall_offset"), "Wall Offset:", value = 25, min = 0, max = 500, width = '100%'),
          numericInput(ns("wall_grout"), "Wall Grout:", value = 5, min = 0, max = 20, width = '100%'),
          selectInput(ns("tile_size"), "Tile Size:", choices = c("30x30", "50x50", "70x70"), selected = "50x50", width = '100%'),
          
          div(
            style = "margin-top: 20px;",
            actionButton(ns("submit_button"), "Next", style = "width: 100%; background-color: #add8e6; color: black; margin-bottom: 10px;"),
            actionButton(ns("back"), "Back", style = "width: 100%; background-color: #ffa500; color: white; margin-top: 10px;")  # Orange for back
          ),
          
          div(
            style = "margin-top: 20px;",
            obstaclesUI(ns("obstacles_module"))  
          )
        )
      )
    )
  )
}


secondPage_server <- function(id, selected_values, switch_ui) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print("second")
    output$selected_layout_option <- renderText({ selected_values$layout_option })

    obstacles <- obstaclesServer("obstacles_module", list(
      wall_height = reactive({ input$wall_height }),
      wall_width = reactive({ input$wall_width })
    ))

    observeEvent(input$submit_button, {
      final_values <- list(
        layout_option = selected_values$layout_option,
        wall_height = input$wall_height,
        wall_width = input$wall_width,
        wall_offset = input$wall_offset,
        wall_grout = input$wall_grout,
        tile_size = input$tile_size,
        obstacles = obstacles()
      )

      switch_ui("finalPage", final_values)
    })

    observeEvent(input$back, {
      # Ensure it only triggers once by invalidating the session or using a condition
      isolate({
        switch_ui("landingPage", NULL)
      })
    })
  })
}