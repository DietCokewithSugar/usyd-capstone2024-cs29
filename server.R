library(shiny)


source("R/controller.R")
source("R/pattern.R")
source("R/designUI.R")

server <- function(input, output, session) {
  herringbone_sv <- reactiveValues(tile_width = 40, tile_height = 20, tile_ratio = 2, input_type = NULL)
  userInput_server_return_values <- userInput_server("userInput", herringbone_sv)
  obstaclesServer_return_values <- obstaclesServer("obstacles", userInput = userInput_server_return_values)
  
  
  
  #dynamic page rendering starts
  
  current_ui <- reactiveVal("landingPage")
  page_data <- reactiveVal(NULL)

  switch_ui <- function(new_ui, values = NULL) {
    if (!is.null(values)) {
      page_data(values)
    }
    current_ui(new_ui)
  }

  observe({
    ui <- current_ui()

    if (ui == "landingPage") {
      output$new_ui_container <- renderUI({
        landingPage_ui("landing_page")
      })
      landingPage_server("landing_page", switch_ui = switch_ui)

    } else if (ui == "secondPage") {
      output$new_ui_container <- renderUI({
        secondPage_ui("second_page")
      })
      secondPage_server("second_page", selected_values = page_data(), switch_ui = switch_ui)

    } else if (ui == "finalPage") {
      output$new_ui_container <- renderUI({
        finalPage_ui("final_page")
      })
      finalPage_server("final_page", input_data = page_data(), switch_ui = switch_ui)
    }
  })
  
  #dynamic page rendering ends
  

  #dynamic page rendering starts
  
  current_ui <- reactiveVal("landingPage")
  page_data <- reactiveVal(NULL)
  
  switch_ui <- function(new_ui, values = NULL) {
    if (!is.null(values)) {
      page_data(values)
    }
    current_ui(new_ui)
  }
  
  observe({
    ui <- current_ui()
    
    if (ui == "landingPage") {
      output$new_ui_container <- renderUI({
        landingPage_ui("landing_page")
      })
      landingPage_server("landing_page", switch_ui = switch_ui)
      
    } else if (ui == "secondPage") {
      output$new_ui_container <- renderUI({
        secondPage_ui("second_page")
      })
      secondPage_server("second_page", selected_values = page_data(), switch_ui = switch_ui)
      
    } else if (ui == "finalPage") {
      output$new_ui_container <- renderUI({
        finalPage_ui("final_page")
      })
      finalPage_server("final_page", input_data = page_data())
    }
  })
  
  output$dynamicUI <- renderUI({
    # Assuming there is a variable `someVar` from the user input that determines which UI to show
    if (userInput_server_return_values$pattern_dropdown() == "Stack") {
      design_ui("horizontalStack")
    } else if (userInput_server_return_values$pattern_dropdown() == "Herringbone") {
      design_ui("herringbone")
    } else if (userInput_server_return_values$pattern_dropdown() == "Basketweave") {
      design_ui("basketweave")
    } else if (userInput_server_return_values$pattern_dropdown() == "Lattice") {
      design_ui("lattice")
    } else {
      # other_module_ui("otherModule")  # Assuming another module UI is defined
    }
  })

  observe({
    if (userInput_server_return_values$pattern_dropdown() == "Stack") {
      horizontalStack_server(
        id = "horizontalStack",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_width = userInput_server_return_values$tile_width,
        tile_spacing = userInput_server_return_values$tile_spacing,
        offset = userInput_server_return_values$offset,
        tile_color = userInput_server_return_values$tile_color,
        texture_option = userInput_server_return_values$texture_option,
        obstacles = obstaclesServer_return_values
      )
    } else if (userInput_server_return_values$pattern_dropdown() == "Herringbone") {
      cat("height: ",herringbone_sv$tile_height, "width: ",herringbone_sv$tile_width, "ratio: ",herringbone_sv$tile_ratio, "type: ",herringbone_sv$input_type, "\n")
      herringbone_server(
        id = "herringbone",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        herringbone_sv = herringbone_sv,
        tile_spacing = userInput_server_return_values$tile_spacing,
        tile_color = userInput_server_return_values$tile_color,
        tile_color_2 = userInput_server_return_values$tile_color_2,
        obstacles = obstaclesServer_return_values,
        input_session = userInput_server_return_values$session
      )
    } else if (userInput_server_return_values$pattern_dropdown() == "Basketweave") {
      basketweave_server(
        id = "basketweave",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_width = userInput_server_return_values$tile_width,
        tile_spacing = userInput_server_return_values$tile_spacing,
        tile_color = userInput_server_return_values$tile_color,
        tile_color_2 = userInput_server_return_values$tile_color_2,
        obstacles = obstaclesServer_return_values
      )
    } else if (userInput_server_return_values$pattern_dropdown() == "Lattice") {
      lattice_server(
        id = "lattice",
        wall_height = userInput_server_return_values$wall_height,
        wall_width = userInput_server_return_values$wall_width,
        tile_height = userInput_server_return_values$tile_height,
        tile_spacing = userInput_server_return_values$tile_spacing,
        tile_color = userInput_server_return_values$tile_color,
        tile_color_2 = userInput_server_return_values$tile_color_2,
        obstacles = obstaclesServer_return_values
      )
    } else {
      # other_module_server("otherModule", ...)  # Assuming other module server is defined
    }
  })



}
