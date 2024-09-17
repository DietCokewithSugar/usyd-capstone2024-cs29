library(colourpicker)
library(later)


source("R/obstacles.R")
source("R/validation.R")

userInput_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        12,
        selectInput(
          ns("pattern_dropdown"),
          "Select a Layout Option:",
          choices = c("Stack", "Herringbone", "Basketweave", "Lattice"),
          selected = "Stack"
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            numericInput(
              ns("wall_height_num"),
              "Wall Height:",
              value = 300,
              min = 100,
              max = 900,
              width = '100%'
            )
        ),
        div(
          style = "color: red; font-weight: bold;",
          textOutput(ns("numeric_error_wall_height"))
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            numericInput(
              ns("wall_width_num"),
              "Wall Width:",
              value = 500,
              min = 100,
              max = 1200,
              width = '100%'
            )
        ),
        div(
          style = "color: red; font-weight: bold;",
          textOutput(ns("numeric_error_wall_width"))
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            numericInput(
              ns("tile_height_num"),
              "Tile Height:",
              value = 50,
              min = 10,
              max = 1000,
              width = '100%'
            )
        ),
        div(
          style = "color: red; font-weight: bold;",
          textOutput(ns("numeric_error_tile_height"))
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            numericInput(
              ns("tile_width_num"),
              "Tile Width:",
              value = 50,
              min = 10,
              max = 150,
              width = '100%'
            )
        ),
        div(
          style = "color: red; font-weight: bold;",
          textOutput(ns("numeric_error_tile_width"))
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            numericInput(
              ns("tile_spacing_num"),
              "Tile Spacing:",
              value = 5,
              min = 0,
              max = 20,
              width = '100%'
            )
        ),
        div(
          style = "color: red; font-weight: bold;",
          textOutput(ns("numeric_error_tile_spacing"))
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            numericInput(
              ns("offset_num"),
              "Offset:",
              value = 25,
              min = 0,
              max = 500,
              width = '100%'
            )
        ),
        div(
          style = "color: red; font-weight: bold;",
          textOutput(ns("numeric_error_offset"))
        )
      )
    )
  )
}



userInput_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$wall_height_num, {
      numeric_error <- validate_numeric_input(input$wall_height_num, min_value = 100, max_value = 900)
      output$numeric_error_wall_height <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
          return("")
        }
      })
    })
    
    observeEvent(input$wall_width_num, {
      numeric_error <- validate_numeric_input(input$wall_width_num, min_value = 100, max_value = 1200)
      output$numeric_error_wall_width <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$tile_height_num, {
      numeric_error <- validate_numeric_input(input$tile_height_num, min_value = 10, max_value = 1000)
      output$numeric_error_tile_height <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$tile_width_num, {
      numeric_error <- validate_numeric_input(input$tile_width_num, min_value = 10, max_value = 150)
      output$numeric_error_tile_width <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$tile_spacing_num, {
      numeric_error <- validate_numeric_input(input$tile_spacing_num, min_value = 0, max_value = 20)
      output$numeric_error_tile_spacing <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    observeEvent(input$offset_num, {
      numeric_error <- validate_numeric_input(input$offset_num, min_value = 0, max_value = 500)
      output$numeric_error_offset <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })
    
    wall_height <- debounce(reactive({
      if (is.null(validate_numeric_input(input$wall_height_num, min_value = 100, max_value = 900))) {
        input$wall_height_num
      } else {
        300
      }
    }), millis = 500)
    
    wall_width <- debounce(reactive({
      if (is.null(validate_numeric_input(input$wall_width_num, min_value = 100, max_value = 1200))) {
        input$wall_width_num
      } else {
        500
      }
    }), millis = 500)
    
    tile_height <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_height_num, min_value = 10, max_value = 1000))) {
        input$tile_height_num
      } else {
        50
      }
    }), millis = 500)
    
    tile_width <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_width_num, min_value = 10, max_value = 150))) {
        input$tile_width_num
      } else {
        50
      }
    }), millis = 500)
    
    tile_spacing <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_spacing_num, min_value = 0, max_value = 20))) {
        input$tile_spacing_num
      } else {
        5
      }
    }), millis = 500)
    
    offset <- debounce(reactive({
      if (is.null(validate_numeric_input(input$offset_num, min_value = 0, max_value = 500))) {
        input$offset_num
      } else {
        25
      }
    }), millis = 500)
    
    pattern_dropdown <- reactive({
      input$pattern_dropdown
    })

    updating <- reactiveValues(flag = TRUE)

    observe({

      if(input$pattern_dropdown == "Herringbone") {
        # cat(herringbone_sv$tile_height,"input\n")
        observeEvent(input$tile_height, {
          if(updating$flag){
            updating$flag <- FALSE
            cat("change tile_height \n")
            herringbone_sv$tile_height <- input$tile_height
            herringbone_sv$input_type <- "tile_height"
            later(function() {
              updating$flag <- TRUE
            }, 0.5)
          }
        })

        observeEvent(input$tile_width, {
          if(updating$flag){
            updating$flag <- FALSE
            cat("change tile_width \n")
            herringbone_sv$tile_width <- input$tile_width
            herringbone_sv$input_type <- "tile_width"
            later(function() {
              updating$flag <- TRUE
            }, 0.5)
          }
        })

        observeEvent(input$tile_ratio, {
          if(updating$flag){
            updating$flag <- FALSE
            cat("change tile_ratio \n")
            herringbone_sv$tile_ratio <- input$tile_ratio
            herringbone_sv$input_type <- "tile_ratio"
            later(function() {
              updating$flag <- TRUE
            }, 0.5)
          }
        })

        observeEvent(input$tile_spacing, {
          if(updating$flag){
            updating$flag <- FALSE
            cat("change tile_spacing \n")
            later(function() {
              updating$flag <- TRUE
            }, 0.5)
          }
        })
      }
    })


    return(
      list(
        wall_height = wall_height,
        wall_width = wall_width,
        tile_height = tile_height,
        tile_width = tile_width,
        tile_spacing = tile_spacing,
        offset = offset,
        tile_ratio = tile_ratio,
        tile_color = tile_color,
        tile_two_color = tile_two_color,
        pattern_dropdown = pattern_dropdown,
        session = session
      )
    )
  })

}