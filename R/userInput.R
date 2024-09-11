library(colourpicker)
library(later)


source("R/obstacles.R")

userInput_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      12,
      selectInput(
        ns("pattern_dropdown"),
        "Select a Layout Option:",
        choices = c("Stack", "Herringbone", "Basketweave", "Lattice"),
        selected = "Stack"
      )
    )),
    fluidRow(
      column(
        6,
        numericInput(
          ns("wall_height_num"),
          "Wall Height:",
          value = 300,
          min = 100,
          max = 900
        ),
        sliderInput(
          ns("wall_height"),
          "",
          min = 100,
          max = 900,
          value = 300
        )
      ),
      column(
        6,
        numericInput(
          ns("wall_width_num"),
          "Wall Width:",
          value = 500,
          min = 100,
          max = 1200
        ),
        sliderInput(
          ns("wall_width"),
          "",
          min = 100,
          max = 1200,
          value = 500
        )
      )
    ),
    fluidRow(
      column(
        6,
        conditionalPanel(
          condition = paste0("input['", ns("pattern_dropdown"), "'] !== 'Lattice'"),
          numericInput(
            ns("tile_height_num"),
            "Tile Height:",
            value = 50,
            min = 10,
            max = 1000
          ),
          sliderInput(
            ns("tile_height"),
            "",
            min = 10,
            max = 1000,
            value = 50
          )
        ),
      ),
      column(
        6,
        numericInput(
          ns("tile_width_num"),
          "Tile Width:",
          value = 50,
          min = 10,
          max = 150
        ),
        sliderInput(
          ns("tile_width"),
          "",
          min = 10,
          max = 150,
          value = 50
        )
      )
    ),
    fluidRow(
      column(
        6,
        conditionalPanel(
          condition = paste0("input['", ns("pattern_dropdown"), "'] !== 'Lattice'"),
          numericInput(
            ns("tile_spacing_num"),
            "Tile Spacing:",
            value = 5,
            min = 0,
            max = 100
          ),
          sliderInput(
            ns("tile_spacing"),
            "",
            min = 0,
            max = 100,
            value = 5
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("pattern_dropdown"), "'] === 'Lattice'"),
          numericInput(
            ns("tile_spacing_num"),
            "Tile Spacing:",
            value = 5,
            min = 0,
            max = 100
          )
        ),

      ),
      column(
        6,
        conditionalPanel(
          condition = paste0("input['", ns("pattern_dropdown"), "'] === 'Stack'"),
          numericInput(
            ns("offset_num"),
            "Offset:",
            value = 25,
            min = 0,
            max = 500
          ),
          sliderInput(
            ns("offset"),
            "",
            min = 0,
            max = 100,
            value = 25
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("pattern_dropdown"), "'] === 'Herringbone'"),
          numericInput(
            ns("tile_ratio"),
            "Tile ratio: (1:x)",
            value = 2,
            min = 2,
            max = 20
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("pattern_dropdown"), "'] !== 'Stack'"),
          colourInput(
            ns("tile_color"),
            "Choose tile color:",
            value = "#E6C89B"
          )
        ),
      )
    ),
    conditionalPanel(
      condition = paste0("input['", ns("pattern_dropdown"), "'] !== 'Stack'"),
      fluidRow(
        column(
          6,
          colourInput(
            ns("tile_two_color"),
            "Choose intersection tile color:",
            value = "#8B4513"
          )
        ),
        column(
          6,
        )
      )
    ),
  )
}


userInput_server <- function(id, herringbone_sv) {
  moduleServer(id, function(input, output, session) {
    # Synchronize the numeric input and slider
    observeEvent(input$wall_height_num, {
      updateSliderInput(session, "wall_height", value = input$wall_height_num)
    })
    observeEvent(input$wall_height, {
      updateNumericInput(session, "wall_height_num", value = input$wall_height)
    })

    observeEvent(input$wall_width_num, {
      updateSliderInput(session, "wall_width", value = input$wall_width_num)
    })
    observeEvent(input$wall_width, {
      updateNumericInput(session, "wall_width_num", value = input$wall_width)
    })

    observeEvent(input$tile_height_num, {
      updateSliderInput(session, "tile_height", value = input$tile_height_num)
    })
    observeEvent(input$tile_height, {
      updateNumericInput(session, "tile_height_num", value = input$tile_height)
    })

    observeEvent(input$tile_width_num, {
      updateSliderInput(session, "tile_width", value = input$tile_width_num)
    })
    observeEvent(input$tile_width, {
      updateNumericInput(session, "tile_width_num", value = input$tile_width)
    })

    observeEvent(input$tile_spacing_num, {
      updateSliderInput(session, "tile_spacing", value = input$tile_spacing_num)
    })
    observeEvent(input$tile_spacing, {
      updateNumericInput(session, "tile_spacing_num", value = input$tile_spacing)
    })

    observeEvent(input$offset_num, {
      updateSliderInput(session, "offset", value = input$offset_num)
    })
    observeEvent(input$tile_ratio, {
      updateNumericInput(session, "tile_ratio", value = input$tile_ratio)
    })
    observeEvent(input$tile_ratio, {
      updateColourInput(session, "tile_color", value = input$tile_color)
    })
    observeEvent(input$tile_ratio, {
      updateColourInput(session, "tile_two_color", value = input$tile_two_color)
    })
    observeEvent(input$pattern_dropdown, {
      pattern_dropdown_value <- input$pattern_dropdown
    })

    # use debounce to postpone response
    wall_height <- debounce(reactive(input$wall_height), millis = 500)
    wall_width <- debounce(reactive(input$wall_width), millis = 500)
    tile_height <- debounce(reactive(input$tile_height), millis = 500)
    tile_width <- debounce(reactive(input$tile_width), millis = 500)
    tile_spacing <- debounce(reactive(input$tile_spacing), millis = 500)
    offset <- debounce(reactive(input$offset), millis = 500)
    tile_ratio <- debounce(reactive(input$tile_ratio), millis = 500)
    tile_color <- debounce(reactive(input$tile_color), millis = 500)
    tile_two_color <- debounce(reactive(input$tile_two_color), millis = 500)
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