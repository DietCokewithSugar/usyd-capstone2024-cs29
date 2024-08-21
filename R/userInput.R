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
        numericInput(
          ns("tile_spacing_num"),
          "Tile Spacing:",
          value = 5,
          min = 0,
          max = 20
        ),
        sliderInput(
          ns("tile_spacing"),
          "",
          min = 0,
          max = 20,
          value = 5
        )
      ),
      column(
        6,
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
      )
    )
  )
}

userInput_server <- function(id) {
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
    observeEvent(input$offset, {
      updateNumericInput(session, "offset_num", value = input$offset)
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
    pattern_dropdown <- reactive({
      input$pattern_dropdown
    })

    return(
      list(
        wall_height = wall_height,
        wall_width = wall_width,
        tile_height = tile_height,
        tile_width = tile_width,
        tile_spacing = tile_spacing,
        offset = offset,
        pattern_dropdown = pattern_dropdown
      )
    )
  })
}