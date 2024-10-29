design_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(column(
    12,
    div(
      style = "display: flex; justify-content: center; align-items: center; flex-direction: column; height: 100%;",

      uiOutput(ns("dynamicWallPlot")),

      div(
        style = "margin-top: 20px; text-align: center; display: flex; justify-content: center;",
        div(style = "margin-right: 20px;", textOutput(ns("fullTilesCount"))),
        div(style = "margin-left: 20px;", textOutput(ns("splitTilesCount"))),
        div(style = "margin-left: 20px;", textOutput(ns("tileCostSum"))),
      ),

      div(
        style = "margin-top: 20px; display: flex; justify-content: center; align-items: center;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; width: 320px;",
          actionButton(
            ns("left"),
            "",
            icon = icon("arrow-left"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          ),
          actionButton(ns("up"), "", icon = icon("arrow-up"), style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"),
          actionButton(
            ns("down"),
            "",
            icon = icon("arrow-down"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          ),
          actionButton(
            ns("right"),
            "",
            icon = icon("arrow-right"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          ),
          actionButton(
            ns("reset"),
            "",
            icon = icon("refresh"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center;"
          )
        )
      )
    )
  )))
}

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
              max = 100,
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
              ns("tile_ratio_num"),
              "Tile Ratio:",
              value = 2,
              min = 2,
              max = 20,
              width = '100%'
            )
        ),
        div(
          style = "color: red; font-weight: bold;",
          textOutput(ns("numeric_error_tile_ratio"))
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
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            colourInput(
              ns("tile_color"),
              "Choose tile color:",
              value = "#ADD8E6"
            )
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            colourInput(
              ns("tile_color_2"),
              "Choose second tile color:",
              value = "#A2EBAC"
            )
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "position: relative;",
            selectInput(
              ns("texture_option"),
              "Choose tile texture:",
              choices = c("None", "Brick", "Marble"),
              selected = "None"
            )
        )
      )
    )
  )
}
