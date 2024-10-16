library(colourpicker)
library(later)



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
        div(style = "margin-left: 20px;", textOutput(ns(
          "splitTilesCount"
        )))
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




obstaclesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        div(
          style = "text-align: center;",
          actionButton(ns("add_obstacle"), "Add Obstacle")
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(
          style = "text-align: center;",
          uiOutput(ns("obstacle_tiles"))
        )
      )
    )
  )
}






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



finalPage_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(
      HTML("
        .button-shadow {
          box-shadow: 0px 8px 15px rgba(0, 0, 0, 0.4);
          transition: all 0.3s ease;
        }
        .button-shadow:hover {
          box-shadow: 0px 15px 20px rgba(0, 0, 0, 0.5);
          transform: translateY(-3px);
        }
        .centered-container {
          display: flex;
          justify-content: center;
          align-items: center;
          height: calc(100vh - 120px); /* full height minus space for buttons */
        }
        .bottom-controls {
          position: fixed;
          bottom: 20px;
          width: 100%;
          display: flex;
          flex-direction: column;
          align-items: center;
        }
        .button-group {
          display: flex;
          justify-content: space-between;
          width: 400px;
        }
        .tile-count-container {
          display: flex;
          justify-content: center;
          margin-bottom: 10px;
        }
        .tile-count {
          margin-right: 20px;
          font-size: 16px;
          font-weight: bold;
        }
      ")
    )),
    
    fluidPage(
      # Center the tile plot container in the middle of the screen
      div(class = "centered-container", uiOutput(ns("dynamicWallPlot"))),
      
      # Bottom controls container that holds tile counts and buttons
      div(
        class = "bottom-controls",
        # Tile counts displayed above the arrow buttons
        div(
          class = "tile-count-container",
          div(class = "tile-count", textOutput(ns("fullTileCount"))),
          div(class = "tile-count", textOutput(ns("splitTileCount")))
        ),
        
        # Arrow buttons and controls at the bottom
        div(
          class = "button-group",
          actionButton(
            ns("fast_left"),
            "",
            icon = icon("fast-backward"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("left"),
            "",
            icon = icon("arrow-left"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("up"),
            "",
            icon = icon("arrow-up"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("down"),
            "",
            icon = icon("arrow-down"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("right"),
            "",
            class = "button-shadow",
            icon = icon("arrow-right"),
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("fast_right"),
            "",
            icon = icon("fast-forward"),
            class = "button-shadow",
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          actionButton(
            ns("reset"),
            "",
            class = "button-shadow",
            icon = icon("refresh"),
            style = "border-radius: 50%; width: 50px; height: 50px; margin-right: 10px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          ),
          downloadButton(
            ns("download_plot"),
            "",
            class = "button-shadow",
            icon = icon("download"),
            style = "border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: lightblue;"
          )
        )
      )
    )
  )
}
