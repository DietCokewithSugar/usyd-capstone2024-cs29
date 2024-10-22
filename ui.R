library(shiny)
library(jpeg)
library(colourpicker)
source("R/designUI.R")

ui <- navbarPage(
  id = "navbar",
  "Tile app",
  tabPanel("Home", htmlTemplate("www/home.html")),
  tabPanel("Gallery", htmlTemplate("www/design_gallery.html")),
  tabPanel(
    "Design",
    uiOutput("new_ui_container")
  ),
  tabPanel("About Us", htmlTemplate("www/about_us.html")),
  tabPanel("Help", htmlTemplate("www/help.html")),
  
  
  
  
  
  
  tabPanel("", fluidPage(
    tags$head(
      tags$style(HTML("
      #userInputDiv, #obstaclesDiv {
        display: none !important;  /* 完全隐藏，不占用空间 */
      }
    "))
    ),
    sidebarLayout(
      sidebarPanel(
        div(id = "userInputDiv", userInput_ui("userInput")),
        div(id = "obstaclesDiv", obstaclesUI("obstacles"))
      ),
      mainPanel(uiOutput("dynamicUI"))
    )
  ))
)