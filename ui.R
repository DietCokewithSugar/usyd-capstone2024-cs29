library(shiny)
library(jpeg)
source("R/designUI.R")
source("OldUI/old_ui.R")

ui <- navbarPage(
  id = "navbar",
  "Tile app",
  tabPanel("Home", htmlTemplate("www/home.html")),
  tabPanel("Gallery", htmlTemplate("www/design_gallery.html")),
  tabPanel("Parameter Design", fluidPage(
    sidebarLayout(
      sidebarPanel(userInput_ui("userInput"), obstaclesUI("obstacles")),
      mainPanel(uiOutput("dynamicUI"))
    )
  )),
  tabPanel(
    "Family Design",
    uiOutput("new_ui_container")
  ),
  tabPanel("About Us", htmlTemplate("www/about_us.html")),
  tabPanel("Help", htmlTemplate("www/help.html"))
)