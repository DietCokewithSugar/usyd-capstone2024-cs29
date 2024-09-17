library(shiny)
library(jpeg)

ui <- navbarPage(
  id = "navbar",
  "Tile app",
  tabPanel("Home", htmlTemplate("www/home.html")),
  tabPanel("Gallery", htmlTemplate("www/design_gallery.html")),
  tabPanel("Design", fluidPage(
    sidebarLayout(
      sidebarPanel(userInput_ui("userInput"), obstaclesUI("obstacles")),
      mainPanel(uiOutput("dynamicUI"))
    )
  )),
  tabPanel("About Us", htmlTemplate("www/about_us.html")),
  tabPanel("Help", htmlTemplate("www/help.html")),
  tabPanel(
    "New UI",
    uiOutput("new_ui_container")
  )
)