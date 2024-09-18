library(shiny)
library(jpeg)

ui <- navbarPage(
  id = "navbar",
  "Tile app",
  tabPanel("Home", htmlTemplate("www/home.html")),
  tabPanel("Gallery1", htmlTemplate("www/Design_Gallery.html")),
  tabPanel("algorithm ", fluidPage(
    sidebarLayout(
      sidebarPanel(userInput_ui("userInput"), obstaclesUI("obstacles")),
      mainPanel(uiOutput("dynamicUI"))
    )
  )),
  tabPanel("Design1", htmlTemplate("www/customer_design.html")),
  tabPanel("Gallery2", htmlTemplate("www/Design_Gallery.html")),
  tabPanel("Design2", htmlTemplate("www/customer_design.html")),
  tabPanel("About Us", htmlTemplate("www/about_us.html")),
  tabPanel("Help", htmlTemplate("www/help.html")),
  tabPanel(
    "New UI",
    uiOutput("new_ui_container")
  )
)