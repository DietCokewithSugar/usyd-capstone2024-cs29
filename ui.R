library(shiny)
library(jpeg)

ui <- navbarPage(
  "Tile app",
  tabPanel(
    "Home",
    htmlTemplate("www/home.html")
  ),
  tabPanel(
    "Gallery",
    htmlTemplate("www/gallery.html")
  ),
  tabPanel("algorithm ",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 userInput_ui("userInput"),
                 obstaclesUI("obstacles")
               ),
               mainPanel(
                 uiOutput("dynamicUI")
               )
             )
           )
  ),
  tabPanel(
    "Design",
    htmlTemplate("www/design.html")
  ),
  tabPanel(
    "About Us",
    htmlTemplate("www/about_us.html")
  ),
  tabPanel(
    "Help",
    htmlTemplate("www/help.html")
  ),
)