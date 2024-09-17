library(shiny)
library(jpeg)

ui <- navbarPage(
  id = "navbar",
  "Tile app",
  tabPanel(
    "Home",
    htmlTemplate("www/home.html")
  ),
  tabPanel(
    "Gallery",
    htmlTemplate("www/Design_Gallery.html")
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
    htmlTemplate("www/customer_design.html")
  ),
  tabPanel(
    "Gallery",
    htmlTemplate("www/Design_Gallery.html")
  ),
  tabPanel(
    "Design",
    htmlTemplate("www/customer_design.html")
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