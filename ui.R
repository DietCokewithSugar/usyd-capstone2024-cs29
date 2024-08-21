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
    htmlTemplate("www/Design_Gallery.html")
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
