library(shiny)

ui <- navbarPage("CS29",
                 tabPanel("Home",
                          fluidPage(
                            titlePanel("Home"),
                            p("This is the Home page.")
                          )
                 ),
                 tabPanel("About Us",
                          fluidPage(
                            titlePanel("About Us"),
                            p("This is the About Us page.")
                          )
                 ),
                 tabPanel("Help",
                          fluidPage(
                            titlePanel("Help"),
                            p("This is the Help page.")
                          )
                 )
)