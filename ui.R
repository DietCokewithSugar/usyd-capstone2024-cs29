library(shiny)

ui <- navbarPage("CS29",
                 tabPanel("Home",

                          htmlTemplate("www/home.html")

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
                 tabPanel("Gallery",

                          htmlTemplate("www/Design_Gallery.html")

                 ),
                 tabPanel("Design",

                          htmlTemplate("www/customer_design.html")

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

