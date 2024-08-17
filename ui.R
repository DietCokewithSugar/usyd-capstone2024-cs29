library(shiny)

ui <- navbarPage("CS29",
                 tabPanel("Home",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                userInput_ui("userInput")
                              ),
                              mainPanel(
                                horizontalStack_ui("horizontalStack")
                              )
                            )
                          )
                 ),
                 tabPanel("About Us",
                          fluidPage(
                            style = "margin-top: 20px;",
                            sidebarLayout(
                              sidebarPanel(
                                h3("Explore More"),
                                p("Check out our projects and services."),
                                tags$ul(
                                  tags$li(a(href = "#", "Our Projects")),
                                  tags$li(a(href = "#", "Our Services")),
                                  tags$li(a(href = "#", "Contact Us"))
                                ),
                                width = 3
                              ),
                              mainPanel(
                                fluidPage(
                                  tags$iframe(
                                    src = "about.html",
                                    width = "100%",
                                    height = "600px",
                                    frameborder = 0
                                  )
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("Help",
                          fluidPage(
                            titlePanel("Help"),
                            p("This is the Help page.")
                          )
                 )
)