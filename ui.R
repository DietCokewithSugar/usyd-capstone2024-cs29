library(shiny)



ui <- navbarPage("CS29",
                 
                 # First tab: Main application with modules
                 tabPanel("Home",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                module1_ui("mod1")
                              ),
                              mainPanel(
                                module2_ui("mod2")
                              )
                            )
                          )
                 ),
                 
                 # Additional tabs can be added here
                 tabPanel("About Us",
                          fluidPage(
                            # Adding some top margin to the page
                            style = "margin-top: 20px;",
                            
                            sidebarLayout(
                              # Adding some content or a placeholder in the sidebar to balance the layout
                              sidebarPanel(
                                h3("Explore More"),
                                p("Check out our projects and services."),
                                tags$ul(
                                  tags$li(a(href = "#", "Our Projects")),
                                  tags$li(a(href = "#", "Our Services")),
                                  tags$li(a(href = "#", "Contact Us"))
                                ),
                                width = 3  # Adjust sidebar width if needed
                              ),
                              
                              mainPanel(
                                # Centering and styling the content in the main panel
                                div(
                                  style = "padding: 20px; background-color: #f9f9f9; border-radius: 10px; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
                                  includeHTML("html/about.html")
                                ),
                                width = 9  # Adjust main panel width if needed
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