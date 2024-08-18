library(shiny)
library(jpeg)

ui <- navbarPage("Tile app",
                 includeCSS("styles.css"),
                 tabPanel("Home",
                          fluidPage(
                            # 顶部欢迎信息
                            titlePanel("Welcome to the Tile app Homepage"),
                            p("This is the Home page where you can find the latest updates and information about the Tile app project."),
                            
                            # 产品背景
                            h3("Product Background"),
                            wellPanel(
                              p("Our tile app is designed to provide the best user experience in customizing and designing tiles. Our main objective is to meet the needs of designers and homeowners alike.")
                            ),
                            
                            # 功能介绍
                            h3("Features"),
                            fluidRow(
                              column(4, 
                                     wellPanel(
                                       icon("check-circle"), 
                                       h4("Easy Customization"),
                                       p("Easily customize your tiles with our intuitive tools.")
                                     )),
                              column(4, 
                                     wellPanel(
                                       icon("palette"), 
                                       h4("Variety of Designs"),
                                       p("Access a wide range of designs suitable for any space.")
                                     )),
                              column(4, 
                                     wellPanel(
                                       icon("shopping-cart"), 
                                       h4("Easy Ordering"),
                                       p("Order your custom tiles directly from the app.")
                                     ))
                            ),
                            
                            # 产品作用
                            h3("How it Works"),
                            p("Our app helps you visualize, customize, and order tiles for your space. Whether you are an architect, interior designer, or homeowner, our tool simplifies the process."),

                            
                            # Learn More 链接
                            h3("Learn More"),
                            p("For more details, visit our ", a(href = "https://example.com", "official website"), ".")
                          )
                 ),
                 tabPanel("Design Gallery",
                          fluidPage(
                            titlePanel("Design Gallery"),
                            p("This is the Design Gallery page.")
                          )
                 ),
                 tabPanel("Custom Design",
                          fluidPage(
                            titlePanel("Custom Design"),
                            p("This is the Custom Design page.")
                          )
                 ),
                 tabPanel("FAQ",
                          fluidPage(
                            titlePanel("FAQ"),
                            p("This is the FAQ page.")
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
                 ),
                 
)