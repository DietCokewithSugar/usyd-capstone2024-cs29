library(shiny)
library(jpeg)

ui <- navbarPage("Tile app",
                 includeCSS("styles.css"),
                 tabPanel("Home",
                          
                          htmlTemplate("www/home.html")
                          
                 ),
                 tabPanel("Design Gallery",
                          
                          htmlTemplate("www/Design_Gallery.html")
                          
                 ),
                 tabPanel("Customer Design",
                          
                          htmlTemplate("www/customer_design.html")
                          
                 ),
                 tabPanel("About Us",
                          
                          htmlTemplate("www/about_us.html")
                          
                 ),
                 tabPanel("Help",
                          
                          htmlTemplate("www/help.html")
                          
                 ),
                 
)