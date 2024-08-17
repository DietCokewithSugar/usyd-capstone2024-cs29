library(shiny)

ui <- navbarPage("CS29",
                 tabPanel("Home",
                          fluidPage(
                            titlePanel("Welcome to the CS29 Homepage"),
                            p("This is the Home page where you can find the latest updates and information about the CS29 project."),
                            
                            # 添加一个子标题
                            h3("Project Overview"),
                            p("The CS29 project is focused on building a user-friendly interface that meets the needs of our clients and stakeholders."),
                            
                            # 添加一个图片
                            img(src = "https://via.placeholder.com/400x200", alt = "Project Image", height = "200px"),
                            
                            # 添加一个表格
                            h3("Key Milestones"),
                            tableOutput("milestonesTable"),
                            
                            # 添加一个链接
                            h3("Learn More"),
                            p("For more details, visit our ", a(href = "https://example.com", "official website"), ".")
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
                 tabPanel("Contact",
                          fluidPage(
                            titlePanel("Contact"),
                            p("This is the Contact page.")
                          )
                 )
)