library(shiny)

source("server.R")
source("ui.R")

source("R/userInput_server.R")
source("R/obstaclesServer.R")
source("R/landingPage_server.R")
source("R/secondPage_server.R")
source("R/finalPage_server.R")
source("R/validation_functions.R")

shinyApp(ui = ui, server = server)
 