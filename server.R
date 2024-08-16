library(shiny)

source("R/module1.R")
source("R/module2.R")


server <- function(input, output, session) {
  
  # module1_server("mod1")
  # module2_server("mod2")
  
  mod1_vals <- module1_server("mod1")
  module2_server("mod2", wallHeight = mod1_vals$wallHeight,
                 wallWidth = mod1_vals$wallWidth,
                 tileWidth = mod1_vals$tileWidth,
                 tileHeight = mod1_vals$tileHeight)
  
  
}
