# Load required packages
library(shiny)
library(testthat)
library(shinytest2)

# Define the test for the module
test_that("obstaclesServer module works correctly without UI rendering", {
  
  # Create a mock Shiny app for testing
  app <- shinyApp(
    ui = fluidPage(
      actionButton("add_obstacle", "Add Obstacle") # Only include necessary UI elements for testing
    ),
    server = function(input, output, session) {
      # Mock user input for wall dimensions
      userInput <- reactiveValues(wall_height = reactive({ 500 }), wall_width = reactive({ 500 }))
      
      # Call the module
      obstacles <- obstaclesServer("obstacles", userInput)
    }
  )
  
  # Start testing the app
  testServer(app, {
    
    # Test input validations
    session$setInputs(add_obstacle = 1)
    session$setInputs(new_obstacle_name = "", new_obstacle_width = 100, new_obstacle_height = 100)
    session$setInputs(confirm_add_obstacle = 1)
    expect_error(session$flushReact(), "Required", info = "Name is required") # Test for missing required inputs
    
    # Test adding an obstacle within bounds
    session$setInputs(new_obstacle_name = "TestObstacle", 
                      new_obstacle_width = 100, 
                      new_obstacle_height = 100,
                      top = 50, 
                      left = 50, 
                      right = 350, 
                      bottom = 350)
    session$setInputs(confirm_add_obstacle = 1)
    session$flushReact()
    expect_equal(length(obstacles()), 1) # Check if one obstacle is added
    expect_equal(obstacles()[[1]]$name, "TestObstacle") # Verify the name of the added obstacle
    
    # Test adding an obstacle outside bounds
    session$setInputs(new_obstacle_name = "InvalidObstacle", 
                      new_obstacle_width = 600, # Width exceeds wall width
                      new_obstacle_height = 100,
                      top = 50, 
                      left = 50, 
                      right = 50, 
                      bottom = 350)
    session$setInputs(confirm_add_obstacle = 1)
    session$flushReact()
    expect_error(session$flushReact(), "outside the wall boundaries", info = "Obstacle should not exceed wall boundaries") # Check for boundary errors
    
    # Test deleting an obstacle
    session$setInputs(delete_obstacle = obstacles()[[1]]$id) # Use the ID of the first obstacle
    session$flushReact()
    expect_equal(length(obstacles()), 0) # Verify the obstacle is deleted
    
  })
})
