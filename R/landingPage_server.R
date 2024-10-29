# R/landingPage_server.R

landingPage_server <- function(id, selected_values, switch_ui) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Pre-fill inputs if selected_values are provided (from the second page)
    if (!is.null(selected_values)) {
      # If selected_values includes wall-related fields (from secondPage), do nothing
      if (!is.null(selected_values$wall_height)) {
        # wall values already provided, no need to update class, family, or layout
      } else {
        # This is from the landing page, update class, family, and layout
        updateSelectInput(session, "class_option", selected = selected_values$class_option)
        updateSelectInput(session, "family_option", selected = selected_values$family_option)
        updateSelectInput(session, "layout_option", selected = selected_values$layout_option)
      }
    }

    # Define class-to-family mapping
    family_choices <- list(
      "One tile patterns" = c("Family A", "Family B", "Family C"),
      "Two tile patterns" = c("Family D", "Family E", "Family F"),
      "Three tile patterns" = c("Family G", "Family H", "Family I"),
      "Four (and more) tile patterns" = c("Family J", "Family K", "Family L")
    )

    layout_choices <- list(
      "Family A" = c("Stack", "Herringbone"),
      "Family B" = c("Basketweave", "Lattice"),
      "Family C" = c("Hexagon", "Diamond"),
      "Family D" = c("Square", "Rectangle"),
      "Family E" = c("Linear", "Diagonal"),
      "Family F" = c("Grid", "Chevron"),
      "Family G" = c("Flemish", "Windmill"),
      "Family H" = c("Versailles", "Cobblestone"),
      "Family I" = c("Mosaic", "Basketweave"),
      "Family J" = c("Herringbone", "Chevron"),
      "Family K" = c("Basketweave", "Stack"),
      "Family L" = c("Lattice", "Hexagon")
    )

    # Reset the inputs to their default values
    observeEvent(input$reset, {
      updateSelectInput(session, "class_option", selected = "One tile patterns")
      updateSelectInput(session, "family_option", selected = family_choices[["One tile patterns"]][1])
      updateSelectInput(session, "layout_option", selected = layout_choices[["Family A"]][1])
    })

    # Update family choices when class_option changes
    observeEvent(input$class_option, {
      selected_class <- input$class_option
      families <- family_choices[[selected_class]]

      # Update the family selectInput based on the selected class
      updateSelectInput(session, "family_option",
                        choices = families,
                        selected = families[1])

      # Update layout for the first family in the updated family list
      layouts <- layout_choices[[families[1]]]
      updateSelectInput(session, "layout_option",
                        choices = layouts,
                        selected = layouts[1])
    })

    # Update layout choices when family_option changes
    observeEvent(input$family_option, {
      selected_family <- input$family_option
      layouts <- layout_choices[[selected_family]]

      # Update the layout selectInput based on the selected family
      updateSelectInput(session, "layout_option",
                        choices = layouts,
                        selected = layouts[1])
    })

    # Handle the NEXT button click to move to final page
    observeEvent(input$next_button, {
      # Collect the values from the landing page
      landing_page_values <- list(
        class_option = input$class_option,
        family_option = input$family_option,
        layout_option = input$layout_option
      )

      # Combine the landingPage values with the secondPage values
      final_values <- if (!is.null(selected_values)) {
        c(landing_page_values, selected_values)  # Merge both lists
      } else {
        landing_page_values  # Only landing page values
      }

      # Switch to final page with all values
      switch_ui("finalPage", final_values)
    })
  })
}
