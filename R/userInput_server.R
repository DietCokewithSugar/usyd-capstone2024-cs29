# R/userInput_server.R

userInput_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    # 加载验证函数
    source("R/validation_functions.R", local = TRUE)

    observeEvent(input$wall_height_num, {
      numeric_error <- validate_numeric_input(input$wall_height_num, min_value = 100, max_value = 900)
      output$numeric_error_wall_height <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })

    # ... 其他 observeEvent 与 reactive 表达式（保持原样） ...
    observeEvent(input$wall_width_num, {
      numeric_error <- validate_numeric_input(input$wall_width_num, min_value = 100, max_value = 1200)
      output$numeric_error_wall_width <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })

    observeEvent(input$tile_height_num, {
      numeric_error <- validate_numeric_input(input$tile_height_num, min_value = 10, max_value = 1000)
      output$numeric_error_tile_height <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })

    observeEvent(input$tile_width_num, {
      numeric_error <- validate_numeric_input(input$tile_width_num, min_value = 10, max_value = 150)
      output$numeric_error_tile_width <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })

    observeEvent(input$tile_spacing_num, {
      numeric_error <- validate_numeric_input(input$tile_spacing_num, min_value = 0, max_value = 100)
      output$numeric_error_tile_spacing <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })

    observeEvent(input$tile_ratio_num, {
      numeric_error <- validate_numeric_input(input$tile_ratio_num, min_value = 2, max_value = 20)
      output$numeric_error_tile_ratio <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })

    observeEvent(input$offset_num, {
      numeric_error <- validate_numeric_input(input$offset_num, min_value = 0, max_value = 500)
      output$numeric_error_offset <- renderText({
        if (!is.null(numeric_error)) {
          return(numeric_error)
        } else {
          return("")
        }
      })
    })

    wall_height <- debounce(reactive({
      if (is.null(validate_numeric_input(input$wall_height_num, min_value = 100, max_value = 900))) {
        input$wall_height_num
      } else {
        300
      }
    }), millis = 500)

    wall_width <- debounce(reactive({
      if (is.null(validate_numeric_input(input$wall_width_num, min_value = 100, max_value = 1200))) {
        input$wall_width_num
      } else {
        500
      }
    }), millis = 500)

    tile_height <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_height_num, min_value = 10, max_value = 1000))) {
        input$tile_height_num
      } else {
        50
      }
    }), millis = 500)

    tile_width <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_width_num, min_value = 10, max_value = 150))) {
        input$tile_width_num
      } else {
        50
      }
    }), millis = 500)

    tile_spacing <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_spacing_num, min_value = 0, max_value = 100))) {
        input$tile_spacing_num
      } else {
        5
      }
    }), millis = 500)

    tile_ratio <- debounce(reactive({
      if (is.null(validate_numeric_input(input$tile_ratio_num, min_value = 2, max_value = 20))) {
        input$tile_ratio_num
      } else {
        2
      }
    }), millis = 500)

    offset <- debounce(reactive({
      if (is.null(validate_numeric_input(input$offset_num, min_value = 0, max_value = 500))) {
        input$offset_num
      } else {
        25
      }
    }), millis = 500)

    tile_color <- debounce(reactive(
      input$tile_color
    ), millis = 500)

    tile_color_2 <- debounce(reactive(
      input$tile_color_2
    ), millis = 500)

    pattern_dropdown <- reactive({
      input$pattern_dropdown
    })

    texture_option <- debounce(reactive(
      input$texture_option
    ), millis = 500)

    # 2 way sync
    updating <- reactiveValues(flag = TRUE)

    # 返回值
    return(
      list(
        wall_height = wall_height,
        wall_width = wall_width,
        tile_height = tile_height,
        tile_width = tile_width,
        tile_spacing = tile_spacing,
        offset = offset,
        tile_ratio = tile_ratio,
        tile_color = tile_color,
        tile_color_2 = tile_color_2,
        pattern_dropdown = pattern_dropdown,
        texture_option = texture_option,
        session = session
      )
    )
  })
}