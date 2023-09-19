#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define server logic required to draw a histogram
function(input, output, session) {
  
  result_container <- reactiveValues()
  
  filtered_data <- reactive({
    if (is.null(input$userID)) {
      return(NULL)
    }
    
    filtered <- dataset %>% filter(userID == input$userID)
    
    if (!is.null(input$intensity) && length(input$intensity) > 0) {
      filtered <- filtered %>% filter(intensity %in% input$intensity)
    }
    
    if (!is.null(input$distance) && length(input$distance) > 0) {
      filtered <- filtered %>% filter(distance %in% input$distance)
    }
    
    return(filtered)
  })
  
  filtered_dframe <- reactive({
    if (is.null(input$userID)) {
      return(NULL)
    }
    
    filtered <- dframe 
    
    if (!is.null(input$intensity) && length(input$intensity) > 0) {
      filtered <- filtered %>% filter(intensity %in% input$intensity)
    }
    
    if (!is.null(input$distance) && length(input$distance) > 0) {
      filtered <- filtered %>% filter(distance %in% input$distance)
    }
    
    if (regression_state()) {
      req(filtered_data())
      
      fitted_model <- glm(as.formula(paste0("cbind(Yes, No) ~", isolate(input$formula_rhs))),
                          family=binomial(),
                          data = filtered_data())
      filtered$pred <- predict(fitted_model, newdata=filtered, type="response")
      result_container[['fitted_model']] <- fitted_model
    }
    
    return(filtered)
  }) %>% bindEvent(input$update_button, regression_state())
  
  output$filteredTable <- DT::renderDataTable({
    req(filtered_data())
    datatable(filtered_data(), filter = 'top',
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                stateSave = TRUE
              ))
  })
  
  output$filteredPlot <- renderPlot({
    # Replace this part with your ggplot2 code
    result_plot <- ggplot(filtered_data(), aes(x = signal, y = y)) +
      geom_point() +
      facet_grid(as.formula(paste0(input$facet1, "~", input$facet2)),
                 labeller="label_both")
    
    if (regression_state()) {
      
      if (input$groupVar1 == ".") {
        result_plot <- result_plot + 
          geom_line(aes(y = pred), #group = distance, colour = factor(distance) 
                    data = filtered_dframe())
      } else {
        result_plot <- result_plot + 
          geom_line(aes(y = pred, group = .data[[input$groupVar1]],
                        colour = factor(.data[[input$groupVar1]])), 
                    data = filtered_dframe())
      }
      
    }
    
    result_plot
  })
  
  regression_state <- reactiveVal(FALSE)
  
  observeEvent(input$logistic_regression, {
    regression_state(!regression_state())
  })
  
  output$model_summary <- renderPrint({
    req(result_container[['fitted_model']])
    
    summary(result_container[['fitted_model']])
  })
  
  observeEvent(input$draw_effect, {
    req(result_container[['fitted_model']])
    
    coefs <- broom::tidy(result_container[['fitted_model']])
    coefs <- coefs %>% mutate(
      variable_value = parse_number(term)
    ) %>% drop_na()
    
    output$effectPlot <- renderPlot({
      result_effect_plot <- coefs %>% 
        ggplot(aes(x = variable_value, y = estimate)) + 
        geom_point() +
        scale_x_continuous(breaks = coefs$variable_value,
                           trans = input$trans1) +
        scale_y_continuous(trans = input$trans2)
      
      if(input$effect_lm) {
        result_effect_plot <- result_effect_plot +
          geom_smooth(method = "lm", se = FALSE)
      }
      
      result_effect_plot
      
    })
  })
  
  # base model
  observe({
    req(input$userID_value, 
        input$intensity_value,
        input$distance_value)
    
    result_container[['base_model_data']] <- dataset %>% filter(userID %in% input$userID_value)
    result_container[['base_model_dframe']] <- dframe
    if (!is.null(input$intensity_value)) {
      result_container[['base_model_data']] <- 
        result_container[['base_model_data']] %>% filter(intensity %in% input$intensity_value)
      result_container[['base_model_dframe']] <- 
        result_container[['base_model_dframe']] %>% filter(intensity %in% input$intensity_value)
    }
    
    if (!is.null(input$distance_value)) {
      result_container[['base_model_data']] <- 
        result_container[['base_model_data']] %>% filter(distance %in% input$distance_value)
      result_container[['base_model_dframe']] <- 
        result_container[['base_model_dframe']] %>% filter(distance %in% input$distance_value)
    }
    
  }) %>% bindEvent(input$userID_value, input$intensity_value, input$distance_value)
  
  observe({
    
    req(input$userID_value,
        result_container[['base_model_data']],
        result_container[['base_model_dframe']])
    
    output$base_model_formula <- renderPrint({
      intercept_value <- if_else(input$check_intercept, "1", "-1")
      formula_string <- paste0("cbind(Yes, No) ~ ", intercept_value, " + signal")
      result_container[['base_formula']] <- as.formula(formula_string)
      cat(formula_string)
    })
    
    output$base_model_logit_fit <- renderPlot({
      req(result_container[['base_model_data']], 
          result_container[['base_model_dframe']],
          result_container[['base_formula']])
      
      base_model <- glm(result_container[['base_formula']], 
                        family = binomial(),
                        data = result_container[['base_model_data']])
      result_container[['base_model_dframe']]$pred <- predict(
        base_model, newdata = result_container[['base_model_dframe']], type = 'response'
      )
      result_container[['base_model']] <- base_model
      
      result_container[['base_model_data']] %>% ggplot(aes(x = signal)) +
        geom_point(aes(y = y)) +
        geom_line(aes(y = pred), data = result_container[['base_model_dframe']])
      
    })
    
    output$base_model_summary <- renderPrint({
      req(result_container[['base_model']])
      summary(result_container[['base_model']])
    })
    
  }) %>% bindEvent(input$base_model_draw)
  
  
  ################## END OF BASE MODEL #################
  
  # distance
  observe({
    req(input$userID_value)
    
    result_container[['distance_model_data']] <- dataset %>% filter(userID %in% input$userID_value)
    result_container[['distance_model_dframe']] <- dframe
    if (!is.null(input$intensity_value_distance)) {
      result_container[['distance_model_data']] <- 
        result_container[['distance_model_data']] %>% filter(intensity %in% input$intensity_value_distance)
      result_container[['distance_model_dframe']] <- 
        result_container[['distance_model_dframe']] %>% filter(intensity %in% input$intensity_value_distance)
    }
    
    output$distance_factor_fit_plot <- renderPlot({
      req(result_container[['distance_model_data']],
          result_container[['distance_model_dframe']])
      
      intercept_value <- if_else(input$check_intercept, "1", "-1")
      formula_string <- paste0("cbind(Yes, No) ~ ", intercept_value, " + signal:factor(distance)")
      factor_distance_model <- glm(as.formula(formula_string), 
                                   family = binomial(),
                                   data = result_container[['distance_model_data']])
      result_container[['distance_model_dframe']]$pred_factor <- predict(
        factor_distance_model, newdata = result_container[['distance_model_dframe']], type = 'response'
      )
      
      output$distance_factor_formula <- renderPrint({
        cat(formula_string)
      })
      
      
      result_container[['factor_distance_model']] <- factor_distance_model
      result_container[['base_distance_model']] <- 
        glm(as.formula(paste0("cbind(Yes, No) ~ ", intercept_value, " + signal")),
            family = binomial(),
            data = result_container[['distance_model_data']])
      
      
      result_factor_fit_plot <- result_container[['distance_model_data']] %>% ggplot(aes(x = signal)) +
        geom_point(aes(y = y)) + 
        geom_line(aes(y = pred_factor, group = factor(distance)), 
                  data = result_container[['distance_model_dframe']]) +
        facet_grid(distance~.)
      
      # factor model
      coefs <- broom::tidy(result_container[['factor_distance_model']])
      coefs <- coefs %>% mutate(
        variable_value = parse_number(term)
      ) %>% drop_na()
      
      result_effect_plot <- coefs %>% 
        ggplot(aes(x = variable_value, y = estimate)) + 
        geom_point() +
        scale_x_continuous(breaks = coefs$variable_value) +
        geom_smooth(aes(colour = "linear"), method = 'lm', formula = y ~ x, se = FALSE) +
        geom_smooth(aes(colour = "log"), method = 'glm', formula = y ~ log(x), se = FALSE) +
        guides(colour = guide_legend("Transformation")) +
        xlab("distance") +
        ylab("estimated effect")
      
      result_factor_fit_plot + result_effect_plot
      
    })
    
  }) %>% bindEvent(input$intensity_value_distance, input$userID_value)
  
  # distance plot
  observe({
    req(input$userID_value, result_container[['distance_model_data']])
    
    output$distance_model_formula <- renderPrint({
      intercept_value <- if_else(input$check_intercept, "1", "-1")
      log_value <- if_else(input$check_distance_log, " + signal:log(distance)", " + signal:distance")
      formula_string <- paste0("cbind(Yes, No) ~ ", intercept_value, " + signal", log_value)
      result_container[['distance_formula']] <- as.formula(formula_string)
      cat(formula_string)
    })
    
    output$distance_model_logit_fit_plot <- renderPlot({
      req(result_container[['distance_model_data']], 
          result_container[['distance_model_dframe']],
          result_container[['distance_formula']],
          result_container[['factor_distance_model']])
      
      distance_model <- glm(result_container[['distance_formula']], 
                            family = binomial(),
                            data = result_container[['distance_model_data']])
      result_container[['distance_model_dframe']]$pred <- predict(
        distance_model, newdata = result_container[['distance_model_dframe']], type = 'response'
      )
      result_container[['distance_model']] <- distance_model
      
      result_container[['distance_model_data']] %>% ggplot(aes(x = signal)) +
        geom_point(aes(y = y)) + 
        geom_line(aes(y = pred, group = factor(distance), linetype = "numeric"), 
                  data = result_container[['distance_model_dframe']]) +
        geom_line(aes(y = pred_factor, group = factor(distance), linetype = "factor"),
                  data = result_container[['distance_model_dframe']]) +
        facet_grid(distance~.) +
        scale_linetype_manual(values = c("twodash", "solid"),
                              name = "Models",
                              guide = guide_legend(reverse = TRUE) )
      
    })
    
    output$distance_model_summary <- renderPrint({
      req(result_container[['distance_model']])
      summary(result_container[['distance_model']])
    })
    
    output$anova_distance <- renderPrint({
      req(result_container[['distance_model']],
          result_container[['factor_distance_model']])
      
      anova(result_container[['base_distance_model']],
            result_container[['distance_model']],
            result_container[['factor_distance_model']])
      
    })
    
    
  }) %>% bindEvent(input$distance_model_draw)
  
  ######## END OF DISTANCE ##############
  
  # intensity
  observe({
    req(input$userID_value)
    
    result_container[['intensity_model_data']] <- dataset %>% filter(userID %in% input$userID_value)
    result_container[['intensity_model_dframe']] <- dframe
    if (!is.null(input$distance_value_intensity)) {
      result_container[['intensity_model_data']] <- 
        result_container[['intensity_model_data']] %>% filter(distance %in% input$distance_value_intensity)
      result_container[['intensity_model_dframe']] <- 
        result_container[['intensity_model_dframe']] %>% filter(distance %in% input$distance_value_intensity)
    }
    
    output$intensity_factor_fit_plot <- renderPlot({
      req(result_container[['intensity_model_data']],
          result_container[['intensity_model_dframe']])
      
      intercept_value <- if_else(input$check_intercept, "1", "-1")
      formula_string <- paste0("cbind(Yes, No) ~ ", intercept_value, " + signal:factor(intensity)")
      factor_intensity_model <- glm(as.formula(formula_string), 
                                    family = binomial(),
                                    data = result_container[['intensity_model_data']])
      result_container[['intensity_model_dframe']]$pred_factor <- predict(
        factor_intensity_model, newdata = result_container[['intensity_model_dframe']], type = 'response'
      )
      
      output$intensity_factor_formula <- renderPrint({
        cat(formula_string)
      })
      
      
      result_container[['factor_intensity_model']] <- factor_intensity_model
      result_container[['base_intensity_model']] <- 
        glm(as.formula(paste0("cbind(Yes, No) ~ ", intercept_value, " + signal")),
            family = binomial(),
            data = result_container[['intensity_model_data']])
      
      result_factor_fit_plot <- result_container[['intensity_model_data']] %>% ggplot(aes(x = signal)) +
        geom_point(aes(y = y)) + 
        geom_line(aes(y = pred_factor, group = factor(intensity)), 
                  data = result_container[['intensity_model_dframe']]) +
        facet_grid(intensity~.)
      
      # factor model
      coefs <- broom::tidy(result_container[['factor_intensity_model']])
      coefs <- coefs %>% mutate(
        variable_value = parse_number(term)
      ) %>% drop_na()
      
      result_effect_plot <- coefs %>% 
        ggplot(aes(x = variable_value, y = estimate)) + 
        geom_point() +
        scale_x_continuous(breaks = coefs$variable_value) +
        geom_smooth(aes(colour = "linear"), method = 'lm', formula = y ~ x, se = FALSE) +
        geom_smooth(aes(colour = "log"), method = 'glm', formula = y ~ log(x), se = FALSE) +
        guides(colour = guide_legend("Transformation")) +
        xlab("intensity") +
        ylab("estimated effect")
      
      result_factor_fit_plot + result_effect_plot
      
    })
    
  }) %>% bindEvent(input$distance_value_intensity, input$userID_value)
  
  # intensity plot
  observe({
    req(input$userID_value, result_container[['intensity_model_data']])
    
    output$intensity_model_formula <- renderPrint({
      intercept_value <- if_else(input$check_intercept, "1", "-1")
      log_value <- if_else(input$check_intensity_log, " + signal:log(intensity)", " + signal:intensity")
      formula_string <- paste0("cbind(Yes, No) ~ ", intercept_value, " + signal", log_value)
      result_container[['intensity_formula']] <- as.formula(formula_string)
      cat(formula_string)
    })
    
    output$intensity_model_logit_fit_plot <- renderPlot({
      req(result_container[['intensity_model_data']], 
          result_container[['intensity_model_dframe']],
          result_container[['intensity_formula']],
          result_container[['factor_intensity_model']])
      
      intensity_model <- glm(result_container[['intensity_formula']], 
                             family = binomial(),
                             data = result_container[['intensity_model_data']])
      result_container[['intensity_model_dframe']]$pred <- predict(
        intensity_model, newdata = result_container[['intensity_model_dframe']], type = 'response'
      )
      result_container[['intensity_model']] <- intensity_model
      
      result_container[['intensity_model_data']] %>% ggplot(aes(x = signal)) +
        geom_point(aes(y = y)) + 
        geom_line(aes(y = pred, group = factor(intensity), linetype = "numeric"), 
                  data = result_container[['intensity_model_dframe']]) +
        geom_line(aes(y = pred_factor, group = factor(intensity), linetype = "factor"),
                  data = result_container[['intensity_model_dframe']]) +
        facet_grid(intensity~.) +
        scale_linetype_manual(values = c("twodash", "solid"),
                              name = "Models",
                              guide = guide_legend(reverse = TRUE) )
      
    })
    
    output$intensity_model_summary <- renderPrint({
      req(result_container[['intensity_model']])
      summary(result_container[['intensity_model']])
    })
    
    output$anova_intensity <- renderPrint({
      req(result_container[['intensity_model']],
          result_container[['factor_intensity_model']])
      
      anova(result_container[['base_intensity_model']],
            result_container[['intensity_model']],
            result_container[['factor_intensity_model']])
      
    })
    
    
  }) %>% bindEvent(input$intensity_model_draw)
  
  ######## END OF INTENSITY ##############
  
  # both
  observe({
    req(input$userID_value)
    
    result_container[['both_model_data']] <- dataset %>% filter(userID %in% input$userID_value)
    result_container[['both_model_dframe']] <- dframe
    
    output$both_fit_plot <- renderPlot({
      req(result_container[['both_model_data']],
          result_container[['both_model_dframe']])
      
      intercept_value <- if_else(input$check_intercept, "1", "-1")
      formula_string <- paste0("cbind(Yes, No) ~ ", intercept_value, " + signal:factor(intensity):factor(distance)")
      factor_both_model <- glm(as.formula(formula_string), 
                               family = binomial(),
                               data = result_container[['both_model_data']])
      result_container[['both_model_dframe']]$pred_factor <- predict(
        factor_both_model, newdata = result_container[['both_model_dframe']], type = 'response'
      )
      
      result_container[['both_factor_formula']] <- formula_string
      
      result_container[['factor_both_model']] <- factor_both_model
      result_container[['base_both_model']] <- 
        glm(as.formula(paste0("cbind(Yes, No) ~ ", intercept_value, " + signal")),
            family = binomial(),
            data = result_container[['both_model_data']])
      
      result_factor_fit_plot <- result_container[['both_model_data']] %>% ggplot(aes(x = signal)) +
        geom_point(aes(y = y)) + 
        geom_line(aes(y = pred_factor), 
                  data = result_container[['both_model_dframe']]) +
        facet_grid(intensity~distance)
      
      result_container[['both_factor_plot']] <- result_factor_fit_plot
      
      result_factor_fit_plot
      
    })
    
    output$both_factor_formula <- renderPrint({
      req(result_container[['both_factor_formula']])
      cat(result_container[['both_factor_formula']])
    })
    
    output$both_model_formula <- renderPrint({
      cat("")
    })
    
    output$anova_both <- renderPrint({
      cat("")
    })
    
    output$both_model_summary <- renderPrint({
      cat("")
    })
    
    
  }) %>% bindEvent(input$userID_value, input$check_intercept)
  
  # both plot
  observe({
    req(input$userID_value, result_container[['both_model_data']])
    
    output$both_model_formula <- renderPrint({
      intercept_value <- if_else(input$check_intercept, "1", "-1")
      log_value_intensity <- if_else(input$check_intensity_log, " + signal:log(intensity)", " + signal:intensity")
      log_value_distance <- if_else(input$check_distance_log, " + signal:log(distance)", " + signal:distance")
      formula_string <- paste0(
        "cbind(Yes, No) ~ ", intercept_value, " + signal", 
        if_else(input$include_distance, log_value_distance, ""),
        if_else(input$include_intensity, log_value_intensity, "")
      )
      result_container[['both_formula']] <- as.formula(formula_string)
      cat(formula_string)
    })
    
    output$both_fit_plot <- renderPlot({
      req(result_container[['both_model_data']], 
          result_container[['both_model_dframe']],
          result_container[['both_formula']],
          result_container[['factor_both_model']],
          result_container[['both_factor_plot']])
      
      both_model <- glm(result_container[['both_formula']], 
                        family = binomial(),
                        data = result_container[['both_model_data']])
      result_container[['both_model_dframe']]$pred <- predict(
        both_model, newdata = result_container[['both_model_dframe']], type = 'response'
      )
      result_container[['both_model']] <- both_model
      
      result_container[['both_model_data']] %>% ggplot(aes(x = signal)) +
        geom_point(aes(y = y)) + 
        geom_line(aes(y = pred, linetype = "numeric"), 
                  data = result_container[['both_model_dframe']]) +
        geom_line(aes(y = pred_factor, linetype = "factor"),
                  data = result_container[['both_model_dframe']]) +
        facet_grid(intensity~distance) +
        scale_linetype_manual(values = c("twodash", "solid"),
                              name = "Models",
                              guide = guide_legend(reverse = TRUE) )
      
    })
    
    output$both_model_summary <- renderPrint({
      req(result_container[['both_model']])
      summary(result_container[['both_model']])
    })
    
    output$anova_both <- renderPrint({
      req(result_container[['both_model']],
          result_container[['factor_both_model']])
      
      anova(result_container[['base_both_model']],
            result_container[['both_model']],
            result_container[['factor_both_model']])
      
    })
    
    
  }) %>% bindEvent(input$both_model_draw)
  
  observe({
    req(result_container[['both_model']],
        result_container[['both_formula']])
    
    glm_formula <- result_container[['both_formula']]
    glm_formula_string <- format(glm_formula)
    
    result_container[['RE_formula_part_str']] <- " + (1 + signal + signal:log(distance) | userID)"
    
    m2_formula_string <- paste0(glm_formula_string, 
                                result_container[['RE_formula_part_str']])
    m2_formula <- as.formula(m2_formula_string)
    
    result_container[['m2']] <- m2 <- lme4::glmer(m2_formula, 
                                                  data = dataset, 
                                                  family = binomial())
    
    output$RE_model_summary <- renderPrint({
      summary(result_container[['m2']])
    })
    
    # browser()
    
  }) %>% bindEvent(input$add_RE)
  
  observe({
    req(input$include_distance, input$include_intensity)
    
    output$select_FE <- renderUI({
      tagList(
        checkboxInput("include_distance_fe", "include distance?", input$include_distance),
        checkboxInput("include_intensity_fe", "include intensity?", input$include_intensity)
      )
    })
    
  })
  
  observe({
    req(result_container[['both_model']],
        result_container[['both_formula']],
        result_container[['RE_formula_part_str']],
        result_container[['m2']])
    
    # determine the new formula
    log_value_intensity <- if_else(input$check_intensity_log, 
                                   " + signal:log(intensity)", " + signal:intensity")
    log_value_distance <- if_else(input$check_distance_log, 
                                  " + signal:log(distance)", " + signal:distance")
    distance_part <- if_else(input$include_distance_fe, 
                             log_value_distance, "")
    intensity_part <- if_else(input$include_intensity_fe,
                              log_value_intensity, "")
    intercept_value <- if_else(input$check_intercept, "1", "-1")
    
    comp_formula_string <- paste0(
      "cbind(Yes, No) ~ ", intercept_value, " + signal", 
      distance_part,
      intensity_part,
      result_container[['RE_formula_part_str']]
    )
    
    comp_formula <- as.formula(comp_formula_string)
    
    result_container[['m_update']] <- m_update <- lme4::glmer(comp_formula, 
                                                          data = dataset, 
                                                          family = binomial())
    m0 <- result_container[['m2']]
    
    output$RE_updated_summary <- renderPrint({
      summary(m_update)
    })
    
    output$RE_model_comparison_anova <- renderPrint({
      anova(m0,
            m_update)
      
    })
    
    
  }) %>% bindEvent(input$update_FE_of_RE)
  
  # observe({
  #   req(result_container[['m0']], 
  #       result_container[['m1']],
  #       result_container[['m2']],
  #       input$re_type)
  #   
  #   output$RE_model_summary <- renderPrint({
  #     if (input$re_type == 'm0') {
  #       result_container[['random_type']] <- ""
  #       summary(result_container[['m0']])
  #     } else if (input$re_type == 'm1') {
  #       result_container[['random_type']] <- " + (-1 + signal | userID)"
  #       summary(result_container[['m1']])
  #     } else if (input$re_type == 'm2') {
  #       result_container[['random_type']] <- " + (1 + signal | userID)"
  #       summary(result_container[['m2']])
  #     } else {
  #       result_container[['random_type']] <- NULL
  #       cat("")
  #     }
  #   })
  #   
  #   # browser()
  #   
  # }) %>% bindEvent(input$re_type)
  
  
  
}













