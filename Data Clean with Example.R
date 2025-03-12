library(shiny)
library(bslib)
library(pwr)

ui <- page_sidebar(
  title = "Sample Size Calculator",
  sidebar = sidebar(
    selectInput("test_type", "Select Test Type",
                choices = c("t-test (Two Sample)" = "t_test",
                            "Proportion Test" = "prop_test",
                            "Correlation Test" = "correlation")),
    
    # Parameters for t-test
    conditionalPanel(
      condition = "input.test_type == 't_test'",
      numericInput("mean1", "Mean of Group 1",
                   value = 10, step = 0.1),
      numericInput("mean2", "Mean of Group 2",
                   value = 12, step = 0.1),
      numericInput("sd_pooled", "Pooled Standard Deviation",
                   value = 4, min = 0.1, step = 0.1),
      numericInput("power_t", "Statistical Power",
                   value = 0.8, min = 0.1, max = 0.99, step = 0.05),
      numericInput("sig_level_t", "Significance Level (α)",
                   value = 0.05, min = 0.01, max = 0.1, step = 0.01)
    ),
    
    # Parameters for proportion test
    conditionalPanel(
      condition = "input.test_type == 'prop_test'",
      numericInput("prop1", "Proportion 1",
                   value = 0.5, min = 0, max = 1, step = 0.05),
      numericInput("prop2", "Proportion 2",
                   value = 0.6, min = 0, max = 1, step = 0.05),
      numericInput("power_p", "Statistical Power",
                   value = 0.8, min = 0.1, max = 0.99, step = 0.05),
      numericInput("sig_level_p", "Significance Level (α)",
                   value = 0.05, min = 0.01, max = 0.1, step = 0.01)
    ),
    
    # Parameters for correlation test
    conditionalPanel(
      condition = "input.test_type == 'correlation'",
      numericInput("correlation", "Expected Correlation",
                   value = 0.3, min = -0.99, max = 0.99, step = 0.05),
      numericInput("power_c", "Statistical Power",
                   value = 0.8, min = 0.1, max = 0.99, step = 0.05),
      numericInput("sig_level_c", "Significance Level (α)",
                   value = 0.05, min = 0.01, max = 0.1, step = 0.01)
    )
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Required Sample Size",
      value = textOutput("sample_size"),
      theme = "primary"
    ),
    value_box(
      title = "Effect Size (Cohen's d)",
      value = textOutput("cohens_d"),
      theme = "secondary"
    ),
    card(
      card_header("Description"),
      textOutput("description")
    )
  )
)

server <- function(input, output) {
  
  # Calculate Cohen's d for t-test
  cohens_d <- reactive({
    if (input$test_type == "t_test") {
      d <- abs(input$mean1 - input$mean2) / input$sd_pooled
      return(d)
    }
  })
  
  sample_size <- reactive({
    if (input$test_type == "t_test") {
      result <- pwr.t.test(d = cohens_d(),
                           power = input$power_t,
                           sig.level = input$sig_level_t,
                           type = "two.sample")
      ceiling(result$n)  # n per group for t-test
      
    } else if (input$test_type == "prop_test") {
      h <- ES.h(input$prop1, input$prop2)  # Calculate effect size
      result <- pwr.2p.test(h = h,
                            power = input$power_p,
                            sig.level = input$sig_level_p)
      ceiling(result$n)  # n per group for proportion test
      
    } else if (input$test_type == "correlation") {
      result <- pwr.r.test(r = input$correlation,
                           power = input$power_c,
                           sig.level = input$sig_level_c)
      ceiling(result$n)  # total n for correlation
    }
  })
  
  output$sample_size <- renderText({
    paste0(sample_size(), " participants", 
           if(input$test_type %in% c("t_test", "prop_test")) " per group" else "")
  })
  
  output$cohens_d <- renderText({
    if (input$test_type == "t_test") {
      sprintf("%.3f", cohens_d())
    } else {
      "N/A"
    }
  })
  
  output$description <- renderText({
    if (input$test_type == "t_test") {
      paste0("This calculation provides the required sample size per group for a two-sample t-test. ",
             "Cohen's d is calculated from the means and pooled standard deviation. ",
             "Current effect size (d) = ", sprintf("%.3f", cohens_d()))
    } else if (input$test_type == "prop_test") {
      "This calculation provides the required sample size per group for comparing two proportions. Enter the expected proportions for each group."
    } else {
      "This calculation provides the total sample size needed to detect the specified correlation coefficient with the given power and significance level."
    }
  })
}

shinyApp(ui, server)
