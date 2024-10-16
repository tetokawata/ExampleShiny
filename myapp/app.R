library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("n",
                  "Sample Size:",
                  min = 1,
                  max = 50,
                  value = 30),
      numericInput("i",
                  "ID:",
                  min = 1,
                  max = 100,
                  value = 1),
      selectInput("method", label = "Method", choices = c("OLS","OLS with secondaly","OLS with 6th","LASSO","Random Forest"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    set.seed(input$i)
    data = tibble::tibble(
      X = sample(seq(0,1,0.1),input$n, replace = TRUE),
      Y = runif(input$n,-1,1) + X^2,
      True = X^2
      )

    # draw the histogram with the specified number of bins
    # 
    
    if(input$method == "OLS with secondaly"){
      data |> 
        dplyr::mutate(
          Pred = lm(
            Y ~ poly(X,2), 
            data = dplyr::pick(dplyr::everything())) |> 
            predict(dplyr::pick(dplyr::everything()))
        ) |> 
        ggplot2::ggplot(
          ggplot2::aes(
            x = X,
            y = Y
          )
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(
          alpha = 0.2
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = Pred,
            color = "Prediction"
          ),
          se = FALSE
        ) +
        ggplot2::xlim(0,1) +
        ggplot2::ylim(-1,2) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = True,
            color = "Population"
          ),
          method = "lm",
          formula = y ~ poly(x,2),
          se = FALSE
        )
    }
    else if(input$method == "OLS"){
      data |> 
        dplyr::mutate(
          Pred = lm(
            Y ~ X, 
            data = dplyr::pick(dplyr::everything())) |> 
            predict(dplyr::pick(dplyr::everything()))
        ) |> 
        ggplot2::ggplot(
          ggplot2::aes(
            x = X,
            y = Y
          )
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(
          alpha = 0.2
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = Pred,
            color = "Prediction"
          ),
          se = FALSE
        ) +
        ggplot2::xlim(0,1) +
        ggplot2::ylim(-1,2) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = True,
            color = "Population"
          ),
          method = "lm",
          formula = y ~ poly(x,2),
          se = FALSE
        )
    }
    else if(input$method == "OLS with 6th"){
      data |> 
        dplyr::mutate(
          Pred = lm(
            Y ~ poly(X,6), 
            data = dplyr::pick(dplyr::everything())) |> 
            predict(dplyr::pick(dplyr::everything()))
        ) |> 
        ggplot2::ggplot(
          ggplot2::aes(
            x = X,
            y = Y
          )
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(
          alpha = 0.2
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = Pred,
            color = "Prediction"
          ),
          se = FALSE
        ) +
        ggplot2::xlim(0,1) +
        ggplot2::ylim(-1,2) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = True,
            color = "Population"
          ),
          method = "lm",
          formula = y ~ poly(x,2),
          se = FALSE
        )
    }
    else if(input$method == "LASSO"){
      data |> 
        dplyr::mutate(
          Pred = hdm::rlasso(
            Y ~ poly(X,6), 
            data = dplyr::pick(dplyr::everything()),
            post = FALSE) |> 
            predict(dplyr::pick(dplyr::everything())) |> 
            as.numeric()
        ) |> 
        ggplot2::ggplot(
          ggplot2::aes(
            x = X,
            y = Y
          )
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(
          alpha = 0.2
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = Pred,
            color = "Prediction"
          ),
          se = FALSE
        ) +
        ggplot2::xlim(0,1) +
        ggplot2::ylim(-1,2) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = True,
            color = "Population"
          ),
          method = "lm",
          formula = y ~ poly(x,2),
          se = FALSE
        )
    }
    else if(input$method == "Random Forest"){
      data |> 
        dplyr::mutate(
          Pred = ranger::ranger(
            Y ~ X, 
            data = dplyr::pick(dplyr::everything())) |> 
            predict(dplyr::pick(dplyr::everything())) |> 
            magrittr::extract2("predictions") |> 
            as.numeric()
        ) |> 
        ggplot2::ggplot(
          ggplot2::aes(
            x = X,
            y = Y
          )
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(
          alpha = 0.2
        ) +
        ggplot2::geom_line(
          ggplot2::aes(
            y = Pred,
            color = "Prediction"
          )
        ) +
        ggplot2::xlim(0,1) +
        ggplot2::ylim(-1,2) +
        ggplot2::geom_smooth(
          ggplot2::aes(
            y = True,
            color = "Population"
          ),
          method = "lm",
          formula = y ~ poly(x,2),
          se = FALSE
        )
    }
    }
    )
  }

# Run the application 
shinyApp(ui = ui, server = server)
