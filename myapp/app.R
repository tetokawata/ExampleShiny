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
      numericInput("d",
                  "Degree:",
                  min = 2,
                  max = 10,
                  value = 2),
      numericInput("i",
                  "ID:",
                  min = 1,
                  max = 100,
                  value = 1)
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
    data |> 
      dplyr::mutate(
        Pred = lm(
          Y ~ poly(X,input$d), 
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
      ggplot2::geom_point() +
      ggplot2::geom_smooth(
        ggplot2::aes(
          y = Pred,
          color = "y ~ poly(x,d)"
        ),
        se = FALSE
      ) +
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        ggplot2::aes(
          color = "y ~ x"
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
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
