simulate_S <- function(t, lambda, mu, nrep=100000) {
  Svals <- numeric(nrep)
  for (i in 1:nrep) {
    n <- rpois(1, lambda * t)
    if (n > 0) Svals[i] <- sum(rexp(n, mu))
  }
  Svals
}

# Example histograms
set.seed(1)
lambda <- 0.5
mu <- 1

tvals <- c(10, 100, 1000, 10000)

par(mfrow=c(2,2))
for (tt in tvals) {
  S <- simulate_S(tt, lambda, mu)
  hist(S, 50, main=paste("t =", tt), xlab="S(t)", col="gray")
  
}
library(shiny)

simulate_S <- function(t, lambda, mu, nrep=20000) {
  Svals <- numeric(nrep)
  for (i in 1:nrep) {
    n <- rpois(1, lambda * t)
    if (n > 0) Svals[i] <- sum(rexp(n, mu))
  }
  Svals
}

ui <- fluidPage(
  titlePanel("Compound Poisson Process: S(t) = sum X_i"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda", "Arrival rate λ:", 
                  min = 0.1, max = 5, value = 1, step = 0.1),
      sliderInput("mu", "Jump rate μ:", 
                  min = 0.1, max = 5, value = 1, step = 0.1),
      sliderInput("t", "Time t:", 
                  min = 1, max = 10000, value = 100, step = 1, 
                  animate = TRUE),
      sliderInput("nrep", "Simulations:", 
                  min = 5000, max = 100000, value = 20000, step = 5000)
    ),
    
    mainPanel(
      plotOutput("histPlot"),
      verbatimTextOutput("summaryOut")
    )
  )
)

server <- function(input, output) {
  
  data_sim <- reactive({
    simulate_S(input$t, input$lambda, input$mu, input$nrep)
  })
  
  output$histPlot <- renderPlot({
    hist(data_sim(), breaks=50, col="gray",
         main = paste("Histogram of S(t) at t =", input$t),
         xlab="S(t)")
  })
  
  output$summaryOut <- renderPrint({
    x <- data_sim()
    cat("Empirical mean =", mean(x), "\n")
    cat("Empirical var  =", var(x), "\n\n")
    cat("Theory mean  =", input$lambda * input$t / input$mu, "\n")
    cat("Theory var   =", 2 * input$lambda * input$t / input$mu^2, "\n")
  })
}

shinyApp(ui, server)

