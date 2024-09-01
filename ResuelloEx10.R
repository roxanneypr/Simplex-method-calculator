# Name: Roxanne Ysabel Resuello
# Date: December 23, 2022 - 11:00pm
# Exercise 08: A web application that implements quadratic spline interpolation and simplex method

# Load R packages
library(shiny)
library(shinythemes)
library(shinyMatrix)
source("ResuelloEx08.R")
source("Resuello09.R")

m <- matrix(NA, 4, 2, dimnames = list(NULL, c("x", "f(x)")))


# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "Exercise 10",
                  # QSI page
                  tabPanel("Quadratic Spline Interpolation",
                           h2("Quadratice Spline Interpolation Calculator"),
                           h2(" "),
                           sidebarPanel(
                             tags$h3("Input:"),
                             matrixInput(
                               "qsiIntput",
                               value = m,
                               class = "numeric",
                               rows = list(
                                 extend = TRUE,
                                 names = FALSE
                               ),
                               cols = list(
                                 names = TRUE
                               )
                             ),
                             p("Leave last row empty if no inputs left"),
                             numericInput("value", "Value to be approximated:", 0),
                             actionButton("submit", label = "Submit")
                             
                           ), # sidebarPanel
                           mainPanel(
                             
                             h3("Polynomial Functions"),
                             p("Polynomials per interval"),
                             verbatimTextOutput("functions"),
                             
                             h3("Estimated Value"),
                             verbatimTextOutput("estimate"),
                           ), # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  # Simplex method page
                  tabPanel("Simplex Method", 
                           h2("Simplex Method Calculator"), 
                           
                           selectInput("method", "Method:", choices = c("maximize", "minimize")),
                           
                           h3("Initial Tableau:"),
                           matrixInput(
                             "matrix",
                             value = matrix(NA, 3, 4, dimnames = list(NULL,c("colName","colName","colName","colName"))),
                             class = "numeric",
                             rows = list(
                               extend = TRUE,
                               names = FALSE
                             ),
                             cols = list(
                               editableNames = TRUE,
                               extend = TRUE,
                               names = TRUE
                             )
                           ),
                           p("Leave last row and column empty if no inputs left"),
                           
                           actionButton("simplex", label = "Submit"),
                           h3("Output: "),
                           h4("Final Tableau", align = "center"),
                           tableOutput("finalTab"),
                           h4("Basic Solution", align = "center"),
                           verbatimTextOutput("basicSol"),
                           h4("Optimal Value", align = "center"),
                           verbatimTextOutput("optVal"),
                  ),
                  # Shipping problem page
                  tabPanel("Shipping Problem",
                           h2("Minimize Shipping Cost"),

                           h3("Initial Tableau:"),
                           matrixInput(
                             "matrix2",
                             value = matrix(0, nrow = 16, ncol = 25, dimnames = list(NULL,c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "Z", "Solution"))),
                             class = "numeric",
                             rows = list(
                               extend = TRUE,
                               names = FALSE
                             ),
                             cols = list(
                               editableNames = TRUE,
                               extend = TRUE,
                               names = TRUE
                             )
                           ),
                           p("Leave last row and column empty if no inputs left."),

                           actionButton("shipping", label = "Submit"),
                           h3("Output: "),
                           h4("Final Tableau", align = "center"),
                           tableOutput("sfinalTab"),
                           h4("Basic Solution", align = "center"),
                           verbatimTextOutput("sbasicSol"),
                           h4("Optimal Value", align = "center"),
                           verbatimTextOutput("soptVal"),
                           h4("Shipped items matrix", align = "center"),
                           tableOutput("shippingNum"),
                  )
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  # Quadratic Spline Interpolation
  qsinterpolation <- eventReactive( input$submit, {
    xValues <- c(input$qsiIntput[,1])
    yValues <- c(input$qsiIntput[,2])
    x <- xValues[-length(xValues)]
    y <- yValues[-length(yValues)]
    data <- list(x,y)
    
    result = poly.qsi(data, input$value)
    return(result)
  })
  
  #QSI outputs to be shown
  output$functions = renderPrint({qsinterpolation()$qsi.fxns})
  output$estimate = renderText({qsinterpolation()$y})
  
  # Simplex Method
  simplexMethod <- eventReactive( input$simplex, {
    tableau <- input$matrix
    renderPrint(tableau)
    #print(tableau)
    
    isMax <- TRUE
    
    if(input$method == "minimize"){
      isMax <- FALSE
      print(input$method)
    }
    
    result <- simplex(tableau, isMax, FALSE)
  } )

  #Simplex method outputs to be shown
  output$finalTab <- renderTable(simplexMethod()$final.tableau)
  output$basicSol <- renderPrint(simplexMethod()$basic.solution)
  output$optVal <- renderText({simplexMethod()$opt.val})
  
  #Shipping problem
  shippingProblem <- eventReactive( input$shipping, {
    tableau1 <- input$matrix2
    renderPrint(tableau1)
    
    result <- simplex(tableau1, FALSE, TRUE)
  } )
  
  #Shipping minimization outputs to be shown
  output$sfinalTab <- renderTable(shippingProblem()$final.tableau)
  output$sbasicSol <- renderPrint(shippingProblem()$basic.solution)
  output$soptVal <- renderText({shippingProblem()$opt.val})
  output$shippingNum <- renderTable({shippingProblem()$shipping.num})
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
