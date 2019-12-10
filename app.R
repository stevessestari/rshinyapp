# Loading Shine Library
library(shiny)
library(spuRs)

# Function that defines the Secant method to be used by this application
secant <- function(ftn, x0, x1, tol = 1e-9, max.iter = 100) {
  
  # initialise
  fxold <-ftn(x0)
  fxnew <- ftn(x1)
  x2 <-0
  iter <- 0
  
  # continue iterating until stopping conditions are met
  for (i in 1:max.iter) {
    x2 <- x1 - fxnew / ((fxnew-fxold)/(x1-x0))
    if (abs(x2 - x1) < tol){
      cat("Algorithm converged\n")
      return(x2)
    }
    x0 <- x1
    x1 <- x2
    fxold <- fxnew
    fxnew <- ftn(x1)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", x2, "\n")
  }
  cat("Algorithm failed to converge\n")
  return(NULL)
}

# Defining the functions that returns the first equation 
ftn1 <- function(x) cos(x) -x
ftn1new <- function(x){
  fx <- cos(x) -x
  dfx <- -sin(x) -1
  return(c(fx,dfx))
}
ftn1fix <- function(x) cos(x)

# Defining the functions that returns the second equation 
ftn2 <- function(x) x^2 -x -1
ftn2new <- function(x){
  fx <- x^2 -x -1
  dfx <- eval(D(expression(x^2 -x -1), 'x'))
  return(c(fx,dfx))
}
ftn2fix <- function(x) 1+1/x

# Defining the functions that returns the thirdy equation 
ftn3 <- function(x) log(x) -exp(-x)
ftn3new <- function(x){
  fx <- log(x) -exp(-x)
  dfx <- x^-1 + exp(-x)
  return(c(fx,dfx))
}

ftn3fix <- function(x) exp(exp(-x))

# Defining the functions that returns the fourth equation 
ftn4 <- function(x) exp(2*x) -x -6
ftn4new <- function(x){
  fx <- exp(2*x) -x -6
  dfx <- -1 +2*exp(2*x)
  return(c(fx,dfx))
}
ftn4fix <- function(x) 0.5*log(x+6)

# Defining the functions that returns the fifth equation 
ftn5 <- function(x) exp(-x) -x
ftn5new <- function(x){
  fx <- exp(-x) -x
  dfx <- eval(D(expression(exp(-x) -x), 'x'))
  return(c(fx,dfx))
}
ftn5fix <- function(x) exp(-x)


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Root Finding Methods Demonstration"),
    br(),
    br(),
    
    fluidRow(
      column(3,
             h3("Function"),
             br(),
             radioButtons("sel_func", h4("Select one function"),
                          choices = list("f(x) = cos(x) -x" = 1, "f(x) = xe2 -x -1" = 2,
                                         "f(x) = log(x) − exp(-x)" = 3, "f(x) = exp(2x) -x -6" = 4, 
                                         "f(x) = exp(-x) -x" = 5),selected = 1),
             br(),
             h4("We consider XL for the Bisection method and X0 for the Secant method to be equal to 0 
                for all the above examples functions.")
      ),
      column(4, offset = 1,
             h3("Initial value"),
             br(),
             sliderInput('init_value', h4('Select the initial value'), 
                         min=1, max=3, value=1, 
                         step=1, round=0),
             br(),
             br(),
             br(),
             br(),
             h4("The selected initial value corresponds to X0 for the Newton Raphson and Fixed Point methods. 
                For Bisection, the selected initial value corresponds to XR, and for the Secant method corresponds to X1.")
      ),
      column(4,
             h3("Output"),
             br(),
             # Output for Newton Raphson Method 
             htmlOutput("root_methods1"),
             # Output for Bisection Method
             htmlOutput("root_methods2"),
             # Output for Fixed Point Method
             htmlOutput("root_methods3"),
             # Output for Secant Method
             htmlOutput("root_methods4"),
             
      )
    )
)

# Define server logic required to calculate the root and output the result
server <- function(input, output) {
  output$root_methods1 <- renderUI({
    switch(input$sel_func,
    '1'={
      fun_result <- newtonraphson(ftn1new,input$init_value)
    },
    '2'={
      if(input$init_value != 1){
        fun_result <- newtonraphson(ftn2new,input$init_value)
      } else{
        fun_result <- "failed to converge"
      }
    },
    '3'={
        fun_result <- newtonraphson(ftn3new,input$init_value)
    },
    '4'={
      fun_result <- newtonraphson(ftn4new,input$init_value)
    },
    '5'={
      fun_result <- newtonraphson(ftn5new,input$init_value)
    }
    )
    h4("Newton Raphson =",fun_result)
  })
  output$root_methods2 <- renderUI({
    switch(input$sel_func,
           '1'={
             fun_result <- bisection(ftn1,0,input$init_value)
           },
           '2'={
             if(input$init_value != 1){
               fun_result <- bisection(ftn2,0,input$init_value)
             } else{
               fun_result <- "failed to converge"
             }
           },
           '3'={
             if(input$init_value != 1){
               fun_result <- bisection(ftn3,0,input$init_value)
             }else{
               fun_result <- "failed to converge"
             }
           },
           '4'={
             fun_result <- bisection(ftn4,0,input$init_value)
           },
           '5'={
             fun_result <- bisection(ftn5,0,input$init_value)
           }
    )
    h4("Bisection = ",fun_result)
  })
  output$root_methods3 <- renderUI({
    switch(input$sel_func,
           '1'={
             fun_result <- fixedpoint(ftn1fix,input$init_value)
           },
           '2'={
               fun_result <- fixedpoint(ftn2fix,input$init_value)
           },
           '3'={
               fun_result <- fixedpoint(ftn3fix,input$init_value)
           },
           '4'={
             fun_result <- fixedpoint(ftn4fix,input$init_value)
           },
           '5'={
             fun_result <- fixedpoint(ftn5fix,input$init_value)
           }
    )
    h4("Fixed Point = ",fun_result)
  })
  output$root_methods4 <- renderUI({
    switch(input$sel_func,
           '1'={
             fun_result <- secant(ftn1,0,input$init_value)
           },
           '2'={
             if(input$init_value != 1){
               fun_result <- secant(ftn2,0,input$init_value)
             } else{
               fun_result <- "failed to converge"
             }
           },
           '3'={
               fun_result <- secant(ftn3,0,input$init_value)
           },
           '4'={
             fun_result <- secant(ftn4,0,input$init_value)
           },
           '5'={
             fun_result <- secant(ftn5,0,input$init_value)
           }
    )
    h4("Secant = ",fun_result)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
