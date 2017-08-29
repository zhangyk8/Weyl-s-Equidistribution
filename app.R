library(shiny)

# Define UI for application that draws a histogram
ui = fluidPage(
  
  # Application title
  titlePanel("Visualization of Weyl's Equidistribution Theorem"),
  h4("By Yikun Zhang"),
  
  h3("1. Background Introduction"), 
  p("As we learned in the class, the Fourier series theory can be used to prove Weyl's 
    Equidistribution theorem. Here we recall the statement of the theorem, visualize 
    the theorem on the real line, i.e. [0,1) interval, and display its geometric 
    interpretation in a unit square via R Shiny."),
  
  p("R is a statistical programming language, excelling at data processing and data 
    analysis. The users of R often rely on the RStudio platform to run their R codes. 
    As for R Shiny, it is a web application framework from RStudio and can turn the 
    data analyses in R into an interactive webpage."),
  
  p("For those who want to download the 
    latest version of R, please visit the ", 
    a("R homepage ", href = "https://www.r-project.org/"), "and choose an appropriate 
    CRAN mirror. For an introduction and live examples of R Shiny, see the ",
    a("Shiny homepage.", href = "http://shiny.rstudio.com/")),
  
  h3("2. Weyl's equidistribution theorem"),
  p(strong("Theorem. "), em("If q is irrational, then the sequence of fractional parts 
                            <q>, <2q>, <3q>, ... is equidistributed in [0,1).")), 
  
  h3("3. Visualization of Weyl's equidistribution theorem"), 
  
  p("Here an interactive visualization of the Weyl's equidistribution theorem is 
    displayed on the [0,1) interval. You can change the number of points on the plotting
    by ", strong("rolling the slider bar")," on the left panel. Meanwhile, you can input 
    a different generating number, i.e. the ", em("q "), "in the above statement of 
    the theorem by ", strong("typing a different base and exponent of the generating 
                             number."), " For example, to let the generating number be the square root of 3, 
    you can simply type '3' in the base box and '0.5' in the exponent box, as 
    the default value. Then the plotting will be changed correspondingly within a 
    second."), 
  
  sidebarLayout(
    sidebarPanel(
      # Sidebar with a slider input for number of points
      sliderInput("samples",
                  label = "Number of points:",
                  min = 1,
                  max = 200,
                  value = 50),
      br(),
      strong("Input the base and exponential part of the generating number:"),
      numericInput("base",
                   label = "Base",
                   value = 3),
      numericInput("expo",
                   "Exponent",
                   value = 0.5)),
    
    mainPanel(
      # Show a plot of the distribution of distributed points within the interval
      plotOutput("plot1")
    )
  ),
  
  h3("4. Geometric interpretation of the Weyl's equidistribution theorem"),
  
  p("What will happen if we extend the equidistributed concept to the two-dimension 
    space? Suppose that the sides of a square are reflecting mirrors and 
    that a ray of light leaves a point inside the square. With an appropriate 
    choice of axis, the path of the light corresponds to a straight line 
    in the plane, whose slope can determine whether the path will be periodic 
    or dense in the plane."),
  p("Here we again use the interactive plotting in R Shiny to verify the fact that 
    the rationality of the value of the slope can determine the periodicity of the 
    path of the light in the square. Similarly, some widgets are available to adjust 
    the plotting. By typing a specific value in the input boxes, one can modify the 
    coodinate of the initial point. Moreover, the slope of the light can also be changed 
    as in the last plotting. Finally, both the initial direction of the light and the 
    number of reflecting times is free to be adjusted."),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("x",
                   "The x coordinate of the initial point:",
                   min = 0,
                   max = 1,
                   value = 0.2),
      numericInput("y",
                   "The y coordinate of the initial point:",
                   min = 0,
                   max = 1,
                   value = 0.8),
      br(),
      strong("Input the slope of the initial direction of the light:"),
      numericInput("base2",
                   "Base",
                   value = 2),
      numericInput("expo2",
                   "Exponent",
                   value = 0.5),
      br(),
      radioButtons("direct",
                   "The initial direction of the light:",
                   c("right", "left")),
      sliderInput("point_num",
                  "Number of reflecting times",
                  min = 0,
                  max = 500,
                  value = 100)
    ),
    
    mainPanel(plotOutput("plot2", height = "600px"))
  ),
  
  h4("5. Reference"),
  p(em("Elias Stein and Rami Shakarchi"), strong("Fourier Analysis An Introduction."), 
    "Princeton University Press 2003")
  )

# Define server logic required to draw a histogram
server = function(input, output) {
  
  output$plot1 = renderPlot({
    
    library(ggplot2)
    
    N = input$samples
    n = 1:N
    
    num = (input$base)^(input$expo)
    
    irra_seq = sapply(n, function(x){
      x * num - floor(x * num)
    })
    data_seq = data.frame(x = irra_seq, y = rep(0, N))
    
    ggplot(data_seq) + geom_point(mapping = aes(x = x, y = y), color = "red") + labs(x = "Distributed Points", y = "")
    
  })
  
  output$plot2 = renderPlot({
    library(ggplot2)
    
    ## The geometric interpretation of the Weyl's equidistribution theorem
    # The slope of the arc
    gam = (input$base2)^(input$expo2)
    N = input$point_num
    direct = input$direct
    point = data.frame(x = c(input$x, rep(0, N)), y = c(input$y, rep(0, N)))
    
    for(i in 1:N){
      b = point$y[i] - gam * point$x[i]
      
      if((direct == "right" & gam + b > 1) | (direct == "left" & b > 1)){
        point$y[i+1] = 1
        point$x[i+1] = (1 - b)/gam
        gam = - gam
      }else if((direct == "right" & gam + b >= 0 & gam + b <= 1)){
        point$x[i+1] = 1
        point$y[i+1] = gam + b
        gam = - gam
        direct = "left"
      }else if((direct == "right" & gam + b < 0) |(direct == "left" & b < 0)){
        point$x[i+1] = -b/gam
        point$y[i+1] = 0
        gam = - gam
      }else if(direct == "left" & b >= 0 & b <= 1){
        point$x[i+1] = 0
        point$y[i+1] = b
        gam = -gam
        direct = "right"
      }
    }
    
    point$Type = c("Initial Point", rep("Reflecting Points", N))
    ggplot(point) + geom_path(mapping = aes(x, y), color = "green") + geom_point(mapping = aes(x, y, color = Type)) + scale_color_manual(values = c("red", "blue"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

