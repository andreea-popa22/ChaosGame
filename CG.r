library(shiny)


linebreaks <- function(n){HTML(strrep(br(), n))}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    h1("THE CHAOS GAME", align = "center"),
    
    linebreaks(2),
    tags$div(
        HTML(paste("As ", 
                   tags$i("Wikipedia"), 
                   " says, ", 
                   tags$blockquote(
                       tags$strong("The Chaos Game"), 
                       " is a method of creating a fractal, using a polygon and an initial point selected at random inside it."),
                   sep=""))
        
    ),
    
    linebreaks(1),
    
    
    tags$div(
        HTML(paste(
            tags$h4("You can play with the number of points below: the more points you select, the clearer the fractal will appear."),
            sep = " "
            
        )
        )
    ),
    
    linebreaks(1),
    
    sidebarLayout(
        sidebarPanel(
            # slider input for the number of drawn points
            sliderInput(inputId = "points",
                        label = "Number of points:",
                        min = 1,
                        max = 10000,
                        value = 1000,
                        sep = "."
            ),
            
            # slider input for the color of the points
            selectInput(inputId = "color",
                        label = "Color of points:",
                        c("Black" = "black",
                          "Grey" = "gray61",
                          "Red" = "indianred3", 
                          "Blue" = "royalblue3",
                          "Purple" = "plum",
                          "Multicolored" = "multi")
            ),
            
            # slider input for the shape of the polygon
            selectInput(inputId = "shape",
                        label = "Shape of polygon:",
                        c("Triangle" = "tri",
                          "Square" = "sq",
                          "Pentagon" = "penta",
                          "Hexagon" = "hex")
            ),
            
            # slider input for the coefficient of the distance between the current point and the next one
            selectInput(inputId = "coef",
                        label = "Distance coefficient:",
                        c("1/2" = "1/2",
                        "2/3" = "2/3")
            )
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
        # the shape is chosen by the user
        shape = input$shape
        
        # the distance coefficient is chosen by the user
        # we manipulate its value from string to float
        coef = input$coef
        numerator = strsplit(coef, "/")[[1]][1]
        denominator = strsplit(coef, "/")[[1]][2]
        coef = as.integer(numerator) / as.integer(denominator)
        
        if (shape == "tri"){
            # Triangle coordinates
            p <- c(0, 400, 800) 
            q <- c(0, 800, 0)
            
            
            par(mar = rep(0, 4))
            # We draw the coordinates 
            plot(p, q, col= "black", pch = 15, cex = 1, axes = FALSE)
            
            # Random starting point
            x <- sample(0:1000, 1)
            y <- sample(0:1000, 1)
            
            # Chaos game
            # Number of points to be drawn
            pct <- input$points
            for (i in 1:pct) {
                
                # We take a vertex of the triangle at random
                n <- sample(1:3, 1)
                
                # We calculate the coordinates of the next point
                x <- floor(x + (p[n] - x) * coef)
                y <- floor(y + (q[n] - y) * coef)
                
                # We add it in the plot
                if (input$color == "multi")
                {
                    colors <- c("indianred3", "gray61", "royalblue3", "plum", "yellow1", "deeppink", "seagreen", "rosybrown")
                    color <- sample(colors, 1)
                    
                    points(x, y, pch = 16, col = color, cex = 0.5)
                    
                }
                else
                {
                    points(x, y, pch = 16, col = input$color, cex = 0.5)
                }
            }
        }
        else
            if (shape == "sq"){
                # Square coordinates
                p <- c(0, 0, 1000, 1000)
                q <- c(0, 1000, 1000, 0)
                
                par(mar = rep(0, 4))
                # We draw the coordinates
                plot(p, q, col = "black", pch = 15, cex = 1, axes = FALSE)
                
                # Random starting point
                x <- sample(0:1000, 1)
                y <- sample(0:1000, 1)
                
                aux = 0 # auxiliary variable to help us check if the previous vertex is the same with the current one
                
                # Chaos game
                # Number of points to be drawn
                pct <- input$points
                for (i in 1:pct) {
                    # We take a vertex of the square at random
                    n <- sample(1:4, 1)
                    
                    # We start the test from the second point
                    if(i != 1)
                    {
                        # We take a new vertex until it differs from the previous one
                        while(aux == n)
                        {
                            n <- sample(1:4, 1)
                        }
                    }
                    
                    # We update the previous vertex
                    aux = n
                    
                    # We calculate the coordinates of the next point
                    x <- floor(x + (p[n] - x) * coef)
                    y <- floor(y + (q[n] - y) * coef)
                    
                    # We add it in the plot
                    if (input$color == "multi")
                    {
                        colors <- c("indianred3", "gray61", "royalblue3", "plum", "yellow1", "deeppink", "seagreen", "rosybrown")
                        color <- sample(colors, 1)
                        
                        points(x, y, pch = 16, col = color, cex = 0.5)
                        
                    }
                    else
                    {
                        points(x, y, pch = 16, col = input$color, cex = 0.5)
                    }
                    
                }
            }
        else
            if (shape == "penta"){
                # Pentagon coordinates
                a <- c(550, 455, 491, 609, 645)
                b <- c(450, 519, 631, 631, 519)
                
                par(mar = rep(0, 4))
                # We draw the coordinates
                plot(a, b, col= "red", pch = 15, cex = 1, axes = FALSE)
                
                # Random starting point
                x <- sample(0:645, 1)
                y <- sample(0:631, 1)
                
                # Chaos game
                # Number of points to be drawn
                pct <- input$points
                
                aux = 0
                
                for (i in 1:pct) {
                    
                    # We take a vertex of the hexagon at random
                    vertex <- sample(1:5, 1)
                    
                    if(i != 1)
                    {
                        # we check the previous vertex
                        while(aux == vertex)
                        {
                            vertex <- sample(1:5, 1)
                        }
                    }
                    
                    aux = vertex
                    
                    # We calculate the coordinates of the next point
                    x <- floor(x + (a[vertex] - x) * coef)
                    y <- floor(y + (b[vertex] - y) * coef)
                    
                    # We add it in the plot
                    if (input$color == "multi")
                    {
                        colors <- c("indianred3", "gray61", "royalblue3", "plum", "yellow1", "deeppink", "seagreen", "rosybrown")
                        color <- sample(colors, 1)
                        
                        points(x, y, pch = 16, col = color, cex = 0.5)
                        
                    }
                    
                    else
                    {
                        points(x, y, pch = 16, col = input$color, cex = 0.5)
                    }
                }
                
                
            }
            else
                if (shape == "hex"){
                    # Hexagon coordinates
                    p <- c(500, 1500, 2000,1500,500,0)
                    q <- c(0, 0, 866.025,1732.05,1732.05,866.025)
                    
                    par(mar = rep(0, 4))
                    # We draw the coordinates
                    plot(p, q, col= "black", pch = 15, cex = 1, axes = FALSE)
                    
                    # Random starting point
                    x <- sample(0:1000, 1)
                    y <- sample(0:1000, 1)
                    
                    # Chaos game
                    # Number of points to be drawn
                    pct <- input$points
                    for (i in 1:pct) {
                        
                        # We take a vertex of the hexagon at random
                        n <- sample(1:6, 1)
                        
                        # We calculate the coordinates of the next point
                        x <- floor(x + (p[n] - x) * coef)
                        y <- floor(y + (q[n] - y) * coef)
                        
                        # We add it in the plot
                        if (input$color == "multi")
                        {
                            colors <- c("indianred3", "gray61", "royalblue3", "plum", "yellow1", "deeppink", "seagreen", "rosybrown")
                            color <- sample(colors, 1)
                            
                            points(x, y, pch = 16, col = color, cex = 0.5)
                            
                        }
                        
                        else
                        {
                            points(x, y, pch = 16, col = input$color, cex = 0.5)
                        }
                    }
                }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)