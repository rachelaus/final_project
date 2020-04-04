#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MS 6"),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("age_plot")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$age_plot <- renderImage({
       filename <- normalizePath(file.path("age_plot.png"))
    
       # list containing the filename
       list(src = filename, 
            width = 600,
            height = 500)
       }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
