#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

joined_data <- read_rds("joined_data.rds")
viewbyage <- read_rds("viewbyage.rds")
viewbyrace <- read_rds("viewbyrace.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
  theme = shinytheme("flatly"),
  
  # Title describes the overarching question of my project
  navbarPage("Investigating bias of mainstream media coverage and readers",
             
             # I created a "Visualizations" page to show different candidates' ad strategies
             # as a first pass at finding a relationship between ad spend and poll results
             
             tabPanel("Comparison of Candidate Coverage",
                      tabsetPanel(
                        
                        # The "Overview" panel looks at the relationship between ad spend 
                        # and polling
                        
                        tabPanel("Joe vs. Bernie",

    
                        # Sidebar with a slider input for the number of bins
                          sidebarPanel(
                            sliderInput("bin",
                                       "Date of article:",
                                        min = as.Date("2020-01-01","%Y-%m-%d"),
                                        max = as.Date("2020-04-17","%Y-%m-%d"),
                                        value=as.Date("2020-02-16"),
                                        timeFormat="%Y-%m-%d")
                          ),
                          
                          mainPanel(
                            tableOutput("tweetComparison"))
        
      ) # end tabset panel
    ) # end tab panel
    
    
    
    ) # end navbar
    
    
    
) # end fluid page
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$tweetComparison <- renderTable({
      joined_data %>% filter(pub_date == input$bin) %>% select(headline.main)
    }, colnames = FALSE)
   
}

output$age_plot <- renderPlot({
  age_plot <- viewbyage %>%
    ggplot(aes(x = news_source, y = num_news, fill = age_range)) + 
    geom_col() + 
    labs(title = "Ages of readers of each news source", fill = "Age Range", x = "News Source", y = "Number of Readers/Viewers") + theme_classic() + theme(axis.text.x=element_text(angle=45,hjust=1)) +
    
    # renamed x axis labels in order, not the most efficient way but ok for now
    
    scale_x_discrete(labels = c("AM Radio", "CNN", "Facebook", "Fox", "Local News", "Local TV", "MSNBC", "Network TV", "New York Times", "NPR", "Telemundo"))
  })

output$race_plot <- renderPlot({
  
  race_plot <- viewbyrace %>%
    ggplot(aes(x = news_source, y = num_news, fill = race_ethnicity)) + 
    geom_col() + 
    labs(title = "Races of readers of each news source", 
         fill = "Race", 
         x = "News Source", 
         y = "Number of Readers/Viewers") + 
    theme_classic() + 
    theme(axis.text.x=element_text(angle=45,hjust=1)) + 
    scale_x_discrete(labels = c("AM Radio", "CNN", "Facebook", "Fox", "Local News", "Local TV", "MSNBC", "Network TV", "New York Times", "NPR", "Telemundo"))
  
})



# Run the application 
shinyApp(ui = ui, server = server)
