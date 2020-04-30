# imported libraries 

library(tidyverse)
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)


# imported data sources

joined_data <- read_rds("joined_data.rds")
viewbyage <- read_rds("viewbyage.rds")
viewbyrace <- read_rds("viewbyrace.rds")

# created a vector of news sources to be used in the checkbox picker

news_sources_vector = c("AM Radio", "CNN", "Facebook", "Fox", "Local News", "Local TV", 
           "MSNBC", "Network TV", "New York Times", "NPR", "Telemundo")

# UI of Shiny app

ui <- fluidPage(
  
  # added a shiny theme
  
  theme = shinytheme("flatly"),
  
  # Title describes the overarching question of my project
  navbarPage("Investigating readers of mainstream media coverage and potential bias",
             
          
  # investigating the presidential candidate headlines by different sources
             
  tabPanel("Comparison of Presidential Candidate Coverage",
     tabsetPanel(
                        
          # New York Times headlines
          
          tabPanel("New York Times",

              # Sidebar with a slider input for the number of bins
                sidebarPanel(
                  sliderInput("bin",
                             "Date of article:",
                              min = as.Date("2020-01-01","%Y-%m-%d"),
                              max = as.Date("2020-04-17","%Y-%m-%d"),
                              value=as.Date("2020-02-16"),
                              timeFormat="%Y-%m-%d")
                ),
                
               # displayed comparison of headlines
              
                mainPanel(
                  tableOutput("headlineComparison")
                  )
              ) # end new york times
    ) # end tabset panel
  ), # end tab panel
    
    
    # investigating who reads different sources
    
    tabPanel("Readership",
             
          tabsetPanel(
             
               tabPanel("Age",
             
       h3("News sources popular among different age demographics"),       
       mainPanel(plotOutput("viewbyage"))
               ), 
       sidebarPanel(
                 checkboxGroupInput("news_checkbox","Select News Sources",
                                    choices = news_sources_vector,
                                    selected = news_sources_vector)),
          
          tabPanel("Race",
                   h3("News sources popular among different racial demographics"),       
                   mainPanel(
                   plotOutput("viewbyrace"))
                   
                   )
                   )
    
    ))
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$headlineComparison <- renderTable({
    joined_data %>% filter(pub_date == input$bin) %>% select(headline.main)
  }, colnames = FALSE)
  
  output$age_plot <- renderPlot({
    viewbyage %>%
    filter(news_source %in% input$news_checkbox) %>%
    ggplot(aes(x = age_range, y = percent, fill = news_source)) + 
      geom_col() + 
      labs(title = "Ages of readers of each news source", 
           fill = "News Source", 
           x = "Age Range", y = "Percent of Readers/Viewers") + 
      theme_classic() + theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$race_plot <- renderPlot({
    viewbyrace %>%
      ggplot(aes(x = news_source, y = num_news, fill = race_ethnicity)) + 
      geom_col() + 
      labs(title = "Races of readers of each news source", 
           fill = "Race", 
           x = "News Source", 
           y = "Number of Readers/Viewers") + 
      theme_classic() + 
      theme(axis.text.x=element_text(angle=45,hjust=1)) + 
      scale_x_discrete(labels = c("AM Radio", "CNN", "Facebook", "Fox", 
                                  "Local News", "Local TV", "MSNBC", 
                                  "Network TV", "New York Times", "NPR", 
                                  "Telemundo"))
  })
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
