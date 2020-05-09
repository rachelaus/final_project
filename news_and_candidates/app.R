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
news_sources_vector  <- c("AM Radio", "CNN", "Facebook", "Fox", "Local News",
                          "Local TV", "MSNBC", "Network TV", "New York Times", 
                          "NPR", "Telemundo")

#news_sources_values <- c(am_radio, cnn, fb, fox, local_news, local_tv, msnbc, 
   #                      network_tv, new_york_times, npr, telemundo)


#news_sources_vector = c("MSNBC", "New York Times", "CNN", "Facebook", "Fox", "Network TV", 
         #  "Local TV", "Telemundo", "NPR", "AM Radio", "Local News")

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
          
          tabPanel("Compare New York Times Headlines",

              # Sidebar with a slider input for the number of bins
                sidebarPanel(
                  sliderInput("bin",
                             "Date of article:",
                              min = as.Date("2020-01-01","%Y-%m-%d"),
                              max = as.Date("2020-04-17","%Y-%m-%d"),
                              value=as.Date("2020-02-25"),
                              timeFormat="%Y-%m-%d")
                ),
                
               # displayed comparison of headlines
              
                mainPanel(
                  tableOutput("headlineComparison")
                  )
              ), # end new york times tab
          tabPanel("Graph of New York Times Headlines",
                   
                   mainPanel(
                     plotOutput("nyt_plot")
                   )
                   
                   
            ) # end line graph tab
    ) # end tabset panel
  ), # end comparison tab panel
    
    
    # investigating who reads different sources
    
  tabPanel("Readership",
             
        tabsetPanel(
             
              tabPanel("Age",
             
                 mainPanel(h3("News sources popular among different age demographics"),
                   plotOutput("age_plot")),
            
                 
                 # i changed choices to be choice names and choice values. i needed to filter through the values and display the vector of proper names associated with each value
                 # my data in my ggplot was not displaying because i was comparing values to names instead of values to values disguised as names
                 
                 
                 
                 # i NEED to make sure the names are corresponding correctly to the values and the legend names too
                 
                 sidebarPanel(
                 checkboxGroupInput("news_checkbox","Select News Sources", selected = unique(viewbyage$news_source),
                                    choiceValues = unique(viewbyage$news_source), choiceNames = news_sources_vector))),
            #  unique(viewbyage$news_source)
               tabPanel("Race",
                  
                  mainPanel(h3(""),
                    plotOutput("race_plot"), p("Percent of readers displayed is out of each race category rather than total readers in dataset - no percentage is greater than 20")),
                  
                  sidebarPanel(
                    checkboxGroupInput("news_race_checkbox","Select News Sources", selected = unique(viewbyrace$news_source),
                                       choiceValues = unique(viewbyrace$news_source), choiceNames = news_sources_vector))
                  
                  ) # end race tab
              
         ) # end tabset panel
    
    ), # end readership tab panel
  
  tabPanel("Model",
           
           
  ),
  
  tabPanel("About",
           
       mainPanel(
             includeHTML("about.Rhtml")
           )  
    ) # end about tab panel
  
)) # end fluid page and navbar page

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
      theme_classic() + theme(axis.text.x=element_text(angle=45,hjust=1)) #+ 
      #scale_fill_discrete(labels=c("AM Radio", "CNN", "Facebook", "Fox", 
                                 #  "Local News", "Local TV", "MSNBC", 
                                 #  "Network TV", "New York Times", 
                                 #  "NPR", "Telemundo"))
      
  })
  
  output$race_plot <- renderPlot({
    viewbyrace %>%
      filter(news_source %in% input$news_race_checkbox) %>%
      ggplot(aes(x = news_source, y = percent, fill = race_ethnicity)) + 
      geom_col(position = "dodge") + 
      labs(title = "Popularity of News Source by Race", 
           fill = "Race", 
           x = "News Source", 
           y = "Percent of Readers/Viewers") + 
      theme_classic() + 
      theme(axis.text.x=element_text(angle=45,hjust=1))
    
  })
    
    
  output$party_plot <- renderPlot ({
    viewbyparty %>%
      filter(news_source %in% input$news_checkbox) %>%
      mutate(news_source =
               case_when(
                 news_sources_fox == 1 & news_sources_cnn == 2 ~ "Fox",
                 news_sources_fox == 2 & news_sources_cnn == 1 ~ "CNN",
                 news_sources_fox == 1 & news_sources_cnn == 1 ~ "Both",
                 news_sources_fox == 2 & news_sources_cnn == 2 ~ "Neither"
               )) 
    
    
    party_source_plot <- viewbyparty %>%
      ggplot(aes(x = news_source, fill = party)) + geom_histogram(stat = "count")
    
    party_source_plot
  })
  
  
  output$nyt_plot <- renderPlot ({
    sort_by_date %>% 
      ggplot(aes(x = pub_date, y = articles_per_date, color = candidate)) + 
      geom_line() + 
      theme_classic() + 
      labs(title = "Number of New York Times Headlines about Joe Biden and Bernie Sanders", 
           color = "Candidate", 
           x = "Publication Date", 
           y = "Number of Headlines") +
      theme_classic() 
  })
  
  
}



# Run the application 
shinyApp(ui, server)
