#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# \/ here is html code for embedding your figure
#<iframe src="https://stla.shinyapps.io/3Dsliced/" style="border: none; width: 900px; height: 500px"></iframe>

library(shiny)
library(tidyverse)
library(maps)
library(lubridate)
library(extrafont)

state <- read_csv("statedata.csv")
county <- read_csv("countydata.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    hr(),
           plotOutput("distPlot"),
           fluidRow(column(3, align="center",
                           radioButtons("radio", h3("Level:"),
                                        choices = list("State"=1,
                                                       "County"=2), selected = 1),
                           ),
                    column(4, align = "center",
                           sliderInput("dater", h3("Dates:"),
                                       min = as.Date("2020-01-13", "%Y-%m-%d"),
                                       max = as.Date("2020-05-25", "%Y-%m-%d"),
                                       value = as.Date("2020-01-13", "%Y-%m-%d"),
                                       step = 7,
                                       animate = TRUE)
                           )
           ),
    hr()
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        if(input$radio==1){
            
            state %>% 
                filter(date==input$dater) %>% 
                ggplot(aes(long, lat, group = group))+
                geom_polygon(aes(fill = mean), color = "black")+
                theme_void()+
                scale_fill_gradient2(low="blue", high="red", midpoint = 100, limits=c(0,250))+
                labs(fill="Mobility", 
                     caption = "Data source: Apple Maps      ",
                     title = "Mobility Trends Jan-May 2020",
                     subtitle = paste(c(months(as.Date(input$dater))," " , day(as.Date(input$dater)),",", " " , "2020"), collapse = "" ))+
                theme(text = element_text(family = "Courier New", size=15) ,
                      plot.title = element_text(hjust = .5, size = 34, family = "Andale Mono"),
                      plot.subtitle = element_text(hjust = .5, size = 30),
                      legend.position = c(.89,.25),
                      legend.title = element_text(face = 'bold'))
        }
        else if(input$radio==2){
            county %>% 
                filter(date==input$dater) %>% 
                ggplot(aes(long, lat, group = group))+
                geom_polygon(aes(fill = mean), color = "black")+
                theme_void()+
                scale_fill_gradient2(low="blue", high="red", midpoint = 100, limits=c(0,300))+
                labs(fill="Mobility", 
                     caption = "Data source: Apple Maps      ",
                     title = "Mobility Trends Jan-May 2020",
                     subtitle = paste(c(months(as.Date(input$dater))," " , day(as.Date(input$dater)),",", " " , "2020"), collapse = "" ))+
                theme(text = element_text(family = "Courier New", size=15) ,
                      plot.title = element_text(hjust = .5, size = 34, family = "Andale Mono"),
                      plot.subtitle = element_text(hjust = .5, size = 30),
                      legend.position = c(.89,.25),
                      legend.title = element_text(face = 'bold'))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
