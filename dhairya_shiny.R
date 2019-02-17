library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)


df<-read.csv("Vehicle_Accident_Data.csv")
len<-length(df$Hit.And.Run)

df <- tidyr::separate(data=df,
                      col=Crash.Date.Time,
                      into=c("Date", "Time"),
                      sep=" ",
                      remove=TRUE)

df <- tidyr::separate(data=df,
                      col=Location,
                      into=c("Latitude", "Longitude"),
                      sep=",",
                      remove=TRUE)
df$Latitude <- stringr::str_replace_all(df$Latitude, "[(]", "")
df$Longitude <- stringr::str_replace_all(df$Longitude, "[)]", "")


df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel('Shiny Assignement 1'),

             sidebarLayout(
               
               # Inputs
               sidebarPanel(
                 
                 dateInput("date", "Date:",
                                min    = "2018-10-10",
                                max    = "2018-11-01",
                                startview = "month",
                                format = "mm/dd/yy",
                                value = "2018-10-10"),
                 
                 radioButtons("fatality", "Fatality:",
                              c("True" = "TRUE",
                                "False" = "FALSE"),
                              selected = "FALSE"),
                 
                 radioButtons("hit", "Hit and RUn:",
                              c("True" = "TRUE",
                                "False" = "FALSE"),
                              selected = "FALSE")
                 
               ),
               # Outputs
               mainPanel(
                 leafletOutput("mymap",height = 400),
                 DT::dataTableOutput("mytable")
               )
             )
            
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    
    
    htrue<-which(df$Hit.And.Run %in% input$hit )
    ftrue<-which(df$Fatality %in% input$fatality)
    date<-input$date
    date<-format(date, "%m/%d/%Y")
    dtrue<- which(df$Date %in% date)
    
    Longitude<-df$Longitude[htrue] 
    Longitude<-Longitude[ftrue]
    Longitude<-Longitude[dtrue]
    Longitude<-Longitude[!is.na(Longitude)]
    Longitude<-Longitude[Longitude != 0]
    
    Latitude<-df$Latitude[htrue]
    Latitude<-Latitude[ftrue]
    Latitude<-Latitude[dtrue]
    Latitude<-Latitude[!is.na(Latitude)]
    Latitude<-Latitude[Latitude != 0]
    
    
    final <- data.frame(Longitude,Latitude)
    print(final)
    m <- leaflet() %>%
        addTiles() %>%
      addMarkers(lng = Longitude,
                 lat = Latitude)
    m
  })

    output$mytable <- DT::renderDataTable({
      htrue<-which(df$Hit.And.Run %in% input$hit )
      ftrue<-which(df$Fatality %in% input$fatality)
      date<-input$date
      date<-format(date, "%m/%d/%Y")
      dtrue<- which(df$Date %in% date)
      
      Longitude<-df$Longitude[htrue] 
      Longitude<-Longitude[ftrue]
      Longitude<-Longitude[dtrue]
      Longitude<-Longitude[!is.na(Longitude)]
      Longitude<-Longitude[Longitude != 0]
      
      Latitude<-df$Latitude[htrue]
      Latitude<-Latitude[ftrue]
      Latitude<-Latitude[dtrue]
      Latitude<-Latitude[!is.na(Latitude)]
      Latitude<-Latitude[Latitude != 0]
      
      
      final <- data.frame(Longitude,Latitude)
      
      
      final})
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

