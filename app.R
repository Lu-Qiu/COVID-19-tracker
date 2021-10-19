library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library (scales)
library(leaflet)
library(shinydashboard)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)

#import the COVID-19 data frame from Johns Hopkins University Github page
confirmed<- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirmed_data <- read.csv(confirmed,header =T)
confirmed_data <-confirmed_data[,c(1,2,3,4,ncol(confirmed_data))]
colnames(confirmed_data)[5] <- 'Confirmed'
death <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
death_data <- read.csv(death,header = T)
death_data <-death_data[,c(1,2,3,4,ncol(death_data))]
colnames(death_data)[5] <- 'Death'

global_data <- merge(confirmed_data,death_data) 


time_series <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
time_series_data <- read.csv(time_series,header =T)
  US_time_series <- subset(time_series_data,Country.Region=='US')
  US_time_series <- US_time_series[,-c(1,2,3,4)]
  US_time_series <-as.data.frame(t(US_time_series))
  US_time_series$Date <- rownames(US_time_series)
  US_time_series$Date <- gsub("X", "", US_time_series$Date)
  colnames(US_time_series) <-c('Confirmed','Date')
  US_time_series$Date <- factor(US_time_series$Date, levels = US_time_series$Date[order(US_time_series$Confirmed)])
  US_time_series$Country <- 'US'
  
  India_time_series <- subset(time_series_data,Country.Region=='India')
  India_time_series <- India_time_series[,-c(1,2,3,4)]
  India_time_series <-as.data.frame(t(India_time_series))
  India_time_series$Date <- rownames(India_time_series)
  India_time_series$Date <- gsub("X", "", India_time_series$Date)
  colnames(India_time_series) <-c('Confirmed','Date')
  India_time_series$Date <- factor(India_time_series$Date, levels = India_time_series$Date[order(India_time_series$Confirmed)])
  India_time_series$Country <- 'India'
  
  Brazil_time_series <- subset(time_series_data,Country.Region=='Brazil')
  Brazil_time_series <- Brazil_time_series[,-c(1,2,3,4)]
  Brazil_time_series <-as.data.frame(t(Brazil_time_series))
  Brazil_time_series$Date <- rownames(Brazil_time_series)
  Brazil_time_series$Date <- gsub("X", "", Brazil_time_series$Date)
  colnames(Brazil_time_series) <-c('Confirmed','Date')
  Brazil_time_series$Date <- factor(Brazil_time_series$Date, levels = Brazil_time_series$Date[order(Brazil_time_series$Confirmed)])
  Brazil_time_series$Country <- 'Brazil'
  #combine the time_series dataframes of the three countries
  time_series <- rbind(US_time_series,India_time_series,Brazil_time_series)
  
  time_series$Date <- as.Date(time_series$Date,"%m.%d.%Y")
  time_series$Date <- gsub('00','20',time_series$Date)
  time_series$Date <- as.Date(time_series$Date,"%Y-%m-%d")

#build the shiny app
ui <- dashboardPage(
  
    skin  = 'blue',
    dashboardHeader(title = 'COVID-19 Tracker'),
    dashboardSidebar(
      
    sidebarMenu(
         # selectInput('continent','1.Select the continent',choices = c('All','Asia','Europe','North America','South America','Africa','Oceania')),
          #sliderInput('slide','2.Select the number of countries shown:',min=0,max=50,value=15,animate=FALSE,step=5),
    
           menuItem('Map', tabName = 'map', icon = icon('map-marker')),
           menuItem('Time Series', tabName = 'time-series', icon = icon('chart-line')),
            menuItem('Data', tabName = 'data', icon = icon('th')),
            menuItem('Plot', tabName = 'plot', icon = icon('bar-chart-o')), 
            menuItem('Vaccines', tabName = 'vaccines', icon = icon('crutch')), 
            menuItem('Information', tabName = 'info', icon = icon('info'))
        )),
    dashboardBody( 
        tabItems(
             tabItem(tabName = 'map',leafletOutput('map',height=720)),
             tabItem(tabName = 'time-series',fluidPage(box(plotOutput('time_series_plot'),width=25),box(sliderInput('time_series_slider','Select the date:',min=min(time_series$Date),max=max(time_series$Date),value=max(time_series$Date),animate=FALSE),width=25))),
             tabItem(tabName = 'data', fluidPage(box(sliderInput('slider','Select the number of countries shown:',min=0,max=20,value=10,animate=FALSE,step=1),width = 25),box(tableOutput('data'),width=25))),
             tabItem(tabName = 'plot', fluidPage(box(plotOutput("plot1"),width=25),box(plotOutput("plot2"), collapsible = TRUE, width = 25))),
             tabItem(tabName = 'info',includeMarkdown("info.Rmd"))
            )
        ))
    
   
       

server <- function(input, output) {
  
  output$time_series_plot <- renderPlot({ggplot(data=time_series,aes(x=Date,y=Confirmed,group=Country,color=Country)) +
      geom_line(lwd=1)+
      #scale_x_date (breaks =date_breaks ('40 days'))+
      xlim(min(time_series$Date),input$time_series_slider)+
      theme_bw()+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    })
  
  output$data <- renderTable({
      head(global_data[order(global_data$Confirmed,decreasing=TRUE), ],input$slider)
    })
    
    confirmed_decreasing <- head(global_data[order(global_data$Confirmed,decreasing=TRUE), ],10 )
    death_decreasing <- head(global_data[order(global_data$Death,decreasing=TRUE), ],10 )
  
    output$plot1 <-renderPlot({ggplot(data =confirmed_decreasing,aes(x=Confirmed,y=reorder(Country.Region,Confirmed))) + geom_bar(stat = 'identity',width = 0.75,fill='dodgerblue')+
                              geom_text(aes(label = as.character(Confirmed),hjust = -0.03))+
                             labs(title='Rank of Confirmed Count(TOP 10)')+theme(plot.title = element_text(face="plain",size=15,hjust=0.5),axis.text.y =element_text(size=12))})
    output$plot2 <-renderPlot({ggplot(data = death_decreasing,aes(x = Death,y=reorder(Country.Region,Death))) + geom_bar(stat = 'identity',width = 0.75)+
                              geom_text(aes(label = as.character(Death),hjust = -0.03))+
                              labs(title='Rank of Death Count(TOP 10)')+theme(plot.title = element_text(face="plain",size=15,hjust=0.5),axis.text.y =element_text(size=12))})
        

  basemap <- leaflet(global_data)%>%setView(lng=116.38,lat=39.9,zoom=3)%>%
              addTiles()%>%
              addCircles(lat =  ~Lat, lng =~Long,color = 'orange')%>%
              addCircleMarkers(data = global_data, lat =  ~Lat, lng =~Long, radius =~(Confirmed)^(1/5), color ='orange', stroke = FALSE, fillOpacity = 0.3,
                               label= sprintf("<strong>%s </strong><br/><strong>%s </strong><br/>Cases: %g<br/>Deaths: %d<br/>", global_data$Country.Region,global_data$Province.State, global_data$Confirmed, global_data$Death) %>% lapply(htmltools::HTML),
                               labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", "color" = 'orange'),textsize = "15px", direction = "auto"))
 
    output$map <-renderLeaflet({basemap
      })
    
    output$info <- renderText({
      tags$h4("Background")
      })
}
   


shinyApp(ui = ui, server = server)
