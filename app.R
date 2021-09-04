library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(shinydashboard)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)


ui <- dashboardPage(
    dashboardHeader(title = 'COVID 19'),
    dashboardSidebar(

    sidebarMenu(
          selectInput('continent','1.Select the continent',choices = c('All','Asia','Europe','North America','South America','Africa','Oceania')),
          sliderInput('slide','2.Select the number of countries shown:',min=0,max=50,value=15,animate=FALSE,step=5),
            menuItem('Map', tabName = 'map', icon = icon('map-marker')),
            menuItem('Data', tabName = 'data', icon = icon('th')),
            menuItem('Plot', tabName = 'plot', icon = icon('bar-chart-o')), 
            menuItem('Summary', tabName = 'summary', icon = icon('envelope-o')),
            menuItem('Structure', tabName = 'structure',icon = icon('gear'), badgeColor = "green"),
            menuItem('Information', tabName = 'info', icon = icon('info'))
        )),
    dashboardBody( 
        #shinyDashboardThemes(theme = 'blue_gradient'), 
        #shinyDashboardThemes(theme = 'grey_dark'),    
        tabItems(
            tabItem(tabName = 'summary', verbatimTextOutput('summary')),
            tabItem(tabName ='structure',verbatimTextOutput('str')),
            tabItem(tabName = 'data', fluidPage(box(width=15,align='center',tableOutput('data')))),
            tabItem(tabName = 'plot', fluidPage(box(plotOutput("plot1"),width=25),box(plotOutput("plot2"), collapsible = TRUE, width = 25))),
            tabItem(tabName = 'map',leafletOutput('map',height=720))
            )
        ))
    
   
       


# Define server logic required to draw a histogram
server <- function(input, output) {
 
  map_data <- data.frame(map_data)
  map_data$Lat <-  as.numeric(map_data$Lat)
  map_data$Long <-  as.numeric(map_data$Long)
  map_data=filter(map_data, Lat != "NA")
    
  continentInput <- reactive({
        switch (input$continent,
                'All'= head(a,n=input$slide),
                'Asia' = head(AsiaData,n=input$slide),
                'Europe'=head(EuropeData,n=input$slide),
                'North America'=head(NorthAmericaData,n=input$slide),
                'South America'=head(SouthAmericaData,n=input$slide),
                'Africa'=head(AfricaData,n=input$slide),
                'Oceania'=head(OceaniaData,n=input$slide)
        )
    })
    
    output$summary <- renderPrint({
        summary(a)
    })
    output$str <- renderPrint({
        str(a)
        
    })
    output$data <- renderTable({
        continentInput()
    })
    
    a = a[order(a$country_confirmedCount,decreasing=T),][1:10,]
    b = a[order(a$country_deadCount,decreasing=T),][1:10,]
  
    a<-mutate(a,countryEnglishName = fct_reorder(countryEnglishName,country_confirmedCount))
    b<-mutate(b,countryEnglishName = fct_reorder(countryEnglishName,country_deadCount))
    output$plot1 <-renderPlot({ggplot(data =a,aes(x = country_confirmedCount,y=countryEnglishName)) + geom_bar(stat = 'identity',width = 0.75,fill='#f68060')+
                              geom_text(aes(label = as.character(country_confirmedCount),  hjust = -0.03))+
                             labs(title='Rank of Confirmed Count(TOP 10)')+theme(plot.title = element_text(face="plain",size=15,hjust=0.5))})
    output$plot2 <-renderPlot({ggplot(data = b,aes(x = country_deadCount,y=countryEnglishName)) + geom_bar(stat = 'identity',width = 0.75)+
                              geom_text(aes(label = as.character(country_deadCount), hjust = -0.03))+
                              labs(title='Rank of Dead Count(TOP 10)')+theme(plot.title = element_text(face="plain",size=15,hjust=0.5))})
        

  
    basemap <- leaflet(map_data)%>%setView(lng=116.38,lat=39.9,zoom=4)%>%
              addTiles()%>%
              addCircles(lat =  ~Lat, lng =~Long,color = 'orange')%>%
              addCircleMarkers(data = map_data, lat =  ~Lat, lng =~Long, radius =~(Confirmed)^(1/5), color ='orange', stroke = FALSE, fillOpacity = 0.3,
                               label= sprintf("<strong>%s </strong><br/><strong>%s </strong><br/>Cases: %g<br/>Deaths: %d<br/>", map_data$Country_Region,map_data$Province_State, map_data$Confirmed, map_data$Deaths) %>% lapply(htmltools::HTML),
                               labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", "color" = 'orange'),textsize = "15px", direction = "auto"))
    
    #basemap <- leaflet()%>%addProviderTiles("CartoDB.Positron") %>% setView(lng=116.38,lat=39.9,zoom=2)

    output$map <- renderLeaflet({basemap
        
    })
    }


shinyApp(ui = ui, server = server)
