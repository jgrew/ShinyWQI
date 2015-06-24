library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(dygraphs)



df <- read.csv('data/sar_dw_data.csv', as.is=T)
stns <- read.csv('data/stations.csv', as.is=T)
stns <- filter(stns, ProjectProgramCode == 6, !(StationCode %in% c('QC A', 'QC B', 'Synthetic', 'TripBlank', 'Trip Blank', 'Tripblank', 'EquipBlank')))


header <- dashboardHeader(
  title = 'WQI Demo'
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", tabName = "about", icon = icon("question")),
    menuItem("OCWatersheds.com", icon = icon("file-code-o"),
             href = "http://www.ocwatersheds.com")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem('dashboard',
      fluidRow(
        leafletOutput('map1', width = '100%', height = 400)
      ),
      fluidRow(
        valueBoxOutput('mLat', width = 3),
        valueBoxOutput('mLng', width = 3),
        valueBoxOutput('mZoom', width = 3),
        valueBoxOutput('mStns', width = 3)
      ),
      fluidRow(
        box(
          title = 'Top SAR Dry Weather Monitoring Scores',
          status = 'primary',
          solidHeader = TRUE,
          div(style = 'height:400px; overflow-y:scroll',
              tableOutput('tblStns')
          )
        ),
        box(
          title = 'historical graph of visible stations',
          status = 'primary', 
          solidHeader = TRUE,
          plotOutput('pltStns')
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$map1 <- renderLeaflet({
    leaflet(stns) %>% addTiles() %>% setView(-117.82, 33.76, zoom = 10) %>% 
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~StationCode,
        color = '#FF1919'
      )
  })
  
  observeEvent(input$map1_bounds, {
      stns <- stationsInBounds()
      
      if (nrow(stns)==0)
        return()
      
      leafletProxy('map1', session) %>% clearShapes() %>% addCircleMarkers(
        lng = stns$Longitude,
        lat = stns$Latitude,
        popup = stns$StationCode,
        color = '#FF1919'
      )
      
    }
  )
  
  stationsInBounds <- reactive({
    bounds <- input$map1_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(stns,
          Latitude >= latRng[1] & Latitude <= latRng[2] &
          Longitude >= lngRng[1] & Longitude <= lngRng[2])
  })
  
  getStationData <- reactive({
    stations <- stationsInBounds()$StationCode
    df <- filter(df, Station %in% stations)
  })
  
  output$mZoom <- renderValueBox({
    bounds <- input$map1_bounds
    
    lat = mean(c(bounds$north, bounds$south))
    lng = mean(c(bounds$east, bounds$west))
    zoom = input$map1_zoom
    
    valueBox(
      "Zoom",
      zoom,
      icon = icon("zoom-in", lib = 'glyphicon')
    )
  })
  output$mLat <- renderValueBox({
    bounds <- input$map1_bounds   
    lat = mean(c(bounds$north, bounds$south))

    valueBox(
      "Lat",
      lat,
      icon = icon("list")
    )
  })
  output$mLng <- renderValueBox({
    bounds <- input$map1_bounds
    lng = mean(c(bounds$east, bounds$west))
  
    valueBox(
      "Lng",
      lng,
      icon = icon("list")
    )
  })
  output$mStns <- renderValueBox({
    bounds <- input$map1_bounds
    lng = mean(c(bounds$east, bounds$west))
    
    valueBox(
      "Stations",
      paste0(nrow(stationsInBounds()), ' out of ', nrow(stns), ' stations are visible.'),
      icon = icon("list")
    )
  })
  
  output$tblStns <- renderTable({
      tbl <- getStationData() %>% select(Station, WQI) %>% group_by(Station) %>% summarise_each(funs(mean)) %>% arrange(desc(WQI))
  })
  
  output$pltStns <- renderPlot({
    tbl <- getStationData() %>% group_by(WaterYear) %>% summarise_each(funs(mean))
    p <- ggplot(tbl, aes(x=WaterYear, y=WQI)) + geom_line()
    print(p)          
  })
  
#   output$dygraph <- renderDygraphs({
#     tbl <- getStationData() %>% group_by(WaterYear) %>% summarise_each(funs(mean))
#     dygraphs(tbl, main='test') %>%
#       dySeries(c('WaterYear', 'WQI'), label = 'label')
#   })

}


shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = server
)