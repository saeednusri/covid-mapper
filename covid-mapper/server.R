#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(shinydashboard)
require(sp)
require(rgdal)
library(leaflet.extras)


map<-function(){
    #setwd("~/development/covid-dashboard")
    setwd("/srv/shiny-server")
    service <- readRDS("data-files/world_alberta_map.RDS")
    proj4string(service) <- CRS("+init=epsg:4326")
    service
}

covid<-function(){
    #setwd("~/development/covid-dashboard")
    setwd("/srv/shiny-server")
    dat <- readRDS("data-files/final.RDS")
    dat
}

server <- function(input, output, session){
    
    filteredData <- reactive({
        maps <- map()
        dat <- covid()
        new <- dat[dat$Date == input$DatesMerge, ]
        new_dat<- sp::merge(maps, new, by = "NAME", duplicateGeoms = TRUE)
    })

    output$Map <- renderLeaflet({
        leaflet()%>%
            enableTileCaching() %>%
            setView(lat = 55.000000, lng = -115.000000, zoom = 4) %>%
            addTiles(group = 'Street') %>% addProviderTiles(providers$Esri.NatGeoWorldMap, group ='Terrain')  %>% 
            addLayersControl(baseGroups = c("Steet", "Terrain"), 
                             overlayGroups = c("Cases", "Deaths"),
                             options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Cases") %>% hideGroup("Deaths") 
    })
    observe({
        new_dat <- filteredData()
        labels <- sprintf("<strong>%s</strong>
                              <br/> Cases: %s
                              <br/> Death: %s",
                          new_dat$NAME, new_dat$Cases, new_dat$Deaths) %>% lapply(htmltools::HTML)
        
        pal1 <- colorFactor(palette = c("Blues"), domain = new_dat$Cases)
        pal2 <- colorFactor(palette = c("Reds"), domain = new_dat$Deaths)
        leafletProxy("Map", data = new_dat) %>%
            addPolygons(data=new_dat, weight = 1, color = "gray",
                        fillOpacity=0.5, opacity = 1.0, 
                        popup = labels,
                        popupOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto", closeOnClick = TRUE),
                        fillColor = ~pal1(`Cases`),
                        highlightOptions = highlightOptions(
                            color='white', opacity = 1, weight = 2, fillOpacity = 1,
                            bringToFront = TRUE), group = "Cases") %>% 
            addPolygons(data=new_dat, weight = 1, color = "gray",
                        fillOpacity=0.5, opacity = 1.0, 
                        popup = labels,
                        popupOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto", closeOnClick = TRUE),
                        fillColor = ~pal2(`Deaths`),
                        highlightOptions = highlightOptions(
                            color='white', opacity = 1, weight = 2, fillOpacity = 1,
                            bringToFront = TRUE), group = "Deaths")
    })
}

