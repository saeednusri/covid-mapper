library(shiny)
library(leaflet)
require(shinyjs)
require(shinydashboard)
require(shinyWidgets)

covid<-function(){
    #setwd("~/development/covid-dashboard")
    setwd("/srv/shiny-server")
    dat <- readRDS("data-files/final.RDS")
    dat
}

ui <- fluidPage(
    chooseSliderSkin("Flat"),
    setSliderColor(c("DarkSlateGrey ", "#FF4500", "", "Teal"), c(1, 2, 4)),
    sliderInput("DatesMerge",
                "Flatten the Curve",
                min = min(covid()$Date),
                max = max(covid()$Date),
                value = max(covid()$Date),
                step = 1,
                timeFormat="%m-%d",
                width = "80%",
                animate = animationOptions(interval = 4000,
                                           playButton = icon('play', "fas fa-play-circle-4x"),
                                           pauseButton = icon('pause', "fa-1x"))),
    leafletOutput("Map", height = 600)
)
