require(sp)
require(rgdal)
library(sf)
library(raster)
library(maptools)
library(rgeos)
library(rmapshaper)

setwd("~/development/covid-dashboard")

world <- readOGR("data/world/ne_50m_admin_0_countries.shp")
world_2 <- world[, "NAME_LONG"]
names(world_2) <- "NAME"
world_2 <- spTransform(world_2, CRS("+init=epsg:4326"))
#proj4string(world_2) <- CRS("+init=epsg:4326")
world_2 <- world_2[!world_2$NAME == "Canada", ]
world_2$NAME <- gsub("United States", "United States of America", world_2$NAME)
world_2$NAME <- gsub("Russian Federation", "Russia", world_2$NAME)
world_2$NAME <- gsub("Republic of Korea", "South Korea", world_2$NAME)
world_2$NAME <- gsub("Côte d'Ivoire", "Cote dIvoire", world_2$NAME)
world_2$NAME <- gsub("Tanzania", "United Republic of Tanzania", world_2$NAME)
world_2$NAME <- gsub("The Gambia", "Gambia", world_2$NAME)
world_2$NAME <- gsub("Guinea-Bissau", "Guinea Bissau", world_2$NAME)
world_2$NAME <- gsub("Macedonia", "North Macedonia", world_2$NAME)
world_2$NAME <- gsub("Macedonia", "North Macedonia", world_2$NAME)
world_2$NAME <- gsub("Czech Republic", "Czechia", world_2$NAME)
world_2$NAME <- gsub("Falkland Islands", "Falkland Islands (Malvinas)", world_2$NAME)
world_2$NAME <- gsub("Laos", "Lao PDR", world_2$NAME)



world_2 <- spChFIDs(world_2, as.character(world_2$NAME))

#canada <- readRDS("data/Canada/CAN_adm1.rds")
canada <- readOGR("data/Cananda2/Canada.shp")
canada_2 <- canada[, "NAME"]
names(canada_2) <- "NAME"
canada_2 <- canada_2[!canada_2$NAME=="Alberta", ]
canada_2 <- spChFIDs(canada_2, as.character(canada_2$NAME))
#proj4string(canada_2) <- CRS("+init=epsg:4326")
canada_2 <- spTransform(canada_2, CRS("+init=epsg:4326"))
#regions_gSimplify <- gSimplify(canada_2, tol = 0.05, topologyPreserve = T)

world_can <- spRbind(canada_2, world_2)
#canada <- spTransform(canada, CRS("+init=epsg:4326"))

alberta <- readOGR("data/Alberta/Agg Local Geographic Area.shp")
alberta_2 <- alberta[, "ZONE_NAME"]
names(alberta_2) <- "NAME"
alberta_2 <- spChFIDs(alberta_2, as.character(alberta$AGG_NAME))
alberta_2 <- spTransform(alberta_2, CRS("+init=epsg:4326"))

alberta_2$NAME <- gsub("SOUTH", "South Zone", alberta_2$NAME)
alberta_2$NAME <- gsub("CALGARY", "Calgary Zone", alberta_2$NAME)
alberta_2$NAME <- gsub("CENTRAL", "Central Zone", alberta_2$NAME)
alberta_2$NAME <- gsub("EDMONTON", "Edmonton Zone", alberta_2$NAME)
alberta_2$NAME <- gsub("NORTH", "North Zone", alberta_2$NAME)

world_can_al <- spRbind(alberta_2, world_can)

saveRDS(world_can_al, "data-files//world_alberta_map.RDS")

dat <- readRDS("final.RDS")
new <- dat[dat$Date == "2020-04-05", ]
new_dat<- sp::merge(world_can_al, new, by = "NAME",duplicateGeoms = TRUE)

df <- as.data.frame(new)

labels <- sprintf("<strong>%s</strong>
                              <br/> Cases: %s
                              <br/> Death: %s",
                        new_dat$NAME, new_dat$Cases, new_dat$Deaths) %>% lapply(htmltools::HTML)

pal1 <- colorFactor(palette = c("Blues"), domain = new_dat$Cases)

leaflet(new_dat)%>%
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
              bringToFront = TRUE), group = "Cases")


