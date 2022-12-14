library(SpatialEpi)
library(dplyr)

library(sp)
library(leaflet)


# intro -------------------------------------------------------------------
## https://www.paulamoraga.com/tutorial-areal-data/

data(pennLC)

?pennLC

head(pennLC$geo, 10)
head(pennLC$data, 10)
head(pennLC$smoking, 10)

pennLC$spatial.polygon
plot(pennLC$spatial.polygon)


# analisis spasial (1) ----------------------------------------------------

d <- group_by(pennLC$data, county) %>% summarize(Y = sum(cases),
                                                 pop = sum(population))
head(d)

pennLC$data <- pennLC$data[order(pennLC$data$county, pennLC$data$race, pennLC$data$gender, pennLC$data$age), ]
E <- expected(population = pennLC$data$population, cases = pennLC$data$cases, n.strata = 16)

E

d$E <- E
d$Exp <- (d$pop/sum(d$pop)) * sum(d$Y)

d$SMR <- d$Y/d$E
d$SMRv2 <- d$Y/d$Exp

d$SMR_diff <- d$SMR-d$SMRv2
d$SMR_diff_abs <- abs(d$SMR_diff)

rownames(d) <- d$county

map <- pennLC$spatial.polygon
plot(map)

map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)
head(map@data)

l <- leaflet(map) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = map$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# tugas 3 - silakan dilengkapi! (dengan SMRv2) ----------------------------

pal <- colorNumeric(palette = "YlOrRd", domain = map$______)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(______), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~______, opacity = 0.5, title = "SMR", position = "bottomright")


# analisis spasial (2) ----------------------------------------------------

dat <- read.csv("dat/Yk_kasus.csv")
dat$pop <- as.integer((dat$Pop_2016_II + dat$Pop_2017_I)/2)
head(dat, 10)

totPop <- sum(dat$pop)
totKasus <- sum(dat$kasus)

dat$E <- as.integer(dat$pop/totPop * totKasus)
dat$SMR <- dat$kasus/dat$E
head(dat, 10)

library(rgdal)
mapYk <- readOGR("dat/map/yogyakarta-village.shp", verbose = FALSE)
plot(mapYk)

rownames(dat) <- dat$Idx
mapYk <- SpatialPolygonsDataFrame(mapYk, dat, match.ID = TRUE)
head(mapYk@data,45)

library(leaflet)
l <- leaflet(mapYk) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = mapYk$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

## extra

labels <- sprintf("<strong>%s</strong><br/>Kasus: %s <br/>Expected: %s <br/>Jumlah Penduduk: %s <br/>SMR: %s",
                  mapYk$village, mapYk$kasus, mapYk$E, mapYk$pop, round(mapYk$SMR,2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# analisis spasial (3) ----------------------------------------------------

dat <- read.csv("dat/Yk_points.csv")
dat$n <- c(1:nrow(dat))
head(dat, 10)

# https://rstudio.github.io/leaflet/markers.html
leaflet(data = dat) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(keterangan)) %>% 
  addProviderTiles(providers$CartoDB.Positron)

leaflet(data = dat) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, radius = ~n) %>% 
  addProviderTiles(providers$CartoDB.Positron)

pal <- colorFactor(c("green", "orange", "red"), domain = c("I", "II", "III"))

leaflet(data = dat) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, color = ~pal(BM)) %>% 
  addProviderTiles(providers$CartoDB.Positron)

leaflet(data = dat) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, color = ~pal(BM), radius = ~log(BM_val)) %>% 
  addProviderTiles(providers$CartoDB.Positron)


# tugas 4 - silakan dilengkapi! -------------------------------------------

dat <- read.csv("______")
dat$n <- c(1:nrow(dat))
head(dat, 10)

# keterangan
leaflet(data = ______) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(keterangan)) %>% 
  addProviderTiles(providers$CartoDB.Positron)

# NIM
leaflet(data = dat) %>% addTiles() %>%
  addCircleMarkers(~______, ~______, color = "blue", radius = 3, label = ~as.character(______)) %>%
  addProviderTiles(providers$CartoDB.Positron)

# cluster
leaflet(data = ______) %>% addTiles() %>% 
  addMarkers(~______, ~______, clusterOptions = markerClusterOptions())
