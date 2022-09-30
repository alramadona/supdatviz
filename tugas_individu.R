library(dplyr)
library(ggplot2)

dat <- read.csv("dat/DatasetAfricaMalaria.csv")
names(dat)

dat <- select(dat, 
              Country.Name,Year,Country.Code,
              Incidence.of.malaria..per.1.000.population.at.risk.,Malaria.cases.reported,latitude,longitude)   
names(dat) <- c("Country.Name","Year","Country.Code",
                "Incidence","Cases","latitude","longitude")

dat_Algeria_Botswana <- filter(dat, Country.Name==c("Algeria", "Botswana"))
str(dat_Algeria_Botswana)

ggplot(dat_Algeria_Botswana, aes(x = Year, y = Cases, col = Country.Name)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(2007:2017)) +
  theme_bw()

dat_2007 <- filter(dat, Year==2007)
str(dat_2007)

library(leaflet)
pal <- colorNumeric(palette = "plasma", domain = dat_2007$Cases)

leaflet(data = dat_2007) %>% addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, color = ~pal(Cases)) %>% 
  addProviderTiles(providers$CartoDB.Positron)

leaflet(data = dat_2007) %>% addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, color = ~pal(Cases), radius = ~log(Cases))
