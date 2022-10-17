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

##

dat <- read.csv("dat/emdat-country-profiles.csv")
dat <- dat[-c(1),]
names(dat)

dat <- select(dat, 
              Year:Total.Deaths)   

unique(dat$Country)
str(dat)

dat_India <- filter(dat, Country=="India")
dat_Bhutan <- filter(dat, Country=="Bhutan")

dat_country <- rbind(dat_India,dat_Bhutan)

dat_country$Year <- as.numeric(as.character(dat_country$Year))
dat_country$Total.Affected <- as.numeric(as.character(dat_country$Total.Affected))

ggplot(dat_country, aes(x = Year, y = Total.Affected, col = Country)) +
  geom_point() +
  geom_line() +
  #scale_x_continuous(breaks=c(2007:2017)) +
  theme_bw()

ggplot(dat_country, aes(x = Year, y = Total.Affected)) +
  geom_point() +
  geom_line() +
  #scale_x_continuous(breaks=c(2007:2017)) +
  theme_bw() +
  facet_wrap(~Country)
