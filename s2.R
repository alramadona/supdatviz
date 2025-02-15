library(dplyr)

dat <- read.csv("dat/Berau_puskesmas_v2.csv")
dat$X <- NULL
names(dat)

datPenduduk <- select(dat, 
                      puskesmas, kecamatan, 
                      jlh_penduduk_2021,jlh_penduduk_2020,jlh_penduduk_2019,jlh_penduduk_2018)

datDiare <- select(dat,
                   puskesmas, kecamatan,
                   diare_2021,diare_2020,diare_2019,diare_2018)

datDBD <- select(dat,
                 puskesmas,kecamatan,
                 dbd_2021,dbd_2020,dbd_2019,dbd_2018)

################## spasial

# dat$lat <- as.numeric(as.character(sub(',.*', '', dat$lat_long)))
# dat$long <- as.numeric(as.character(sub('.*,', '', dat$lat_long)))

library(leaflet)

leaflet(data = dat) %>% addTiles() %>%
  addCircleMarkers(~long, ~lat, color = "red", label = ~as.character(puskesmas), radius = 3) %>%
  addProviderTiles(providers$OpenStreetMap)

leaflet(data = dat) %>% addTiles() %>% 
  addCircleMarkers(~long, ~lat, clusterOptions = markerClusterOptions())

leaflet(data = dat) %>% addTiles() %>%
  addCircleMarkers(~long, ~lat, color = "red", label = ~as.character(puskesmas), radius = ~sqrt(bumil_sasaran)) %>%
  addProviderTiles(providers$MapBox)

library(sp)
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("ggspatial")

mapBerau <- read_sf("dat/map/batas_luar_adm_Berau_pola_kecamatan20.shp")

ggplot(data = mapBerau) +
  geom_sf()

ggplot(data = mapBerau) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Kabupaten Berau", subtitle = paste0("(", length(unique(mapBerau$KECAMATAN)), " kecamatan)"))

ggplot(data = mapBerau) +
  geom_sf(aes(fill = ID)) +
  xlab("Longitude") + ylab("Latitude")

ggplot(data = mapBerau) +
  geom_sf(aes(fill = ID)) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

datDBD <- dat %>% 
  select(puskesmas, kecamatan, 
         jlh_penduduk_2021,jlh_penduduk_2020,jlh_penduduk_2019,jlh_penduduk_2018,
         dbd_2021,dbd_2020,dbd_2019,dbd_2018) %>%
  group_by(kecamatan) %>%
  summarise(jlh_penduduk_2021 = sum(jlh_penduduk_2021),
            jlh_penduduk_2020 = sum(jlh_penduduk_2020),
            jlh_penduduk_2019 = sum(jlh_penduduk_2019),
            jlh_penduduk_2018 = sum(jlh_penduduk_2018),
            dbd_2021 = sum(dbd_2021),
            dbd_2020 = sum(dbd_2020),
            dbd_2019 = sum(dbd_2019),
            dbd_2018 = sum(dbd_2018))

tot_penduduk_2021 <- sum(datDBD$jlh_penduduk_2021)
tot_dbd_2021 <- sum(datDBD$dbd_2021)
datDBD$E_dbd_2021 <- (datDBD$jlh_penduduk_2021/tot_penduduk_2021)*tot_dbd_2021
datDBD$SMR2021 <- datDBD$dbd_2021/datDBD$E_dbd_2021
mapBerau$SMR2021 <- datDBD$SMR2021[match(mapBerau$KECAMATAN, datDBD$kecamatan)]

tot_penduduk_2020 <- sum(datDBD$jlh_penduduk_2020)
tot_dbd_2020 <- sum(datDBD$dbd_2020)
datDBD$E_dbd_2020 <- (datDBD$jlh_penduduk_2020/tot_penduduk_2020)*tot_dbd_2020
datDBD$SMR2020 <- datDBD$dbd_2020/datDBD$E_dbd_2020
mapBerau$SMR2020 <- datDBD$SMR2020[match(mapBerau$KECAMATAN, datDBD$kecamatan)]

tot_penduduk_2019 <- sum(datDBD$jlh_penduduk_2019)
tot_dbd_2019 <- sum(datDBD$dbd_2019)
datDBD$E_dbd_2019 <- (datDBD$jlh_penduduk_2019/tot_penduduk_2019)*tot_dbd_2019
datDBD$SMR2019 <- datDBD$dbd_2019/datDBD$E_dbd_2019
mapBerau$SMR2019 <- datDBD$SMR2019[match(mapBerau$KECAMATAN, datDBD$kecamatan)]

tot_penduduk_2018 <- sum(datDBD$jlh_penduduk_2018)
tot_dbd_2018 <- sum(datDBD$dbd_2018)
datDBD$E_dbd_2018 <- (datDBD$jlh_penduduk_2018/tot_penduduk_2018)*tot_dbd_2018
datDBD$SMR2018 <- datDBD$dbd_2018/datDBD$E_dbd_2018
mapBerau$SMR2018 <- datDBD$SMR2018[match(mapBerau$KECAMATAN, datDBD$kecamatan)]

library(viridis)
my_breaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

mapBerau_points <- st_centroid(mapBerau)
mapBerau_points <- cbind(mapBerau, st_coordinates(st_centroid(mapBerau$geometry)))

datPoints <- dat %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform()

datPoints <- cbind(datPoints, st_coordinates(st_centroid(datPoints$geometry)))

ggplot(data = mapBerau) +
  geom_sf(aes(fill = SMR2018)) +
  scale_fill_gradientn(colours=rev(magma(10)),
                       na.value = "grey100", 
                       breaks = my_breaks, labels = my_breaks) +
  geom_text(data= mapBerau_points, aes(x=X, y=Y, label=KECAMATAN),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  xlab("Longitude") + ylab("Latitude")

p1 <- ggplot(data = mapBerau) +
  geom_sf(aes(fill = SMR2018)) +
  scale_fill_gradient(low = "green", high = "darkred",
                      limits = c(0,5)) +
  geom_text(data= mapBerau_points, aes(x=X, y=Y, label=KECAMATAN),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SMR Demam Berdarah", subtitle = "2018")

p2 <- ggplot(data = mapBerau) +
  geom_sf(aes(fill = SMR2019)) +
  scale_fill_gradient(low = "green", high = "darkred",
                      limits = c(0,5)) +
  geom_text(data= mapBerau_points, aes(x=X, y=Y, label=KECAMATAN),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SMR Demam Berdarah", subtitle = "2019")

p3 <- ggplot(data = mapBerau) +
  geom_sf(aes(fill = SMR2020)) +
  scale_fill_gradient(low = "green", high = "darkred",
                      limits = c(0,5)) +
  geom_text(data= mapBerau_points, aes(x=X, y=Y, label=KECAMATAN),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SMR Demam Berdarah", subtitle = "2020")

p4 <- ggplot(data = mapBerau) +
  geom_sf(aes(fill = SMR2021)) +
  scale_fill_gradient(low = "green", high = "darkred",
                      limits = c(0,5)) +
  geom_text(data= mapBerau_points, aes(x=X, y=Y, label=KECAMATAN),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SMR Demam Berdarah", subtitle = "2021")

library(ggpubr)
ggarrange(p1, p2, p3, p4,
          labels = c("", "", "", ""),
          ncol = 2, nrow = 2)

ggplot(data = mapBerau) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Kabupaten Berau", subtitle = paste0("(", length(unique(mapBerau$KECAMATAN)), " kecamatan)")) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

ggplot(data = mapBerau) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Kabupaten Berau", subtitle = paste0("(", length(unique(mapBerau$KECAMATAN)), " kecamatan)")) +
  geom_text(data= mapBerau_points, aes(x=X, y=Y, label=KECAMATAN),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

ggplot(data = mapBerau) +
  geom_sf() +
  geom_sf(data = datPoints, 
          color = "red", size = 1) + 
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Kabupaten Berau", subtitle = paste0("(", length(unique(mapBerau$KECAMATAN)), " kecamatan)")) +
  geom_text(data= mapBerau_points, aes(x=X, y=Y, label=KECAMATAN),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

