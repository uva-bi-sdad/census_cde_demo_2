library(tigris)
library(tidyverse)
library(sf)
library(crsuggest)
library(ggplot2)
library(shiny)
options(tigris_use_cache = TRUE)

#https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html
#https://api.dhsprogram.com/#/introapi.cfm

# CRS = NAD83(2011) Virginia Census Tracts
va_tracts<-map_dfr(c("VA"), ~{
  tracts(.x, cb=TRUE, year=2020)
}) %>%
  st_transform(8528)  

suggest_crs(va_tracts)

#Core based statistical area Richmond, VA
richmond_metro<-core_based_statistical_areas(cb=TRUE, year=2020) %>%
  filter(str_detect(NAME, "Richmond, VA")) %>%
  st_transform(8528)
 
#a plot of VA census tracts with the Richmond core based statistical 
#area outlined in red
ggplot() + 
  geom_sf(data=va_tracts, fill="white", color="grey") + 
  geom_sf(data=richmond_metro, fill=NA, color="red") + 
  theme_void()

richmond_tracts <- va_tracts[richmond_metro, ]
  suggest_crs(richmond_tracts)

ggplot() + 
  geom_sf(data=richmond_tracts, fill="white", color="grey") + 
  geom_sf(data=richmond_metro, fill=NA, color="red") + 
  theme_void()

richmond_tracts_within<-va_tracts %>%
  st_filter(richmond_metro, .predicate=st_within)

ggplot() + 
  geom_sf(data=richmond_tracts_within, fill="white", color="grey") + 
  geom_sf(data=richmond_metro, fill=NA, color="black") + 
  theme_void()

#Bring in hospital data
hospital_url<-"https://opendata.arcgis.com/api/v3/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0/downloads/data?format=geojson&spatialRefId=4326"
hospitals<-st_read(hospital_url) %>%
  st_transform(8528) %>%
  distinct(ID, .keep_all=TRUE)
names(hospitals)
richmond_hospitals<-hospitals %>%
      st_filter(richmond_metro, 
                .predicate=st_is_within_distance,
                dist=500)
hospitals<-st_read(hospital_url) %>%
  st_transform(8528) %>%
  distinct(ID, .keep_all=TRUE)
names(hospitals)
richmond_hospitals<-hospitals %>%
  st_filter(richmond_metro, 
            .predicate=st_is_within_distance,
            dist=500)

#Bring in SNF data
snf_url<-"https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/virginia_skilled_nursing_facility/facility/va_cms_provider_final_2022-07.csv"
snf<-read.csv(snf_url); dim(snf); names(snf); snf495327<-snf[snf$FPN=="495327", c(1,5,4)]
SNF_sf<-snf495327 %>%
st_as_sf(coords=c("long", "lat"),
            crs=4326) %>%
  st_transform(8528)

facilities_sf<-INFR_ASSETS %>%
  st_as_sf(coords=c("LNG", "LAT"),
           crs=4326) %>%
  st_transform(8528)

ggplot() + 
  geom_sf(data=richmond_tracts_within, fill="white", color=cbPalette[1]) + 
  geom_sf(data=richmond_metro, fill=NA, color="black") + 
  geom_sf(data=richmond_hospitals, fill="black", pch=21, cex=2, color="black") + 
  geom_sf(data=SNF_sf, fill=cbPalette[2], pch=21, cex=3, color="black") + 
  theme_void()

#dist<-SNF_sf %>% 
#  st_distance(richmond_hospitals) 
dist<-SNF_sf %>% 
  st_distance(facilities_sf) 

library(mapboxapi)
#HIFDL API ArcGIS REST APIs  https://developers.arcgis.com/rest/location-based-services/
#mb_access_token("", install = TRUE)
#times<-mb_matrix(SNF_sf, richmond_hospitals)
times<-mb_matrix(SNF_sf, facilities_sf)

buf5km<-st_buffer(SNF_sf, dist=5000) 

#iso10min<-mb_isochrone(
#  SNF_sf, 
#  time=10, 
#  profile="driving-traffic",
#  depart_at="2022-11-05T17:00"
#)

library(leaflet)
library(leafsync)
SNF_icon<-makeAwesomeIcon(icon="ios-medical", 
                          markerColor="orange",
                          library="ion")
#The Leaflet package requires data be in CRS 4326
map2<-leaflet() %>% 
  addTiles() %>%
  addPolygons(data=iso10min) %>% 
  addAwesomeMarkers(data=st_transform(SNF_sf, 4326),
                    icon=SNF_icon) %>%
#  addCircles(lng=richmond_hospitals$LONGITUDE,
#             lat=richmond_hospitals$LATITUDE,
#             radius=6,
#             color="black",
#             label=richmond_hospitals$NAME) 
addCircles(lng=facilities_sf$LNG,
           lat=facilities_sf$LAT,
           radius=6,
           color="black",
           label=facilities_sf$FAC) 
map2

