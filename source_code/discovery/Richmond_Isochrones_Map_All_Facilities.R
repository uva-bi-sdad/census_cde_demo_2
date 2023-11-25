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
  geom_sf(data=richmond_metro, fill=NA, color="black") + 
  theme_void()

richmond_tracts <- va_tracts[richmond_metro, ]
suggest_crs(richmond_tracts)

ggplot() + 
  geom_sf(data=richmond_tracts, fill="white", color="grey") + 
  geom_sf(data=richmond_metro, fill=NA, color="black") + 
  theme_void()

richmond_tracts_within<-va_tracts %>%
  st_filter(richmond_metro, .predicate=st_within)

ggplot() + 
  geom_sf(data=richmond_tracts_within, fill="white", color="grey") + 
  geom_sf(data=richmond_metro, fill=NA, color="black") + 
  theme_void()

#Bring in facility/shelter data see below
#only keeping the ones with valid lats and longs
hospital_url<-"https://opendata.arcgis.com/api/v3/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0/downloads/data?format=geojson&spatialRefId=4326"
va_facilities<-INFR_ASSETS %>% 
   st_as_sf(coords=c("LNG", "LAT"), crs=4326) %>%
      st_transform(8528) 
richmond_facilities<-va_facilities %>%
   st_filter(richmond_metro, .predicate=st_is_within_distance, dist=500)

#Bring in SNF data
snf_url<-"https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/virginia_skilled_nursing_facility/facility/va_cms_provider_final_2022-07.csv"
snf<-read.csv(snf_url); dim(snf); names(snf); snf495327<-snf[snf$FPN=="495327", c(1,5,4)]
SNF_sf<-snf495327 %>%
  st_as_sf(coords=c("long", "lat"), crs=4326) %>%
     st_transform(8528)

ggplot() + 
  geom_sf(data=richmond_tracts_within, fill="white", color=cbPalette[1]) + 
  geom_sf(data=richmond_metro, fill=NA, color="black") + 
  geom_sf(data=richmond_facilities, fill="black", pch=21, cex=2, color="black") + 
  geom_sf(data=SNF_sf, fill=cbPalette[2], pch=21, cex=3, color="black") + 
  theme_void()

dist<-SNF_sf %>% 
  st_distance(richmond_facilities) 

library(mapboxapi)
#mb_access_token("", install = TRUE)
times<-mb_matrix(SNF_sf, richmond_facilities)

buf5km<-st_buffer(SNF_sf, dist=5000) 

#iso15min<-mb_isochrone(
#  SNF_sf, 
#  time=15, 
#  profile="driving-traffic",
#  depart_at="2022-11-05T17:00"
#)
#iso30min<-mb_isochrone(
#  SNF_sf, 
#  time=30, 
#  profile="driving-traffic",
#  depart_at="2022-11-05T17:00"
#)


#change the CRS for leaflet to 4326
library(sfheaders)
richmond_facilities_4326<-st_transform(richmond_facilities, 4326)
temp<-sf_to_df(richmond_facilities_4326)[,c(3,4)]; dim(temp); View(temp)
facilities<-temp; names(facilities)<-c("LONGITUDE", "LATITUDE")
 
library(leaflet)
library(leafsync)
SNF_icon<-makeAwesomeIcon(icon="ios-medical", 
                          markerColor="orange",
                          library="ion")
#The Leaflet package requires data be in CRS 4326
map2<-leaflet() %>% 
  addTiles() %>%
  addPolygons(data=iso30min) %>% 
  addPolygons(data=iso15min) %>% 
  addAwesomeMarkers(data=st_transform(SNF_sf, 4326),
                    icon=SNF_icon) %>%
  addCircles(lng=facilities$LONGITUDE,
             lat=facilities$LATITUDE,
             radius=6,
             color="black",
             label=richmond_hospitals$NAME) 
 map2

 
 
 
 
 
 ############################################################################################################
 #County level disaster evacuation assests
 library(stringr)
 library(tidyr)
 library(ggplot2)
 
 # Copy the url of the raw data
 urlA1 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_hilfd_hospitals_2022.csv"
 urlA2 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_hifld_american_red_cross_chapter_facilities_2022.csv"
 urlA3 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_hilfd_emergency_medical_service_stations_2022.csv"
 urlA4 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_hilfd_fire_stations_2020.csv"
 urlA5 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_hilfd_national_shelter_system_facilities_2022.csv"
 urlA6 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_hilfd_urgent_care_facilities_2018.csv"
 urlA7 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_county_city_fip_crosswalk.csv"
 #Read in the HIFLD hospital data from the url; there are 144 unique locations; County FIPS
 HOS<-read.csv(urlA1); dim(HOS); names(HOS); #View(HOS) 
 hos<-HOS[,c(18,19)]; names(hos)<-c("LAT","LNG")
 hos<-data.frame(FAC=rep("hos",dim(hos)[1]), hos)
 #Read in the HIFLD American Red Cross data from the url; there are 33 unique locations; County FIPS
 ARC<-read.csv(urlA2); dim(ARC); names(ARC); #View(ARC) 
 arc<-ARC[,c(29,28)]; names(arc)<-c("LAT","LNG")
 arc<-data.frame(FAC=rep("arc",dim(arc)[1]), arc) 
 #Read in the HIFLD EMS data from the url; there are 1119 unique locations; County FIPS
 #remove parenthesis around city
 EMS<-read.csv(urlA3); dim(EMS); names(EMS); #View(EMS) 
 ems<-EMS[,c(17,16)]; names(ems)<-c("LAT","LNG")
 ems<-data.frame(FAC=rep("ems",dim(ems)[1]), ems)
 #Read in the HIFLD fire station data from the url; there are 1009 unique locations; City name only
 #Need to assign county names based on zip codes  
 FS<-read.csv(urlA4); dim(FS); names(FS); #View(FS) 
 fs<-FS[,c(2,1)]; names(fs)<-c("LAT","LNG")
 fs<-data.frame(FAC=rep("fs",dim(fs)[1]), fs)
 #Read in the HIFLD national shelter facilities data from the url; there are 1385 unique locations; County name only
 #Missing zip codes
 NSF<-read.csv(urlA5); dim(NSF); names(NSF); #View(NSF)
 nsf<-NSF[,c(27,28)]; names(nsf)<-c("LAT","LNG")
 nsf<-data.frame(FAC=rep("nsf",dim(nsf)[1]), nsf)
 #Read in the HIFLD urgent care facilities data from the url; there are 141 unique locations; County FIPS
 UCF<-read.csv(urlA6); dim(UCF); names(UCF); #View(UCF)
 ucf<-UCF[,c(26,25)]; names(ucf)<-c("LAT","LNG")
 ucf<-data.frame(FAC=rep("ucf",dim(ucf)[1]), ucf)
 #In order to count the number of facilities per county/city, change all county/city names to FIPS codes
 #Put all the facility data in a single data set
 INFR_ASSETS<-data.frame(rbind(nsf,ems,ucf,hos)); dim(INFR_ASSETS)
  INFR_ASSETS<-INFR_ASSETS[complete.cases(INFR_ASSETS), ]
  dim(INFR_ASSETS); View(INFR_ASSETS)
