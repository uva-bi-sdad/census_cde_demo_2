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
 urlA8 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_town_county_crosswalk.csv"
 urlA9 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_zipcode_town_county_crosswalk.csv"
urlA10 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_total_county_population_2020.csv"

#Read in all the facility locations and keep only the lats, longs, fips (if available), zip code, and county/city name
#Three data sources have the FIP code; need to change county to uppercase; remove city from the county name;
#make sure zipcodes only are 5 digits, remove the -####

#Read in the Town-to-County file
TWN<-read.csv(urlA8); dim(TWN); View(TWN)
  TWN$COUNTY<-toupper(TWN$COUNTY)
  TWN$COUNTY<-as.character(sub(" COUNTY", "", TWN$COUNTY, fixed=TRUE))
  
#Read in the Zip-to-Town-to-County file
ZIP<-read.csv(urlA9); dim(ZIP); View(ZIP)
  ZIP$COUNTY<-toupper(ZIP$COUNTY)
  ZIP$COUNTY<-as.character(sub(" COUNTY", "", ZIP$COUNTY, fixed=TRUE))

#Read in the HIFLD hospital data from the url; there are 144 unique locations; County FIPS
HOS<-read.csv(urlA1); dim(HOS); names(HOS); #View(HOS) 
  hos<-HOS[,c(18,19,15,9,16)]; names(hos)<-c("LAT","LNG","COUNTY","ZIP","FIP")
  hos<-data.frame(FAC=rep("hos",dim(hos)[1]), hos)
  hos$ZIP<-as.character(hos$ZIP)
  hos$COUNTY<-toupper((hos$COUNTY))
  #View(hos)
  
#Read in the HIFLD American Red Cross data from the url; there are 33 unique locations; County FIPS
ARC<-read.csv(urlA2); dim(ARC); names(ARC); #View(ARC) 
  arc<-ARC[,c(29,28,14,12,15)]; names(arc)<-c("LAT","LNG","COUNTY","ZIP","FIP")
  arc<-data.frame(FAC=rep("arc",dim(arc)[1]), arc) 
  arc$ZIP<-as.character(arc$ZIP)
  arc$COUNTY<-toupper(arc$COUNTY) 
  #View(arc)
  
#Read in the HIFLD EMS data from the url; there are 1119 unique locations; County FIPS
#remove parenthesis around city
EMS<-read.csv(urlA3); dim(EMS); names(EMS); #View(EMS) 
  ems<-EMS[,c(17,16,12,10,13)]; names(ems)<-c("LAT","LNG","COUNTY","ZIP","FIP")
  ems<-data.frame(FAC=rep("ems",dim(ems)[1]), ems)
  ems$ZIP<-as.character(ems$ZIP)
  ems$COUNTY<-toupper(ems$COUNTY)
  ems$COUNTY<-as.character(sub(" (CITY)", " CITY", ems$COUNTY, fixed=TRUE))
  #View(ems)
  
#Read in the HIFLD fire station data from the url; there are 1009 unique locations; City name only
#Need to assign county names based on zip codes  
FS<-read.csv(urlA4); dim(FS); names(FS); #View(FS) 
  fs<-FS[,c(2,1,6,8)]; names(fs)<-c("LAT","LNG","TOWN","ZIP")
  fs<-data.frame(FAC=rep("fs",dim(fs)[1]), fs, FIP=rep(1,dim(fs)[1]))
  fs$ZIP<-as.character(fs$ZIP)
  fs$ZIP<-substr(as.character(fs$ZIP), 1, 5)
  temp<-merge(fs, ZIP, by=c("TOWN", "ZIP"), all.x=TRUE, all.y=FALSE); View(temp)
  fs<-unique(temp); rm(temp); fs<-fs[,c(3,4,5,7,2,6)]
  #View(fs)
  
#Read in the HIFLD national shelter facilities data from the url; there are 1385 unique locations; County name only
#Missing zip codes
NSF<-read.csv(urlA5); dim(NSF); names(NSF); #View(NSF)
  nsf<-NSF[,c(27,28,7,44)]; names(nsf)<-c("LAT","LNG","COUNTY","ZIP")
  nsf<-data.frame(FAC=rep("nsf",dim(nsf)[1]), nsf, FIP=rep(1,dim(nsf)[1]))
  nsf$ZIP<-as.character(nsf$ZIP)
  nsf$COUNTY<-toupper(nsf$COUNTY)
  nsf$COUNTY<-ifelse(nsf$COUNTY=="VIRGINIA BEACH","VIRGINIA BEACH CITY",nsf$COUNTY)
  #View(nsf)
  
#Read in the HIFLD urgent care facilities data from the url; there are 141 unique locations; County FIPS
UCF<-read.csv(urlA6); dim(UCF); names(UCF); #View(UCF)
  ucf<-UCF[,c(26,25,11,9)]; names(ucf)<-c("LAT","LNG","COUNTY","ZIP"); ucf$ZIP<-as.character(ucf$ZIP)
  ucf<-data.frame(FAC=rep("ucf",dim(ucf)[1]), ucf, FIP=rep(1,dim(ucf)[1]))
  ucf$ZIP<-as.character(ucf$ZIP)
  ucf$COUNTY<-toupper(ucf$COUNTY)
  ucf$COUNTY<-ifelse(ucf$COUNTY=="SOUTH BOSTON CITY","HALIFAX",ucf$COUNTY)
  #View(ucf)
  
#Read in the HUD ZIP-to-FIP crosswalk  
CRWK<-read.csv(urlA7); dim(CRWK); names(CRWK); View(CRWK)
  CRWK$COUNTY<-toupper(CRWK$COUNTY)

#In order to count the number of facilities per county/city, change all county/city names to FIPS codes
#Put all the facility data in a single data set
INFR_ASSETS<-data.frame(rbind(ems,nsf,hos,arc,fs,ucf)); dim(INFR_ASSETS)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="VIRGINIA BEACH","VIRGINIA BEACH CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="ALEXANDRIA","ALEXANDRIA CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="BEDFORD CITY","BEDFORD",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="BRISTOL","BRISTOL CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="CHESAPEAKE","CHESAPEAKE CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="HAMPTON","HAMPTON CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="EMPORIA","EMPORIA CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="LEXINGTON","LEXINGTON CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="RADFORD","RADFORD CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="NORFOLK","NORFOLK CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="SALEM","SALEM CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="PORTSMOUTH","PORTSMOUTH CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="CHARLOTTESVILLE","CHARLOTTESVILLE CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="GALAX","GALAX CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="HARRISONBURE","HARRISONBURG CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="HOPEWELL","HOPEWELL CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="MARTINSVILLE","MARTINSVILLE CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="WILLIAMSBURG","WILLIAMSBURG CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="HARRISONBURG","HARRISONBURG CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="HOPEWELL","HOPEWELL CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="DANVILLE","DANVILLE CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="FREDICKSBURG CITY","FREDERICKSBURG CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="FREDERICKSBURG","FREDERICKSBURG CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="HARRISONBURG","HARRISONBURG CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="LYNCHBURG","LYNCHBURG CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="MANASSAS","MANASSAS CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="NEWPORT NEWS","NEWPORT NEWS CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="NORTON","NORTON CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="MANASSAS","MANASSAS CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="PETERSBURG","PETERSBURG CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="STAUNTON","STAUNTON CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="SUFFOLK","SUFFOLK CITY",INFR_ASSETS$COUNTY)
  INFR_ASSETS$COUNTY<-ifelse(INFR_ASSETS$COUNTY=="WINCHESTER","WINCHESTER CITY",INFR_ASSETS$COUNTY)
  View(INFR_ASSETS); dim(INFR_ASSETS)
  
temp<-merge(INFR_ASSETS, CRWK, by="COUNTY", all.x=TRUE, all.y=TRUE); View(temp)
  temp$FIP.y<-ifelse(is.na(temp$FIP.y),temp$FIP.x,temp$FIP.y)
#Remove facilities that are not in Virginia using FIP.y
  temp<-temp[as.numeric(substr(temp$FIP.y, 1, 2))==51, -6]; dim(temp)
    names(temp)<-c("COUNTY","FAC","LAT","LNG","ZIP","FIP")
    temp2<-data.frame(table(temp$COUNTY, temp$FAC))
    names(temp2)<-c("COUNTY","FAC","FREQ")
    CO_assets<-pivot_wider(temp2, names_from="FAC", values_from="FREQ")
    CO_assets$resilience<-apply(CO_assets[,2:7], 1, sum)
    CO_assets<-as.data.frame(CO_assets)
    names(CO_assets)<-c("COUNTY","ems","red_cross","fire_station","hospital","national_shelter","urgent_care","total")
    CO_assets<-as.data.frame(CO_assets)
  write.csv(CO_assets, "~/Documents/Counting People/Data/Derived Variables/va_county_shelter_and_emergency_facility_resilience_index.csv", row.names=FALSE)
  rm(temp2, temp)
  
#Read in county/city total population data
#the data are from https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html  
POP<-read.csv(urlA10); dim(POP); View(POP) 
  POP$COUNTY<-toupper(POP$COUNTY)
  POP$COUNTY<-as.character(sub(", VIRGINIA", "", POP$COUNTY, fixed=TRUE))
  POP$COUNTY<-as.character(sub(" COUNTY", "", POP$COUNTY, fixed=TRUE))
  POP$COUNTY<-as.character(sub(".", "", POP$COUNTY, fixed=TRUE))
   
#Merge the county/city population totals with the facility/shelter totals
#Calculate the number of facilities/shelters per 10,000 residents  
#Merge with the crosswalk to add the FIPs for each county/city   
    T1<-merge(POP, CO_assets, by="COUNTY", all.x=TRUE, all.y=TRUE); View(T1)
    T1$PER10000<-(T1$resilience*10000)/T1$POP
  CNTY_ASSETS<-T1; View(CNTY_ASSETS)
    temp<-merge(CNTY_ASSETS, CRWK, by="COUNTY", all.x=TRUE, all.y=TRUE); View(temp)
    CNTY_ASSETS<-temp[,c(11,1,2,3,4,5,6,7,8,9,10)]
    names(CNTY_ASSETS)<-c("FIP", "county_city", "total_pop_2020", "ems", "red_cross", "fire_station", 
                          "hospital", "national_shelter", "urgent_care", "total_facilities_shelters",
                          "facilities_shelters_per_10000")
    write.csv(CNTY_ASSETS, "~/Documents/Counting People/Data/Derived Variables/va_county_shelter_and_emergency_facility_resilience_index.csv", row.names=FALSE)
    rm(T1)
    
#############################################################################
#Display the County Assets using a choropleth
#Read in the SNF deficiency index
url12 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/processes/derived_variables/va_snf_deficiency_indices_k_e.csv"

#Read in the Deficiency Index file
DE<-read.csv(url12); dim(DE); View(DE); 

#Use a Jenk Breaks to categorize the SNF combined K+E deficiencies
BREAKS_KE<-getJenksBreaks(DE$KE_DEF_IND[DE$KE_DEF_IND>0], 4, subset = NULL); BREAKS_KE

DE$group_KE<-rep(NA, length=dim(DE)[1])
DE$group_KE[which(DE$KE_DEF_IND==0)]="[0]"
DE$group_KE[which(DE$KE_DEF_IND>=BREAKS_KE[1] & DE$KE_DEF_IND<BREAKS_KE[2])]="[19.7, 86.1)"
DE$group_KE[which(DE$KE_DEF_IND>=BREAKS_KE[2] & DE$KE_DEF_IND<BREAKS_KE[3])]="[86.1, 157.5)"
DE$group_KE[which(DE$KE_DEF_IND>=BREAKS_KE[3])]="[157.5, 314.0]"
DE$group_KE<-factor(DE$group_KE, 
                    levels=c("[0]","[19.7, 86.1)","[86.1, 157.5)","[157.5, 314.0]"))

DEpalette<-c("[0]"="#999999", "[19.7, 86.1)"="#56B4E9",
             "[86.1, 157.5)"="#0072B2", "[157.5, 314.0]"="#E69F00")

#Read in the County asset file
T1<-CNTY_ASSETS[,c(2,3,10,11,1)]
names(T1)<-c("COUNTY","TOTAL","FREQ","PER10000","GEOID" )
CA<-merge(GEOMETRY, T1, by="GEOID", all=TRUE); dim(CA)

#Use a Jenk Breaks
BREAKS_CA<-getJenksBreaks(CA$PER10000, 8, subset=NULL)
  
CA$group<-rep(NA, length=dim(CA)[1])
CA$group[which(CA$PER10000>=BREAKS_CA[1] & CA$PER10000<BREAKS_CA[2])]="[1.27, 3.65)"
CA$group[which(CA$PER10000>=BREAKS_CA[2] & CA$PER10000<BREAKS_CA[3])]="[3.65, 6.39)"
CA$group[which(CA$PER10000>=BREAKS_CA[3] & CA$PER10000<BREAKS_CA[4])]="[6.39, 9.03)"
CA$group[which(CA$PER10000>=BREAKS_CA[4] & CA$PER10000<BREAKS_CA[5])]="[9.03, 12.3)"
CA$group[which(CA$PER10000>=BREAKS_CA[5] & CA$PER10000<BREAKS_CA[6])]="[12.3, 18.2)"
CA$group[which(CA$PER10000>=BREAKS_CA[6] & CA$PER10000<BREAKS_CA[7])]="[18.2, 24.4)"
CA$group[which(CA$PER10000>=BREAKS_CA[7])]="[24.4, 40.3]"
CA$group<-factor(CA$group, 
                 levels=c("[1.27, 3.65)","[3.65, 6.39)","[6.39, 9.03)","[9.03, 12.3)",
                          "[12.3, 18.2)","[18.2, 24.4)","[24.4, 40.3]"))
    
CApalette<-c("[1.27, 3.65)"="#F9F1CB", "[3.65, 6.39)"="#DFDAB3",
             "[6.39, 9.03)"="#C2C097", "[9.03, 12.3)"="#A5A67B",
             "[12.3, 18.2)"="#888C5E", "[18.2, 24.4)"="#6B7242",
             "[24.4, 40.3]"="#4E5827")


CA_KE_plot<-ggplot() + 
  geom_sf(data=CA, aes(fill=group), color="grey") + 
  #  geom_sf(fill="transparent", color="#B2521A", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==-1,]) +
  #  geom_sf(fill="transparent", color="#31596E", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==1,]) +
  scale_fill_manual(values=CApalette, 
                    name="Number of Facilities & Services/10,000 Residents",
                    drop=FALSE, 
                    labels=c("1.27           3.65","6.39","9.03","12.3","18.2","24.4","40.3"),
                    na.translate=FALSE,
                    guide=guide_legend(
                      direction="horizontal",
                      keyheight=unit(4, units="mm"),
                      keywidth=unit(25/length(labels), units="mm"),
                      title.position="top",
                      title.hjust=0.5,
                      label.hjust=1,
                      nrow=1,
                      byrow=TRUE,
                      reverse=FALSE,
                      label.position="bottom"
                    )) +
  geom_point(data=DE[DE$group_KE=="[0]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DE[DE$group_KE=="[0]", ], aes(x=long, y=lat), colour="#999999", size=3.0) +
  geom_point(data=DE[DE$group_KE=="[19.7, 86.1)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DE[DE$group_KE=="[19.7, 86.1)", ], aes(x=long, y=lat), colour="#56B4E9", size=3.0) +
  geom_point(data=DE[DE$group_KE=="[86.1, 157.5)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DE[DE$group_KE=="[86.1, 157.5)", ], aes(x=long, y=lat), colour="#0072B2", size=3.0) +
  geom_point(data=DE[DE$group_KE=="[157.5, 314.0]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DE[DE$group_KE=="[157.5, 314.0]", ], aes(x=long, y=lat), colour="#E69F00", size=3.0) +
  labs(title="Shelter Facilities and Emergency Service Providers by\n                       County and Independent City", 
       subtitle="                        Facilities and Services per 10,000 Residents", 
       caption="Homeland Infrastructure Foundation-Level Data: https://hifld-geoplatform.opendata.arcgis.com/\n    Fire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") + 
  theme(panel.background=element_rect(fill="transparent"),
        text=element_text(color="#22211d"),
        plot.title=element_text(size=18, face="bold", hjust=0.2, vjust=-22.0),
        plot.subtitle=element_text(size=15, hjust=0.2, vjust=-27.0),
        plot.caption=element_text(size=10, hjust=0.1),
        legend.title=element_text(size=15),
        legend.text=element_text(size=13),
        legend.position="bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  annotate("text", x=-81.50000, y=38.30989, adj=0, size=5.0, label="Most Deficient") +
  annotate("point", x=-81.60000, y=38.30989, colour="black", size=4.25) + 
  annotate("point", x=-81.60000, y=38.30989, colour="#E69F00", size=3.75) +
  annotate("point", x=-81.60000, y=38.20989, colour="black", size=4.25) + 
  annotate("point", x=-81.60000, y=38.20989, colour="#0072B2", size=3.75) +
  annotate("text", x=-81.50000, y=38.10989, adj=0, size=5.0, label="Least Deficient") + 
  annotate("point", x=-81.60000, y=38.10989, colour="black", size=4.25) + 
  annotate("point", x=-81.60000, y=38.10989, colour="#56B4E9", size=3.75) +
  annotate("text", x=-81.50000, y=38.00989, adj=0, size=5.0, label="No Deficiencies") +
  annotate("point", x=-81.60000, y=38.00989, colour="black", size=4.25) + 
  annotate("point", x=-81.60000, y=38.00989, colour="#999999", size=3.75) +
  annotate("text",  x=-82.80000, y=38.60000, adj=0, size=5.75, fontface="bold", label="     Skilled Nursing Facility Emergency\n         Preparedness Deficiency Index") 
#ggsave("CA_KE_plot.pdf", width=14, height=10)
CA_KE_plot

    
    