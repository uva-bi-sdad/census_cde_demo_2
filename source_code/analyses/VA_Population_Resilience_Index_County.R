library(ggrepel)
library(BAMMtools)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(sf)
library(scales)
library(extrafont)
library(extrafontdb)
library(Rttf2pt1)
options(tigris_use_cache=TRUE)

#This code is used to download and view data directly from the CENSUS API
#census_api_key("95313390ff9a8c4afc2c080ea4d907a18a29f727")
#V2020<-load_variables(2020, "acs5", cache=TRUE)
#View(V2020)

#Set the color palette for 7 categories
SEQ<-c("#F9F1CB","#4E5827")
SEQolive<-colorRampPalette(SEQ)(42)
show_col(SEQolive)
show_col(SEQolive[c(1,7,14,21,28,35,42)])
SEQ_OLIVE<-SEQolive[c(1,7,14,21,28,35,42)]; show_col(SEQ_OLIVE)

url1 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/data_tables/skilled_nursing_facility/va_snf_deficiency_indices_k_e.csv"

#Read in the Deficiency Index file
DE<-read.csv(url1); dim(DE); View(DE) 

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

#############################################################################
#Resilience by County/City
#############################################################################
#Percentage of Population with no high school degree
#Calculated as ((S1501_C01_002E+S1501_C01_007E+S1501_C01_008E)/
#(S1501_C01_001E+S1501_C01_006E))*100

NHSvars<-c(
  #Total Population 18 to 24 years
  "S1501_C01_001E",
  #Population 18 to 24 years!!Less than high school graduate
  "S1501_C01_002E",
  #Total Population 25 years and over
  "S1501_C01_006E",
  #Population 25 years and over!!Less than 9th 
  "S1501_C01_007E",
  #Total Population 25 years and over!!9th to 12th grade, no diploma
  "S1501_C01_008E")

NHS<-get_acs(geography="county", 
             state="VA", 
             variables=NHSvars, #No high school education
             year=2020, 
             survey="acs5", 
             output="wide", 
             geometry=TRUE,
             cache_table = TRUE) 
#View(NHS); dim(NHS)

#Percent of the population 18 years and older with no HS diploma
NHS$PNS<-round(((NHS$S1501_C01_002E+NHS$S1501_C01_007E+NHS$S1501_C01_008E)/(NHS$S1501_C01_001E+NHS$S1501_C01_006E))*100, 1)
dim(NHS)

#############################################################################
#Percentage of Population with a Disability
#Calculated as 
#Percentage disabled male (DIS$B18101_004E+DIS$B18101_007E+DIS$B18101_010E+DIS$B18101_013E+DIS$B18101_016E)
#Percentage disabled female (DIS$B18101_023E+DIS$B18101_026E+DIS$B18101_029E+DIS$B18101_032E+DIS$B18101_035E)
#Percentage total disabled (DIS$MALE+DIS$FEMALE)/DIS$B18101_001E)*100 

DISvar<-c(
  #Total 
  "B18101_001E",
  #Male under 5 with any disability
  "B18101_004E",
  #Male 5-17 with any disability
  "B18101_007E",
  #Male 18-34 with any disability
  "B18101_010E",
  #Male 35-64 with any disability
  "B18101_013E",
  #Male 65-74 with any disability
  "B18101_016E",
  #Male 75 and over with any disability
  "B18101_019E",
  #Female under 5 with any disability
  "B18101_023",
  #Female 5-17 with any disability
  "B18101_026",
  #Female 18-34 with any disability
  "B18101_029",
  #Female 35-64 with any disability
  "B18101_032",
  #Female 65-74 with any disability
  "B18101_035",
  #Female 75 and over with any disability
  "B18101_038")

DIS<-get_acs(geography="county", 
             state="VA", 
             variables=DISvar, #Percentage with a disability
             year=2020, 
             survey="acs5", 
             output="wide", 
             geometry=TRUE,
             cache_table = TRUE) 
#View(DIS); dim(DIS)

#Percentage of the population 1with any disability
DIS$MALE<-(DIS$B18101_004E+DIS$B18101_007E+DIS$B18101_010E+DIS$B18101_013E+DIS$B18101_016E)
DIS$FEMALE<-(DIS$B18101_023E+DIS$B18101_026E+DIS$B18101_029E+DIS$B18101_032E+DIS$B18101_035E)
DIS$PDIS<-round(((DIS$MALE+DIS$FEMALE)/DIS$B18101_001E)*100, 1)

#############################################################################
#Percentage of Population that is Unemployed
#Calculated as 
#Percentage unemployed male (DIS$B18101_004E+DIS$B18101_007E+DIS$B18101_010E+DIS$B18101_013E+DIS$B18101_016E)
#Percentage unemployed female (DIS$B18101_023E+DIS$B18101_026E+DIS$B18101_029E+DIS$B18101_032E+DIS$B18101_035E)
#Percentage total unemployed (B23025_005E16/B23025_002E)*100 

UNEvar<-c(
  #Total in the labor force 16 and over
  "B23025_002E",
  #Unemployed 16 and over in the civilian labor force 
  "B23025_005E")

UNE<-get_acs(geography="county", 
             state="VA", 
             variables=UNEvar, #Percentage 16 Years and Over Unemployed
             year=2020, 
             survey="acs5", 
             output="wide", 
             geometry=TRUE,
             cache_table = TRUE) 
#View(UNE); dim(UNE)

#Percentage of the population 16 years and older unemployed
UNE$PUNE<-round((UNE$B23025_005E/UNE$B23025_002E)*100, 1)

#############################################################################
#Percentage of Households with no vehicle
#Calculated as 
#Percentage of households with no vehicle (B08201_002E/B08201_001E)*100 

NVEHvar<-c(
  #Total number of households
  "B08201_001E",
  #1 person household with no vehicle
  "B08201_008E",
  #2 person household with no vehicle
  "B08201_014E",
  #3 person household with no vehicle
  "B08201_020E",
  #4 person household with no vehicle
  "B08201_026E",
  #Households with no vehicle
  "B08201_002E")

NoVE<-get_acs(geography="county", 
              state="VA", 
              variables=NVEHvar, #Number of vehicles per household
              year=2020, 
              survey="acs5", 
              output="wide", 
              geometry=TRUE,
              cache_table = TRUE) 
#View(NoVE); dim(NoVE)

#Percentage of households with no vehicle
NoVE$PNoVE<-round((NoVE$B08201_002E/NoVE$B08201_001E)*100, 1)


#############################################################################
#Gini Index of inequality
#1=perfect inequality / 0=perfect equality

GINIvar<-c(
  #Gini Index
  "B19083_001E")

GINI<-get_acs(geography="county", 
              state="VA", 
              variables=GINIvar, #Income inequality
              year=2020, 
              survey="acs5", 
              output="wide", 
              geometry=TRUE,
              cache_table=TRUE) 
#View(GINI); dim(GINI)

#1=perfect inequality / 0=perfect equality
#Change to 100 perfect equality / 0 perfect inequality 
GINI$GINI <-GINI$B19083_001E
#Calculate the Reverse Gini
      A<-0.5*GINI$GINI
      B<-0.5-A
REVGINI<-(B/(B+A))

#Used to merge with derived variables
        RESco<-NoVE[,-c(3:14,16,17)]
   RESco$PNHS<-100-NHS$PNS 
  RESco$PNoVE<-100-NoVE$PNoVE
   RESco$PUNE<-100-UNE$PUNE
   RESco$PDIS<-100-DIS$PDIS 
   RESco$GINI<-GINI$B19083_001E
RESco$REVGINI<-100*REVGINI
RESco$RES3<-(RESco$PNHS+RESco$PNoVE+RESco$PUNE+RESco$PDIS+RESco$REVGINI)/500
View(RESco)

#Use a Jenk Breaks
BREAKS3_RESco<-getJenksBreaks(RESco$RES3, 8, subset = NULL); BREAKS3_RESco

RESco$group3<-rep(NA, length=dim(RESco)[1])
RESco$group3[which(RESco$RES3>=BREAKS3_RESco[1] & RESco$RES3<BREAKS3_RESco[2])]="[0.76202, 0.78648)"
RESco$group3[which(RESco$RES3>=BREAKS3_RESco[2] & RESco$RES3<BREAKS3_RESco[3])]="[0.78648, 0.81032)"
RESco$group3[which(RESco$RES3>=BREAKS3_RESco[3] & RESco$RES3<BREAKS3_RESco[4])]="[0.81032, 0.82716)"
RESco$group3[which(RESco$RES3>=BREAKS3_RESco[4] & RESco$RES3<BREAKS3_RESco[5])]="[0.82716, 0.84098)"
RESco$group3[which(RESco$RES3>=BREAKS3_RESco[5] & RESco$RES3<BREAKS3_RESco[6])]="[0.84098, 0.85606)"
RESco$group3[which(RESco$RES3>=BREAKS3_RESco[6] & RESco$RES3<BREAKS3_RESco[7])]="[0.85606, 0.87118)"
RESco$group3[which(RESco$RES3>=BREAKS3_RESco[7])]="[0.87118, 0.89112]"
RESco$group3<-factor(RESco$group3, 
                   levels=c("[0.76202, 0.78648)","[0.78648, 0.81032)","[0.81032, 0.82716)",
                            "[0.82716, 0.84098)","[0.84098, 0.85606)","[0.85606, 0.87118)",
                            "[0.87118, 0.89112]"))

write.csv(RESco, "~/Documents/Counting People/Data/Derived Variables/va_county_population_resilience.csv", row.names=FALSE)


#plot the resilience index by count
RESco3palette<-c("[0.76202, 0.78648)"="#F9F1CB", "[0.78648, 0.81032)"="#DFDAB3",
               "[0.81032, 0.82716)"="#C2C097", "[0.82716, 0.84098)"="#A5A67B",
               "[0.84098, 0.85606)"="#888C5E", "[0.85606, 0.87118)"="#6B7242",
               "[0.87118, 0.89112]"="#4E5827")

RESco3_CI_KE_plot<-ggplot() + 
  geom_sf(data=RESco, aes(fill=group3), color="grey") + 
  #  geom_sf(fill="transparent", color="#B2521A", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==-1,]) +
  #  geom_sf(fill="transparent", color="#31596E", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==1,]) +
  scale_fill_manual(values=RESco3palette, 
                    name="Population Resiliency Composite Index\n(0=Least Resilient, 1=Most Resilient)",
                    drop=FALSE, 
                    labels=c("0.762     0.786","0.810","0.827","0.841","0.856","0.871","0.891"),
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
  labs(title="      2020 Population Resilience Composite Index by Census Tract", 
       subtitle="        Fraction Employed, No Disability, High School Diploma or Greater,\n              Households with at Least One Vehicle, and Reverse Gini Index",
       caption="ACS 2020 5-Year Estimates Detailed Tables (B18101, B19083, B23025, S0101, S1501)\n    Fire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") +
  theme(panel.background=element_rect(fill="transparent"),
        text=element_text(color="#22211d"),
        plot.title=element_text(size=18, face="bold", hjust=0.1, vjust=-22.0),
        plot.subtitle=element_text(size=15, hjust=0.1, vjust=-27.0),
        plot.caption=element_text(size=10, hjust=0.1),
        legend.title=element_text(size=15),
        legend.text=element_text(size=13),
        legend.position="bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  annotate("text", x=-81.80000, y=38.30989, adj=0, size=5.0, label="Most Deficient") +
  annotate("point", x=-81.90000, y=38.30989, colour="black", size=4.25) + 
  annotate("point", x=-81.90000, y=38.30989, colour="#E69F00", size=3.75) +
  annotate("point", x=-81.90000, y=38.20989, colour="black", size=4.25) + 
  annotate("point", x=-81.90000, y=38.20989, colour="#0072B2", size=3.75) +
  annotate("text", x=-81.80000, y=38.10989, adj=0, size=5.0, label="Least Deficient") + 
  annotate("point", x=-81.90000, y=38.10989, colour="black", size=4.25) + 
  annotate("point", x=-81.90000, y=38.10989, colour="#56B4E9", size=3.75) +
  annotate("text", x=-81.80000, y=38.00989, adj=0, size=5.0, label="No Deficiencies") +
  annotate("point", x=-81.90000, y=38.00989, colour="black", size=4.25) + 
  annotate("point", x=-81.90000, y=38.00989, colour="#999999", size=3.75) +
  annotate("text",  x=-82.850000, y=38.60000, adj=0.12, size=5.75, fontface="bold", label="Skilled Nursing Facility Fire Safety & Emergency\n               Preparedness Deficiency Index") 
#This will save the map in your session directory
ggsave("RESco3_CI_KE_plot.pdf", width=14, height=10)
RESco3_CI_KE_plot


