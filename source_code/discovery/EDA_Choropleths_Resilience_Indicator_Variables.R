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

#census_api_key("95313390ff9a8c4afc2c080ea4d907a18a29f727")

V2020<-load_variables(2020, "acs5", cache=TRUE)
View(V2020)

#Set the color palette for 7 categories
SEQ<-c("#F9F1CB","#4E5827")
SEQolive<-colorRampPalette(SEQ)(42)
show_col(SEQolive)
show_col(SEQolive[c(1,7,14,21,28,35,42)])
SEQ_OLIVE<-SEQolive[c(1,7,14,21,28,35,42)]; show_col(SEQ_OLIVE)

url12 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/processes/derived_variables/va_snf_deficiency_indices_k_e.csv"

#Read in the Deficiency Index file
DEE<-read.csv(url12); dim(DEE); View(DEE); 
DEE$E_DEF_IND<-round(DEE$E_DEF_IND, 1)
#Use a Jenk Breaks
BREAKS_E<-getJenksBreaks(DEE$E_DEF_IND[DEE$E_DEF_IND>0], 4, subset = NULL); BREAKS_E

DEE$group_E<-rep(NA, length=dim(DEE)[1])
DEE$group_E[which(DEE$E_DEF_IND==0)]="[0]"
DEE$group_E[which(DEE$E_DEF_IND>=BREAKS_E[1] & DEE$E_DEF_IND<BREAKS_E[2])]="[19.0, 41.3)"
DEE$group_E[which(DEE$E_DEF_IND>=BREAKS_E[2] & DEE$E_DEF_IND<BREAKS_E[3])]="[41.3, 65.7)"
DEE$group_E[which(DEE$E_DEF_IND>=BREAKS_E[3])]="[65.7, 129.8]"
DEE$group_E<-factor(DEE$group_E, 
                    levels=c("[0]","[19.0, 41.3)","[41.3, 65.7)","[65.7, 129.8]"))

#############################################################################
#Age Dependency Ratios by County
ADRvars<-c(
  #Age Dependency Ratio
  "S0101_C01_034E",
  #Old-Age (>64)
  "S0101_C01_035E",
  #Child (<18)
  "S0101_C01_036E")

ADR<-get_acs(geography="county", 
             state="VA", 
             variables=ADRvars, #Age dependency variables
             year=2020, 
             survey="acs5", 
             output="wide", 
             geometry=TRUE,
             cache_table = TRUE) 
View(ADR); dim(ADR)

#Used to merge with derived variables
GEOMETRY<-ADR[,c(1,9)]

#Use a Jenk Breaks
BREAKS_ADR<-getJenksBreaks(ADR$S0101_C01_035E, 8, subset = NULL)

ADR$group<-rep(NA, length=dim(ADR)[1])
ADR$group[which(ADR$S0101_C01_035E>=BREAKS_ADR[1] & ADR$S0101_C01_035E<BREAKS[2])]="[11.0, 18.9)"
ADR$group[which(ADR$S0101_C01_035E>=BREAKS_ADR[2] & ADR$S0101_C01_035E<BREAKS[3])]="[18.9, 25.6)"
ADR$group[which(ADR$S0101_C01_035E>=BREAKS_ADR[3] & ADR$S0101_C01_035E<BREAKS[4])]="[25.6, 32.2)"
ADR$group[which(ADR$S0101_C01_035E>=BREAKS_ADR[4] & ADR$S0101_C01_035E<BREAKS[5])]="[32.2, 40.0)"
ADR$group[which(ADR$S0101_C01_035E>=BREAKS_ADR[5] & ADR$S0101_C01_035E<BREAKS[6])]="[40.0, 49.9)"
ADR$group[which(ADR$S0101_C01_035E>=BREAKS_ADR[6] & ADR$S0101_C01_035E<BREAKS[7])]="[49.9, 61.8)"
ADR$group[which(ADR$S0101_C01_035E>=BREAKS_ADR[7])]="[61.8, 82.2]"
ADR$group<-factor(ADR$group, 
                  levels=c("[11.0, 18.9)","[18.9, 25.6)","[25.6, 32.2)","[32.2, 40.0)",
                           "[40.0, 49.9)","[49.9, 61.8)","[61.8, 82.2]"))

ADRpalette<-c("[11.0, 18.9)"="#F9F1CB", "[18.9, 25.6)"="#DFDAB3",
              "[25.6, 32.2)"="#C2C097", "[32.2, 40.0)"="#A5A67B",
              "[40.0, 49.9)"="#888C5E", "[49.9, 61.8)"="#6B7242",
              "[61.8, 82.2]"="#4E5827")

ggplot() + 
  geom_sf(data=ADR, aes(fill=factor(group)), color="grey") + 
  #  geom_sf(fill="transparent", color="#B2521A", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==-1,]) +
  #  geom_sf(fill="transparent", color="#31596E", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==1,]) +
  scale_fill_manual(values=ADRpalette, 
                    name="Old Age Dependency Ratio",
                    drop=FALSE, 
                    labels=c("11.0           18.9","25.6","32.2","40.1","49.9","61.8","82.2"),
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
  geom_point(data=DEE[DEE$group_E=="[0]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[0]", ], aes(x=long, y=lat), colour="#999999", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[19.0, 41.3)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[19.0, 41.3)", ], aes(x=long, y=lat), colour="#56B4E9", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[41.3, 65.7)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[41.3, 65.7)", ], aes(x=long, y=lat), colour="#0072B2", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[65.7, 129.8]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[65.7, 129.8]", ], aes(x=long, y=lat), colour="#E69F00", size=3.0) +
  labs(title="  2020 Old Age Dependency Ratio by\n       County and Independent City", 
       subtitle=paste0("Ratio of (", intToUtf8(8805),"65) to the working age population (18-64)"), 
       caption="S0101 AGE AND SEX (2020): American Community Survey 5-Year Estimates Subject Tables\n    Fire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") +
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

#############################################################################


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

NHS<-get_acs(geography="tract", 
             state="VA", 
             variables=NHSvars, #No high school education
             year=2020, 
             survey="acs5", 
             output="wide", 
             geometry=TRUE,
             cache_table = TRUE) 
View(NHS); dim(NHS)

#Percent of the population 18 years and older with no HS diploma
NHS$PNS<-round(((NHS$S1501_C01_002E+NHS$S1501_C01_007E+NHS$S1501_C01_008E)/(NHS$S1501_C01_001E+NHS$S1501_C01_006E))*100, 1)

#Use a Jenk Breaks
BREAKS_NHS<-getJenksBreaks(NHS$PNS, 8, subset = NULL)

NHS$group<-rep(NA, length=dim(NHS)[1])
NHS$group[which(NHS$PNS>=BREAKS_NHS[1] & NHS$PNS<BREAKS_NHS[2])]="[0.0, 4.0)"
NHS$group[which(NHS$PNS>=BREAKS_NHS[2] & NHS$PNS<BREAKS_NHS[3])]="[4.0, 7.8)"
NHS$group[which(NHS$PNS>=BREAKS_NHS[3] & NHS$PNS<BREAKS_NHS[4])]="[7.8, 11.8)"
NHS$group[which(NHS$PNS>=BREAKS_NHS[4] & NHS$PNS<BREAKS_NHS[5])]="[11.8, 16.1)"
NHS$group[which(NHS$PNS>=BREAKS_NHS[5] & NHS$PNS<BREAKS_NHS[6])]="[16.1, 21.5)"
NHS$group[which(NHS$PNS>=BREAKS_NHS[6] & NHS$PNS<BREAKS_NHS[7])]="[21.5, 30.0)"
NHS$group[which(NHS$PNS>=BREAKS_NHS[7])]="[30.0, 56.5]"
NHS$group<-factor(NHS$group, 
                  levels=c("[0.0, 4.0)","[4.0, 7.8)","[7.8, 11.8)","[11.8, 16.1)",
                           "[16.1, 21.5)","[21.5, 30.0)","[30.0, 56.5]"))

NHSpalette<-c("[0.0, 4.0)"="#F9F1CB", "[4.0, 7.8)"="#DFDAB3",
              "[7.8, 11.8)"="#C2C097", "[11.8, 16.1)"="#A5A67B",
              "[16.1, 21.5)"="#888C5E", "[21.5, 30.0)"="#6B7242",
              "[30.0, 56.5]"="#4E5827")
ggplot() + 
  geom_sf(data=NHS, aes(fill=group), color="grey") + 
  #  geom_sf(fill="transparent", color="#B2521A", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==-1,]) +
  #  geom_sf(fill="transparent", color="#31596E", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==1,]) +
  scale_fill_manual(values=NHSpalette, 
                    name="Percentage with No High School Diploma",
                    drop=FALSE, 
                    labels=c("0.0           4.0","7.8","11.8","16.1","21.5","30.0","56.5"),
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
  geom_point(data=DEE[DEE$group_E=="[0]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[0]", ], aes(x=long, y=lat), colour="#999999", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[19.0, 41.3)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[19.0, 41.3)", ], aes(x=long, y=lat), colour="#56B4E9", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[41.3, 65.7)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[41.3, 65.7)", ], aes(x=long, y=lat), colour="#0072B2", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[65.7, 129.8]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[65.7, 129.8]", ], aes(x=long, y=lat), colour="#E69F00", size=3.0) +
  labs(title=" 2020 Percentage with No High School Diploma by Census Tract", 
       subtitle="", 
       caption="S1501 EDUCATIONAL ATTAINMENT (2020): ACS 5-Year Estimates Subject Tables\n    Fire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") +
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

#############################################################################


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

DIS<-get_acs(geography="tract", 
             state="VA", 
             variables=DISvar, #Percentage with a disability
             year=2020, 
             survey="acs5", 
             output="wide", 
             geometry=TRUE,
             cache_table = TRUE) 
View(DIS); dim(DIS)

#Percentage of the population 1with any disability
  DIS$MALE<-(DIS$B18101_004E+DIS$B18101_007E+DIS$B18101_010E+DIS$B18101_013E+DIS$B18101_016E)
DIS$FEMALE<-(DIS$B18101_023E+DIS$B18101_026E+DIS$B18101_029E+DIS$B18101_032E+DIS$B18101_035E)
  DIS$PDIS<-round(((DIS$MALE+DIS$FEMALE)/DIS$B18101_001E)*100, 1)
DIS$PDIS[which(is.na(DIS$PDIS))]<-0

#Use a Jenk Breaks
BREAKS_DIS<-getJenksBreaks(DIS$PDIS, 8, subset = NULL)

DIS$group<-rep(NA, length=dim(DIS)[1])
DIS$group[which(DIS$PDIS>=BREAKS_DIS[1] & DIS$PDIS<BREAKS_DIS[2])]="[0.0, 3.8)"
DIS$group[which(DIS$PDIS>=BREAKS_DIS[2] & DIS$PDIS<BREAKS_DIS[3])]="[3.8, 6.7)"
DIS$group[which(DIS$PDIS>=BREAKS_DIS[3] & DIS$PDIS<BREAKS_DIS[4])]="[6.7, 9.6)"
DIS$group[which(DIS$PDIS>=BREAKS_DIS[4] & DIS$PDIS<BREAKS_DIS[5])]="[9.6, 13.0)"
DIS$group[which(DIS$PDIS>=BREAKS_DIS[5] & DIS$PDIS<BREAKS_DIS[6])]="[13.0, 17.4)"
DIS$group[which(DIS$PDIS>=BREAKS_DIS[6] & DIS$PDIS<BREAKS_DIS[7])]="[17.4, 23.5)"
DIS$group[which(DIS$PDIS>=BREAKS_DIS[7])]="[23.5, 34.3]"
DIS$group<-factor(DIS$group, 
                  levels=c("[0.0, 3.8)","[3.8, 6.7)","[6.7, 9.6)","[9.6, 13.0)",
                           "[13.0, 17.4)","[17.4, 23.5)","[23.5, 34.3]"))

DISpalette<-c("[0.0, 3.8)"="#F9F1CB", "[3.8, 6.7)"="#DFDAB3",
              "[6.7, 9.6)"="#C2C097", "[9.6, 13.0)"="#A5A67B",
              "[13.0, 17.4)"="#888C5E", "[17.4, 23.5)"="#6B7242",
              "[23.5, 34.3]"="#4E5827")
ggplot() + 
  geom_sf(data=DIS, aes(fill=group), color="grey") + 
  #  geom_sf(fill="transparent", color="#B2521A", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==-1,]) +
  #  geom_sf(fill="transparent", color="#31596E", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==1,]) +
  scale_fill_manual(values=DISpalette, 
                    name="Percentage with Any Disability",
                    drop=FALSE, 
                    labels=c("0.0           4.4","7.0","9.8","13.1","17.5","23.5","34.3"),
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
  geom_point(data=DEE[DEE$group_E=="[0]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[0]", ], aes(x=long, y=lat), colour="#999999", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[19.0, 41.3)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[19.0, 41.3)", ], aes(x=long, y=lat), colour="#56B4E9", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[41.3, 65.7)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[41.3, 65.7)", ], aes(x=long, y=lat), colour="#0072B2", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[65.7, 129.8]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[65.7, 129.8]", ], aes(x=long, y=lat), colour="#E69F00", size=3.0) +
  labs(title=" 2020 Percentage of the Population with Any Disability by Census Tract", 
       subtitle="", 
       caption="B18101 SEX BY AGE BY DISABILITY STATUS (2020): ACS 5-Year Estimates Detailed Tables\n    Fire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") +
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
#############################################################################



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

UNE<-get_acs(geography="tract", 
             state="VA", 
             variables=UNEvar, #Percentage 16 Years and Over Unemployed
             year=2020, 
             survey="acs5", 
             output="wide", 
             geometry=TRUE,
             cache_table = TRUE) 
View(UNE); dim(UNE)

#Percentage of the population 16 years and older unemployed
UNE$PUNE<-round((UNE$B23025_005E/UNE$B23025_002E)*100, 1)

#Use a Jenk Breaks
BREAKS_UNE<-getJenksBreaks(UNE$PUNE, 8, subset=NULL)

UNE$group<-rep(NA, length=dim(UNE)[1])
UNE$group[which(UNE$PUNE>=BREAKS_UNE[1] & UNE$PUNE<BREAKS_UNE[2])]="[0.0, 2.6)"
UNE$group[which(UNE$PUNE>=BREAKS_UNE[2] & UNE$PUNE<BREAKS_UNE[3])]="[2.6, 5.0)"
UNE$group[which(UNE$PUNE>=BREAKS_UNE[3] & UNE$PUNE<BREAKS_UNE[4])]="[5.0, 8.0)"
UNE$group[which(UNE$PUNE>=BREAKS_UNE[4] & UNE$PUNE<BREAKS_UNE[5])]="[8.0, 12.1)"
UNE$group[which(UNE$PUNE>=BREAKS_UNE[5] & UNE$PUNE<BREAKS_UNE[6])]="[12.1, 19.3)"
UNE$group[which(UNE$PUNE>=BREAKS_UNE[6] & UNE$PUNE<BREAKS_UNE[7])]="[19.3, 36.0)"
UNE$group[which(UNE$PUNE>=BREAKS_UNE[7])]="[36.0, 76.0]"
UNE$group<-factor(UNE$group, 
                  levels=c("[0.0, 2.6)","[2.6, 5.0)","[5.0, 8.0)","[8.0, 12.1)",
                           "[12.1, 19.3)","[19.3, 36.0)","[36.0, 76.0]"))

UNEpalette<-c("[0.0, 2.6)"="#F9F1CB", "[2.6, 5.0)"="#DFDAB3",
              "[5.0, 8.0)"="#C2C097", "[8.0, 12.1)"="#A5A67B",
              "[12.1, 19.3)"="#888C5E", "[19.3, 36.0)"="#6B7242",
              "[36.0, 76.0]"="#4E5827")
ggplot() + 
  geom_sf(data=UNE, aes(fill=group), color="grey") + 
  #  geom_sf(fill="transparent", color="#B2521A", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==-1,]) +
  #  geom_sf(fill="transparent", color="#31596E", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==1,]) +
  scale_fill_manual(values=UNEpalette, 
                    name="Percentage 16 Years and Over Unemployed",
                    drop=FALSE, 
                    labels=c("0.0           4.4","7.0","9.8","13.1","17.5","23.5","34.3"),
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
  geom_point(data=DEE[DEE$group_E=="[0]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[0]", ], aes(x=long, y=lat), colour="#999999", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[19.0, 41.3)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[19.0, 41.3)", ], aes(x=long, y=lat), colour="#56B4E9", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[41.3, 65.7)", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[41.3, 65.7)", ], aes(x=long, y=lat), colour="#0072B2", size=3.0) +
  geom_point(data=DEE[DEE$group_E=="[65.7, 129.8]", ], aes(x=long, y=lat), color="black", size=3.5) +
  geom_point(data=DEE[DEE$group_E=="[65.7, 129.8]", ], aes(x=long, y=lat), colour="#E69F00", size=3.0) +
  labs(title=" 2020 Percentage of the Population 16 and Over\n              Unemployed by Census Tract", 
       subtitle="", 
       caption="   B23025 EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (2020): ACS 5-Year Estimates Detailed Tables\nFire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") +
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


#############################################################################





