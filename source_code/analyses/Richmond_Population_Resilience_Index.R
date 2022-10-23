library(ggrepel)
library(BAMMtools)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(sf)
library(scales)
library(Rttf2pt1)
options(tigris_use_cache=TRUE)

#census_api_key("95313390ff9a8c4afc2c080ea4d907a18a29f727")

#V2020<-load_variables(2020, "acs5", cache=TRUE)
#View(V2020)

#Set the color palette for 7 categories
SEQ<-c("#F9F1CB","#4E5827")
SEQolive<-colorRampPalette(SEQ)(42)
show_col(SEQolive)
show_col(SEQolive[c(1,7,14,21,28,35,42)])
SEQ_OLIVE<-SEQolive[c(1,7,14,21,28,35,42)]; show_col(SEQ_OLIVE)

url1<-"https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/processes/derived_variables/va_snf_deficiency_indices_k_e.csv"
url2<-"https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/processes/derived_variables/va_census_tract_population_resilience.csv"

#Read in the Deficiency Index file
DE<-read.csv(url1); dim(DE); View(DE) 

#Use a Jenk Breaks
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

DERichmond<-DE[DE$FPN %in% c("495423","495393","495327","495260","49A022","49E084"), ]
dim(DERichmond); View(DERichmond)

#########################################################################################
#GINI INDEX PLOT
#########################################################################################

GINIvar<-c(
  #Gini Index
  "B19083_001E")

GINI<-get_acs(geography="tract", 
              county=c("Richmond city"),
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
GINI$GINI<-GINI$B19083_001E
#Calculate the Reverse Gini
      A<-0.5*GINI$GINI
      B<-0.5-A
REVGINI<-(B/(B+A))
GINI$REVGINI<-REVGINI; View(GINI) 

#Use a Jenk Breaks
BREAKS_GINI<-getJenksBreaks(GINI$REVGINI, 8, subset = NULL); BREAKS_GINI

GINI$group<-rep(NA, length=dim(GINI)[1])
GINI$group[which(GINI$REVGINI>=BREAKS_GINI[1] & GINI$REVGINI<BREAKS_GINI[2])]="[0.2956, 0.3607)"
GINI$group[which(GINI$REVGINI>=BREAKS_GINI[2] & GINI$REVGINI<BREAKS_GINI[3])]="[0.3607, 0.4449)"
GINI$group[which(GINI$REVGINI>=BREAKS_GINI[3] & GINI$REVGINI<BREAKS_GINI[4])]="[0.4449, 0.4901)"
GINI$group[which(GINI$REVGINI>=BREAKS_GINI[4] & GINI$REVGINI<BREAKS_GINI[5])]="[0.4901, 0.5312)"
GINI$group[which(GINI$REVGINI>=BREAKS_GINI[5] & GINI$REVGINI<BREAKS_GINI[6])]="[0.5312, 0.5662)"
GINI$group[which(GINI$REVGINI>=BREAKS_GINI[6] & GINI$REVGINI<BREAKS_GINI[7])]="[0.5662, 0.6006)"
GINI$group[which(GINI$REVGINI>=BREAKS_GINI[7])]="[0.6006, 0.6581]"

GINI$group<-factor(GINI$group, 
                   levels=c("[0.2956, 0.3607)","[0.3607, 0.4449)","[0.4449, 0.4901)",
                            "[0.4901, 0.5312)","[0.5312, 0.5662)","[0.5662, 0.6006)",
                            "[0.6006, 0.6581]"))

GINIpalette<-c("[0.2956, 0.3607)"="#F9F1CB", "[0.3607, 0.4449)"="#DFDAB3",
               "[0.4449, 0.4901)"="#C2C097", "[0.4901, 0.5312)"="#A5A67B",
               "[0.5312, 0.5662)"="#888C5E", "[0.5662, 0.6006)"="#6B7242",
               "[0.6006, 0.6581]"="#4E5827")

RICHMOND_GINI_plot<-ggplot() + 
  geom_sf(data=GINI, aes(fill=group), color="grey") + 
  #  geom_sf(fill="transparent", color="#B2521A", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==-1,]) +
  #  geom_sf(fill="transparent", color="#31596E", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==1,]) +
  scale_fill_manual(values=GINIpalette, 
                    name="Reverse Gini Index\n(0=Perfect Inequality, 1=Perfect Equality)",
                    drop=FALSE, 
                    labels=c("0.2956     0.3607","0.4449","0.4901","0.5312","0.5662","0.6006","0.6581"),
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
  geom_point(data=DERichmond[DERichmond$group_KE=="[0]", ], aes(x=long, y=lat), color="black", size=5.5) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[0]", ], aes(x=long, y=lat), colour="#999999", size=5.0) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[19.7, 86.1)", ], aes(x=long, y=lat), color="black", size=5.5) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[19.7, 86.1)", ], aes(x=long, y=lat), colour="#56B4E9", size=5.0) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[86.1, 157.5)", ], aes(x=long, y=lat), color="black", size=5.5) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[86.1, 157.5)", ], aes(x=long, y=lat), colour="#0072B2", size=5.0) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[157.5, 314.0]", ], aes(x=long, y=lat), color="black", size=5.5) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[157.5, 314.0]", ], aes(x=long, y=lat), colour="#E69F00", size=5.0) +
  labs(title="2020 Richmond Reverse Gini Index by Census Tract", 
       subtitle="",
       caption="ACS 2020 5-Year Estimates Detailed Table B19083\n  Fire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") +
  theme(panel.background=element_rect(fill="transparent"),
        text=element_text(color="#22211d"),
        plot.title=element_text(size=18, face="bold", hjust=0.6, vjust=-5),
        plot.subtitle=element_text(size=15, hjust=0.5, vjust=-9.0),
        plot.caption=element_text(size=10, hjust=0),
        legend.title=element_text(size=15),
        legend.text=element_text(size=13),
        legend.position="bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  annotate("text", x=-77.590, y=37.510, adj=0, size=5.0, label="Most Deficient") + 
  annotate("point", x=-77.595, y=37.510, colour="black", size=5.25) + 
  annotate("point", x=-77.595, y=37.510, colour="#E69F00", size=4.75) +  
  annotate("point", x=-77.595, y=37.505, colour="black", size=5.25) + 
  annotate("point", x=-77.595, y=37.505, colour="#0072B2", size=4.75) +  
  annotate("text", x=-77.590, y=37.500, adj=0, size=5.0, label="Least Deficient") + 
  annotate("point", x=-77.595, y=37.500, colour="black", size=5.25) + 
  annotate("point", x=-77.595, y=37.500, colour="#56B4E9", size=4.75) +
  annotate("text", x=-77.590, y=37.495, adj=0, size=5.0, label="No Deficiencies") +
  annotate("point", x=-77.595, y=37.495, colour="black", size=5.25) + 
  annotate("point", x=-77.595, y=37.495, colour="#999999", size=4.75) +
  annotate("text",  x=-77.610, y=37.520, adj=0.12, size=5.0, fontface="bold", label="  Fire Safety & Emergency\nPreparedness Deficiency Index") 
ggsave("RICHMOND_GINI_plot.pdf", width=14, height=10)
RICHMOND_GINI_plot


#########################################################################################
#Resilience Composite Index PLOT
#########################################################################################
RESct<-read.csv(url2); dim(GINI); dim(RESct) 
  names(RESct); names(GINI); names(RESct)[1]<-"GEOID" 
RESctred<-RESct[,c(1,9)] 
RES<-merge(GINI, RESctred, by="GEOID", all.x=TRUE, all.y=FALSE); dim(RES)

RICHMOND_RES_plot<-ggplot() + 
  geom_sf(data=RES, aes(fill=group3), color="grey") + 
  #  geom_sf(fill="transparent", color="#B2521A", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==-1,]) +
  #  geom_sf(fill="transparent", color="#31596E", size=0.7, 
  #          data=VIRGINIA[VIRGINIA$Conclusion==1,]) +
  scale_fill_manual(values=RESct3palette, 
                    name="Population Resiliency Composite Index\n(0=Least Resilient, 1=Most Resilient)",
                    drop=FALSE, 
                    labels=c("0.61132     0.72164","0.78462","0.82152","0.5312","0.87426","0.89712","0.95796"),
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
  geom_point(data=DERichmond[DERichmond$group_KE=="[0]", ], aes(x=long, y=lat), color="black", size=5.5) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[0]", ], aes(x=long, y=lat), colour="#999999", size=5.0) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[19.7, 86.1)", ], aes(x=long, y=lat), color="black", size=5.5) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[19.7, 86.1)", ], aes(x=long, y=lat), colour="#56B4E9", size=5.0) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[86.1, 157.5)", ], aes(x=long, y=lat), color="black", size=5.5) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[86.1, 157.5)", ], aes(x=long, y=lat), colour="#0072B2", size=5.0) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[157.5, 314.0]", ], aes(x=long, y=lat), color="black", size=5.5) +
  geom_point(data=DERichmond[DERichmond$group_KE=="[157.5, 314.0]", ], aes(x=long, y=lat), colour="#E69F00", size=5.0) +
  labs(title="2020 Richmond Resiliance Composite Index by Census Tract", 
       subtitle="Fraction Employed, No Disability, High School Diploma or Greater,\nHouseholds with at Least One Vehicle, and Reverse Gini Index",
       caption=" ACS 2020 5−Year Estimates Detailed Tables (B18101, B19083, B23025, S0101, S1501)\n  Fire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") +
  theme(panel.background=element_rect(fill="transparent"),
        text=element_text(color="#22211d"),
        plot.title=element_text(size=18, face="bold", hjust=0.6, vjust=-3),
        plot.subtitle=element_text(size=15, hjust=0.5, vjust=-5.0),
        plot.caption=element_text(size=10, hjust=0),
        legend.title=element_text(size=15),
        legend.text=element_text(size=13),
        legend.position="bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  annotate("text", x=-77.595, y=37.510, adj=0, size=5.0, label="Most Deficient") + 
  annotate("point", x=-77.600, y=37.510, colour="black", size=5.25) + 
  annotate("point", x=-77.600, y=37.510, colour="#E69F00", size=4.75) +  
  annotate("point", x=-77.600, y=37.505, colour="black", size=5.25) + 
  annotate("point", x=-77.600, y=37.505, colour="#0072B2", size=4.75) +  
  annotate("text", x=-77.595, y=37.500, adj=0, size=5.0, label="Least Deficient") + 
  annotate("point", x=-77.600, y=37.500, colour="black", size=5.25) + 
  annotate("point", x=-77.600, y=37.500, colour="#56B4E9", size=4.75) +
  annotate("text", x=-77.595, y=37.495, adj=0, size=5.0, label="No Deficiencies") +
  annotate("point", x=-77.600, y=37.495, colour="black", size=5.25) + 
  annotate("point", x=-77.600, y=37.495, colour="#999999", size=4.75) +
  annotate("text",  x=-77.615, y=37.520, adj=0.12, size=5.0, fontface="bold", label="  Fire Safety & Emergency\nPreparedness Deficiency Index") +
  annotate("text",  x=-77.52570, y=37.50156, adj=0.1, size=5.0, label="495260") +
  annotate("text",  x=-77.48127, y=37.51664, adj=0.1, size=5.0, label="495327") +
  annotate("text",  x=-77.53057, y=37.53432, adj=0.1, size=5.0, label="495423") +
  annotate("text",  x=-77.46780, y=37.48907, adj=0.1, size=5.0, label="495393") +
  annotate("text",  x=-77.45081, y=37.56677, adj=0.1, size=5.0, label="49A022") +
  annotate("text",  x=-77.47134, y=37.53655, adj=0.1, size=5.0, label="49E084") 
#ggsave("RICHMOND_RES_plot.pdf", width=13.68, height=9.41)
RICHMOND_RES_plot

names(RESct)


