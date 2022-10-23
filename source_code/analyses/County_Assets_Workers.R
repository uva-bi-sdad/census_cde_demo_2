#County level disaster evacuation assets
library(stringr)
library(ggrepel)
library(BAMMtools)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(sf)
library(scales)
options(tigris_use_cache=TRUE)

#Copy the url of the raw data
url1 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/community/resilience/va_hrsa_ahrf_2021.csv"

#Read in the Area Resource File
#Keep location variables and the health professional shortage categories
AHRF<-read.csv(url1); dim(AHRF); names(AHRF)
  AHRFx<-AHRF[,c(1,11,12,47)]
  names(AHRFx)<-c("CNTY","FIPst","FIPco","HPSA")
  AHRFx$FIPco<-str_pad(AHRFx$FIPco, 3, pad="0")
  AHRFx$GEOID<-paste0("51", AHRFx$FIPco)
  AHRFx$HPSA<-ifelse(AHRFx$HPSA==0, 3, AHRFx$HPSA)
  AHRFx$GEOID<-as.factor(AHRFx$GEOID) 
  AHRFx$HPSA<-as.factor(AHRFx$HPSA) 
#Counties/cities in persisent poverty  
  AHRFx$PERPOV<-ifelse((AHRFx$GEOID=="51027" | AHRFx$GEOID=="51105" | AHRFx$GEOID=="51195" | AHRFx$GEOID=="51660" |
                        AHRFx$GEOID=="51720" | AHRFx$GEOID=="51730" | AHRFx$GEOID=="51750"), 1, 0)
  sum(AHRFx$PERPOV)
  AHRFx$PERPOV<-as.factor(AHRFx$PERPOV)

#census_api_key("95313390ff9a8c4afc2c080ea4d907a18a29f727")
  
#V2020<-load_variables(2020, "acs5", cache=TRUE)
#View(V2020)
  
#Set the color palette for 7 categories
SEQ<-c("#F9F1CB","#4E5827")
SEQolive<-colorRampPalette(SEQ)(42)
show_col(SEQolive)
show_col(SEQolive[c(1,7,14,21,28,35,42)])
SEQ_OLIVE<-SEQolive[c(1,7,14,21,28,35,42)]; show_col(SEQ_OLIVE)
  
url12 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/processes/derived_variables/va_snf_deficiency_indices_k_e.csv"
#Read in the Deficiency Index file
DE<-read.csv(url12); dim(DE); View(DE)
DE$KE_DEF_IND<-round(DE$E_DEF_IND+DE$K_DEF_IND, 1)

#Use a Jenk Breaks
BREAKS_KE<-getJenksBreaks(DE$KE_DEF_IND[DE$KE_DEF_IND>0], 4, subset = NULL); BREAKS_KE
  
DE$group_KE<-rep(NA, length=dim(DE)[1])
DE$group_KE[which(DE$KE_DEF_IND==0)]="[0]"
DE$group_KE[which(DE$KE_DEF_IND>=BREAKS_KE[1] & DE$KE_DEF_IND<BREAKS_KE[2])]="[19.7, 86.1)"
DE$group_KE[which(DE$KE_DEF_IND>=BREAKS_KE[2] & DE$KE_DEF_IND<BREAKS_KE[3])]="[86.1, 157.5)"
DE$group_KE[which(DE$KE_DEF_IND>=BREAKS_KE[3])]="[157.5, 314.0]"
DE$group_KE<-factor(DE$group_KE, 
                    levels=c("[0]","[19.7, 86.1)","[86.1, 157.5)","[157.5, 314.0]"))

#Use the Census API to download the geometry variables
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

#Merge the geometry variable with the health professional shortage desgination
#from HRSA/ARF
GEOMETRY<-ADR[,c(1,9)]; dim(GEOMETRY)
HPSA<-merge(GEOMETRY, AHRFx, by="GEOID", all=TRUE); dim(HPSA); View(HPSA)

HPSApalette<-c("1"="#F9F1CB", "2"="#A5A67B", "3"="#4E5827", "NA"="grey")

#############################################################################
#Display the HPSA using a choropleth
HPSA_KE_plot<-ggplot() + 
  geom_sf(data=HPSA, aes(fill=factor(HPSA)), color="grey") + 
#  geom_sf(fill="transparent", color="black", size=0.7, 
#          data=temp[HPSA$PERPOV=="1",]) +
  scale_fill_manual(values=HPSApalette, 
                    name="Health Professional Shortage Area\n(Entire=shortage in the entire area)",
                    drop=FALSE, 
                    labels=c("Entire","Part","None"),
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
  labs(title="2021 Health Professional Shortage Area by\n         County and Independent City", 
       subtitle=paste0("M.D. and D.O. providing direct patient care who practice principally in\n  one of the four primary care specialties-general or family practice."), 
       caption="Health Resources and Services Administration Area Health Resources File 2021\n      Fire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services") +
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
  annotate("text",  x=-82.850000, y=38.60000, adj=0.05, size=5.75, fontface="bold", label="Skilled Nursing Facility Fire Safety & Emergency\n         Preparedness Deficiency Index") 
#ggsave("HPSA_KE_plot.pdf", width=14, height=10)
HPSA_KE_plot

#write.csv(HPSA, "~/Documents/Counting People/Data/HPSA.csv", row.names=FALSE)  