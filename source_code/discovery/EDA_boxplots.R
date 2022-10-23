library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

#Read in the derived data from from GitHub
url1 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/processes/derived_variables/va_county_population_resilience.csv"
RES<-read.csv(url1); dim(RES); names(RES)

###########################################################################                                 
#Transpose data set to display as boxplots

IND<-as.factor(rep(c("HS","DIS","EMP","VEH","RGINI"), rep(dim(RES)[1],5))); length(IND) 
PER<-c(RES$percent_high_school, RES$percent_not_disabled, RES$percent_employed, RES$percent_vehicle, RES$reverse_gini); length(PER) 
RES.BOX<-data.frame(IND, PER); View(RES.BOX)
RES.BOX$IND<-ordered(RES.BOX$IND, levels=c("HS","DIS","EMP","VEH","RGINI"))

ggplot(RES.BOX, aes(x=as.factor(IND), y=PER)) +
       geom_boxplot(aes(x=as.factor(IND), y=PER), outlier.shape=NA, 
                    fill=cbPalette[1], colour="black") + 
       geom_jitter(aes(y=PER), width=0.20, pch=16, cex=1.75, colour="black") +
       xlab("") + ylab("Percentage") +
       scale_fill_manual(values=cbPalette) +  
       scale_y_continuous(limits=c(30, 100),
                          breaks=seq(from=30, to=100, by=5)) + 
       scale_x_discrete(labels=c("At Least\nHigh School\nDiploma",
                                 "No Disability",
                                 "Employed",
                                 "At Least\nOne Vehicle",
                                 "Reverse\nGini Index")) +
       geom_point(x="HS", y=mean(RES.BOX$PER[RES.BOX$IND=="HS"], na.rm=TRUE), fill=cbPalette[7], color="black", cex=2.5, pch=21) +
       geom_point(x="DIS", y=mean(RES.BOX$PER[RES.BOX$IND=="DIS"], na.rm=TRUE), fill=cbPalette[7], color="black", cex=2.5, pch=21) +
       geom_point(x="EMP", y=mean(RES.BOX$PER[RES.BOX$IND=="EMP"], na.rm=TRUE), fill=cbPalette[7], color="black", cex=2.5, pch=21) +
       geom_point(x="VEH", y=mean(RES.BOX$PER[RES.BOX$IND=="VEH"], na.rm=TRUE), fill=cbPalette[7], color="black", cex=2.5, pch=21) +
       geom_point(x="RGINI", y=mean(RES.BOX$PER[RES.BOX$IND=="RGINI"], na.rm=TRUE), fill=cbPalette[7], color="black", cex=2.5, pch=21) +
       theme_minimal() + 
       labs(title="Virginia County Resilience Indicators", 
            subtitle="Virginia Average Percentage: orange circle; County Percentage: black circle", 
            caption="ACS 2020 5-Year Estimates Detailed Tables (B18101, B19083, B23025, S0101, S1501)") +
       theme(plot.title=element_text(size=16, face="bold", hjust=-0.0, vjust=-5.0),
             plot.subtitle=element_text(size=13, hjust=0.0, vjust=-5.5),
             axis.text.x=element_text(size=14), 
             axis.text.y=element_text(size=14, hjust=1.5), 
             axis.title.y=element_text(size=13), 
             legend.position="none")


##################################################################################
