library(ggrepel)
library(BAMMtools)
library(tidyverse)
library(ggplot2)
library(plyr)
library(scales)

library(Rttf2pt1)

#These flat violin plots display the SNF deficiency index by natural hazard
#coastal, riverine, and combined flooding 


#Set the color palette for 7 categories of the deficiency index
SEQ<-c("#F9F1CB","#4E5827")
SEQolive<-colorRampPalette(SEQ)(42)
show_col(SEQolive)
show_col(SEQolive[c(1,7,14,21,28,35,42)])
SEQ_OLIVE<-SEQolive[c(1,7,14,21,28,35,42)]; show_col(SEQ_OLIVE)

url12 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/processes/derived_variables/va_snf_deficiency_indices_k_e.csv"

#Read in the Deficiency Index file
DE<-read.csv(url12); dim(DE); View(DE); 

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

RISK_plot<-data.frame(DEF=rep(DE$group_KE, 3), 
                      TYPE=rep(c("COMB","RIVER","COAST"),rep(dim(DE)[1],3)),
                      RISK=c(DE$max_flood_risks,DE$max_rfld_risks,DE$max_cfld_risks))
                       

#Calculate summary stats
#to be displayed in the plots
lb<-function(x) mean(x, na.rm=TRUE)-sd(x, na.rm=TRUE)
ub<-function(x) mean(x, na.rm=TRUE)+sd(x, na.rm=TRUE)
STATS.Risk<-ddply(RISK_plot, ~TYPE, summarise, 
                  mean=mean(RISK, na.rm=TRUE), 
                  median=median(RISK, na.rm=TRUE), 
                  lower=lb(RISK), 
                  upper=ub(RISK))
STATS.Risk


#############################################################################

RISK_PLOT_KE<- 
  ggplot(data=RISK_plot, 
         aes(x=TYPE, y=RISK), fill=DEF) +
  geom_flat_violin(position=position_nudge(x=0.2, y=0.0), trim=TRUE, 
                   alpha=0.50, fill=cbPalette[1], na.rm=TRUE) +
  geom_boxplot(width=0.2, outlier.shape=NA, alpha=0.50, colour="black", 
               fill=cbPalette[1], na.rm=TRUE) +
  geom_point(aes(y=RISK, fill=DEF), pch=21, colour="black",
             position=position_jitter(width=0.15), size=2.5, show.legend=F) +
  geom_point(data=STATS.Risk, aes(x=factor(TYPE), y=mean), 
             position=position_nudge(x=0.3), size=2.75) +
  geom_errorbar(data=STATS.Risk, aes(ymin=lower, ymax=upper, y=mean), 
                position=position_nudge(x=0.3), width=0) +
  xlab("") + ylab("Risk") + 
  expand_limits(x=1.00) +
  scale_y_continuous(limits=c(-5.0, 60.0), breaks=c(seq(from=-5.0, to=60.0, by=5.0))) +
  scale_x_discrete(labels=c("COMB"="Coastal+\nRiverine\nFlooding", "RIVER"="Risk of Riverine\nFlooding at SNF", "COAST"="Risk of Coastal\nFlooding at SNF")) +
  scale_fill_manual(values=DEpalette, 
                    labels=levels(DE$DEF),
                    drop=F) + 
  annotate("text", x=0.80, y=41.0, adj=0, size=4, label="Most Deficient") +
  annotate("point", x=0.80, y=40.0, colour="black", size=4.25) + 
  annotate("point", x=0.80, y=40.0, colour="#E69F00", size=3.75) +
  annotate("point", x=0.90, y=40.0, colour="black", size=4.25) + 
  annotate("point", x=0.90, y=40.0, colour="#0072B2", size=3.75) +
  annotate("text", x=1.00, y=41.0, adj=0, size=4, label="Least Deficient") +
  annotate("point", x=1.00, y=40.0, colour="black", size=4.25) + 
  annotate("point", x=1.00, y=40.0, colour="#56B4E9", size=3.75) +
  annotate("text", x=1.10, y=41.0, adj=0, size=4, label="No Deficiencies") +
  annotate("point", x=1.10, y=40.0, colour="black", size=4.25) + 
  annotate("point", x=1.10, y=40.0, colour="#999999", size=3.75) +
  annotate("text", x=1.25, y=39.5, adj=0, size=5, label="Deficiency Index") +
  coord_flip() + 
  theme_SDAD() +
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        plot.title=element_text(size=20, face="bold", hjust=0),
        plot.subtitle=element_text(size=15, hjust=0),
        plot.caption=element_text(size=10, hjust=0)) +
  labs(title="Coastal and Riverine Flooding Risk for each Skilled Nursing Facility", 
       subtitle=paste("Black Circle = Mean; Black Horizontal Line = Mean","\u00B1","Std. Dev."),
       caption="FEMA National Risk Index (Nov 2021)\nFire Safety Deficiencies and Inspection Dates (01/2018-03/2022): The Centers for Medicare & Medicaid Services")
#ggsave("RISK_PLOT_KE.pdf", width=13.88, height=7.29)
RISK_PLOT_KE



