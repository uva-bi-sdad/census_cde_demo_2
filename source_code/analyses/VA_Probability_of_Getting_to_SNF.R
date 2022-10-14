library(dplyr)
library(broom)
library(boot)
library(ggplot2)

# Go to github repo
# Find the csv file you want to download and click it
# Click "Download" or "View raw"
# Copy the url of the raw data
url1 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/virginia_skilled_nursing_facility/facility/va_provider_final_2022-07.csv"
url2 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/snf_to_worker_flood_risk/snf_flood_risks.csv"
url3 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/documents/products/data_tables/skilled_nursing_facility/va_snf_deficiency_indices_k_e.csv"
url4 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/virginia_skilled_nursing_facility/nursing_staff/va_pbj_puf_payroll_nursing_staff_2019-Q4.csv"

#Data wrangling    
PROV<-read.csv(url1); dim(PROV)
RISK<-read.csv(url2); dim(RISK); View(RISK); names(RISK)
 DEF<-read.csv(url3); dim(DEF) 
 
#merge the SNF nursing staff and resident numbers with the routes to the SNF 
WORKERS<-PROV[,c(1:3)]
names(RISK)[2]<-"FPN"; names(RISK)
  FINAL<-merge(RISK, WORKERS, by="FPN"); dim(FINAL) 
  #remove a SNF with no nursing number
  FINAL<-FINAL[!is.na(FINAL$AVE_DailyNursingStaff), ]; dim(FINAL)
   length(unique(as.factor(FINAL$FPN)))
  FINAL<-FINAL[with(FINAL, order(FPN)), ]; View(FINAL)
   
        ID<-unique(FINAL$FPN)
         N<-length(ID)
   WORKERS<-WORKERS[with(WORKERS, order(FPN)), ]; View(WORKERS)
        SS<-WORKERS[!is.na(WORKERS$AVE_DailyNursingStaff), 3]; length(SS)
       RES<-WORKERS[!is.na(WORKERS$AVE_DailyResidents), 2]; length(RES)

#create the data frame to contain the derived variables               
EST<-data.frame(FPN=unique(FINAL$FPN), WRK=SS, RES=RES, ProbLower=rep(NA, N), ProbMean=rep(NA, N), ProbUpper=rep(NA, N))

#Monte Carlo simulation
#randomly sample the vector of route risks to a particular nursing facility
#by the number of skilled nursing staff employed at the nursing facility
#calculate the average daily nursing staff mean and the 95% nonparametric 
#confidence interval
for(i in 1:N) {
    temp1<-unique(FINAL$route_max_combined_risks[FINAL$FPN==ID[i]])
    #the vector of probabilities of making it to the 
    #nursing facility for a particular route
    temp2<-1-temp1/100
    temp3<-sample(temp2, SS[i], replace=TRUE)
    EST[i,4]<-quantile(temp2, 0.05/2)
    EST[i,5]<-mean(temp2)
    EST[i,6]<-quantile(temp2, 1-0.05/2)
}  

EST$EstLower<-round(EST$WRK*EST$ProbLower, 0)
 EST$EstMean<-round(EST$WRK*EST$ProbMean, 0)
EST$EstUpper<-round(EST$WRK*EST$ProbUpper, 0)

#order the data frame by the estimate of the mean
#number of nursing staff that will make it to the 
#nursing facility for plotting
EST<-EST[order(EST$EstMean),]
EST$Order<-1:dim(EST[1]); View(EST)
EST$PerLoss<-round((1-(EST$EstMean/EST$WRK))*100, 0); View(EST)

write.csv(EST, "~/Documents/Counting People/Data/Derived Variables/EST.csv", row.names=FALSE)

summary(EST$WRK)
#plot the estimate and confidence interval and the actual nursing staff number
ggplot(data=EST, aes(x=Order, y=WRK)) +
  geom_line(aes(x=Order, y=WRK), colour=cbPalette[7]) +
  geom_ribbon(aes(x=Order, ymin=EstLower, ymax=EstUpper), color=cbPalette[1], alpha=0.5) +
  geom_line(aes(x=Order, EstMean), color="black") +
  scale_y_continuous(breaks=seq(0, 270, by=15)) +
  scale_x_continuous(breaks=seq(1, 283, by=10),
                     labels=rep("", 29)) +
  ylab("Average Daily Nursing Staff") +
  xlab("283 Skilled Nursing Facilities") +
  labs(title="Observed and Estimated Average Daily Nursing Staff* Under an Extreme Climate Event\nby Skilled Nursing Facility", 
       subtitle=paste("Orange Line = Observed Average Daily Nursing Staff\nBlack Line = Estimated Average Daily Nursing Staff\nGrey Band = 95% Nonparametric Confidence Interval"),
       caption="FEMA National Risk Index (Nov 2021)\nLTCFocus Public Use Data sponsored by the National Institute on Aging (P01 AG027296)\n\n*Nursing staff includes: Med aide/Technician, Nurse's aide, CNA, LPN, LPN with administrative duties, RN, RN with administrative duties, RN Director of Nursing") +
  theme_SDAD() +
  theme(plot.title=element_text(size=18, face="bold", vjust=-4),
        plot.subtitle=element_text(size=14, vjust=-5),
        plot.caption=element_text(size=10, hjust=0))

