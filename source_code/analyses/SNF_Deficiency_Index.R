#Calculate Deficiency Composites for EP and LSC
library(tibble)
library(dplyr)
library(WriteXLS)
library(stringr)
library(readr)
library(ggplot2)

# Copy the url of the data github
url1 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/virginia_skilled_nursing_facility/facility/va_cms_fire_safety_deficiencies_2022-12.csv"
url2 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/virginia_skilled_nursing_facility/facility/va_cms_provider_final_2022-07.csv"
url3 <- "https://github.com/uva-bi-sdad/census_cde_demo_2/raw/main/data/virginia_skilled_nursing_facility/facility/us_cms_inspection_dates_2022-06.csv"

#Read in the CMS deficiency data from the url; there are 292 unique providers
DEF<-read.csv(url1); dim(DEF); View(DEF) #(3952 x 22)
#Read in the CMS provider data from the url; there are 288 unique providers
PRO<-read.csv(url2); dim(PRO); View(PRO) #(288 X 98)
#Read in the CMS provider data from the url; there are 288 unique providers
INSP<-read.csv(url3); dim(INSP); View(INSP) #(211032 X 5)

#Remove the facilities that qualified for Medicare/Medicaid payment after 2021 and do not have staffing data
#CMS<-subset(PRO, FPN != "495427" & FPN != "495428" & FPN != "495429" & FPN != "495430" & FPN != "495431"); dim(CMS)
#Create a provider data sets with lats and longs and penalities and fines
DEF_CMSx<-PRO[,c(1,4,5,6,93,94,96)]; dim(DEF_CMSx) #(288 x 7)
  names(DEF_CMSx)<-c("FPN","LAT","LONG","NAME","NFINES","AMTFINES","NPENAL")
  DEF_CMSx<-subset(DEF_CMSx, FPN != "495427" & FPN != "495428" & FPN != "495429" & FPN != "495430" & FPN != "495431")
  dim(DEF_CMSx) #(283 x 7)

#Calculate the number of standard inspections/complaints from 2018 to 2022
#Inspection dates are for the entire US
#https://data.cms.gov/provider-data/dataset/svdt-c123
#Keep only those inspections that happened in 2018 to present
#For Virginia SNFs
#View(INSP)
INSP<-INSP[,c(1:4)]; dim(INSP)
names(INSP)<-c("FPN","Date","Type","Cycle")
INSP$FPN<-as.character(INSP$FPN)
INSP$Date<-as.Date(INSP$Date, "%m/%d/%y"); dim(INSP); summary(INSP)
#Align the inspection dates with the available deficiency data
INSP<-INSP[(INSP$Date>="2018-01-01" & INSP$Date<="2022-02-25"),]; dim(INSP) #(195380)
#Only keep those that are Fire Safety Deficiencies
#Keep only VA SNFs with inspection Types Fire Safety Complaint or Fire Safety Standard
INSP<-INSP[str_detect(INSP$Type, "Fire"),]; dim(INSP); View(INSP)
KEEP<-grepl("^49", INSP$FPN)
INSP<-INSP[KEEP,]; dim(INSP); length(unique(INSP$FPN)) #(682 x 4) 
View(INSP)

#Calculate the number of times each SNF was inspected since 2018 
INSP_NUM<-as.data.frame(table(INSP$FPN, INSP$Type)) 
names(INSP_NUM)<-c("FPN","TYPE","FREQ"); dim(INSP_NUM)
INSP_NUM<-INSP_NUM[order(INSP_NUM$FPN, INSP_NUM$TYPE),]  
View(INSP_NUM); dim(INSP_NUM) #(576 x 3)
#Separate out the standard inspections from the complaints which are not the result of an inspection
INSP_NUM_STD<-INSP_NUM[INSP_NUM$TYPE=="Fire Safety Standard",]; dim(INSP_NUM_STD); View(INSP_NUM_STD) #(288 x 3)
INSP_NUM_STD<-INSP_NUM_STD[,c(1,3)]; names(INSP_NUM_STD)<-c("FPN","NUM_STD")
INSP_NUM_COMPL<-INSP_NUM[INSP_NUM$TYPE=="Fire Safety Complaint",]; dim(INSP_NUM_COMPL); View(INSP_NUM_COMPL) #(288 x 3)
INSP_NUM_COMPL<-INSP_NUM_COMPL[,c(1,3)]; names(INSP_NUM_COMPL)<-c("FPN","NUM_COMPL")
INSP_NUM_FINAL<-merge(INSP_NUM_STD, INSP_NUM_COMPL, all.x=TRUE, all.y=TRUE, by="FPN"); dim(INSP_NUM_FINAL); View(INSP_NUM_FINAL)
#Remove the SNF that have no information in the CMS payroll data base (no employee or resident average estimates)   
INSP_NUM_FINAL<-subset(INSP_NUM_FINAL, FPN != "495427" & FPN != "495428" & FPN != "495429" & FPN != "495430" & FPN != "495431")
dim(INSP_NUM_FINAL) #(283 x 3)

#Merge Deficiency Data with Inspection Data
#Format the deficiency variables
     DEF$survey_date<-as.Date(DEF$survey_date, "%m/%d/%y")
 DEF$correction_date<-as.Date(DEF$correction_date, "%m/%d/%y")
#Correction data is missing for many of the duplicated entries so when the duplicates are removed  
#I want to make sure we don't remove the ones with the correction dates 
                        DEF<-DEF[-which(is.na(DEF$correction_date)),]; dim(DEF) #(3916 x 22)
DEF$federal_provider_number<-as.character(DEF$federal_provider_number)
                    DEF2018<-DEF[DEF$survey_date>="2018-01-01",]; dim(DEF2018); View(DEF2018) #(3350 x 22)
                    DEF2018<-DEF2018[order(DEF2018$federal_provider_number, DEF2018$survey_date, DEF2018$inspection_cycle, DEF2018$deficiency_prefix),]  
                DEF2018$DTC<-as.numeric(DEF2018$correction_date-DEF2018$survey_date)
#Remove the deficiencies with a health tage
DEF2018<-DEF2018[DEF2018$deficiency_prefix != "F",]; dim(DEF2018) #(3350 x 23)
#Remove the duplicated rows
DEF2018unique<-distinct(DEF2018, .keep_all=TRUE); dim(DEF2018unique) #(3350 x 23)
#Remove the inspection cycles that are greater than 1
DEF2018unique<-DEF2018unique[DEF2018unique$inspection_cycle==1,]; dim(DEF2018unique); #View(DEF2018unique) #(1193 x 23)
#Change the severity scope from a letter to a number
table(DEF2018unique$scope_severity)
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="A")]<-0
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="B")]<-0
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="C")]<-0
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="D")]<-4
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="E")]<-8
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="F")]<-16
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="G")]<-20
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="H")]<-35
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="I")]<-45
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="J")]<-50
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="K")]<-100
DEF2018unique$scope_severity[which(DEF2018unique$scope_severity=="L")]<-150
DEF2018unique$scope_severity<-as.numeric(DEF2018unique$scope_severity)
table(DEF2018unique$scope_severity)

#Create separate data set for Emergency Preparedness Deficiencies and merge with Inspection Data
#95 SNF have Emergency Preparedness Deficiencies
DEF_E<-DEF2018unique[DEF2018unique$deficiency_prefix=="E", c(1,24,23)]
  View(DEF_E); dim(DEF_E) #(151 x 3)
  names(DEF_E)<-c("FPN","SCORE","DTC"); DEF_E$FPN<-as.character(DEF_E$FPN); length(unique(DEF_E$FPN))
  DEF_E_MN<-aggregate(. ~ FPN, data=DEF_E, mean); dim(DEF_E_MN) #(41 x 3)
  DEF_NUM<-as.data.frame(table(DEF_E$FPN)); dim(DEF_NUM); names(DEF_NUM)<-c("FPN","NUMofDEF"); View(DEF_NUM)
  temp1<-merge(DEF_E_MN, DEF_NUM, all.x=TRUE, all.y=TRUE, by="FPN"); dim(temp1); View(temp1) #(41 x 4)
  FINAL_E<-merge(temp1, INSP_NUM_FINAL, all.x=TRUE, all.y=TRUE, by="FPN")  
  names(FINAL_E)<-c("FPN","SCORE_E","DTC_E","NUM_DEF_E","NUM_INSP_E","NUM_COMPL_E"); View(FINAL_E); dim(FINAL_E)
rm(temp1, DEF_NUM)  

#Create a data frame of the Fire Life Safety Code Deficiencies and merge with Inspection Data
#265 SNF have Fire Life Safety Code Deficiencies  
DEF_K<-DEF2018unique[DEF2018unique$deficiency_prefix=="K",c(1,24,23)]
  View(DEF_K); dim(DEF_K) #(1042 x 3)
  names(DEF_K)<-c("FPN","SCORE","DTC"); DEF_K$FPN<-as.character(DEF_K$FPN); length(unique(DEF_K$FPN))
  DEF_K_MN<-aggregate(. ~ FPN, data=DEF_K, sum); dim(DEF_K_MN) #(211 x 3)
  DEF_NUM<-as.data.frame(table(DEF_K$FPN)); dim(DEF_NUM); names(DEF_NUM)<-c("FPN","NUMofDEF"); View(DEF_NUM)
  temp1<-merge(DEF_K_MN, DEF_NUM, all.x=TRUE, all.y=TRUE, by="FPN"); dim(temp1); View(temp1) #(211 x 4)
  FINAL_K<-merge(temp1, INSP_NUM_FINAL, all.x=TRUE, all.y=TRUE, by="FPN"); dim(FINAL_K); View(FINAL_K) #(285 x 6)
  names(FINAL_K)<-c("FPN","SCORE_K","DTC_K","NUM_DEF_K","NUM_INSP_K","NUM_COMPL_K")
rm(temp1, DEF_NUM)    
  
#Merge with deficiency data from the Provider file
#put all data frames into list
temp_list<-list(DEF_CMSx, FINAL_K, FINAL_E)      
#Create a data file with both types of deficiences and the 283 SNFs
#Merge all data frames together
#Remove SNFs with no lats & longs
DEFICIENCY<-Reduce(function(x, y) merge(x, y, all=TRUE), temp_list); dim(DEFICIENCY)
DEFICIENCY<-DEFICIENCY[!is.na(DEFICIENCY$LAT),]
dim(DEFICIENCY); View(DEFICIENCY)
rm(temp_list)

#Deficiencies
#Replace all NAs with 0 since they are true 0s  
dim(DEFICIENCY); View(DEFICIENCY) #(283 x 17)
DEFICIENCY[is.na(DEFICIENCY)]<-0
#Deficiency Derived Indicator
K_DPI<-DEFICIENCY$NUM_DEF_K/DEFICIENCY$NUM_INSP_K
E_DPI<-DEFICIENCY$NUM_DEF_E/DEFICIENCY$NUM_INSP_E
K_DPC<-DEFICIENCY$NUM_COMPL_K 
E_DPC<-DEFICIENCY$NUM_COMPL_E

DEFICIENCY$K_DEF_IND<-K_DPI+K_DPC+DEFICIENCY$SCORE_K+DEFICIENCY$DTC_K
DEFICIENCY$E_DEF_IND<-E_DPI+E_DPC+DEFICIENCY$SCORE_E+DEFICIENCY$DTC_E
DEFICIENCY$TOTAL_DEF_IND<-DEFICIENCY$K_DEF_IND+DEFICIENCY$E_DEF_IND
DEFICIENCY[is.na(DEFICIENCY)]<-0; dim(DEFICIENCY)
write.csv(DEFICIENCY, "~/Documents/Counting People/Data/Derived Variables/DEFICIENCY_Index.csv", row.names=FALSE)

#display the indicators
edashape(DEFICIENCY$E_DEF_IND, "Emergency Preparedness", "Deficiency Indicator")
edashape(DEFICIENCY$K_DEF_IND, "Fire Life Safety Code", "Deficiency Indicator")


#Create a data frame of the Emergency Preparedness Deficiencies  
#DEF_E<-data.frame(table(DEF2018unique$deficiency_description[DEF2018unique$deficiency_prefix=="E"]))
#write.csv(DEF_E, "~/Documents/Counting People/Data/Nursing Homes_Interal/DEF_E.csv", 
#          row.names=FALSE); dim(DEF_E) 
#Create a data frame of the Fire Life Safety Code Deficiencies 
#DEF_K<-data.frame(table(DEF2018unique$deficiency_description[DEF2018unique$deficiency_prefix=="K"]))
#write.csv(DEF_K, "~/Documents/Counting People/Data/Nursing Homes_Interal/DEF_K.csv", 
#          row.names=FALSE); dim(DEF_K)


