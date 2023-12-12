library(readxl)
library(dplyr)
library(foreign)
library(readstata13)
library(mgcv)
library(splines)
library(lme4)
library(nlme)
library(dlnm)
library(fields)
library(plot3D)
library(plotly)
library(ggplot2)
library(ggsci)
library(readxl)
library(dplyr)
library(foreign)
library(readstata13)
library(mgcv)
library(splines)
library(lme4)
library(nlme)
library(dlnm)
library(fields)
library(plot3D)
library(plotly)
library(visreg)
library(mediation)
library(mma)
library(tiff)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(mice)
library(quantreg)
library(lubridate)
library(tidyverse)
library(forestploter)
library(pROC)
library(cutpointr)
library(data.table)
library(olsrr)
library(tictoc)
library(fastDummies)
library(forcats)
library(ggplot2)
library(ggprism)
library(cowplot)
library(svglite)

setwd('C:\\Users\\FGQ4\\OneDrive - mails.tsinghua.edu.cn\\airpolution_prosocial\\')
setwd('/Users/guoqingfeng/Library/CloudStorage/OneDrive-mails.tsinghua.edu.cn/airpolution_prosocial')
load("ProcessExposureData.Rdata")
load("ProcessExposureData_PM25.Rdata")
Data_person <- readRDS("Code_donation_PM25_2014_2018_Data_person.rds")
Data_family <- readRDS("Code_donation_PM25_2014_2018_Data_family.rds")
# to merge data

########## read 2018 data ######################
## exposure data
cfps2018person_all <- readRDS("[CFPS Public Data] CFPS 2018\\Data\\Stata\\Chinese Version\\cfps2018person_202012.rds")
cfps2018family_all <- readRDS("[CFPS Public Data] CFPS 2018\\Data\\Stata\\Chinese Version\\cfps2018famecon_202101.rds")
cfps2014family_all <- readRDS("[CFPS Public Data] CFPS 2014\\Data\\Stata\\Chinese Version\\cfps2014famecon_201906.rds")
cfps2012family_all <- readRDS("[CFPS Public Data] CFPS 2012\\Data\\Stata\\Chinese Version\\cfps2012famecon_201906.rds")

######### extract useful variable field ########################

Data2018 <-cfps2018person_all[c("pid","provcd","countyid","cyear","cmonth","qq204",    "gender",     "age",        "qp701",  "qp702",  "kw1004_b_3","qn8011",     "qg12",  "wordtest18","mathtest18","qp402acode","qp402bcode", "qph1",            "qph2",          "qph3"),]
names(Data2018) <-            c("pid","provcd",  "countyid",  "cyear","cmonth","eversmoke","gender",     "age",        "PA_freq","PA_Time","Edu_high",  "IncomeLevel","Income","wordtest",  "mathtest",  "symptomA",  "symptomB",   "DonationBehavior","DonationAmount","DonationApproach")

Index <- which(cfps2018person_all$qp402acode == "5.37"|
                 cfps2018person_all$qp402bcode == "5.37")
Data2018$outcome <- 0 
Data2018[Index,"outcome"] <- 1
Data2018$bmivalue <- (cfps2018person_all$qp102/2)/(cfps2018person_all$qp101/100)^2
Data2018[which(cfps2018person_all$qp102<=0 | cfps2018person_all$qp101<=0),"bmivalue"]<-NA

Data2018_fam <- cfps2018family_all[c("fid18","provcd18","countyid18","cyear","cmonth","fp2",         "fp515",            "fp516"          ,"fp517",          "fp518",        "fq6")]
names(Data2018_fam) <-             c("fid",  "provcd",  "countyid",  "cyear","cmonth","PersonNumber","Donation2Relative","Donation2Others","Donation2Society","OtherExpense","HousePrice")

Data2014_fam <- cfps2014family_all[c("fid14","provcd14","countyid14","cyear","cmonth","fp2",             "fp515",            "fp516"          ,"fp517",           "fp518",       "fq6")]
names(Data2014_fam) <-                 c("fid",  "provcd",  "countyid",  "cyear","cmonth","PersonNumber","Donation2Relative","Donation2Others","Donation2Society","OtherExpense","HousePrice")

Data2012_fam <- cfps2012family_all[c("fid12","provcd",  "countyid",  "cyear","cmonth","fp2",                                                  "fp514",          "fp517",       "fq6")]
names(Data2012_fam) <-             c("fid",  "provcd",  "countyid",  "cyear","cmonth","PersonNumber",                                     "Donation2Society","OtherExpense","HousePrice")
Data2012_fam$Donation2Relative <- NA
Data2012_fam$Donation2Others <- NA

##########  combine exact interview date and exact location #####################
sptl_crosswalk <- read.dta13("ResentialAddress.dta")
Data2012_fam <- left_join(x = Data2012_fam, y = sptl_crosswalk[,c("countycd","countyid")],by=c("countyid" = "countyid"),na_matches = "never")
Data2014_fam <- left_join(x = Data2014_fam, y = sptl_crosswalk[,c("countycd","countyid")],by=c("countyid" = "countyid"),na_matches = "never")
Data2018_fam <- left_join(x = Data2018_fam, y = sptl_crosswalk[,c("countycd","countyid")],by=c("countyid" = "countyid"),na_matches = "never")
Data2018 <- left_join(x = Data2018, y = sptl_crosswalk[,c("countycd","countyid")],by=c("countyid" = "countyid"),na_matches = "never")

##########  combine data together #####################

Data_family <- rbind(Data2014_fam,Data2018_fam,Data2012_fam)
Data_person <- Data2018

################# some functions #############################

## merge with daily and annual data
## VariableName: the variable name of data field to be merged
## Location1, YearCode1: variable field for location and time for health data
## Location2, YearCode2: variable field for location and time for environmental data
AnnualMerge <- function(HealthData,AnnualData,VariableName,LocationCode1,YearCode1,LocationCode2,YearCode2)
{
  # HealthData = Data
  # AnnualData = AnnualPM25
  # VariableName = "PM25"
  # LocationCode1 = "countycd"
  # YearCode1 = "cyear"
  # LocationCode2 = "Code"
  # YearCode2 = "year"
  
  HealthData$Index_location <- HealthData[,LocationCode1]
  HealthData$Index_time <- HealthData[,YearCode1]
  
  AnnualData$Index_location <- AnnualData[,LocationCode2]
  AnnualData$Index_time <- AnnualData[,YearCode2]
  # merge annual data
  names(AnnualData)[which(names(AnnualData)==VariableName)] <- paste0("Annual_",VariableName)
  HealthData <- left_join(x = HealthData,y = AnnualData[,c("Index_location","Index_time",paste0("Annual_",VariableName))],by = c("Index_location" = "Index_location","Index_time" = "Index_time"),na_matches = "never")
  return(HealthData)
}

## merge with daily and annual data with time lag
## VariableName: the variable name of data field to be merged
## Location1, YearCode1: variable field for location and time for health data
## Location2, YearCode2: variable field for location and time for environmental data
AnnualMerge_Lag <- function(HealthData,AnnualData,VariableName,LocationCode1,YearCode1,LocationCode2,YearCode2,LagTime)
{
  # HealthData = Data
  # AnnualData = AnnualPM25
  # VariableName = "PM25"
  # LocationCode1 = "countycd"
  # YearCode1 = "cyear"
  # LocationCode2 = "Code"
  # YearCode2 = "year"
  
  HealthData$Index_location <- HealthData[,LocationCode1]
  HealthData$Index_time <- HealthData[,YearCode1]
  
  AnnualData$Index_location <- AnnualData[,LocationCode2]
  AnnualData$Index_time <- AnnualData[,YearCode2]+LagTime
  # merge annual data
  names(AnnualData)[which(names(AnnualData)==VariableName)] <- paste0("Annual_",VariableName,"_Lag",LagTime)
  HealthData <- left_join(x = HealthData,y = AnnualData[,c("Index_location","Index_time",paste0("Annual_",VariableName,"_Lag",LagTime))],by = c("Index_location" = "Index_location","Index_time" = "Index_time"),na_matches = "never")
  return(HealthData)
}

## merge with daily and annual data
## daily data, four variable fields: "Name", "date" (Date format), "Temperature" (for example), "Code"
## VariableName: the variable name of data field to be merged
# we use LocationCode 1 and 2 to match the data
## Location1, DateCode1: variable field for location and time for health data
## Location2, DateCode2: variable field for location and time for environmental data
DailyMerge <- function(HealthData,DailyData,VariableName,LocationCode1,DateCode1,LocationCode2,DateCode2)
{
  # # ## testing purpose
  # HealthData <- Temp
  # DailyData <- DailyPM25
  # VariableName <- "PM25"
  # LocationCode1 <- "countycd"
  # LocationCode2 <- "Code"
  
  HealthData$Index_location <- HealthData[,LocationCode1]
  HealthData$Index_time <- HealthData[,DateCode1]
  
  DailyData$Index_location <- DailyData[,LocationCode2]
  DailyData$Index_time <- DailyData[,DateCode2]
  
  Index <- which(names(DailyData)==VariableName)
  
  # merge with daily data
  for(i in 0:10)
  {
    cat(sprintf("merging daily data %s lag %d...\n",VariableName,i))
    names(DailyData)[Index] <- paste0(VariableName,"_lag",i)
    DailyData$date_lag <- DailyData$Index_time + i
    HealthData <- left_join(x = HealthData,y = DailyData[,c("Index_location","date_lag",paste0(VariableName,"_lag",i))],by = c("Index_location" = "Index_location","Index_time" = "date_lag"),na_matches = "never")
    gc()
  }
  
  # calculate moving average
  HealthData[,c(paste0(VariableName,"_lag010"))] <- (HealthData[,c(paste0(VariableName,"_lag0"))] + HealthData[,c(paste0(VariableName,"_lag1"))] + HealthData[,c(paste0(VariableName,"_lag2"))] + HealthData[,c(paste0(VariableName,"_lag3"))]+ HealthData[,c(paste0(VariableName,"_lag4"))]+ HealthData[,c(paste0(VariableName,"_lag5"))]+ HealthData[,c(paste0(VariableName,"_lag6"))]+ HealthData[,c(paste0(VariableName,"_lag7"))]+ HealthData[,c(paste0(VariableName,"_lag8"))]+ HealthData[,c(paste0(VariableName,"_lag9"))]+ HealthData[,c(paste0(VariableName,"_lag10"))])/11
  HealthData[,c(paste0(VariableName,"_lag07"))] <- (HealthData[,c(paste0(VariableName,"_lag0"))] + HealthData[,c(paste0(VariableName,"_lag1"))] + HealthData[,c(paste0(VariableName,"_lag2"))] + HealthData[,c(paste0(VariableName,"_lag3"))]+ HealthData[,c(paste0(VariableName,"_lag4"))]+ HealthData[,c(paste0(VariableName,"_lag5"))]+ HealthData[,c(paste0(VariableName,"_lag6"))]+ HealthData[,c(paste0(VariableName,"_lag7"))])/8
  HealthData[,c(paste0(VariableName,"_lag06"))] <- (HealthData[,c(paste0(VariableName,"_lag0"))] + HealthData[,c(paste0(VariableName,"_lag1"))] + HealthData[,c(paste0(VariableName,"_lag2"))] + HealthData[,c(paste0(VariableName,"_lag3"))]+ HealthData[,c(paste0(VariableName,"_lag4"))]+ HealthData[,c(paste0(VariableName,"_lag5"))]+ HealthData[,c(paste0(VariableName,"_lag6"))])/7
  HealthData[,c(paste0(VariableName,"_lag05"))] <- (HealthData[,c(paste0(VariableName,"_lag0"))] + HealthData[,c(paste0(VariableName,"_lag1"))] + HealthData[,c(paste0(VariableName,"_lag2"))] + HealthData[,c(paste0(VariableName,"_lag3"))]+ HealthData[,c(paste0(VariableName,"_lag4"))]+ HealthData[,c(paste0(VariableName,"_lag5"))])/6
  HealthData[,c(paste0(VariableName,"_lag04"))] <- (HealthData[,c(paste0(VariableName,"_lag0"))] + HealthData[,c(paste0(VariableName,"_lag1"))] + HealthData[,c(paste0(VariableName,"_lag2"))] + HealthData[,c(paste0(VariableName,"_lag3"))]+ HealthData[,c(paste0(VariableName,"_lag4"))])/5
  HealthData[,c(paste0(VariableName,"_lag03"))] <- (HealthData[,c(paste0(VariableName,"_lag0"))] + HealthData[,c(paste0(VariableName,"_lag1"))] + HealthData[,c(paste0(VariableName,"_lag2"))] + HealthData[,c(paste0(VariableName,"_lag3"))])/4
  HealthData[,c(paste0(VariableName,"_lag02"))] <- (HealthData[,c(paste0(VariableName,"_lag0"))] + HealthData[,c(paste0(VariableName,"_lag1"))] + HealthData[,c(paste0(VariableName,"_lag2"))])/3
  HealthData[,c(paste0(VariableName,"_lag01"))] <- (HealthData[,c(paste0(VariableName,"_lag0"))] + HealthData[,c(paste0(VariableName,"_lag1"))])/2
  
  return(HealthData)
}

## merge with daily and annual data
## daily data, four variable fields: "Name", "date" (Date format), "Temperature" (for example), "Code"
## VariableName: the variable name of data field to be merged
# we use LocationCode 1 and 2 to match the data
## Location1, DateCode1: variable field for location and time for health data
## Location2, DateCode2: variable field for location and time for environmental data
DailyMerge_OneYear <- function(HealthData,DailyData,VariableName,LocationCode1,DateCode1,LocationCode2,DateCode2)
{
  # # # ## testing purpose
  # HealthData = Data
  # DailyData = DailyPM25
  # VariableName = "PM25"
  # LocationCode1 = "countycd"
  # DateCode1 = "cdate"
  # LocationCode2 = "Code"
  # DateCode2 = "date"

  
  HealthData$Index_location <- HealthData[,LocationCode1]
  HealthData$Index_time <- HealthData[,DateCode1]
  
  DailyData$Index_location <- DailyData[,LocationCode2]
  DailyData$Index_time <- DailyData[,DateCode2]
  
  Index <- which(names(DailyData)==VariableName)
  
  HealthData1 <- HealthData
  # merge with daily data
  for(i in 0:365)
  {
    cat(sprintf("merging daily data %s lag %d...\n",VariableName,i))
    names(DailyData)[Index] <- paste0(VariableName,"_lag",i)
    DailyData$date_lag <- DailyData$Index_time + i
    HealthData1 <- left_join(x = HealthData1,y = DailyData[,c("Index_location","date_lag",paste0(VariableName,"_lag",i))],by = c("Index_location" = "Index_location","Index_time" = "date_lag"),na_matches = "never")
    # gc()
  }
  
  Temp_onyear <- HealthData1[,paste0(VariableName,"_lag",0)]
  for(i in 1:30)
  {
    Temp_onyear <- Temp_onyear + HealthData1[,paste0(VariableName,"_lag",i)]
  }
  HealthData[,paste0(VariableName,"_lag030")]<-Temp_onyear/31
  
  Temp_onyear <- HealthData1[,paste0(VariableName,"_lag",0)]
  for(i in 1:60)
  {
    Temp_onyear <- Temp_onyear + HealthData1[,paste0(VariableName,"_lag",i)]
  }
  HealthData[,paste0(VariableName,"_lag060")]<-Temp_onyear/61
  
  Temp_onyear <- HealthData1[,paste0(VariableName,"_lag",0)]
  for(i in 1:90)
  {
    Temp_onyear <- Temp_onyear + HealthData1[,paste0(VariableName,"_lag",i)]
  }
  HealthData[,paste0(VariableName,"_lag090")]<-Temp_onyear/91
  
  Temp_onyear <- HealthData1[,paste0(VariableName,"_lag",0)]
  for(i in 1:120)
  {
    Temp_onyear <- Temp_onyear + HealthData1[,paste0(VariableName,"_lag",i)]
  }
  HealthData[,paste0(VariableName,"_lag0120")]<-Temp_onyear/121
  
  Temp_onyear <- HealthData1[,paste0(VariableName,"_lag",0)]
  for(i in 1:180)
  {
    Temp_onyear <- Temp_onyear + HealthData1[,paste0(VariableName,"_lag",i)]
  }
  HealthData[,paste0(VariableName,"_lag0180")]<-Temp_onyear/181
  
  Temp_onyear <- HealthData1[,paste0(VariableName,"_lag",0)]
  for(i in 1:240)
  {
    Temp_onyear <- Temp_onyear + HealthData1[,paste0(VariableName,"_lag",i)]
  }
  HealthData[,paste0(VariableName,"_lag0240")]<-Temp_onyear/241
  
  Temp_onyear <- HealthData1[,paste0(VariableName,"_lag",0)]
  for(i in 1:270)
  {
    Temp_onyear <- Temp_onyear + HealthData1[,paste0(VariableName,"_lag",i)]
  }
  HealthData[,paste0(VariableName,"_lag0270")]<-Temp_onyear/271
  
  Temp_onyear <- HealthData1[,paste0(VariableName,"_lag",0)]
  for(i in 1:365)
  {
    Temp_onyear <- Temp_onyear + HealthData1[,paste0(VariableName,"_lag",i)]
  }
  HealthData[,paste0(VariableName,"_lag0365")]<-Temp_onyear/366
  
  return(HealthData)
}

MonthMerge <- function(HealthData,MonthData,VariableName,LocationCode1,yearcode1,monthcode1,LocationCode2,yearcode2,monthcode2)
{
  # # for testing purpose
  # HealthData = Data
  # MonthData = MonthPM25
  # VariableName = "MonthPM25"
  # LocationCode1 = "countycd"
  # yearcode1 = "cyear"
  # monthcode1 = "cmonth"
  # LocationCode2 = "Code"
  # yearcode2 =  "year"
  # monthcode2 = "month"
  # 
  HealthData$Index_location <- HealthData[,LocationCode1]
  HealthData$Index_Year <- HealthData[,yearcode1]
  HealthData$Index_Month <- HealthData[,monthcode1]
  
  MonthData$Index_location <- MonthData[,LocationCode2]
  MonthData$Index_Year <- MonthData[,yearcode2]
  MonthData$Index_Month <- MonthData[,monthcode2]
  
  Index <- which(names(MonthData)==VariableName)
  for(i in 0:12)
  {
    cat(sprintf("merging monthly data %s lag %d...\n",VariableName,i))
    
    TempDate <- as.Date(paste0(MonthData$Index_Year,"-",MonthData$Index_Month,"-15"))
    TempDate1 <- TempDate + 30*i
    names(MonthData)[Index] <- paste0(VariableName,"_monthlag",i)
    MonthData$Index_Year <- as.numeric(format(TempDate1,"%Y"))
    MonthData$Index_Month <- as.numeric(format(TempDate1,"%m"))
    
    HealthData <- left_join(x = HealthData,y=MonthData,by=c("Index_Year" = "Index_Year","Index_Month" = "Index_Month","Index_location" = "Index_location"),na_matches = "never")
  }
  return(HealthData)
}

## merge data during pregancy
# EnvData format: "year", "month", VariableName, "Code",
# we use LocationCode 1 and 2 to match the data
## Location1, birthyear,birthmonth: variable field for location and time for health data
## Location2, yearcode,monthcode: variable field for location and time for environmental data
PregencyMerge <- function(HealthData,birthyear,birthmonth,EnvData,VariableName,LocationCode1,LocationCode2,yearcode,monthcode)
{
  # # # for testing purpose
  # HealthData = Data
  # birthyear = Data$birthyear
  # birthmonth = Data$birthmonth
  # EnvData = MonthTemperature1
  # VariableName = "MonthTemperature"
  # LocationCode1 = "birthplacecd"
  # LocationCode2 = "Code"
  # yearcode = "year"
  # monthcode = "month"

  EnvData$Index_year <- EnvData[,yearcode]
  EnvData$Index_month <- EnvData[,monthcode]
  EnvData$Index_location <- EnvData[,LocationCode2]
  BirthLocation <- HealthData[,LocationCode1]
  BirthDate <- as.Date(paste0(birthyear,"-",birthmonth,"-15"))
  
  BirthDate_lag1 <- BirthDate - 30*0 # lag 1 month 
  BirthDate_lag2 <- BirthDate - 30*1 # lag 2 month
  BirthDate_lag3 <- BirthDate - 30*2 # lag 3 month
  BirthDate_lag4 <- BirthDate - 30*3 # lag 4 month
  BirthDate_lag5 <- BirthDate - 30*4 # lag 5 month
  BirthDate_lag6 <- BirthDate - 30*5 # lag 6 month
  BirthDate_lag7 <- BirthDate - 30*6 # lag 7 month
  BirthDate_lag8 <- BirthDate - 30*7 # lag 8 month
  BirthDate_lag9 <- BirthDate - 30*8 # lag 9 month
  
  BirthDate_lag1_year <- as.numeric(format(BirthDate_lag1,"%Y"))
  BirthDate_lag2_year <- as.numeric(format(BirthDate_lag2,"%Y"))
  BirthDate_lag3_year <- as.numeric(format(BirthDate_lag3,"%Y"))
  BirthDate_lag4_year <- as.numeric(format(BirthDate_lag4,"%Y"))
  BirthDate_lag5_year <- as.numeric(format(BirthDate_lag5,"%Y"))
  BirthDate_lag6_year <- as.numeric(format(BirthDate_lag6,"%Y"))
  BirthDate_lag7_year <- as.numeric(format(BirthDate_lag7,"%Y"))
  BirthDate_lag8_year <- as.numeric(format(BirthDate_lag8,"%Y"))
  BirthDate_lag9_year <- as.numeric(format(BirthDate_lag9,"%Y"))
  
  BirthDate_lag1_month <- as.numeric(format(BirthDate_lag1,"%m"))
  BirthDate_lag2_month <- as.numeric(format(BirthDate_lag2,"%m"))
  BirthDate_lag3_month <- as.numeric(format(BirthDate_lag3,"%m"))
  BirthDate_lag4_month <- as.numeric(format(BirthDate_lag4,"%m"))
  BirthDate_lag5_month <- as.numeric(format(BirthDate_lag5,"%m"))
  BirthDate_lag6_month <- as.numeric(format(BirthDate_lag6,"%m"))
  BirthDate_lag7_month <- as.numeric(format(BirthDate_lag7,"%m"))
  BirthDate_lag8_month <- as.numeric(format(BirthDate_lag8,"%m"))
  BirthDate_lag9_month <- as.numeric(format(BirthDate_lag9,"%m"))
  
  temp <- data.frame(HealthData$pid,
                     BirthDate_lag1_year,BirthDate_lag1_month,
                     BirthDate_lag2_year,BirthDate_lag2_month,
                     BirthDate_lag3_year,BirthDate_lag3_month,
                     BirthDate_lag4_year,BirthDate_lag4_month,
                     BirthDate_lag5_year,BirthDate_lag5_month,
                     BirthDate_lag6_year,BirthDate_lag6_month,
                     BirthDate_lag7_year,BirthDate_lag7_month,
                     BirthDate_lag8_year,BirthDate_lag8_month,
                     BirthDate_lag9_year,BirthDate_lag9_month,
                     BirthLocation
  )
  Index <- which(names(EnvData)==VariableName)
  names(EnvData)[Index] <- paste0(VariableName,"_lag1")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag1"))],by=c("BirthDate_lag1_year" = "Index_year","BirthDate_lag1_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  names(EnvData)[Index] <- paste0(VariableName,"_lag2")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag2"))],by=c("BirthDate_lag2_year" = "Index_year","BirthDate_lag2_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  names(EnvData)[Index] <- paste0(VariableName,"_lag3")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag3"))],by=c("BirthDate_lag3_year" = "Index_year","BirthDate_lag3_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  names(EnvData)[Index] <- paste0(VariableName,"_lag4")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag4"))],by=c("BirthDate_lag4_year" = "Index_year","BirthDate_lag4_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  names(EnvData)[Index] <- paste0(VariableName,"_lag5")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag5"))],by=c("BirthDate_lag5_year" = "Index_year","BirthDate_lag5_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  names(EnvData)[Index] <- paste0(VariableName,"_lag6")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag6"))],by=c("BirthDate_lag6_year" = "Index_year","BirthDate_lag6_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  names(EnvData)[Index] <- paste0(VariableName,"_lag7")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag7"))],by=c("BirthDate_lag7_year" = "Index_year","BirthDate_lag7_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  names(EnvData)[Index] <- paste0(VariableName,"_lag8")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag8"))],by=c("BirthDate_lag8_year" = "Index_year","BirthDate_lag8_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  names(EnvData)[Index] <- paste0(VariableName,"_lag9")
  temp <- left_join(x = temp, y = EnvData[,c("Index_year","Index_month","Index_location",paste0(VariableName,"_lag9"))],by=c("BirthDate_lag9_year" = "Index_year","BirthDate_lag9_month" = "Index_month","BirthLocation" = "Index_location"),na_matches = "never")
  
  Index = ncol(temp)
  HealthData[,paste0(VariableName,"_Preg")] <- rowMeans(temp[,c(paste0(VariableName,"_lag1"),paste0(VariableName,"_lag2"),paste0(VariableName,"_lag3"),paste0(VariableName,"_lag4"),paste0(VariableName,"_lag5"),paste0(VariableName,"_lag6"),paste0(VariableName,"_lag7"),paste0(VariableName,"_lag8"),paste0(VariableName,"_lag9"))],na.rm = TRUE)
  HealthData[,paste0(VariableName,"_Trim1")] <- rowMeans(temp[,c(paste0(VariableName,"_lag9"),paste0(VariableName,"_lag8"),paste0(VariableName,"_lag7"))],na.rm = TRUE)
  HealthData[,paste0(VariableName,"_Trim2")] <- rowMeans(temp[,c(paste0(VariableName,"_lag6"),paste0(VariableName,"_lag5"),paste0(VariableName,"_lag4"))],na.rm = TRUE)
  HealthData[,paste0(VariableName,"_Trim3")] <- rowMeans(temp[,c(paste0(VariableName,"_lag1"),paste0(VariableName,"_lag2"),paste0(VariableName,"_lag3"))],na.rm = TRUE)
  HealthData[,paste0(VariableName,"_PregMonth1")] <- temp[,paste0(VariableName,"_lag9")]
  HealthData[,paste0(VariableName,"_PregMonth2")] <- temp[,paste0(VariableName,"_lag8")]
  HealthData[,paste0(VariableName,"_PregMonth3")] <- temp[,paste0(VariableName,"_lag7")]
  HealthData[,paste0(VariableName,"_PregMonth4")] <- temp[,paste0(VariableName,"_lag6")]
  HealthData[,paste0(VariableName,"_PregMonth5")] <- temp[,paste0(VariableName,"_lag5")]
  HealthData[,paste0(VariableName,"_PregMonth6")] <- temp[,paste0(VariableName,"_lag4")]
  HealthData[,paste0(VariableName,"_PregMonth7")] <- temp[,paste0(VariableName,"_lag3")]
  HealthData[,paste0(VariableName,"_PregMonth8")] <- temp[,paste0(VariableName,"_lag2")]
  HealthData[,paste0(VariableName,"_PregMonth9")] <- temp[,paste0(VariableName,"_lag1")]
                                                       
  return(HealthData)
}


########## merge data ##########
# saveRDS(Data_person,"Code_donation_PM25_2014_2018_Data_person.rds")
# saveRDS(Data_family,"Code_donation_PM25_2014_2018_Data_family.rds")



Data_person <- readRDS("Code_donation_PM25_2014_2018_Data_person.rds")
Data_family <- readRDS("Code_donation_PM25_2014_2018_Data_family.rds")

Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 0)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 1)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 2)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 3)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 4)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 5)

Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 0)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 1)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 2)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 3)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 4)
Data_family <- AnnualMerge_Lag(HealthData = Data_family,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 5)

Data_family$Annual_AnnualPM25_Lag05 <- (Data_family$Annual_AnnualPM25_Lag0+Data_family$Annual_AnnualPM25_Lag1+Data_family$Annual_AnnualPM25_Lag2+Data_family$Annual_AnnualPM25_Lag3+Data_family$Annual_AnnualPM25_Lag4+Data_family$Annual_AnnualPM25_Lag5)/6
Data_family$Annual_AnnualPM25_Lag03 <- (Data_family$Annual_AnnualPM25_Lag0+Data_family$Annual_AnnualPM25_Lag1+Data_family$Annual_AnnualPM25_Lag2+Data_family$Annual_AnnualPM25_Lag3)/4
Data_family$Annual_AnnualPM25_Lag02 <- (Data_family$Annual_AnnualPM25_Lag0+Data_family$Annual_AnnualPM25_Lag1+Data_family$Annual_AnnualPM25_Lag2)/3
Data_family$Annual_AnnualPM25_Lag01 <- (Data_family$Annual_AnnualPM25_Lag0+Data_family$Annual_AnnualPM25_Lag1)/2

Data_family$Annual_AnnualT2m_Lag05 <- (Data_family$Annual_AnnualT2m_Lag0+Data_family$Annual_AnnualT2m_Lag1+Data_family$Annual_AnnualT2m_Lag2+Data_family$Annual_AnnualT2m_Lag3+Data_family$Annual_AnnualT2m_Lag4+Data_family$Annual_AnnualT2m_Lag5)/6
Data_family$Annual_AnnualT2m_Lag03 <- (Data_family$Annual_AnnualT2m_Lag0+Data_family$Annual_AnnualT2m_Lag1+Data_family$Annual_AnnualT2m_Lag2+Data_family$Annual_AnnualT2m_Lag3)/4
Data_family$Annual_AnnualT2m_Lag02 <- (Data_family$Annual_AnnualT2m_Lag0+Data_family$Annual_AnnualT2m_Lag1+Data_family$Annual_AnnualT2m_Lag2)/3
Data_family$Annual_AnnualT2m_Lag01 <- (Data_family$Annual_AnnualT2m_Lag0+Data_family$Annual_AnnualT2m_Lag1)/2

Data_family[which(Data_family$PersonNumber<0),"PersonNumber"] <- NA 
Data_family[which(Data_family$Donation2Relative<0),"Donation2Relative"] <- NA 
Data_family[which(Data_family$Donation2Others<0),"Donation2Others"] <- NA 
Data_family[which(Data_family$Donation2Society<0),"Donation2Society"] <- NA 
Data_family[which(Data_family$OtherExpense<0),"OtherExpense"] <- NA 
Data_family[which(Data_family$HousePrice<0),"HousePrice"] <- NA 

## merge individual level data
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 0)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 1)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 2)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 3)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 4)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualPM25,VariableName = "AnnualPM25",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 5)

Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 0)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 1)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 2)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 3)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 4)
Data_person <- AnnualMerge_Lag(HealthData = Data_person,AnnualData = AnnualT2m,VariableName = "AnnualT2m",LocationCode1 = "countycd",YearCode1 = "cyear",LocationCode2 = "Code",YearCode2 = "year",LagTime = 5)

Data_person$Annual_AnnualPM25_Lag05 <- (Data_person$Annual_AnnualPM25_Lag0+Data_person$Annual_AnnualPM25_Lag1+Data_person$Annual_AnnualPM25_Lag2+Data_person$Annual_AnnualPM25_Lag3+Data_person$Annual_AnnualPM25_Lag4+Data_person$Annual_AnnualPM25_Lag5)/6
Data_person$Annual_AnnualPM25_Lag03 <- (Data_person$Annual_AnnualPM25_Lag0+Data_person$Annual_AnnualPM25_Lag1+Data_person$Annual_AnnualPM25_Lag2+Data_person$Annual_AnnualPM25_Lag3)/4
Data_person$Annual_AnnualPM25_Lag02 <- (Data_person$Annual_AnnualPM25_Lag0+Data_person$Annual_AnnualPM25_Lag1+Data_person$Annual_AnnualPM25_Lag2)/3
Data_person$Annual_AnnualPM25_Lag01 <- (Data_person$Annual_AnnualPM25_Lag0+Data_person$Annual_AnnualPM25_Lag1)/2

Data_person$Annual_AnnualT2m_Lag05 <- (Data_person$Annual_AnnualT2m_Lag0+Data_person$Annual_AnnualT2m_Lag1+Data_person$Annual_AnnualT2m_Lag2+Data_person$Annual_AnnualT2m_Lag3+Data_person$Annual_AnnualT2m_Lag4+Data_person$Annual_AnnualT2m_Lag5)/6
Data_person$Annual_AnnualT2m_Lag03 <- (Data_person$Annual_AnnualT2m_Lag0+Data_person$Annual_AnnualT2m_Lag1+Data_person$Annual_AnnualT2m_Lag2+Data_person$Annual_AnnualT2m_Lag3)/4
Data_person$Annual_AnnualT2m_Lag02 <- (Data_person$Annual_AnnualT2m_Lag0+Data_person$Annual_AnnualT2m_Lag1+Data_person$Annual_AnnualT2m_Lag2)/3
Data_person$Annual_AnnualT2m_Lag01 <- (Data_person$Annual_AnnualT2m_Lag0+Data_person$Annual_AnnualT2m_Lag1)/2

#ever smoke
Data_person[which(Data_person$eversmoke == -1| Data_person$eversmoke == -2),"eversmoke"] <- NA
Data_person[which(Data_person$baseline_smoke == -1| Data_person$baseline_smoke == -2),"baseline_smoke"] <- NA

# physical activity
Data_person[which(Data_person$PA_freq <0 ),"PA_freq"] <- NA
Data_person[which(Data_person$baseline_PAFreq <0 ),"baseline_PAFreq"] <- NA

Data_person[which(Data_person$PA_Time <0 ),"PA_Time"] <- NA
# # income
Data_person[which(Data_person$IncomeLevel >5 | Data_person$IncomeLevel<0 ),"IncomeLevel"] = NA#您的收入在本地
Data_person[which(Data_person$baseline_income >5 | Data_person$baseline_income<0 ),"baseline_income"] = NA#您的收入在本地

Data_person[which(Data_person$Income<0 ),"Income"] = NA
Data_person[which(Data_person$wordtest <0 ),"wordtest_diff"] = NA
Data_person[which(Data_person$mathtest <0 ),"mathtest_diff"] = NA
Data_person[which(Data_person$wordtest <0 ),"wordtest"] = NA
Data_person[which(Data_person$mathtest <0 ),"mathtest"] = NA

Data_person[which(Data_person$bmivalue<15 | Data_person$bmivalue > 35),"bmivalue"] <- NA
Data_person[which(Data_person$PA_freq<0 | Data_person$PA_freq > 20),"PA_freq"] <- NA
Data_person[which(Data_person$bmivalue<15 | Data_person$bmivalue > 35),"bmidiff"] <- NA

Data_person[which(Data_person$DonationBehavior<0),"DonationBehavior"] <- NA
Data_person[which(Data_person$DonationAmount<0),"DonationAmount"] <- NA

Data_person[which(Data_person$age<0),"age"] <- NA
Data_person$countyid <- as.factor(Data_person$countyid)

Data_person$gender <- factor(Data_person$gender)
Data_person$eversmoke <- factor(Data_person$eversmoke)
Data_person$Edu_high <- factor(Data_person$Edu_high)

Data_person$income_cat <- cut(Data_person$IncomeLevel,breaks = c(-Inf,3,Inf))


########## family level ########
Data_family$TotalDonation <- Data_family$Donation2Society + Data_family$Donation2Others + Data_family$Donation2Relative

##PM2.5 reduces donation at family level 
## crude model: univariate model   ???log???1/0.1/0.01????????????
# Annual_AnnualPM25_Lag02 -0.018510   0.002209  -8.378            
summary(lme4::lmer(log(TotalDonation+1)~Annual_AnnualPM25_Lag01+ (1|countyid),data = Data_family[Data_family$cyear>2012,],na.action = na.omit))
library(nlme)
mod <- lme(log(TotalDonation+1)~Annual_AnnualPM25_Lag01, random = ~1|countyid , data = Data_family[Data_family$cyear>2012,],na.action = na.omit)
anova(mod)
# Annual_AnnualPM25_Lag02 -0.013605   0.001258  -10.81
summary(lme4::lmer(log(Donation2Society+1)~Annual_AnnualPM25_Lag01+ (1|countyid),data = Data_family[Data_family$cyear>2012,],na.action = na.omit))
mod <- lme(log(Donation2Society+1)~Annual_AnnualPM25_Lag01, random = ~1|countyid , data = Data_family[Data_family$cyear>2012,],na.action = na.omit)
anova(mod)
# Annual_AnnualPM25_Lag02 -0.010217   0.001079  -9.471
summary(lme4::lmer(log(Donation2Others+1)~Annual_AnnualPM25_Lag01+ (1|countyid),data = Data_family[Data_family$cyear>2012,],na.action = na.omit))
mod <- lme(log(Donation2Others+1)~Annual_AnnualPM25_Lag01, random = ~1|countyid , data = Data_family[Data_family$cyear>2012,],na.action = na.omit)
anova(mod)
# Annual_AnnualPM25_Lag02 -0.008841   0.002118  -4.173  
summary(lme4::lmer(log(Donation2Relative+1)~Annual_AnnualPM25_Lag01+ (1|countyid),data = Data_family[Data_family$cyear>2012,],na.action = na.omit))
mod <- lme(log(Donation2Relative+1)~Annual_AnnualPM25_Lag01, random = ~1|countyid , data = Data_family[Data_family$cyear>2012,],na.action = na.omit)
anova(mod)

## Adjusted model; multivariate model  
# Annual_AnnualPM25_Lag02 -0.014735   0.002165  -6.807
summary(lme4::lmer(log(TotalDonation+1)~Annual_AnnualPM25_Lag01+log(OtherExpense+1) + log(HousePrice) + PersonNumber + (1|countyid),data = Data_family[Data_family$cyear>2012,],na.action = na.omit))
# Annual_AnnualPM25_Lag02 -0.008553   0.001253  -6.825
summary(lme4::lmer(log(Donation2Society+1)~Annual_AnnualPM25_Lag01+log(OtherExpense+1) + log(HousePrice) + PersonNumber+(1|countyid),data = Data_family[Data_family$cyear>2012,],na.action = na.omit))
# Annual_AnnualPM25_Lag02 -0.008266   0.001134  -7.287
summary(lme4::lmer(log(Donation2Others+1)~Annual_AnnualPM25_Lag01+log(OtherExpense+1) + log(HousePrice) + PersonNumber+(1|countyid),data = Data_family[Data_family$cyear>2012,],na.action = na.omit))
# Annual_AnnualPM25_Lag02 -0.008418   0.002061  -4.084
summary(lme4::lmer(log(Donation2Relative+1)~Annual_AnnualPM25_Lag01+log(OtherExpense+1) + log(HousePrice) + PersonNumber+(1|countyid),data = Data_family[Data_family$cyear>2012,],na.action = na.omit))


###### dose - response (plot data prepare)######
### data for total donation and annual PM25
Data1 <- Data_family[Data_family$cyear>2012,]
Data1$variable <- Data1$Annual_AnnualPM25_Lag01

Mod <- mgcv::gam(log(TotalDonation+1)~s(Annual_AnnualPM25_Lag01, k = 4)+log(OtherExpense+1) + log(HousePrice) + PersonNumber + s(countyid,bs="re"),data = Data_family[Data_family$cyear>2012,],na.action = na.omit)
plot(Mod,main = "total donation")

Mod <- gam(log(TotalDonation+1)~ s(variable,k=4)+log(OtherExpense+1) + log(HousePrice) + PersonNumber + s(countyid,bs="re") ,data = Data1,na.action = na.omit)
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- mgcv::predict.gam(Mod, newdata = newdata,se.fit = TRUE)
Ref <- preds$fit[1]
fit <- 100*(exp(preds$fit - Ref) - 1)
lower <- 100*(exp(preds$fit - Ref - 1.96*preds$se.fit)-1)
upper <- 100*(exp(preds$fit - Ref + 1.96*preds$se.fit)-1)
pred_plot1 <- data.frame(PM25 = preds_x,fit = fit,lower = lower, upper = upper)




### data for  donation to relative and annual PM25
Mod <- gam(log(Donation2Relative+1)~s(Annual_AnnualPM25_Lag01, k = 4)+log(OtherExpense+1) + log(HousePrice) + PersonNumber+ s(countyid,bs="re"),data = Data_family[Data_family$cyear>2012,],na.action = na.omit)
plot(Mod,main = "donation to relatives")

Mod <- gam(log(Donation2Relative+1)~ s(variable,k=4)+log(OtherExpense+1) + log(HousePrice) + PersonNumber + s(countyid,bs="re") ,data = Data1,na.action = na.omit)
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
Ref <- preds$fit[1]
fit <- 100*(exp(preds$fit - Ref) - 1)
lower <- 100*(exp(preds$fit - Ref - 1.96*preds$se.fit)-1)
upper <- 100*(exp(preds$fit - Ref + 1.96*preds$se.fit)-1)
pred_plot2 <- data.frame(PM25 = preds_x,fit = fit,lower = lower, upper = upper)



### data for donation to others and annual PM25
Mod <- gam(log(Donation2Others+1)~s(Annual_AnnualPM25_Lag01, k = 4)+log(OtherExpense+1) + log(HousePrice) + PersonNumber+ s(countyid,bs="re"),data = Data_family[Data_family$cyear>2012,],na.action = na.omit)
plot(Mod,main = "donation to others")

Mod <- gam(log(Donation2Others+1)~ s(variable,k=4)+log(OtherExpense+1) + log(HousePrice) + PersonNumber + s(countyid,bs="re") ,data = Data1,na.action = na.omit)
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
Ref <- preds$fit[1]
fit <- 100*(exp(preds$fit - Ref) - 1)
lower <- 100*(exp(preds$fit - Ref - 1.96*preds$se.fit)-1)
upper <- 100*(exp(preds$fit - Ref + 1.96*preds$se.fit)-1)
pred_plot3 <- data.frame(PM25 = preds_x,fit = fit,lower = lower, upper = upper)




### data for donation to society and annual PM25
Mod <- gam(log(Donation2Society+1)~s(Annual_AnnualPM25_Lag01, k = 4)+log(OtherExpense+1) + log(HousePrice) + PersonNumber+ s(countyid,bs="re"),data = Data_family[Data_family$cyear>2012,],na.action = na.omit)
plot(Mod,main = "donation to society")

Mod <- gam(log(Donation2Society+1)~ s(variable,k=4)+log(OtherExpense+1) + log(HousePrice) + PersonNumber + s(countyid,bs="re") ,data = Data1,na.action = na.omit)
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
Ref <- preds$fit[1]
fit <- 100*(exp(preds$fit - Ref) - 1)
lower <- 100*(exp(preds$fit - Ref - 1.96*preds$se.fit)-1)
upper <- 100*(exp(preds$fit - Ref + 1.96*preds$se.fit)-1)
pred_plot4 <- data.frame(PM25 = preds_x,fit = fit,lower = lower, upper = upper)




####### donation to relative and to others plot##########
ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=pred_plot2,lty = 1, lwd = 1,aes(colour = "Donation to Relatives")) +
  geom_ribbon(data=pred_plot2,aes(ymax = ymax, 
                                  ymin = ymin),
              alpha = 0.5,
              fill = "purple",
              colour = NA ) + 
  geom_line(data=pred_plot3,lty = 1, lwd = 1,aes(colour = "Donation to Others")) +
  geom_ribbon(data=pred_plot3,aes(ymax = ymax, ymin = ymin),
              alpha = 0.5,
              fill = "red",
              colour=NA)+
  geom_line(data = pred_plot2, aes(y=1),color = "black",size = 1,linetype="dashed")+
  scale_color_manual(name = "Donation type",values = c("Donation to Others"="red","Donation to Relatives"="purple"))+
  scale_y_continuous(expand = c(0,0),limits = c(0.2,1.3))+
  scale_x_continuous(expand = c(0,0), limits = c(13.43644,85))+
  theme(axis.text=element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85)
  )

#######  total donation and donation to society   plot##########
ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=pred_plot1,lty = 1, lwd = 1,aes(colour = "Total Donation")) +
  geom_ribbon(data=pred_plot1,aes(ymax = ymax, 
                                  ymin = ymin),
              alpha = 0.5,
              fill = "purple",
              colour = NA ) + 
  geom_line(data=pred_plot4,lty = 1, lwd = 1,aes(colour = "Donation to Society")) +
  geom_ribbon(data=pred_plot4,aes(ymax = ymax, ymin = ymin),
              alpha = 0.5,
              fill = "red",
              colour=NA)+
  theme_bw()+
  geom_line(data = pred_plot2, aes(y=1),color = "black",size = 1,linetype="dashed")+
  scale_color_manual(name = "Donation type",values = c("Donation to Society"="red","Total Donation"="purple"))+
  scale_y_continuous(expand = c(0,0),limits = c(0.2,1.3))+
  scale_x_continuous(expand = c(0,0), limits = c(13.43644,85))+
  theme(axis.text=element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85)
  )


##################total donations types in one plot#########
my_palette <- pal_nejm('default', alpha = 0.6)(4)
my_palette
library("scales")
show_col(my_palette)

ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=pred_plot1,lty = 1, lwd = 1,aes(colour = "Total Donation")) +
  geom_ribbon(data=pred_plot1,aes(ymax = ymax, 
                                  ymin = ymin),
              alpha = 0.2,
              fill = '#BC3C2999') + 
  geom_line(data=pred_plot4,lty = 1, lwd = 1,aes(colour = "Donation to Society")) +
  geom_ribbon(data=pred_plot4,aes(ymax = ymax, ymin = ymin),
              alpha = 0.2,
              fill='#0072B599')+
  geom_line(data=pred_plot2,lty = 1, lwd = 1,aes(colour = "Donation to Relatives")) +
  geom_ribbon(data=pred_plot2,aes(ymax = ymax, 
                                  ymin = ymin),
              alpha = 0.2,
              fill='#E1872799') + 
  geom_line(data=pred_plot3,lty = 1, lwd = 1,aes(colour = "Donation to Others")) +
  geom_ribbon(data=pred_plot3,aes(ymax = ymax, ymin = ymin),
              alpha = 0.2,
              fill='#20854E99')+
  geom_line(data = pred_plot2, aes(y=1),color = "black",size = 1,linetype="dashed")+
  scale_color_manual(name = "Donation type",values = c(
    "Total Donation" = "#BC3C2999",
    "Donation to Society" = "#0072B599",
    "Donation to Relatives" ="#E1872799",
    "Donation to Others" ="#20854E99")
  )+
  scale_y_continuous(expand = c(0,0),limits = c(0.1,1.3))+
  scale_x_continuous(expand = c(0,0), limits = c(13.43644,80))+
  theme(axis.text=element_text(size=20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.position = c(0.75,0.85)
  )+
  theme_bw()





##########seperate plot donations types######


A <- ggplot(mapping=aes(x=PM25, y=fit)) + 
  geom_line(data=pred_plot1,lty = 1, lwd = 1,color = "#1F549BFF") +
  geom_ribbon(data=pred_plot1,aes(ymax = upper, 
                                  ymin = lower),
              alpha = 0.3,
              fill = "#1F549BFF") + 
  geom_line(data = pred_plot1, aes(y=0),color = "black",size = 1,linetype="dashed")+
  scale_y_continuous(expand = c(0,0), limits = c(-60,30))+
  scale_x_continuous(expand = c(0,0), limits = c(13.5,85))+
  theme(axis.text=element_text(size=50),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.position = c(0.75,0.85)
  )+theme_bw()
print(A)
ggsave(A,file=paste0("Figure_DoseResponsePM25_CFPS_TotalDonation.pdf"), width=6, height=4)

#donation to society
A <- ggplot(mapping=aes(x=PM25, y=fit)) + 
  geom_line(data=pred_plot4,lty = 2, lwd = 1, color = "#1F549BFF") +
  geom_ribbon(data=pred_plot4,aes(ymax = upper, 
                                  ymin = lower),
              alpha = 0.3,
              fill = "#1F549BFF") + 
  geom_line(data = pred_plot4, aes(y=0),color = "black",size = 1,linetype="dashed")+
  scale_color_manual(values = c("donation"="#1F549BFF"))+
  scale_y_continuous(expand = c(0,0), limits = c(-60,30))+
  scale_x_continuous(expand = c(0,0), limits = c(13.5,85))+
  theme(axis.text=element_text(size=20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.position = c(0.75,0.85)
  )+theme_bw()

#donation to relatives
ggplot(mapping=aes(x=PM25, y=fit)) + 
  geom_line(data=pred_plot2,lty = 3, lwd = 1,aes(colour = "donation")) +
  geom_ribbon(data=pred_plot2,aes(ymax = upper, 
                                  ymin = lower),
              alpha = 0.4,
              fill = "#1F549BFF",
              colour = NA ) + 
  geom_line(data = pred_plot2, aes(y=0),color = "black",size = 1,linetype="dashed")+
  scale_color_manual(values = c("donation"="#1F549BFF"))+
  scale_y_continuous(expand = c(0,0), limits = c(-60,30))+
  scale_x_continuous(expand = c(0,0), limits = c(13.5,85))+
  theme(axis.text=element_text(size=15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.position = c(0.75,0.85)
  )+theme_bw()

#donation to others
ggplot(mapping=aes(x=PM25, y=fit)) + 
  geom_line(data=pred_plot3,lty = 4, lwd = 1,aes(colour = "donation")) +
  geom_ribbon(data=pred_plot3,aes(ymax = upper, 
                                  ymin = lower),
              alpha = 0.4,
              fill = "#1F549BFF",
              colour = NA ) + 
  geom_line(data = pred_plot3, aes(y=0),color = "black",size = 1,linetype="dashed")+
  scale_color_manual(values = c("donation"="#1F549BFF"))+
  scale_y_continuous(expand = c(0,0), limits = c(-60,30))+
  scale_x_continuous(expand = c(0,0), limits = c(13.5,85))+
  theme(axis.text=element_text(size=15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.position = c(0.75,0.85)
  )+theme_bw()



########## personal level ########

# crude model         Annual_AnnualPM25_Lag02 -0.004888   0.002110  -2.317 0.020565 *    
summary(gam(log(DonationAmount+1)~Annual_AnnualPM25_Lag01+wordtest+IncomeLevel+s(countyid,bs = "re"),data = Data_person,na.action = na.omit))
# adjusted model      Annual_AnnualPM25_Lag02 -0.004896   0.002037  -2.404 0.016274 *    
summary(gam(log(DonationAmount+1)~Annual_AnnualPM25_Lag01+Annual_AnnualT2m_Lag02+wordtest+age+as.factor(eversmoke)+as.factor(gender)+PA_freq+as.factor(Edu_high)+IncomeLevel+s(countyid,bs = "re"),data = Data_person,na.action = na.omit))

log(DonationAmount + 1) ~ Annual_AnnualPM25_Lag02 + Annual_AnnualT2m_Lag02 + cognition + age + as.factor(eversmoke) + as.factor(gender) + PA_freq + as.factor(Edu_high) + IncomeLevel + HealthStatus + WorkdaySleep + Marriage + bmivalue + Depression + GDP_Capita



# dose-response   CFPS personal donation amount ~ Annual_AnnualPM25_Lag02
Data1 <- data.frame(Data_person)
Data1 = subset(Data1, select = -c(baseline_smoke,baseline_PAFreq,baseline_income,wordtest_diff,mathtest_diff,bmidiff))
Data1 = subset(Data1, select = -income_cat)
Data1$variable <- Data1$Annual_AnnualPM25_Lag02

Mod <- gam(log(DonationAmount+1)~s(Annual_AnnualPM25_Lag02,k=3)+Annual_AnnualT2m_Lag02+wordtest+age+as.factor(eversmoke)+as.factor(gender)+PA_freq+as.factor(Edu_high)+IncomeLevel+s(countyid,bs = "re"),data = Data_person,na.action = na.omit)
plot(Mod,main = "donation amount  personal level")

Mod <- gam(log(DonationAmount+1)~s(variable,k=3)+Annual_AnnualT2m_Lag02+wordtest+age+as.factor(eversmoke)+as.factor(gender)+PA_freq+as.factor(Edu_high)+IncomeLevel+s(countyid,bs = "re"),data = Data1,na.action = na.omit)
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
preds$fit <- preds$fit #- preds$fit[1]
preds_y <- data.frame(as.numeric(preds$fit) + outer(as.numeric(preds$se.fit), c(0, -1.96, 1.96)))
pred = data.frame(preds_y,data.frame(preds_x))
colnames(pred) <- c("y", "ymin","ymax",'x')
pred_plot_cfps_personal_donation_amount=data.frame(pred)

ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=pred_plot_cfps_personal_donation_amount,lty = 1, lwd = 1,colour = "red") +
  geom_ribbon(data=pred_plot_cfps_personal_donation_amount,
              aes(ymax = ymax, ymin = ymin),
              alpha = 0.5,
              fill = "red",
              colour = NA ) + 
  labs(x = "PM2.5", y = "Percentage change of Donation Amount") + 
  ggtitle("Percentage change of Donation Amount at person-level CFPS")+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme(axis.text=element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85)
  )







# reduced donation behavior~ PM2.5                 and odds ratio dose-response

# Annual_AnnualPM25_Lag02 -0.010706   0.002772  -3.863 0.000112 ***
summary(gam(DonationBehavior~Annual_AnnualPM25_Lag01+wordtest+IncomeLevel+s(countyid,bs = "re"),data = Data_person,na.action = na.omit,family = binomial))
# Annual_AnnualPM25_Lag02  -0.008466   0.002607  -3.248  0.00116 **     
summary(gam(DonationBehavior~Annual_AnnualPM25_Lag01+Annual_AnnualT2m_Lag02+wordtest+age+as.factor(eversmoke)+as.factor(gender)+PA_freq+as.factor(Edu_high)+IncomeLevel+s(countyid,bs = "re"),data = Data_person,na.action = na.omit,family = binomial))

Data1 <- data.frame(Data_person)
Data1 = subset(Data1, select = -c(baseline_smoke,baseline_PAFreq,baseline_income,wordtest_diff,mathtest_diff,bmidiff))
Data1 = subset(Data1, select = -income_cat)
Data1$variable <- Data1$Annual_AnnualPM25_Lag01

Mod <- gam(DonationBehavior~Annual_AnnualPM25_Lag01+Annual_AnnualT2m_Lag02+wordtest+age+as.factor(eversmoke)+as.factor(gender)+PA_freq+as.factor(Edu_high)+IncomeLevel+s(countyid,bs = "re"),data = Data_person,na.action = na.omit,family = binomial)
plot(Mod,main = "")



Mod <- gam(DonationBehavior~variable+Annual_AnnualT2m_Lag02+wordtest+age+as.factor(eversmoke)+as.factor(gender)+PA_freq+as.factor(Edu_high)+IncomeLevel+s(countyid,bs = "re"),data = Data1,na.action = na.omit,family = binomial)
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
Ref <- preds$fit[1]
fit <- exp(preds$fit - Ref)
lower <- exp(preds$fit - Ref - 1.96*preds$se.fit)
upper <- exp(preds$fit - Ref + 1.96*preds$se.fit)
preds_donation_behavior <- data.frame(PM25 = preds_x,fit = fit,lower = lower, upper = upper)

ggplot(data = preds_donation_behavior,aes(x = PM25))+
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "red",alpha = .5)+
  geom_line(aes(y=fit),color = "red",size = 1)+
  geom_line(aes(y=1),color = "black",size = 1,linetype="dashed")+
  scale_y_continuous("Odds ratio", expand = c(0, 0))+
  scale_x_continuous("PM2.5 level", expand = c(0, 0))+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85)
  )+
  ggtitle("odds ratio of donation behavior personal level CFPS")



##### CFPS person-level odds ratio of behavior and donation amount  in 1 plot  ###########
preds_donation_behavior = data.frame(preds_donation_behavior[2],preds_donation_behavior[3],preds_donation_behavior[4],preds_donation_behavior[1])
colnames(preds_donation_behavior) = c("y","ymin","ymax","x")

ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=preds_donation_behavior,lty = 1, lwd = 1, type = 'l',aes(colour = "Odds Ratio of Donation Behavior")) +
  geom_ribbon(data=preds_donation_behavior,aes(ymax = ymax, 
                                  ymin = ymin),
              alpha = 0.5,
              fill = "purple",
              colour = NA ) + 
  geom_line(data=pred_plot_cfps_personal_donation_amount,lty = 1, lwd = 1, type = 'l',aes(colour = "%Change of Donation Amount")) +
  geom_ribbon(data=pred_plot_cfps_personal_donation_amount,aes(ymax = ymax, ymin = ymin),
              alpha = 0.5,
              fill = "red",
              colour=NA)+
  ggtitle("CFPS person-level odds ratio of behavior and donation amount  in 1 plot")+
  theme_bw()+
  scale_color_manual(name = "type",values = c("%Change of Donation Amount"="red","Odds Ratio of Donation Behavior"="purple"))+
  scale_y_continuous("Odds ratio", expand = c(0, 0))+
  scale_x_continuous("PM2.5 level", expand = c(0, 0))+
  theme(axis.text=element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85)
  )+
scale_y_continuous(sec.axis = sec_axis(~. +0.00001, name = "%Change of Donation Amount"))












########## forest plot  table s2#######
library(foreign)
library(ggplot2)
library(plyr)
library(patchwork)
data_s2 = read_excel("tables2.xlsx",sheet = 'data')
data_s2_1 = read_excel('tables2_1.xlsx', sheet = 'data2')

data_crude = data_s2[2:6,1:4]
data_adjusted = data_s2[8:12,1:4]
data_behavior_odds_ratio = data_s2[c(1,7),1:4]
data_behavior_odds_ratio[1,1] = 'crude'
data_behavior_odds_ratio[2,1] = 'adjusted'
data_behavior_odds_ratio$x = as.factor(data_behavior_odds_ratio$x)
levels(data_behavior_odds_ratio$x) = c('crude','adjusted')

data_behavior_odds_ratio_crude = data_behavior_odds_ratio
data_behavior_odds_ratio_adjusted = data_behavior_odds_ratio
data_behavior_odds_ratio_crude[1,c(2,3,4)] = NA
data_behavior_odds_ratio_adjusted[2,c(2,3,4)] = NA



ggplot(data = data_crude) +
  geom_point(aes(x = x,y = mean),size = 4,shape = 1,fill = "#93cc82",color = "red" )+
  geom_errorbar(aes(x = x,y = mean, ymin = lower, ymax = upper),width = 0.2,size = 1,color = "red")+
  theme_bw()+
  ylab('%Change of Donation Amount')+
  scale_y_continuous("%Change of Donation Amount",limits = c(-30,5))+
  geom_hline(yintercept = 0,linetype = "dashed", size = 1.5)+
  theme(axis.text=element_text(size=20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85),
        axis.text.x = element_text(angle = 50,vjust = 0.9,hjust = 0.9)
  )+
  ggplot(data = data_adjusted) +
  geom_point(aes(x = x,y = mean),size = 4,shape = 1,fill = "#93cc82",color = "purple" )+
  geom_errorbar(aes(x = x,y = mean,ymin = lower, ymax = upper),width = 0.2,size = 1,colour = "purple")+
  theme_bw()+
  ylab('%Change of Donation Amount')+
  scale_y_continuous("%Change of Donation Amount",limits = c(-30,5))+
  geom_hline(yintercept = 0,linetype = "dashed", size = 1.5)+
  theme(axis.text=element_text(size=20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85),
        axis.text.x = element_text(angle = 50,vjust = 0.9,hjust = 0.9)
  )+
  ggplot() +
  geom_point(data = data_behavior_odds_ratio_crude, aes(x = x,y = mean),size = 4,shape = 1,fill = "#93cc82",color = "red" )+
  geom_errorbar(data = data_behavior_odds_ratio_crude, aes(x = x,y = mean, ymin = lower, ymax = upper),width = 0.08,size = 1,color = "red")+
  geom_point(data = data_behavior_odds_ratio_adjusted, aes(x = x,y = mean),size = 4,shape = 1,fill = "#93cc82",color = "purple" )+
  geom_errorbar(data = data_behavior_odds_ratio_adjusted, aes(x = x,y = mean, ymin = lower, ymax = upper),width = 0.08,size = 1,color = "purple")+
  theme_bw()+
  ylab('odds ratio')+
  scale_y_continuous("odds ratio",limits = c(0.8,1.033))+
  geom_hline(yintercept = 1,linetype = "dashed", size = 1.5)+
  theme(axis.text=element_text(size=20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85),
        axis.text.x = element_text(angle = 50,vjust = 0.9,hjust = 0.9)
  )

####### #######################

my_palette <- pal_nejm('default')(8)
my_palette
library("scales")
show_col(my_palette)


#install.packages("ggbreak")
library("ggbreak")

data_s2_crude_percentage = read_excel('tables2_1.xlsx', sheet = 'crude_percentage')
figure2_crude_percentage <- ggplot(data_s2_crude_percentage , aes(x = variable, y = mean, color=variable, linetype = ))+
  geom_hline(aes(yintercept=0), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
                              "Total donation_fam_crude" = "Total donation",
                              "Donation to society_fam_crude"="Donation to society",
                              "Donation to relatives_fam_crude"="Donation to relatives",
                              "Donation to others_fam_crude"="Donation to others",
                              "Donation amount_ind_crude" = "Donation amount"))+
  scale_y_continuous(name="% change",limits=c(-25,5), breaks=seq(-25,5,5),position = 'right')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='factors',
                     values = c("Total donation_fam_crude"="#BC3C29FF",
                                "Donation to society_fam_crude"="#0072B5FF",
                                "Donation to relatives_fam_crude"="#E18727FF", 
                                "Donation to others_fam_crude"="#20854EFF",
                                "Donation amount_ind_crude" ="#7876B1FF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure2_crude_percentage)


data_s2_adjusted_percentage = read_excel('tables2_1.xlsx', sheet = 'adjusted_percentage')
figure2_adjusted_percentage <- ggplot(data_s2_adjusted_percentage , aes(x = variable, y = mean, color=variable))+
  geom_hline(aes(yintercept=0), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "Total donation_fam_adjusted" = "Total donation",
    "Donation to society_fam_adjusted"="Donation to society",
    "Donation to relatives_fam_adjusted"="Donation to relatives",
    "Donation to others_fam_adjusted"="Donation to others",
    "Donation amount_ind_adjusted" = "Donation amount")
    )+
  scale_y_continuous(name="% change",limits=c(-25,5), breaks=seq(-25,5,5),position = 'right')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='factors',
                     values = c("Total donation_fam_adjusted"="#BC3C29FF",
                                "Donation to society_fam_adjusted"="#0072B5FF",
                                "Donation to relatives_fam_adjusted"="#E18727FF", 
                                "Donation to others_fam_adjusted"="#20854EFF",
                                "Donation amount_ind_adjusted" ="#7876B1FF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))

plot(figure2_adjusted_percentage)


data_s2_crude_odds_ratio = read_excel('tables2_1.xlsx', sheet = 'crude_odds_ratio')
figure2_crude_odds_ratio <- ggplot(data_s2_crude_odds_ratio , aes(x = variable, y = mean, color=variable))+
  geom_hline(aes(yintercept=1), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c("Donation behavior_ind_crude" = "Donation behavior"))+
  scale_y_continuous(name="odds ratio",limits=c(0.75,1.05), breaks=seq(0.75,1.05,0.05))+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='factors',
                     values = c("Donation behavior_ind_crude"="#6F99ADFF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure2_crude_odds_ratio)



data_s2_adjusted_odds_ratio = read_excel('tables2_1.xlsx', sheet = 'adjusted_odds_ratio')
figure2_adjusted_odds_ratio <- ggplot(data_s2_adjusted_odds_ratio , aes(x = variable, y = mean, color=variable))+
  geom_hline(aes(yintercept=1), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c("Donation behavior_ind_adjusted" = "Donation behavior"))+
  scale_y_continuous(name="odds ratio",limits=c(0.75,1.05), breaks=seq(0.75,1.05,0.05))+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='factors',
                     values = c("Donation behavior_ind_adjusted"="#6F99ADFF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure2_adjusted_odds_ratio)

























####### mediation analysis ########
# word test mediated PM2.5 - donation behavior
Mod  <- gam(DonationBehavior~Annual_AnnualPM25_Lag01+Annual_AnnualT2m_Lag02+wordtest+age+eversmoke+gender+PA_freq+Edu_high+IncomeLevel,data = Data_person,na.action = na.omit,family = binomial)
Mod2 <- gam(wordtest~Annual_AnnualPM25_Lag01+Annual_AnnualT2m_Lag02+age+eversmoke+gender+PA_freq+Edu_high+IncomeLevel,data = Data_person[which(!is.na(Data_person$DonationBehavior)),],na.action = na.omit)

congam<-mediation::mediate(Mod2,Mod,treat="Annual_AnnualPM25_Lag01",mediator="wordtest",sims=20,boot=TRUE)
#Causal Mediation Analysis 
#Nonparametric Bootstrap Confidence Intervals with the Percentile Method
#Estimate 95% CI Lower 95% CI Upper p-value    
#ACME (control)            0.000794     0.000690         0.00  <2e-16 ***
#  ACME (treated)            0.000790     0.000688         0.00  <2e-16 ***
#  ADE (control)            -0.001352    -0.001560         0.00  <2e-16 ***
#  ADE (treated)            -0.001356    -0.001564         0.00  <2e-16 ***
#  Total Effect             -0.000562    -0.000755         0.00  <2e-16 ***
#  Prop. Mediated (control) -1.411865    -2.474316        -1.06  <2e-16 ***
#  Prop. Mediated (treated) -1.406092    -2.466253        -1.06  <2e-16 ***
#  ACME (average)            0.000792     0.000689         0.00  <2e-16 ***
#  ADE (average)            -0.001354    -0.001562         0.00  <2e-16 ***
#  Prop. Mediated (average) -1.408978    -2.470284        -1.06  <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Sample Size Used: 21564 
#Simulations: 10 
summary(congam)
plot(congam)

# Causal Mediation Analysis 
# 
# Nonparametric Bootstrap Confidence Intervals with the Percentile Method
# 
# Estimate 95% CI Lower 95% CI Upper p-value    
# ACME (control)            0.000817     0.000757            0  <2e-16 ***
#   ACME (treated)            0.000814     0.000754            0  <2e-16 ***
#   ADE (control)            -0.001402    -0.001612            0  <2e-16 ***
#   ADE (treated)            -0.001406    -0.001615            0  <2e-16 ***
#   Total Effect             -0.000589    -0.000809            0  <2e-16 ***
#   Prop. Mediated (control) -1.388310    -3.507917           -1  <2e-16 ***
#   Prop. Mediated (treated) -1.382465    -3.495758           -1  <2e-16 ***
#   ACME (average)            0.000815     0.000755            0  <2e-16 ***
#   ADE (average)            -0.001404    -0.001614            0  <2e-16 ***
#   Prop. Mediated (average) -1.385388    -3.501837           -1  <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Sample Size Used: 21564 
# 
# 
# Simulations: 10 




