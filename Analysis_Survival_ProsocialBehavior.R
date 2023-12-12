# PA definitin change to PA score
library(readxl)
library(dplyr)
library(foreign)
library(readstata13)
# library(gam)
library(mgcv)
library(splines)
library(lme4)
library(nlme)
library(dlnm)
library(survival)
library(fields)
library(plot3D)
library(plotly)
library(mice)
library(lmtest)
library(foreach)
library(doParallel)
library(tiff)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(ncdf4)
library(visreg)
library(quantreg)
library(mediation)
# WkDir <- "E:\\CloudStorage\\Document\\Research\\charls\\Temperature_Temp\\"
# WkDir <- "D:\\nutstore\\Document\\Research\\charls\\Temperature_Temp\\"
# WkDir <- "E:\\CloudStorage\\Document\\Research\\charls\\Mercury_Temp\\"
WkDir <- "E:\\CloudStorage\\Document\\Research\\charls\\Temp\\"
# WkDir <- "E:\\CloudStorage\\Document\\Research\\charls\\Temperature_Temp\\"

WkDir <- "C:\\Users\\FGQ4\\OneDrive - mails.tsinghua.edu.cn\\airpolution_prosocial\\"
WkDir <- '/Users/guoqingfeng/Library/CloudStorage/OneDrive-mails.tsinghua.edu.cn/airpolution_prosocial'
setwd(WkDir)

####### read data ########
Data<- readRDS(file="Code_Assemble_Data_2011_2018_PA_AP_interaction.rds")

## doing some sort of imputation
Data_imputed <- readRDS(file="Code_imputation_merged_PA_AP_interaction.rds")
Covariates<-c("PhysicalDisability","BrainDamage","VisionProblem","HearingProblem","SpeechImpediment","SelfCommentHealth","Yibao","UrbanResidentInsurance","WorkLastMonth","SleepLastMonth","PurposeIntensivePA","PurposeModeratePA","PurposeLightPA","Happy","Depressed","Lonely","TroubleMind","Fearful","Hopeful","AirConditioner","HeatingSystem","ConcreteBrickBuilding","Resident4Business","Income","AlcoholDrinking","EverSmoked","MaritalStatus","AgeEducation","Diastolic","Systolic")
Covariates_Full <- paste0(Covariates,"_Full")
Temp <- Data_imputed[,Covariates]
names(Temp) <- Covariates_Full
Data <- cbind(Data,Temp[,Covariates_Full])

## add location/longtidue
CrossWalk <- read_excel("crosswalk.xlsx",sheet = "County_crosswalk_LatLon")
CrossWalk$Code <- as.numeric(CrossWalk$Code)
Data <- left_join(x = Data, y = CrossWalk, by = c("Code" = "Code"), na_matches = "never")


Data$ModeratePAScore <- NA
Data[which(Data$ModeratePA10min     == "2 No"),"ModeratePAScore"] <- 0 
Data[which(Data$ModeratePA10min     == "1 Yes" & is.na(Data$ModeratePA2hr)),"ModeratePAScore"] <- 1 
Data[which(Data$ModeratePA2hr == "1 <2 Hours" & Data$ModeratePA30min == "2 >=30 Minutes"),"ModeratePAScore"] <- 2
Data[which(Data$ModeratePA2hr == "1 <2 Hours" & Data$ModeratePA30min == "1 <30 Minutes"),"ModeratePAScore"] <- 1
Data[which(Data$ModeratePA2hr == "1 <2 Hours" & is.na(Data$ModeratePA30min)),"ModeratePAScore"] <- 1.5
Data[which(Data$ModeratePA2hr == "2 >=2 Hours" & Data$ModeratePA4hr == "1 <4 Hours"),"ModeratePAScore"] <- 3
Data[which(Data$ModeratePA2hr == "2 >=2 Hours" & Data$ModeratePA4hr == "2 >=4 Hours"),"ModeratePAScore"] <- 4
Data[which(Data$ModeratePA2hr == "2 >=2 Hours" & is.na(Data$ModeratePA4hr)),"ModeratePAScore"] <- 3.5
Data$LightPAScore <- NA
Data[which(Data$LightPA10min     == "2 No"),"LightPAScore"] <- 0 
Data[which(Data$LightPA10min     == "1 Yes" & is.na(Data$LightPA2hr)),"LightPAScore"] <- 1 
Data[which(Data$LightPA2hr == "1 <2 Hours" & Data$LightPA30min == "2 >=30 Minutes"),"LightPAScore"] <- 2
Data[which(Data$LightPA2hr == "1 <2 Hours" & Data$LightPA30min == "1 <30 Minutes"),"LightPAScore"] <- 1
Data[which(Data$LightPA2hr == "1 <2 Hours" & is.na(Data$LightPA30min)),"LightPAScore"] <- 1.5
Data[which(Data$LightPA2hr == "2 >=2 Hours" & Data$LightPA4hr == "1 <4 Hours"),"LightPAScore"] <- 3
Data[which(Data$LightPA2hr == "2 >=2 Hours" & Data$LightPA4hr == "2 >=4 Hours"),"LightPAScore"] <- 4
Data[which(Data$LightPA2hr == "2 >=2 Hours" & is.na(Data$LightPA4hr)),"LightPAScore"] <- 3.5
Data$IntensivePAScore <- NA
Data[which(Data$IntensivePA10min     == "2 No"),"IntensivePAScore"] <- 0 
Data[which(Data$IntensivePA10min     == "1 Yes" & is.na(Data$IntensivePA2hr)),"IntensivePAScore"] <- 1 
Data[which(Data$IntensivePA2hr == "1 <2 Hours" & Data$IntensivePA30min == "2 >=30 Minutes"),"IntensivePAScore"] <- 2
Data[which(Data$IntensivePA2hr == "1 <2 Hours" & Data$IntensivePA30min == "1 <30 Minutes"),"IntensivePAScore"] <- 1
Data[which(Data$IntensivePA2hr == "1 <2 Hours" & is.na(Data$IntensivePA30min)),"IntensivePAScore"] <- 1.5
Data[which(Data$IntensivePA2hr == "2 >=2 Hours" & Data$IntensivePA4hr == "1 <4 Hours"),"IntensivePAScore"] <- 3
Data[which(Data$IntensivePA2hr == "2 >=2 Hours" & Data$IntensivePA4hr == "2 >=4 Hours"),"IntensivePAScore"] <- 4
Data[which(Data$IntensivePA2hr == "2 >=2 Hours" & is.na(Data$IntensivePA4hr)),"IntensivePAScore"] <- 3.5
Data$LightPAScore <- 3.3*Data$LightPAScore*Data$LightPADays
Data$ModeratePAScore <- 4*Data$ModeratePAScore*Data$ModeratePADays
Data$IntensivePAScore <-  8*Data$IntensivePAScore*Data$IntensivePADays

Data[which(is.na(Data$LightPAScore)),"LightPAScore"] <- 0 
Data[which(is.na(Data$ModeratePAScore)),"ModeratePAScore"] <- 0 
Data[which(is.na(Data$IntensivePAScore)),"IntensivePAScore"] <- 0 
Data$PAScore <- Data$LightPAScore+Data$ModeratePAScore+Data$IntensivePAScore

OutcomeList <- c("Hypertension","Dyslipidemia","Diabetes","Cancer",  "LungDiseases","LiverDisease","HeartAttack","Stroke",  "KidneyDisease","StomachDisease","EmotionalProblem","MemoryDisease","Arthritis","Asthma","FracturedHip","TrafficAccident","FallenDown")

####### some functions of cox model ################

FormatResult<-function(Beta,Mod,ModelName,Outcome)
{
  # ModelName <- "model"
  # Outcome <- "xxx"
  # option <- "gam"
  B <- as.data.frame(summary(Mod)$coefficients)
  B$Variable <- row.names(B)
  B$Model <- ModelName
  B$Outcome <- Outcome
  B$formula <- paste(deparse(Mod$formula, width.cutoff = 500), collapse="")
  
  B[nrow(B)+1,] <-c(NA,NA,NA,NA,Mod$nevent,"N of events",ModelName,Outcome,NA)
  B[nrow(B)+1,] <-c(NA,NA,NA,NA,Mod$n,"N of observations",ModelName,Outcome,NA)
  B[nrow(B)+1,] <-c(NA,NA,NA,NA,AIC(Mod),"AIC",ModelName,Outcome,NA)
  
  if(is.null(Beta))
  {
    Beta <- B
  }else
  {
    Beta <- rbind(Beta,B)
  }
  return(Beta)
}

FormatResult_gam<-function(Beta,Mod,ModelName,Outcome,option)
{
  # Mod = Mod
  # ModelName <- "model"
  # # formula <- paste(deparse(Mod$formula, width.cutoff = 500), collapse="")
  # option <- "gam"
  
  if("gam" == option)
  {
    B <- as.data.frame(summary(Mod)$p.table)
    B$formula <- paste(deparse(Mod$formula, width.cutoff = 500), collapse="")
    B$Variable <- row.names(B)
    B$Model <- ModelName
    B$Outcome <- Outcome
    
    Temp <- data.frame(IQR = unlist(lapply(Mod$model, IQR)),Variable = names(Mod$model))
    
    B <- left_join(x = B , y = Temp, by = c("Variable"="Variable"),na_matches = "never")
  }else if("cox" == option)
  {
      B <- as.data.frame(summary(Mod)$p.table)
      B$formula <- paste(deparse(Mod$formula, width.cutoff = 500), collapse="")
      B$Variable <- row.names(B)
      B$Model <- ModelName
  }else if("rq" == option)
  {
    B <- as.data.frame(summary(Mod,se = "iid")$coefficients)
    if(B$`Std. Error`[1] < 0.001)## iid is not working
    {
      B <- as.data.frame(summary(Mod,se = "ker")$coefficients)
    }
    names(B) <- c("Estimate","Std. Error","t value","Pr(>|t|)")  
    B$formula <- paste(deparse(Mod$formula, width.cutoff = 500), collapse="")
    B$Variable <- row.names(B)
    B$Model <- ModelName
  }else if("AFT" == option)
  {
    B <- as.data.frame(summary(Mod)$table)
    B$formula <- paste(deparse(Mod$call, width.cutoff = 500), collapse="")
    B$Variable <- row.names(B)
    B$Model <- ModelName
  }

  
  B[nrow(B)+1,] <- NA
  B[nrow(B),"Variable"] <- "AIC"
  B[nrow(B),"Estimate"] <- AIC(Mod)
  B[nrow(B),"Model"] <- ModelName
  
  if(is.null(Beta))
  {
    Beta <- B
  }else
  {
    Beta <- rbind(Beta,B)
  }
  return(Beta)
}

# beta value of dose-response curve
# daly
MC <- function(mean_b,upper_b,lower_b,mean_daly,upper_daly,lower_daly)
{
  N <- 10000
  
  # mean_b <- 0
  # sd_b <- 1
  # 
  # mean_daly <- 0
  # sd_daly <- 1
  # 
  # mean_b <- Result[2,'point']
  # upper_b <- Result[2,'upper']
  # mean_daly <- Result[2,'baselineDALY']
  # upper_daly <- Result[2,'baselineDALY_upper']

  sd_b <- abs(upper_b - mean_b)/1.96
  sd_daly <- abs(upper_daly - mean_daly)/1.96

  b <- rnorm(N,mean = mean_b, sd = sd_b)
  daly <- rnorm(N,mean = mean_daly, sd = sd_daly)

  result <- exp(b)*daly
  
  ## non random version -- for debugging purpose
  return(c(exp(mean_b)*mean_daly,exp(upper_b)*mean_daly,exp(lower_b)*mean_daly))

  ## random version
  # return(c(mean(result,na.rm=T),quantile(result,c(0.05,0.95),na.rm=T)))
}

# i = 4000
# MC(Result[i,'point'],Result[i,'upper'],Result[i,'lower'],Result[i,'baselineDALY'],Result[i,'baselineDALY_upper'],Result[i,'baselineDALY_lower'])

###### liner models for donation amount #####
Data$MentalHealth <- Data$TroubleMind + Data$Depressed + Data$Fearful + Data$Lonely - Data$Happy - Data$Hopeful
Data$wave1 <- as.numeric(Data$iyear >= 2013)
## create IV
Mod <- gam(MonthPM25_monthlag01 ~ MonthPBL_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Data$MonthPM25_monthlag01_Pred <- predict(Mod,newdata = Data)
cor(Data$MonthPM25_monthlag01_Pred,Data$MonthPM25_monthlag01,use ="complete.obs")

## for donation amount
Beta <- NULL
#########Donations2Society annuallag01 monthlag01        #################
Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_DID","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_DID","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01_Pred+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"IV","Donations2Society","gam")

#sensitive analysis annuallag01
Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMete","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutGeogrpahicVariable","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutHealthStatus","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMentalHealth","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingConditions","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMedicalInsurance","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingStyle","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutSES","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ Annual_AnnualPM25_Lag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutDemographic","Donations2Society","gam")

##sensitive analysis monthlag01
Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMete","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutGeogrpahicVariable","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutHealthStatus","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMentalHealth","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingConditions","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMedicalInsurance","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingStyle","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutSES","Donations2Society","gam")

Mod <- gam(log(Donations2Society+1) ~ MonthPM25_monthlag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutDemographic","Donations2Society","gam")

write.csv(Beta,"Charls_Donation.csv")


###### linear model for prosocial behavior ##############
Beta <- NULL
## association  annuallag01
Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational","HelpFamilyFriends","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational","CommunityOrg","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational","VoluntaryCharityWork","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational","CareSick","gam")

## did annuallag01
Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_DID","HelpFamilyFriends","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_DID","CommunityOrg","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_DID","VoluntaryCharityWork","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_DID","CareSick","gam")

## month PM2.5 associational
Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational","HelpFamilyFriends","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational","CommunityOrg","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational","VoluntaryCharityWork","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational","CareSick","gam")

## did 
Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_DID","HelpFamilyFriends","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_DID","CommunityOrg","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_DID","VoluntaryCharityWork","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01*wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_DID","CareSick","gam")

## IV
Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01_Pred+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"IV","HelpFamilyFriends","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01_Pred+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"IV","CommunityOrg","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01_Pred+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"IV","VoluntaryCharityWork","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01_Pred+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"IV","CareSick","gam")

## annuallag01  sensitive analysis 
Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMete","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutGeogrpahicVariable","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutHealthStatus","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMentalHealth","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingConditions","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMedicalInsurance","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingStyle","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutSES","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ Annual_AnnualPM25_Lag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutDemographic","HelpFamilyFriends","gam")


Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMete","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutGeogrpahicVariable","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutHealthStatus","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMentalHealth","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingConditions","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMedicalInsurance","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingStyle","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutSES","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ Annual_AnnualPM25_Lag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutDemographic","CommunityOrg","gam")


Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMete","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutGeogrpahicVariable","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutHealthStatus","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMentalHealth","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingConditions","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMedicalInsurance","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingStyle","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutSES","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ Annual_AnnualPM25_Lag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutDemographic","VoluntaryCharityWork","gam")


Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMete","CareSick","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutGeogrpahicVariable","CareSick","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutHealthStatus","CareSick","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMentalHealth","CareSick","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingConditions","CareSick","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutMedicalInsurance","CareSick","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutLivingStyle","CareSick","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutSES","CareSick","gam")

Mod <- gam(CareSick ~ Annual_AnnualPM25_Lag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Annual_Associational_WithoutDemographic","CareSick","gam")

## monthlag01   sensitive analysis 

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMete","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutGeogrpahicVariable","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutHealthStatus","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMentalHealth","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingConditions","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMedicalInsurance","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingStyle","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutSES","HelpFamilyFriends","gam")

Mod <- gam(HelpFamilyFriends ~ MonthPM25_monthlag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutDemographic","HelpFamilyFriends","gam")


Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMete","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutGeogrpahicVariable","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutHealthStatus","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMentalHealth","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingConditions","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMedicalInsurance","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingStyle","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutSES","CommunityOrg","gam")

Mod <- gam(CommunityOrg ~ MonthPM25_monthlag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutDemographic","CommunityOrg","gam")


Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMete","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutGeogrpahicVariable","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutHealthStatus","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMentalHealth","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingConditions","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMedicalInsurance","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingStyle","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutSES","VoluntaryCharityWork","gam")

Mod <- gam(VoluntaryCharityWork ~ MonthPM25_monthlag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutDemographic","VoluntaryCharityWork","gam")


Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMete","CareSick","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutGeogrpahicVariable","CareSick","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutHealthStatus","CareSick","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMentalHealth","CareSick","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingConditions","CareSick","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutMedicalInsurance","CareSick","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+Age+as.factor(Gender)+as.factor(Income_Full)+AgeEducation+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutLivingStyle","CareSick","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutSES","CareSick","gam")

Mod <- gam(CareSick ~ MonthPM25_monthlag01+PAScore+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data,family = binomial)
Beta <- FormatResult_gam(Beta,Mod,"Month_Associational_WithoutDemographic","CareSick","gam")

write.csv(Beta,"Charls_ProsocialBehavior.csv")



###### dose-response ##### archived######
# donation2society --annual PM25 (associational and DID)
# Annual_AnnualPM25_Lag02                           -5.023e-03  4.508e-04 -11.144  < 2e-16 ***
Mod1 <- gam(log(Donations2Society+1) ~ s(Annual_AnnualPM25_Lag01,k=3)+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)
Data$Annual_AnnualPM25_Lag01_DID <- Data$Annual_AnnualPM25_Lag01*Data$wave1
Mod2 <- gam(log(Donations2Society+1) ~ s(Annual_AnnualPM25_Lag01_DID,k=3)+Annual_AnnualPM25_Lag01+wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
           +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
           +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
           +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
           ,data = Data)

preds_x = seq(20,90,0.1)
Data2 <- Data[complete.cases(Data[,c("Annual_AnnualPM25_Lag01","PAScore","Age","Gender","EverSmoked_Full","Income_Full","AgeEducation","AlcoholDrinking","MaritalStatus","PhysicalDisability_Full","BrainDamage_Full","VisionProblem_Full","HearingProblem_Full","SpeechImpediment_Full","Annual_AnnualT2m_Lag02","Annual_AnnualRH_Lag02","Annual_AnnualPrecipitation_Lag02","GDP_Capita","areatype","urban_nbs","Yibao_Full","UrbanResidentInsurance_Full","WorkLastMonth_Full","SleepLastMonth_Full","Happy_Full","Depressed_Full","Lonely_Full","TroubleMind_Full","Fearful_Full","Hopeful_Full","AirConditioner_Full","HeatingSystem_Full","ConcreteBrickBuilding_Full","Resident4Business_Full")]),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$Annual_AnnualPM25_Lag01 <- preds_x
preds1 <- predict.gam(Mod1, newdata = newdata,se.fit = TRUE)
Ref <- preds1$fit[1]
fit1 <- 100*(exp(preds1$fit - Ref) - 1)
lower <- 100*(exp(preds1$fit - Ref - 1.96*preds1$se.fit)-1)
upper <- 100*(exp(preds1$fit - Ref + 1.96*preds1$se.fit)-1)
preds1 <- data.frame(PM25 = preds_x,fit1 = fit1,lower1 = lower, upper1 = upper)

newdata$Annual_AnnualPM25_Lag01 <- preds_x
preds2 <- predict.gam(Mod2, newdata = newdata,se.fit = TRUE)
Ref <- preds2$fit[1]
fit1 <- 100*(exp(preds2$fit - Ref) - 1)
lower <- 100*(exp(preds2$fit - Ref - 1.96*preds2$se.fit)-1)
upper <- 100*(exp(preds2$fit - Ref + 1.96*preds2$se.fit)-1)
preds2 <- data.frame(PM25 = preds_x,fit2 = fit1,lower2 = lower, upper2 = upper)

preds <- left_join(x = preds1,y = preds2 , by =c("PM25"="PM25"),na_matches = "never")
A<-ggplot(data = preds,aes(x = PM25))+
  geom_ribbon(aes(ymin = lower1,ymax = upper1),fill = "purple",alpha = .5)+
  geom_line(aes(y=fit1,color = "Association"),size = 1)+
  geom_ribbon(aes(ymin = lower2,ymax = upper2),fill = "red",alpha = .5)+
  geom_line(aes(y=fit2,color = "Causal"),size = 1)+
  geom_line(aes(y=0),color = "black",size = 1,linetype="dashed")+
  scale_y_continuous("% change in donation", expand = c(0, 0))+
  scale_x_continuous("Annual PM2.5", expand = c(0, 0))+
  scale_color_manual(name = "PM2.5 type",values = c("Causal"="red","Association"="purple"))+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85)
  )
print(A)
ggsave(A,file="Figure_DoseResponsePM25_CharlsDonation2Society.pdf", width=6, height=4)



######## prosocial behavior association with  monthly/annual PM25 #######

library(ggsci)
my_palette <- 	pal_npg('nrc')(8)
my_palette
library("scales")
show_col(my_palette)



Mod1 <- gam(log(Donations2Society+1) ~ s(Annual_AnnualPM25_Lag01,k=3)+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data)
Mod2 <- gam(log(Donations2Society+1) ~ s(MonthPM25_monthlag01,k=3)+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data)

preds_x = seq(20,90,0.1)
Data2 <- Data[complete.cases(Data[,c("Annual_AnnualPM25_Lag01","MonthPM25_monthlag01","PAScore","Age","Gender","EverSmoked_Full","Income_Full","AgeEducation","AlcoholDrinking","MaritalStatus","PhysicalDisability_Full","BrainDamage_Full","VisionProblem_Full","HearingProblem_Full","SpeechImpediment_Full","Annual_AnnualT2m_Lag02","Annual_AnnualRH_Lag02","Annual_AnnualPrecipitation_Lag02","GDP_Capita","areatype","urban_nbs","Yibao_Full","UrbanResidentInsurance_Full","WorkLastMonth_Full","SleepLastMonth_Full","Happy_Full","Depressed_Full","Lonely_Full","TroubleMind_Full","Fearful_Full","Hopeful_Full","AirConditioner_Full","HeatingSystem_Full","ConcreteBrickBuilding_Full","Resident4Business_Full")]),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$Annual_AnnualPM25_Lag01 <- preds_x

preds1 <- predict.gam(Mod1, newdata = newdata,se.fit = TRUE)
Ref <- preds1$fit[1]
fit1 <- 100*(exp(preds1$fit - Ref) - 1)
lower <- 100*(exp(preds1$fit - Ref - 1.96*preds1$se.fit)-1)
upper <- 100*(exp(preds1$fit - Ref + 1.96*preds1$se.fit)-1)
preds1 <- data.frame(PM25 = preds_x,fit1 = fit1,lower1 = lower, upper1 = upper)

newdata$MonthPM25_monthlag01 <- preds_x
preds2 <- predict.gam(Mod2, newdata = newdata,se.fit = TRUE)
Ref <- preds2$fit[1]
fit1 <- 100*(exp(preds2$fit - Ref) - 1)
lower <- 100*(exp(preds2$fit - Ref - 1.96*preds2$se.fit)-1)
upper <- 100*(exp(preds2$fit - Ref + 1.96*preds2$se.fit)-1)
preds2 <- data.frame(PM25 = preds_x,fit2 = fit1,lower2 = lower, upper2 = upper)

preds <- left_join(x = preds1,y = preds2 , by =c("PM25"="PM25"),na_matches = "never")
A<-ggplot(data = preds,aes(x = PM25))+
  geom_ribbon(aes(ymin = lower1,ymax = upper1),fill = "#1F549BFF",alpha = .3)+
  geom_line(aes(y=fit1,color = "Annual PM2.5"),size = 1)+
  geom_ribbon(aes(ymin = lower2,ymax = upper2),fill = "#E64B35FF",alpha = .3)+
  geom_line(aes(y=fit2,color = "Monthly PM2.5"),size = 1)+
  geom_line(aes(y=0),color = "black",size = 1,linetype="dashed")+
  scale_y_continuous("% Change in donation amount", expand = c(0, 0))+
  scale_x_continuous("Annual PM2.5", expand = c(0, 0))+
  scale_color_manual(name = "PM2.5 type",values = c("Monthly PM2.5"="#E64B35FF","Annual PM2.5"="#1F549BFF"))+
  theme_bw()+
  theme(axis.text=element_text(size=25),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85)
  )
print(A)
ggsave(A,file="Figure_DoseResponsePM25_CharlsDonation2Society_monthly-annual.pdf", width=6, height=4)


VarList <- c("HelpFamilyFriends","CommunityOrg","VoluntaryCharityWork","CareSick")
for(i in 1:length(VarList))
{
  Data[,"Outcome"] <- Data[,VarList[i]]
    
  Mod1 <- gam(Outcome ~ s(Annual_AnnualPM25_Lag01,k=3)+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
              +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
              +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
              +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
              ,data = Data,family = binomial)
  Mod2 <- gam(Outcome ~ s(MonthPM25_monthlag01,k=3)+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
              +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
              +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
              +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
              ,data = Data,family = binomial)
  
  preds_x = seq(20,90,0.1)
  Data2 <- Data[complete.cases(Data[,c("Annual_AnnualPM25_Lag01","PAScore","Age","Gender","EverSmoked_Full","Income_Full","AgeEducation","AlcoholDrinking","MaritalStatus","PhysicalDisability_Full","BrainDamage_Full","VisionProblem_Full","HearingProblem_Full","SpeechImpediment_Full","Annual_AnnualT2m_Lag02","Annual_AnnualRH_Lag02","Annual_AnnualPrecipitation_Lag02","GDP_Capita","areatype","urban_nbs","Yibao_Full","UrbanResidentInsurance_Full","WorkLastMonth_Full","SleepLastMonth_Full","Happy_Full","Depressed_Full","Lonely_Full","TroubleMind_Full","Fearful_Full","Hopeful_Full","AirConditioner_Full","HeatingSystem_Full","ConcreteBrickBuilding_Full","Resident4Business_Full")]),]
  newdata <- Data2[c(rep(1,length(preds_x))),]
  newdata$Annual_AnnualPM25_Lag01 <- preds_x
  preds1 <- predict.gam(Mod1, newdata = newdata,se.fit = TRUE)
  Ref <- preds1$fit[1]
  fit1 <- exp(preds1$fit - Ref)
  lower <- exp(preds1$fit - Ref - 1.96*preds1$se.fit)
  upper <- exp(preds1$fit - Ref + 1.96*preds1$se.fit)
  preds1 <- data.frame(PM25 = preds_x,fit1 = fit1,lower1 = lower, upper1 = upper)
  
  newdata$MonthPM25_monthlag01 <- preds_x
  preds2 <- predict.gam(Mod2, newdata = newdata,se.fit = TRUE)
  Ref <- preds2$fit[1]
  fit1 <- exp(preds2$fit - Ref) 
  lower <- exp(preds2$fit - Ref - 1.96*preds2$se.fit)
  upper <- exp(preds2$fit - Ref + 1.96*preds2$se.fit)
  preds2 <- data.frame(PM25 = preds_x,fit2 = fit1,lower2 = lower, upper2 = upper)
  
  preds <- left_join(x = preds1,y = preds2 , by =c("PM25"="PM25"),na_matches = "never")
  A<-ggplot(data = preds,aes(x = PM25))+
    geom_ribbon(aes(ymin = lower1,ymax = upper1),fill = "#1F549BFF",alpha = .3)+
    geom_line(aes(y=fit1,color = "Annual PM2.5"),size = 1)+
    geom_ribbon(aes(ymin = lower2,ymax = upper2),fill = "#E64B35FF",alpha = .3)+
    geom_line(aes(y=fit2,color = "Monthly PM2.5"),size = 1)+
    geom_line(aes(y=1),color = "black",size = 1,linetype="dashed")+
    scale_y_continuous("Odds ratio", expand = c(0, 0))+
    scale_x_continuous("PM2.5 level", expand = c(0, 0))+
    scale_color_manual(name = "PM2.5 type",values = c("Monthly PM2.5"="#E64B35FF","Annual PM2.5"="#1F549BFF"))+
    theme_bw()+
    theme(axis.text=element_text(size=25),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title=element_text(size=15),
          legend.position = c(0.75,0.85)
    )
  print(A)
  ggsave(A,file=paste0("Figure_DoseResponsePM25_",VarList[i],".pdf"), width=6, height=4)
}


######### DID model for behavior&donation--monthly/annual PM25###########
Data$Annual_AnnualPM25_Lag01_DID <- Data$Annual_AnnualPM25_Lag01*Data$wave1
Mod1 <- gam(log(Donations2Society+1) ~ s(Annual_AnnualPM25_Lag01_DID,k=3)+Annual_AnnualPM25_Lag01+wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data)

Data$MonthPM25_monthlag01_DID <- Data$MonthPM25_monthlag01*Data$wave1
Mod2 <- gam(log(Donations2Society+1) ~ s(MonthPM25_monthlag01_DID,k=3)+MonthPM25_monthlag01+wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data)

preds_x = seq(20,90,0.1)
Data2 <- Data[complete.cases(Data[,c("Annual_AnnualPM25_Lag01","MonthPM25_monthlag01","Annual_AnnualPM25_Lag01_DID","wave1","MonthPM25_monthlag01_DID","PAScore","Age","Gender","EverSmoked_Full","Income_Full","AgeEducation","AlcoholDrinking","MaritalStatus","PhysicalDisability_Full","BrainDamage_Full","VisionProblem_Full","HearingProblem_Full","SpeechImpediment_Full","Annual_AnnualT2m_Lag02","Annual_AnnualRH_Lag02","Annual_AnnualPrecipitation_Lag02","GDP_Capita","areatype","urban_nbs","Yibao_Full","UrbanResidentInsurance_Full","WorkLastMonth_Full","SleepLastMonth_Full","Happy_Full","Depressed_Full","Lonely_Full","TroubleMind_Full","Fearful_Full","Hopeful_Full","AirConditioner_Full","HeatingSystem_Full","ConcreteBrickBuilding_Full","Resident4Business_Full")]),]
newdata <- Data2[c(rep(1,length(preds_x))),]

newdata$Annual_AnnualPM25_Lag01_DID <- preds_x
preds1 <- predict.gam(Mod1, newdata = newdata,se.fit = TRUE)
Ref <- preds1$fit[1]
fit1 <- 100*(exp(preds1$fit - Ref) - 1)
lower <- 100*(exp(preds1$fit - Ref - 1.96*preds1$se.fit)-1)
upper <- 100*(exp(preds1$fit - Ref + 1.96*preds1$se.fit)-1)
preds1 <- data.frame(PM25 = preds_x,fit1 = fit1,lower1 = lower, upper1 = upper)

newdata$MonthPM25_monthlag01_DID <- preds_x
preds2 <- predict.gam(Mod2, newdata = newdata,se.fit = TRUE)
Ref <- preds2$fit[1]
fit1 <- 100*(exp(preds2$fit - Ref) - 1)
lower <- 100*(exp(preds2$fit - Ref - 1.96*preds2$se.fit)-1)
upper <- 100*(exp(preds2$fit - Ref + 1.96*preds2$se.fit)-1)
preds2 <- data.frame(PM25 = preds_x,fit2 = fit1,lower2 = lower, upper2 = upper)

preds <- left_join(x = preds1,y = preds2 , by =c("PM25"="PM25"),na_matches = "never")
A<-ggplot(data = preds,aes(x = PM25))+
  geom_ribbon(aes(ymin = lower1,ymax = upper1),fill = "#1F549BFF",alpha = .3)+
  geom_line(aes(y=fit1,color = "Annual PM2.5"),size = 1)+
  geom_ribbon(aes(ymin = lower2,ymax = upper2),fill = "#E64B35FF",alpha = .3)+
  geom_line(aes(y=fit2,color = "Monthly PM2.5"),size = 1)+
  geom_line(aes(y=0),color = "black",size = 1,linetype="dashed")+
  scale_y_continuous("% change in donation amount", expand = c(0, 0))+
  scale_x_continuous("Annual PM2.5", expand = c(0, 0))+
  scale_color_manual(name = "PM2.5 type",values = c("Monthly PM2.5"="#E64B35FF","Annual PM2.5"="#1F549BFF"))+
  theme_bw()+
  theme(axis.text=element_text(size=30),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.position = c(0.75,0.85)
  )
print(A)
ggsave(A,file="Figure_DoseResponsePM25_CharlsDonation2Society_DID_did.pdf", width=6, height=4)


VarList <- c("HelpFamilyFriends","CommunityOrg","VoluntaryCharityWork","CareSick")
for(i in 1:length(VarList))
{
  Data[,"Outcome"] <- Data[,VarList[i]]
  
  Mod1 <- gam(Outcome ~ s(Annual_AnnualPM25_Lag01_DID,k=3)+Annual_AnnualPM25_Lag01+wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
              +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
              +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
              +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
              ,data = Data,family = binomial)
  
  Mod2 <- gam(Outcome ~ s(MonthPM25_monthlag01_DID,k=3)+MonthPM25_monthlag01+wave1+PAScore+Age+as.factor(Gender)+as.factor(EverSmoked_Full)+as.factor(Income_Full)+AgeEducation+AlcoholDrinking+as.factor(MaritalStatus)
              +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
              +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+as.factor(areatype)+as.factor(urban_nbs)
              +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
              ,data = Data,family = binomial)

  
  preds_x = seq(20,90,0.1)
  Data2 <- Data[complete.cases(Data[,c("Annual_AnnualPM25_Lag01","MonthPM25_monthlag01","Annual_AnnualPM25_Lag01_DID","wave1","MonthPM25_monthlag01_DID","PAScore","Age","Gender","EverSmoked_Full","Income_Full","AgeEducation","AlcoholDrinking","MaritalStatus","PhysicalDisability_Full","BrainDamage_Full","VisionProblem_Full","HearingProblem_Full","SpeechImpediment_Full","Annual_AnnualT2m_Lag02","Annual_AnnualRH_Lag02","Annual_AnnualPrecipitation_Lag02","GDP_Capita","areatype","urban_nbs","Yibao_Full","UrbanResidentInsurance_Full","WorkLastMonth_Full","SleepLastMonth_Full","Happy_Full","Depressed_Full","Lonely_Full","TroubleMind_Full","Fearful_Full","Hopeful_Full","AirConditioner_Full","HeatingSystem_Full","ConcreteBrickBuilding_Full","Resident4Business_Full")]),]
  newdata <- Data2[c(rep(1,length(preds_x))),]
  newdata$Annual_AnnualPM25_Lag01_DID <- preds_x
  preds1 <- predict.gam(Mod1, newdata = newdata,se.fit = TRUE)
  Ref <- preds1$fit[1]
  fit1 <- exp(preds1$fit - Ref)
  lower <- exp(preds1$fit - Ref - 1.96*preds1$se.fit)
  upper <- exp(preds1$fit - Ref + 1.96*preds1$se.fit)
  preds1 <- data.frame(PM25 = preds_x,fit1 = fit1,lower1 = lower, upper1 = upper)
  
  newdata$MonthPM25_monthlag01_DID <- preds_x
  preds2 <- predict.gam(Mod2, newdata = newdata,se.fit = TRUE)
  Ref <- preds2$fit[1]
  fit1 <- exp(preds2$fit - Ref) 
  lower <- exp(preds2$fit - Ref - 1.96*preds2$se.fit)
  upper <- exp(preds2$fit - Ref + 1.96*preds2$se.fit)
  preds2 <- data.frame(PM25 = preds_x,fit2 = fit1,lower2 = lower, upper2 = upper)
  
  preds <- left_join(x = preds1,y = preds2 , by =c("PM25"="PM25"),na_matches = "never")
  A<-ggplot(data = preds,aes(x = PM25))+
    geom_ribbon(aes(ymin = lower1,ymax = upper1),fill = "#1F549BFF",alpha = .3)+
    geom_line(aes(y=fit1,color = "Annual PM2.5"),size = 1)+
    geom_ribbon(aes(ymin = lower2,ymax = upper2),fill = "#E64B35FF",alpha = .3)+
    geom_line(aes(y=fit2,color = "Monthly PM2.5"),size = 1)+
    geom_line(aes(y=1),color = "black",size = 1,linetype="dashed")+
    scale_y_continuous("Odds ratio", expand = c(0, 0))+
    scale_x_continuous("PM2.5 level", expand = c(0, 0))+
    scale_color_manual(name = "PM2.5 type",values = c("Monthly PM2.5"="#E64B35FF","Annual PM2.5"="#1F549BFF"))+
    theme_bw()+
    theme(axis.text=element_text(size=15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title=element_text(size=15),
          legend.position = c(0.75,0.85)
    )
  print(A)
  ggsave(A,file=paste0("Figure_DoseResponsePM25_DID_",VarList[i],".pdf"), width=6, height=4)
}


######## forest plot for CHARLS asscociational result##############
annual_odds = read_excel('charls_association.xlsx', sheet = 'annual_odds')
figure_annual_odds <- ggplot(annual_odds , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=1), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "HelpFamilyFriends"="Help others not for payment",
    "CommunityOrg"="Join community-related organization",
    "VoluntaryCharityWork"="Do voluntary or charity work",
    "CareSick" = "Care for a sick or disabled adult"))+
  scale_y_continuous(name="% change",limits=c(0.6,1.1), breaks=seq(0.6,1.1,0.1),position = 'right')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Annual"="#1F549BFF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_annual_odds)


monthly_odds = read_excel('charls_association.xlsx', sheet = 'monthly_odds')
figure_monthly_odds <- ggplot(monthly_odds , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=1), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "HelpFamilyFriends"="Help others not for payment",
    "CommunityOrg"="Join community-related organization",
    "VoluntaryCharityWork"="Do voluntary or charity work",
    "CareSick" = "Care for a sick or disabled adult"))+
  scale_y_continuous(name="% change",limits=c(0.6,1.1), breaks=seq(0.6,1.1,0.1),position = 'right')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Monthly"="#E64B35FF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_monthly_odds)


annual_percentage = read_excel('charls_association.xlsx', sheet = 'annual_percentage')
figure_annual_percentage <- ggplot(annual_percentage , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=0), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "Donations2Society"="Donation to society"))+
  scale_y_continuous(name="% change",limits=c(-8,2), breaks=seq(-8,2,2),position = 'left')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Annual"="#1F549BFF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_annual_percentage)


monthly_percentage = read_excel('charls_association.xlsx', sheet = 'monthly_percentage')
figure_monthly_percentage <- ggplot(monthly_percentage , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=0), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "Donations2Society"="Donation to society"))+
  scale_y_continuous(name="% change",limits=c(-8,2), breaks=seq(-8,2,2),position = 'left')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Monthly"="#E64B35FF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_monthly_percentage)




###### Table 1 ########


# 2.649835
mean(Data$Annual_AnnualPM25_Lag01)
mean(Data$MonthPM25_monthlag01)

Data_high_AnnualPM25 <- Data[which(Data$Annual_AnnualPM25_Lag01> 54.78848),]
Data_Low_AnnualPM25 <- Data[which(Data$Annual_AnnualPM25_Lag01<= 54.78848),]
Data_high_MonthPM25 <- Data[which(Data$MonthPM25_monthlag01>38.03698),]
Data_Low_MonthPM25 <- Data[which(Data$MonthPM25_monthlag01<=38.03698),]

zz <- file(paste0("Table1.txt"), open = "wt")
sink(zz)
### basic information
cat("N:",
    nrow(Data),
    nrow(Data_high_AnnualPM25),
    nrow(Data_Low_AnnualPM25),
    nrow(Data_high_MonthPM25),
    nrow(Data_Low_MonthPM25),
    "\n")

cat("PY:",
    sum(Data$survival),
    sum(Data_high_AnnualPM25$survival),
    sum(Data_Low_AnnualPM25$survival),
    sum(Data_high_MonthPM25$survival),
    sum(Data_Low_MonthPM25$survival),
    "\n")

### prosocial behavior
cat("average amount donated to society:",
    mean(Data$Donations2Society,na.rm=T),
    mean(Data_high_AnnualPM25$Donations2Society,na.rm=T),
    mean(Data_Low_AnnualPM25$Donations2Society,na.rm=T),
    mean(Data_high_MonthPM25$Donations2Society,na.rm=T),
    mean(Data_Low_MonthPM25$Donations2Society,na.rm=T),
    "\n")

cat("% of helping others not for payment:",
    sum(Data$HelpFamilyFriends == 1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$HelpFamilyFriends == 1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$HelpFamilyFriends == 1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$HelpFamilyFriends == 1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$HelpFamilyFriends == 1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% of joinning community-related organization:",
    sum(Data$CommunityOrg == 1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$CommunityOrg == 1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$CommunityOrg == 1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$CommunityOrg == 1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$CommunityOrg == 1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% of doing voluntary or charity work:",
    sum(Data$VoluntaryCharityWork == 1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$VoluntaryCharityWork == 1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$VoluntaryCharityWork == 1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$VoluntaryCharityWork == 1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$VoluntaryCharityWork == 1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% of caring for a sick or disabled adult:",
    sum(Data$CareSick == 1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$CareSick == 1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$CareSick == 1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$CareSick == 1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$CareSick == 1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

### demogrpahic covariates
cat("Age:",
    mean(Data$Age,na.rm=T),
    mean(Data_high_AnnualPM25$Age,na.rm=T),
    mean(Data_Low_AnnualPM25$Age,na.rm=T),
    mean(Data_high_MonthPM25$Age,na.rm=T),
    mean(Data_Low_MonthPM25$Age,na.rm=T),
    "\n")
## 1 == male
cat("% of males:",
    sum(Data$Gender == 1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$Gender == 1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$Gender == 1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$Gender == 1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$Gender == 1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
## 2 == female
cat("% of females:",
    sum(Data$Gender == 2,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$Gender == 2,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$Gender == 2,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$Gender == 2,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$Gender == 2,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% Married with Spouse Present:",
    sum(Data$MaritalStatus == 1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$MaritalStatus == 1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$MaritalStatus == 1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$MaritalStatus == 1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$MaritalStatus == 1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

#### covariates for socioeconomic status: income and education attainment
cat("% Received Wage Last Year:",
    sum(Data$Income == "1 Yes",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$Income == "1 Yes",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$Income == "1 Yes",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$Income == "1 Yes",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$Income == "1 Yes",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("average age finish schooling:",
    mean(Data$AgeEducation,na.rm=T),
    mean(Data_high_AnnualPM25$AgeEducation,na.rm=T),
    mean(Data_Low_AnnualPM25$AgeEducation,na.rm=T),
    mean(Data_high_MonthPM25$AgeEducation,na.rm=T),
    mean(Data_Low_MonthPM25$AgeEducation,na.rm=T),
    "\n")

###  health status: physical disability, brain damage and mental retardation, vision problem, hearing problem, speech impediment
cat("% with physical disability:",
    sum(Data$PhysicalDisability == "1 Yes",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$PhysicalDisability == "1 Yes",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$PhysicalDisability == "1 Yes",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$PhysicalDisability == "1 Yes",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$PhysicalDisability == "1 Yes",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% with brain damage or mental retardation:",
    sum(Data$BrainDamage == "1 Yes",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$BrainDamage == "1 Yes",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$BrainDamage == "1 Yes",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$BrainDamage == "1 Yes",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$BrainDamage == "1 Yes",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% with vision problem :",
    sum(Data$VisionProblem == "1 Yes",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$VisionProblem == "1 Yes",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$VisionProblem == "1 Yes",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$VisionProblem == "1 Yes",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$VisionProblem == "1 Yes",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% with hearing problem:",
    sum(Data$HearingProblem == "1 Yes",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$HearingProblem == "1 Yes",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$HearingProblem == "1 Yes",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$HearingProblem == "1 Yes",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$HearingProblem == "1 Yes",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% with Speech Impediment:",
    sum(Data$SpeechImpediment == "1 Yes",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$SpeechImpediment == "1 Yes",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$SpeechImpediment == "1 Yes",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$SpeechImpediment == "1 Yes",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$SpeechImpediment == "1 Yes",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

### lifestyle risk factors: smoking status, alcohol drinking
## non-smoker
cat("% of never-smokers:",
    sum(Data$EverSmoked == "2 No",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$EverSmoked == "2 No",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$EverSmoked == "2 No",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$EverSmoked == "2 No",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$EverSmoked == "2 No",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% of Drink More than Once a Month:",
    sum(Data$AlcoholDrinking == "1 Drink More than Once a Month",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$AlcoholDrinking == "1 Drink More than Once a Month",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$AlcoholDrinking == "1 Drink More than Once a Month",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$AlcoholDrinking == "1 Drink More than Once a Month",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$AlcoholDrinking == "1 Drink More than Once a Month",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% of 2 Drink But Less than Once a Month:",
    sum(Data$AlcoholDrinking == "2 Drink But Less than Once a Month",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$AlcoholDrinking == "2 Drink But Less than Once a Month",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$AlcoholDrinking == "2 Drink But Less than Once a Month",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$AlcoholDrinking == "2 Drink But Less than Once a Month",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$AlcoholDrinking == "2 Drink But Less than Once a Month",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
# PA score > 0 some physical activity
cat("% of physically inactive:",
    sum(Data$PAScore == 0,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$PAScore == 0,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$PAScore == 0,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$PAScore == 0,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$PAScore == 0,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
## PA score > 100
cat("% of physically active:",
    sum(Data$PAScore >100,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$PAScore >100,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$PAScore >100,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$PAScore >100,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$PAScore >100,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
## Work for at Least One Hour Last Month 
cat("% people Work for at Least One Hour Last Month:",
    sum(Data$WorkLastMonth == "1 Yes",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$WorkLastMonth == "1 Yes",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$WorkLastMonth == "1 Yes",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$WorkLastMonth == "1 Yes",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$WorkLastMonth == "1 Yes",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("average sleep hours:",
    mean(Data$SleepLastMonth,na.rm=T),
    mean(Data_high_AnnualPM25$SleepLastMonth,na.rm=T),
    mean(Data_Low_AnnualPM25$SleepLastMonth,na.rm=T),
    mean(Data_high_MonthPM25$SleepLastMonth,na.rm=T),
    mean(Data_Low_MonthPM25$SleepLastMonth,na.rm=T),
    "\n")

### covariates for medical insurance: categorical variables for urban employee medical insurance,  urban resident medical insurance; 
#medical insurance
cat("% with urban employee medical insurance:",
    sum(Data$Yibao == 1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$Yibao == 1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$Yibao == 1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$Yibao == 1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$Yibao == 1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% with urban resident medical insurance:",
    sum(Data$UrbanResidentInsurance == 1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$UrbanResidentInsurance == 1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$UrbanResidentInsurance == 1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$UrbanResidentInsurance == 1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$UrbanResidentInsurance == 1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

### covariates for mental status: self-rated happiness, depression, loneliness, troubled mind, fearfulness, and hopefulness
# cat("% feel happy most of the time or occasionally:",
#     sum(Data$Happy >=3,na.rm = T)/nrow(Data),
#     sum(Data_high_AnnualPM25$Happy >=3,na.rm = T)/nrow(Data_high_AnnualPM25),
#     sum(Data_Low_AnnualPM25$Happy >=3,na.rm = T)/nrow(Data_Low_AnnualPM25),
#     sum(Data_high_MonthPM25$Happy >=3,na.rm = T)/nrow(Data_high_MonthPM25),
#     sum(Data_Low_MonthPM25$Happy >=3,na.rm = T)/nrow(Data_Low_MonthPM25),
#     "\n")
# cat("% feel depressed most of the time or occasionally:",
#     sum(Data$Depressed >=3,na.rm = T)/nrow(Data),
#     sum(Data_high_AnnualPM25$Depressed >=3,na.rm = T)/nrow(Data_high_AnnualPM25),
#     sum(Data_Low_AnnualPM25$Depressed >=3,na.rm = T)/nrow(Data_Low_AnnualPM25),
#     sum(Data_high_MonthPM25$Depressed >=3,na.rm = T)/nrow(Data_high_MonthPM25),
#     sum(Data_Low_MonthPM25$Depressed >=3,na.rm = T)/nrow(Data_Low_MonthPM25),
#     "\n")
# cat("% feel lonely most of the time or occasionally:",
#     sum(Data$Lonely >=3,na.rm = T)/nrow(Data),
#     sum(Data_high_AnnualPM25$Lonely >=3,na.rm = T)/nrow(Data_high_AnnualPM25),
#     sum(Data_Low_AnnualPM25$Lonely >=3,na.rm = T)/nrow(Data_Low_AnnualPM25),
#     sum(Data_high_MonthPM25$Lonely >=3,na.rm = T)/nrow(Data_high_MonthPM25),
#     sum(Data_Low_MonthPM25$Lonely >=3,na.rm = T)/nrow(Data_Low_MonthPM25),
#     "\n")
# cat("% had a trouble keeping mind most of the time or occasionally:",
#     sum(Data$TroubleMind >=3,na.rm = T)/nrow(Data),
#     sum(Data_high_AnnualPM25$TroubleMind >=3,na.rm = T)/nrow(Data_high_AnnualPM25),
#     sum(Data_Low_AnnualPM25$TroubleMind >=3,na.rm = T)/nrow(Data_Low_AnnualPM25),
#     sum(Data_high_MonthPM25$TroubleMind >=3,na.rm = T)/nrow(Data_high_MonthPM25),
#     sum(Data_Low_MonthPM25$TroubleMind >=3,na.rm = T)/nrow(Data_Low_MonthPM25),
#     "\n")
# cat("% feel fearful most of the time or occasionally:",
#     sum(Data$Fearful >=3,na.rm = T)/nrow(Data),
#     sum(Data_high_AnnualPM25$Fearful >=3,na.rm = T)/nrow(Data_high_AnnualPM25),
#     sum(Data_Low_AnnualPM25$Fearful >=3,na.rm = T)/nrow(Data_Low_AnnualPM25),
#     sum(Data_high_MonthPM25$Fearful >=3,na.rm = T)/nrow(Data_high_MonthPM25),
#     sum(Data_Low_MonthPM25$Fearful >=3,na.rm = T)/nrow(Data_Low_MonthPM25),
#     "\n")
# cat("% feel hopeful about the future most of the time or occasionally:",
#     sum(Data$Hopeful >=3,na.rm = T)/nrow(Data),
#     sum(Data_high_AnnualPM25$Hopeful >=3,na.rm = T)/nrow(Data_high_AnnualPM25),
#     sum(Data_Low_AnnualPM25$Hopeful >=3,na.rm = T)/nrow(Data_Low_AnnualPM25),
#     sum(Data_high_MonthPM25$Hopeful >=3,na.rm = T)/nrow(Data_high_MonthPM25),
#     sum(Data_Low_MonthPM25$Hopeful >=3,na.rm = T)/nrow(Data_Low_MonthPM25),
#     "\n")
cat("% feel hopeful about the future most of the time or occasionally:",
    sum(Data$Hopeful >=3,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$Hopeful >=3,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$Hopeful >=3,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$Hopeful >=3,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$Hopeful >=3,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")


### indoor environment and housing characteristics
cat("% with air conditioner:",
    sum(Data$AirConditioner ==1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$AirConditioner ==1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$AirConditioner ==1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$AirConditioner ==1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$AirConditioner ==1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% with residential heating system:",
    sum(Data$HeatingSystem =="1 Yes",na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$HeatingSystem =="1 Yes",na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$HeatingSystem =="1 Yes",na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$HeatingSystem =="1 Yes",na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$HeatingSystem =="1 Yes",na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
# 1 Concrete and Steel/Bricks and Wood"
cat("% live in house with concrete and steel/bricks and wood:",
    sum(Data$ConcreteBrickBuilding ==1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$ConcreteBrickBuilding ==1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$ConcreteBrickBuilding ==1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$ConcreteBrickBuilding ==1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$ConcreteBrickBuilding ==1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")
cat("% live in house for business use:",
    sum(Data$Resident4Business ==1,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$Resident4Business ==1,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$Resident4Business ==1,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$Resident4Business ==1,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$Resident4Business ==1,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

### meteorological variables
cat("average temperature (C):",
    mean(Data$Annual_AnnualT2m_Lag0 - 273,na.rm=T),
    mean(Data_high_AnnualPM25$Annual_AnnualT2m_Lag0 - 273,na.rm=T),
    mean(Data_Low_AnnualPM25$Annual_AnnualT2m_Lag0 - 273,na.rm=T),
    mean(Data_high_MonthPM25$Annual_AnnualT2m_Lag0 - 273,na.rm=T),
    mean(Data_Low_MonthPM25$Annual_AnnualT2m_Lag0 - 273,na.rm=T),
    "\n")
cat("Relative humidity (%):",
    mean(Data$Annual_AnnualRH_Lag0,na.rm=T),
    mean(Data_high_AnnualPM25$Annual_AnnualRH_Lag0,na.rm=T),
    mean(Data_Low_AnnualPM25$Annual_AnnualRH_Lag0,na.rm=T),
    mean(Data_high_MonthPM25$Annual_AnnualRH_Lag0,na.rm=T),
    mean(Data_Low_MonthPM25$Annual_AnnualRH_Lag0,na.rm=T),
    "\n")
# 'CPC Merged Analysis of Precipitation (includes NCEP Reanalysis)'
cat("Annual_AnnualPrecipitation_Lag0 (mm/day):",
    mean(Data$Annual_AnnualPrecipitation_Lag0,na.rm=T),
    mean(Data_high_AnnualPM25$Annual_AnnualPrecipitation_Lag0,na.rm=T),
    mean(Data_Low_AnnualPM25$Annual_AnnualPrecipitation_Lag0,na.rm=T),
    mean(Data_high_MonthPM25$Annual_AnnualPrecipitation_Lag0,na.rm=T),
    mean(Data_Low_MonthPM25$Annual_AnnualPrecipitation_Lag0,na.rm=T),
    "\n")

#### regional economic development
cat("average GDP per capita (in RMB yuan):",
    mean(Data$GDP_Capita,na.rm=T),
    mean(Data_high_AnnualPM25$GDP_Capita,na.rm=T),
    mean(Data_Low_AnnualPM25$GDP_Capita,na.rm=T),
    mean(Data_high_MonthPM25$GDP_Capita,na.rm=T),
    mean(Data_Low_MonthPM25$GDP_Capita,na.rm=T),
    "\n")


# 111表示主城区；112表示城乡接合区；121表示镇中心区；122表示镇乡接合区；123表示特殊区域；210表示乡中心区；220表示村庄;http://tjj.cq.gov.cn/tjgz/tjzs/201912/t20191223_2622231_wap.html

cat("% live in urban areas (111):",
    sum(Data$areatype == 111,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$areatype == 111,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$areatype == 111,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$areatype == 111,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$areatype == 111,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% live in urban-rural transition zone (112):",
    sum(Data$areatype == 112,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$areatype == 112,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$areatype == 112,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$areatype == 112,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$areatype == 112,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% live in town center (121):",
    sum(Data$areatype == 121,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$areatype == 121,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$areatype == 121,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$areatype == 121,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$areatype == 121,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% live in town-village transition zone (122):",
    sum(Data$areatype == 122,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$areatype == 122,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$areatype == 122,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$areatype == 122,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$areatype == 122,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% live in special zones (123):",
    sum(Data$areatype == 123,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$areatype == 123,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$areatype == 123,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$areatype == 123,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$areatype == 123,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% live in village center (210):",
    sum(Data$areatype == 210,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$areatype == 210,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$areatype == 210,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$areatype == 210,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$areatype == 210,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

cat("% live in villages (220):",
    sum(Data$areatype == 220,na.rm = T)/nrow(Data),
    sum(Data_high_AnnualPM25$areatype == 220,na.rm = T)/nrow(Data_high_AnnualPM25),
    sum(Data_Low_AnnualPM25$areatype == 220,na.rm = T)/nrow(Data_Low_AnnualPM25),
    sum(Data_high_MonthPM25$areatype == 220,na.rm = T)/nrow(Data_high_MonthPM25),
    sum(Data_Low_MonthPM25$areatype == 220,na.rm = T)/nrow(Data_Low_MonthPM25),
    "\n")

sink()


###### Mediation #####
Data$Gender<-factor(Data$Gender)
Data$EverSmoked_Full<-factor(Data$EverSmoked_Full)
Data$Income_Full<-factor(Data$Income_Full)
Data$MaritalStatus_Full_1<-factor(Data$MaritalStatus_Full == 1)
Data$areatype<-factor(Data$areatype)
Data$urban_nbs<-factor(Data$urban_nbs)

####Annual_AnnualPM25_Lag01 mediation
VarList <- c("HelpFamilyFriends","CommunityOrg","VoluntaryCharityWork","CareSick")
zz <- file("Prosocial_Mediation3.txt", open = "wt")
sink(zz)
for(i in 1:length(VarList))
{
  Data[,"Outcome"] <- Data[,VarList[i]]
  Model2<-glm(Outcome ~ WordScore+Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full_1
              +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
              +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
              +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
              ,data = Data,family = binomial)
  # summary(Model2)
  
  Model1<-glm(WordScore ~         Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full_1
              +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
              +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
              +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full,
              data = Data[which(!is.na(Data$Outcome)),])
  # summary(Model1)
  
  set.seed(1)
  congam<-mediate(Model1,Model2,treat="Annual_AnnualPM25_Lag01",mediator="WordScore",boot=T,sims = 50)
  A<-summary(congam)
  print(VarList[i])
  print(A)
}

Data$Outcome <- log(Data$Donations2Society+1)
Model2<-glm(Outcome ~ WordScore+Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full_1
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data)
# summary(Model2)

Model1<-glm(WordScore ~ Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full_1
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full,
            data = Data[which(!is.na(Data$Outcome)),])
# summary(Model1)

set.seed(1)
congam<-mediate(Model1,Model2,treat="Annual_AnnualPM25_Lag01",mediator="WordScore",boot=T,sims = 50)
A<-summary(congam)
print("log(Data$Donations2Society+1)")
print(A)

sink()


####monthlyPM25lag01 mediaton
VarList <- c("HelpFamilyFriends","CommunityOrg","VoluntaryCharityWork","CareSick")
zz <- file("Prosocial_Mediation222.txt", open = "wt")
sink(zz)
for(i in 1:length(VarList))
{
  Data[,"Outcome"] <- Data[,VarList[i]]
  Model2<-glm(Outcome ~ WordScore+MonthPM25_monthlag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full_1
              +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
              +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
              +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
              ,data = Data,family = binomial)
  # summary(Model2)
  
  Model1<-glm(WordScore ~         MonthPM25_monthlag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full_1
              +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
              +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
              +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full,
              data = Data[which(!is.na(Data$Outcome)),])
  # summary(Model1)
  
  set.seed(1)
  congam<-mediate(Model1,Model2,treat="MonthPM25_monthlag01",mediator="WordScore",boot=T,sims = 200)
  A<-summary(congam)
  print(VarList[i])
  print(A)
}

Data$Outcome <- log(Data$Donations2Society+1)
Model2<-glm(Outcome ~ WordScore+MonthPM25_monthlag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full_1
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data)
# summary(Model2)

Model1<-glm(WordScore ~ MonthPM25_monthlag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full_1
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+cesd10+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full,
            data = Data[which(!is.na(Data$Outcome)),])
# summary(Model1)

set.seed(1)
congam<-mediate(Model1,Model2,treat="MonthPM25_monthlag01",mediator="WordScore",boot=T,sims = 200)
A<-summary(congam)
print("log(Data$Donations2Society+1)")
print(A)

sink()




################
# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
# Estimate 95% CI Lower 95% CI Upper p-value    
# ACME (control)           -3.47e-05    -4.57e-05         0.00  <2e-16 ***
#   ACME (treated)           -3.45e-05    -4.53e-05         0.00  <2e-16 ***
#   ADE (control)            -2.16e-04    -3.82e-04         0.00  <2e-16 ***
#   ADE (treated)            -2.15e-04    -3.81e-04         0.00  <2e-16 ***
#   Total Effect             -2.50e-04    -4.20e-04         0.00  <2e-16 ***
#   Prop. Mediated (control)  1.42e-01     8.72e-02         0.27  <2e-16 ***
#   Prop. Mediated (treated)  1.41e-01     8.66e-02         0.27  <2e-16 ***
#   ACME (average)           -3.46e-05    -4.55e-05         0.00  <2e-16 ***
#   ADE (average)            -2.15e-04    -3.81e-04         0.00  <2e-16 ***
#   Prop. Mediated (average)  1.41e-01     8.69e-02         0.27  <2e-16 ***

Model2<-glm(CommunityOrg ~ WordScore+Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data,family = binomial)
summary(Model2)

Model1<-glm(WordScore ~               Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus_Full
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full,
            data = Data[which(!is.na(Data$CommunityOrg)),])
summary(Model1)

congam<-mediate(Model1,Model2,treat="Annual_AnnualPM25_Lag01",mediator="WordScore",boot=T,sims = 1000)
A<-summary(congam)


alpha <- Model2$coefficients["Depressed"]
c <- Model2$coefficients["Annual_AnnualPM25_Lag01"]
beta <-  Model1$coefficients["Annual_AnnualPM25_Lag01"]

alpha*beta/(alpha*beta+c)

Model2<-glm(HelpFamilyFriends ~ WordScore+Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data,family = binomial)
summary(Model2)

Model1<-glm(WordScore ~ Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking+MaritalStatus
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full,
            data = Data[which(!is.na(Data$HelpFamilyFriends)),])
summary(Model1)


Model2<-glm(HelpFamilyFriends ~ WordScore+Annual_AnnualPM25_Lag01+PAScore+Age+Gender+EverSmoked_Full+Income_Full+AgeEducation+AlcoholDrinking_Full+MaritalStatus_Full
            +PhysicalDisability_Full+BrainDamage_Full+VisionProblem_Full+HearingProblem_Full+SpeechImpediment_Full
            +Annual_AnnualT2m_Lag02+Annual_AnnualRH_Lag02+Annual_AnnualPrecipitation_Lag02+GDP_Capita+areatype+urban_nbs
            +Yibao_Full+UrbanResidentInsurance_Full+WorkLastMonth_Full+SleepLastMonth_Full+AirConditioner_Full+HeatingSystem_Full+ConcreteBrickBuilding_Full+Resident4Business_Full
            ,data = Data,family = binomial)
summary(Model2)






########## DID model forest plot #######
my_palette <- pal_nejm('default')(8)
my_palette
library("scales")
show_col(my_palette)


#install.packages("ggbreak")
library("ggbreak")

annual_odds = read_excel('figure3_did_data.xlsx', sheet = 'annual_odds')
figure_annual_odds <- ggplot(annual_odds , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=1), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "HelpFamilyFriends"="Help others not for payment",
    "CommunityOrg"="Join community-related organization",
    "VoluntaryCharityWork"="Do voluntary or charity work",
    "CareSick" = "Care for a sick or disabled adult"))+
  scale_y_continuous(name="% change",limits=c(0.6,1.1), breaks=seq(0.6,1.1,0.1),position = 'right')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Annual"="#1F549BFF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_annual_odds)


monthly_odds = read_excel('figure3_did_data.xlsx', sheet = 'monthly_odds')
figure_monthly_odds <- ggplot(monthly_odds , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=1), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "HelpFamilyFriends"="Help others not for payment",
    "CommunityOrg"="Join community-related organization",
    "VoluntaryCharityWork"="Do voluntary or charity work",
    "CareSick" = "Care for a sick or disabled adult"))+
  scale_y_continuous(name="% change",limits=c(0.6,1.1), breaks=seq(0.6,1.1,0.1),position = 'right')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Monthly"="#E64B35FF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_monthly_odds)


annual_percentage = read_excel('figure3_did_data.xlsx', sheet = 'annual_percentage')
figure_annual_percentage <- ggplot(annual_percentage , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=0), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "Donations2Society"="Donation to society"))+
  scale_y_continuous(name="% change",limits=c(-8,2), breaks=seq(-8,2,2),position = 'left')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Annual"="#1F549BFF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_annual_percentage)


monthly_percentage = read_excel('figure3_did_data.xlsx', sheet = 'monthly_percentage')
figure_monthly_percentage <- ggplot(monthly_percentage , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=0), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "Donations2Society"="Donation to society"))+
  scale_y_continuous(name="% change",limits=c(-8,2), breaks=seq(-8,2,2),position = 'left')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Monthly"="#E64B35FF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_monthly_percentage)











annual_odds = read_excel('figure3_did_data.xlsx', sheet = 'annual_odds')
figure_annual_odds <- ggplot(annual_odds , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=1), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "HelpFamilyFriends"="1",
    "CommunityOrg"="2",
    "VoluntaryCharityWork"="3",
    "CareSick" = "4"))+
  scale_y_continuous(name="% change",limits=c(0.6,1.1), breaks=seq(0.6,1.1,0.1),position = 'right')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Annual"="#1F549BFF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_annual_odds)


monthly_odds = read_excel('figure3_did_data.xlsx', sheet = 'monthly_odds')
figure_monthly_odds <- ggplot(monthly_odds , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=1), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "HelpFamilyFriends"="1",
    "CommunityOrg"="2",
    "VoluntaryCharityWork"="3",
    "CareSick" = "4"))+
  scale_y_continuous(name="% change",limits=c(0.6,1.1), breaks=seq(0.6,1.1,0.1),position = 'right')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Monthly"="#E64B35FF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_monthly_odds)


annual_percentage = read_excel('figure3_did_data.xlsx', sheet = 'annual_percentage')
figure_annual_percentage <- ggplot(annual_percentage , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=0), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "Donations2Society"="1"))+
  scale_y_continuous(name="% change",limits=c(-8,2), breaks=seq(-8,2,2),position = 'left')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Annual"="#1F549BFF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_annual_percentage)


monthly_percentage = read_excel('figure3_did_data.xlsx', sheet = 'monthly_percentage')
figure_monthly_percentage <- ggplot(monthly_percentage , aes(x = variable, y = mean, color=PM25_type ))+
  geom_hline(aes(yintercept=0), colour="gray", linetype="dashed") +
  geom_errorbar(aes(ymin = (lower), ymax = (upper)) ,width=0.2, 
                position = position_dodge(width=.6))+
  scale_x_discrete(labels = c(
    "Donations2Society"="1"))+
  scale_y_continuous(name="% change",limits=c(-8,2), breaks=seq(-8,2,2),position = 'left')+
  #scale_y_break(c(0.9, 1.9),scales = "fixed",expand=expansion(add = c(0, 0)))+
  geom_point(position = position_dodge(width=.6),size=3.5)+
  scale_color_manual(name='PM25 type',
                     values = c("Monthly"="#E64B35FF"))+
  theme_prism(border = TRUE, base_size = 12,base_fontface = "plain") +
  xlab("") +ylab("")+guides(color="none")+
  facet_grid(group~., scales="free", space = "free") +coord_flip()+
  theme(strip.text.y = element_text(angle = 0, size = 14,face = "bold"),
        strip.background = element_rect(fill="#E3E3E3"),
        axis.title.x = element_text(size = 12,face = "bold"))
plot(figure_monthly_percentage)
