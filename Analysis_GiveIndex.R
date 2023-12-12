library('readxl')
library("dplyr")
library(mice)
library(lme4)
library(nlme)
library(mgcv)
library(mediation)
library(interplot)
library(cluster)
library(factoextra)
library(quantreg)
library(splines)
library(merTools)
library(boot)
library(lubridate)
library("ggpubr")
library(WRS2)
library(dplyr)
library(ggplot2)

setwd('C:\\Users\\FGQ4\\OneDrive - mails.tsinghua.edu.cn\\airpolution_prosocial')

############## read and merge data #######################
HDI <- read.csv("AverageHDIByCountry.csv")
Heatwave <- read.csv("HeatwaveByCountry.csv")
Temperature <- read.csv("AverageTemperatureByCountry.csv")
TemperatureVar <- read.csv("1DayVariationTemperatureByCountry.csv")
PM25 <- read.csv("AveragePM25ByCountry.csv")
CrossWalk <- read.csv("wikipedia-iso-country-codes.csv")

Data_2018 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2018")
Data_2018 <- Data_2018[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2018) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2018$year <- 2018

Data_2017 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2017")
Data_2017 <- Data_2017[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2017) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2017$year <- 2017

Data_2016 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2016")
Data_2016 <- Data_2016[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2016) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2016$year <- 2016

Data_2015 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2015")
Data_2015 <- Data_2015[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2015) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2015$year <- 2015

Data_2014 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2014")
Data_2014 <- Data_2014[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2014) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2014$year <- 2014

Data_2013 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2013")
Data_2013 <- Data_2013[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2013) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2013$year <- 2013

Data_2012 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2012")
Data_2012 <- Data_2012[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2012) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2012$year <- 2012

Data_2011 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2011")
Data_2011 <- Data_2011[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2011) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2011$year <- 2011

Data_2010 <- read_excel("world_giving_index_2010-2018.xlsx",sheet="2010")
Data_2010 <- Data_2010[,c("Alpha.2.code","world giving index score","helping a stranger score","donating money score","volunteering time score")]
names(Data_2010) <- c("Code","GiveIndexScore","HelpStrangerScore","DonateMoneyScore","VolunteerTimeScore")
Data_2010$year <- 2010

Data <- rbind(Data_2010,Data_2011,Data_2012,Data_2013,Data_2014,Data_2015,Data_2016,Data_2017,Data_2018)
Data[which(Data$Code == "NA"), 1] = "NAM"

Data <- left_join(x = Data,y=CrossWalk[,c("Alpha.2.code","Alpha.3.code")],by=c("Code"= "Alpha.2.code"))
Data <- left_join(x = Data,y=Temperature[,c("Temperature","ISO","YEAR")],by=c("Code"= "ISO","year" = "YEAR"))
Data <- left_join(x = Data,y=TemperatureVar[,c("TemperatureVar","ISO","YEAR")],by=c("Code"= "ISO","year" = "YEAR"))
Data <- left_join(x = Data,y=Heatwave[,c("Heatwave","ISO","YEAR")],by=c("Code"= "ISO","year" = "YEAR"))
Data <- left_join(x = Data,y=HDI[,c("HDI","ISO","YEAR")],by=c("Code"= "ISO","year" = "YEAR"))

PM25$YEAR_lag <- PM25$YEAR + 2
PM25$PM25_lag2 <- PM25$PM25
Data <- left_join(x = Data,y=PM25[,c("PM25_lag2","ISO","YEAR_lag")],by=c("Code"= "ISO","year" = "YEAR_lag"))

PM25$YEAR_lag <- PM25$YEAR + 1
PM25$PM25_lag1 <- PM25$PM25
Data <- left_join(x = Data,y=PM25[,c("PM25_lag1","ISO","YEAR_lag")],by=c("Code"= "ISO","year" = "YEAR_lag"))

PM25$YEAR_lag <- PM25$YEAR + 0
PM25$PM25_lag0 <- PM25$PM25
Data <- left_join(x = Data,y=PM25[,c("PM25_lag0","ISO","YEAR_lag")],by=c("Code"= "ISO","year" = "YEAR_lag"))

## match population
Temp <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_2252106.csv")
for(i in 1990:2019)
{
  Temp1 <- Temp[,c("Country.Code",paste0("X",i))]
  names(Temp1) <- c("Country.Code","Population")
  Temp1$Year <- i
  if(1990 == i)
  {
    Population <- Temp1
  }else
  {
    Population <- rbind(Population,Temp1)
  }
}
Data <- left_join(x = Data,y=Population,by=c("Alpha.3.code"= "Country.Code","year" = "Year"))

## match GDP per capita
Temp <- read.csv("API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_2252074.csv")
for(i in 1990:2019)
{
  Temp1 <- Temp[,c("Country.Code",paste0("X",i))]
  names(Temp1) <- c("Country.Code","GDP_Capita")
  Temp1$Year <- i
  if(1990 == i)
  {
    GDP <- Temp1
  }else
  {
    GDP <- rbind(GDP,Temp1)
  }
}
Data <- left_join(x = Data,y=GDP,by=c("Alpha.3.code"= "Country.Code","year" = "Year"))

## match Life expectancy
Temp <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2_2252393.csv")
for(i in 1990:2019)
{
  Temp1 <- Temp[,c("Country.Code",paste0("X",i))]
  names(Temp1) <- c("Country.Code","LifeExpectance")
  Temp1$Year <- i
  if(1990 == i)
  {
    LifeExpectance <- Temp1
  }else
  {
    LifeExpectance <- rbind(LifeExpectance,Temp1)
  }
}
Data <- left_join(x = Data,y=LifeExpectance,by=c("Alpha.3.code"= "Country.Code","year" = "Year"))

Data$HelpStrangerScore <- as.numeric(Data$HelpStrangerScore)

############analysis on PM2.5 ###############
#### univariate model
mod <- glm(log(GiveIndexScore)~ PM25_lag0 +HDI+as.factor(year), data = Data,weights = log(Population))
summary(mod)
confint(mod, level = 0.95)

mod <- glm(log(HelpStrangerScore)~ PM25_lag0 +HDI+as.factor(year),data = Data,weights = log(Population))
summary(mod)
confint(mod, level = 0.95)

mod <- glm(log(DonateMoneyScore)~ PM25_lag0 + HDI+as.factor(year),data = Data,weights = log(Population))
summary(mod)
confint(mod, level = 0.95)

mod <- glm(log(VolunteerTimeScore)~ PM25_lag0 + HDI+as.factor(year),data = Data,weights = log(Population))
summary(mod)
confint(mod, level = 0.95)

##### adjusted model :+ Temperature+GDP_Capita+HDI+LifeExpectance
mod <- glm(log(GiveIndexScore)~ PM25_lag0 + Temperature+GDP_Capita+HDI+LifeExpectance+as.factor(year),data = Data,weights = log(Population))
summary(mod)
confint(mod, level = 0.95)

mod <- glm(log(HelpStrangerScore)~ PM25_lag0 + Temperature+GDP_Capita+HDI+LifeExpectance+as.factor(year),data = Data,weights = log(Population))
summary(mod)
confint(mod, level = 0.95)

mod <- glm(log(DonateMoneyScore)~ PM25_lag0 + Temperature+GDP_Capita+HDI+LifeExpectance+as.factor(year),data = Data,weights = log(Population))
summary(mod)
confint(mod, level = 0.95)

mod <- glm(log(VolunteerTimeScore)~ PM25_lag0 + Temperature+GDP_Capita+HDI+LifeExpectance+as.factor(year),data = Data,weights = log(Population))
summary(mod)
confint(mod, level = 0.95)

## dose-response ##########

Mod <- (mgcv::gam(log(GiveIndexScore)~ s(PM25_lag0,k=3) + GDP_Capita+HDI+LifeExpectance+as.factor(year),data = Data,weights = log(Population)))
plot(Mod,xlim = c(4,60),main = "Giving index")
AIC(Mod)  #min AIC=556.3861 while k=5
summary(Mod) # k=3 most natural

Data1 <- Data
Data1$variable <- Data1$PM25_lag0
Mod <- gam(log(GiveIndexScore)~ s(variable,k=3)+HDI+GDP_Capita+as.factor(year)+Temperature+LifeExpectance ,data = Data1,weights = log(Population))
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),] 
#use the 1st case in the newdata with certain covativates except PM2.5(X), and asign linspace X to newdata then predict Y
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
# when this is TRUE (not default) standard error estimates are returned for each prediction.
preds$fit <- preds$fit - preds$fit[1]
preds_y <- data.frame(as.numeric(preds$fit) + outer(as.numeric(preds$se.fit), c(0, -1.96, 1.96)))
#matplot(preds_x,exp(preds_y),col=1,lty=c(1,2,2),type='l',lwd=c(2,1,1), main = "", xlab="PM2.5 level(??g/m3)",ylab="% Change in Giving Index Score",xlim=c(5,80))
preds_y_plot = (exp(preds_y)-1)*100
preds_x = data.frame(preds_x)
pred = data.frame(preds_y_plot, preds_x)
colnames(pred) <- c("y", "ymin","ymax",'x')


my_palette <- pal_igv('default')(8)
my_palette
library("scales")
show_col(my_palette)


A <- ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=pred, lty=1, lwd=1.2, color='#00A1D5FF') +
  geom_ribbon(data=pred, aes(ymax=ymax, ymin=ymin), alpha=0.2, fill="#00A1D5FF") +
  scale_y_continuous(expression("% Change in Giving Index Score"), expand=c(0,0)) +
  scale_x_continuous(expression("Annual PM"[2.5]*" exposure  (??g/m"^3*")"), expand=c(0,0), limits=c(min(pred$x),65)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.position=c(0.75,0.85)
  )


print(A)
ggsave(A,file=".\\figure\\figure1\\Figure_DoseResponsePM25_GivingIndex_Givingindexscore.pdf", width=9, height=6)



Mod <- (mgcv::gam(log(HelpStrangerScore)~ s(PM25_lag0,k=3) + GDP_Capita+HDI+LifeExpectance+as.factor(year),data = Data,weights = log(Population)))
plot(Mod,xlim = c(4,55),main = "Helping Strangers")
AIC(Mod)  #min AIC=313.9936 while k=8
summary(Mod)

Mod <- gam(log(HelpStrangerScore)~ s(variable,k=3)+HDI+GDP_Capita+as.factor(year)+Temperature+LifeExpectance ,data = Data1,weights = log(Population))
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
preds$fit <- preds$fit - preds$fit[1]
preds_y <- as.numeric(preds$fit) + outer(as.numeric(preds$se.fit), c(0, -1.96, 1.96))
matplot(preds_x,exp(preds_y),col=1,lty=c(1,2,2),type='l',lwd=c(2,1,1), main = "", xlab="PM2.5(??g/m3)",ylab="Helping Stranger Score",xlim=c(5,80))

Data1 <- Data
Data1$variable <- Data1$PM25_lag0
Mod <- gam(log(HelpStrangerScore)~ s(variable,k=3)+HDI+GDP_Capita+as.factor(year)+Temperature+LifeExpectance ,data = Data1,weights = log(Population))
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),] 
#use the 1st case in the newdata with certain covativates except PM2.5(X), and asign linspace X to newdata then predict Y
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
# when this is TRUE (not default) standard error estimates are returned for each prediction.
preds$fit <- preds$fit - preds$fit[1]
preds_y <- data.frame(as.numeric(preds$fit) + outer(as.numeric(preds$se.fit), c(0, -1.96, 1.96)))
#matplot(preds_x,exp(preds_y),col=1,lty=c(1,2,2),type='l',lwd=c(2,1,1), main = "", xlab="PM2.5 level(??g/m3)",ylab="% Change in Giving Index Score",xlim=c(5,80))
preds_y_plot = (exp(preds_y)-1)*100
pred = data.frame(preds_y_plot,data.frame(preds_x))
colnames(pred) <- c("y", "ymin","ymax",'x')

A <- ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=pred, lty=1, lwd=1.2, color='#374E55FF') +
  geom_ribbon(data=pred, aes(ymax=ymax, ymin=ymin), alpha=0.2, fill="#374E55FF") +
  scale_y_continuous(expression("% Change in Helping Strangers Score"), expand=c(0,0)) +
  scale_x_continuous(expression("Annual PM"[2.5]*" exposure  (??g/m"^3*")"), expand=c(0,0), limits=c(min(pred$x),65)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.position=c(0.75,0.85)
  )

print(A)
ggsave(A,file=".\\figure\\figure1\\Figure_DoseResponsePM25_GivingIndex_Helpingstranger.pdf", width=9, height=6)



Mod <- (mgcv::gam(log(DonateMoneyScore)~ s(PM25_lag0,k=3) + GDP_Capita+HDI+LifeExpectance+as.factor(year),data = Data,weights = log(Population)))
plot(Mod,xlim = c(4,55),main = "Donate Money")
AIC(Mod)  #min AIC=1701.479 while k=5
summary(Mod)

Mod <- gam(log(DonateMoneyScore)~ s(variable,k=3)+HDI+GDP_Capita+as.factor(year)+Temperature+LifeExpectance,data = Data1,weights = log(Population))
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
preds$fit <- preds$fit - preds$fit[1]
preds_y <- as.numeric(preds$fit) + outer(as.numeric(preds$se.fit), c(0, -1.96, 1.96))
matplot(preds_x,exp(preds_y),col=1,lty=c(1,2,2),type='l',lwd=c(2,1,1), main = "", xlab="PM2.5(??g/m3)",ylab="Donating Money Score",xlim=c(5,80))

Data1 <- Data
Data1$variable <- Data1$PM25_lag0
Mod <- gam(log(DonateMoneyScore)~ s(variable,k=3)+HDI+GDP_Capita+as.factor(year)+Temperature+LifeExpectance ,data = Data1,weights = log(Population))
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),] 
#use the 1st case in the newdata with certain covativates except PM2.5(X), and asign linspace X to newdata then predict Y
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
# when this is TRUE (not default) standard error estimates are returned for each prediction.
preds$fit <- preds$fit - preds$fit[1]
preds_y <- data.frame(as.numeric(preds$fit) + outer(as.numeric(preds$se.fit), c(0, -1.96, 1.96)))
#matplot(preds_x,exp(preds_y),col=1,lty=c(1,2,2),type='l',lwd=c(2,1,1), main = "", xlab="PM2.5 level(??g/m3)",ylab="% Change in Giving Index Score",xlim=c(5,80))
preds_y_plot = (exp(preds_y)-1)*100
pred = data.frame(preds_y_plot,data.frame(preds_x))
colnames(pred) <- c("y", "ymin","ymax",'x')



A <- ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=pred, lty=1, lwd=1.2, color='#374E55FF') +
  geom_ribbon(data=pred, aes(ymax=ymax, ymin=ymin), alpha=0.2, fill="#374E55FF") +
  scale_y_continuous(expression("% Change in Donating Money Score"), expand=c(0,0)) +
  scale_x_continuous(expression("Annual PM"[2.5]*" exposure  (??g/m"^3*")"), expand=c(0,0), limits=c(min(pred$x),65)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.position=c(0.75,0.85)
  )

print(A)
ggsave(A,file=".\\figure\\figure1\\Figure_DoseResponsePM25_GivingIndex_Donatingmoney.pdf", width=9, height=6)

Mod <- (mgcv::gam(log(VolunteerTimeScore)~ s(PM25_lag0,k=3) + GDP_Capita+HDI+LifeExpectance+as.factor(year),data = Data,weights = log(Population)))
plot(Mod,xlim = c(4,55),main = "Volunteering time")
AIC(Mod)  #min AIC=556.3861 while k=5
summary(Mod)

Mod <- gam(log(VolunteerTimeScore)~ s(variable,k=3)+HDI+GDP_Capita+as.factor(year)+Temperature+LifeExpectance,data = Data1,weights = log(Population))
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),]
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
preds$fit <- preds$fit - preds$fit[1]
preds_y <- as.numeric(preds$fit) + outer(as.numeric(preds$se.fit), c(0, -1.96, 1.96))
matplot(preds_x,exp(preds_y),col=1,lty=c(1,2,2),type='l',lwd=c(2,1,1), main = "", xlab="PM2.5(??g/m3)",ylab="Volunteering Time Score",xlim=c(5,80))

Data1 <- Data
Data1$variable <- Data1$PM25_lag0
Mod <- gam(log(VolunteerTimeScore)~ s(variable,k=3)+HDI+GDP_Capita+as.factor(year)+Temperature+LifeExpectance ,data = Data1,weights = log(Population))
preds_x = seq(min(Data1$variable,na.rm=T),max(Data1$variable,na.rm=T),(max(Data1$variable,na.rm=T) - min(Data1$variable,na.rm=T))/1000)
Data2 <- Data1[complete.cases(Data1),]
newdata <- Data2[c(rep(1,length(preds_x))),] 
#use the 1st case in the newdata with certain covativates except PM2.5(X), and asign linspace X to newdata then predict Y
newdata$variable <- preds_x
preds <- predict.gam(Mod, newdata = newdata,se.fit = TRUE)
# when this is TRUE (not default) standard error estimates are returned for each prediction.
preds$fit <- preds$fit - preds$fit[1]
preds_y <- data.frame(as.numeric(preds$fit) + outer(as.numeric(preds$se.fit), c(0, -1.96, 1.96)))
#matplot(preds_x,exp(preds_y),col=1,lty=c(1,2,2),type='l',lwd=c(2,1,1), main = "", xlab="PM2.5 level(??g/m3)",ylab="% Change in Giving Index Score",xlim=c(5,80))
preds_y_plot = (exp(preds_y)-1)*100
pred = data.frame(preds_y_plot,data.frame(preds_x))
colnames(pred) <- c("y", "ymin","ymax",'x')


A <- ggplot(mapping=aes(x=x, y=y)) + 
  geom_line(data=pred, lty=1, lwd=1.2, color='#374E55FF') +
  geom_ribbon(data=pred, aes(ymax=ymax, ymin=ymin), alpha=0.2, fill="#374E55FF") +
  scale_y_continuous(expression("% Change in Volunteering Time Score"), expand=c(0,0)) +
  scale_x_continuous(expression("Annual PM"[2.5]*" exposure  (??g/m"^3*")"), expand=c(0,0), limits=c(min(pred$x),65)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.position=c(0.75,0.85)
  )

print(A)
ggsave(A,file=".\\figure\\figure1\\Figure_DoseResponsePM25_GivingIndex_Volunteeringtime.pdf", width=9, height=6)


#####global distribution####
library(maps)
library(plyr)
library(rgdal)
library(maptools)
library(ggplot2)
library(dplyr)
library(plyr)
library(rgdal)

setwd('C:\\Users\\FGQ4\\OneDrive - mails.tsinghua.edu.cn\\airpolution_prosocial\\')

world_map = readOGR( "World_Countries__Generalized_.shp" )
world_map1 = fortify(world_map)
x = world_map@data
xs<-data.frame(x,id=seq(0:248)-1)
world_map_data<-join(world_map1,xs,type="full")
world_map_data[which(world_map_data$ISO == "NA"), 10] = "NAM"

# giving index score distribution
givingindex_plotdata = Data_2015[c("GiveIndexScore","Code")]
colnames(givingindex_plotdata) <- c("gis", "ISO")
world_data1 <- left_join(world_map_data,givingindex_plotdata,by = 'ISO')

my_palette <- pal_locuszoom('default')(8)
my_palette
library("scales")
show_col(my_palette)

breaks <- c(10,20,30,40,50,60)
colors <- c("#D43F3AFF", "#EEA236FF", "#5CB85CFF", "#46B8DAFF", "#357EBDFF", "#9632B8FF")

ggplot(world_data1, aes(x = long, y = lat, group = group, fill = givingindexscore)) +
  geom_polygon(colour = "grey40", linewidth = 0.2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_gradientn(
    name = "",
    breaks = breaks,
    colors = colors,
    labels = c("10","20","30", "40", "50", "60")
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Global Distribution of Giving Index Score in 2015")


ggplot(world_data1, aes(x = long, y = lat, group = group, fill = gis)) +
  geom_polygon(colour = "grey40", linewidth = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_gradient(low = "#E7EDF5FF", high = '#00A1D5FF'
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Global Distribution of Giving Index Score in 2015")

ggsave(A,file=".\\figure\\figure1\\GD_GivingIndex_2016.pdf", width=16, height=9)






# PM2.5 distribution   PM25 latest 2016
PM25data<-PM25[c('ISO',"PM25","YEAR")]
PM25data_year = filter(PM25data,YEAR==2015)
PM25_plotdata = PM25data_year[c('ISO',"PM25")]
world_data2 <- left_join(world_map_data,PM25_plotdata,by = 'ISO')


my_palette <- pal_jama('default')(8)
my_palette
library("scales")
show_col(my_palette)

breaks <- c(10,20,30,50,70,90)
colors <- c("#9632B8FF","#357EBDFF","#46B8DAFF","#5CB85CFF","#EEA236FF","#D43F3AFF")

ggplot(world_data2, aes(x = long, y = lat, group = group, fill = PM25)) +
  geom_polygon(colour = "grey40", linewidth = 0.2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_gradientn(
    name = "",
    breaks = breaks,
    colors = colors,
    labels = c("10","20","30", "50", "70", "90")
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Global Distribution of PM2.5 in 2015")


ggplot(world_data2, aes(x = long, y = lat, group = group, fill = PM25)) +
  geom_polygon(colour = "grey40", linewidth = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_gradient(low = "#79AF9766", high = '#DF8F44FF'
  ) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Global Distribution of PM2.5 in 2015")


print(A)
ggsave(A,file=".\\figure\\figure1\\GD_PM25_2015.pdf", width=16, height=9)
