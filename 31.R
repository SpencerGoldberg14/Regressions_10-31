# install.packages(c("lubridate", "dplyr", "ggplot2"))
# start of class 1
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/Deemer_GHG_Data.csv")

# log transformations
ghg$log.ch4 <- log(ghg$ch4+1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

# identify binary variables
unique(ghg$Region)
# isolate binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
# isolate binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)
# binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)
# binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

# multiple regression
# creates a model object
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg)
summary(mod.full)

# check assumptions
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)
# qq plot to check normality
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)
# shapiro-wilks test to check normality (<200 sample size, null: data normally distributed, p > .05 confidence -> accept null)
shapiro.test(res.full)
# check residuals
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)
# check multicollinearity
# isolate continuous model variables into data frame:
reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)
# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# model selection
# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 
# check full model
full.step$model
# plot AIC over time
plot(full.step )

# predictions
# prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")
# look at prediction with 95% confidence interval of the mean
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")

# start of class 2
ETdat <- read.csv("/cloud/project/ETdata.csv")
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

# average fields for each month for almonds
almond <- ETdat %>% 
  filter(crop == "Almonds") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, 
                start = c(2016,1), 
                frequency= 12) 

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , 
                order = c(1,0,0)) 
model1

model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4

# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")
--------------------------------------------------------------
# start of HW
# question 1
# recommended CO2 transformation
ghg <- read.csv("/cloud/project/Deemer_GHG_Data.csv")
ghg$co2 <- 1/(ghg$co2+1000)
ghg$co2

# transform variables that are predicted to impact CO2 most
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

# multiple regression
# creates a model object
mod.full <- lm(ghg$co2 ~ airTemp+
                 log.age+
                 mean.depth+
                 log.DIP+
                 log.precip,data=ghg) 
summary(mod.full)

# checking assumptions
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

# check for normality
# qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

# check residuals from assumptions 2-4
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

# check for multicollinearity 
# isolate continuous model variables into data frame:
reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# question 2
# decompose the evapotranspiration time series for almonds
# average fields for each month for almonds
almond <- ETdat %>% 
  filter(crop == "Almonds") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, 
                start = c(2016,1), 
                frequency= 12) 

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

# decompose the evapotranspiration time series for pistachios
# average fields for each month for pistachios
pistachios <- ETdat %>% 
  filter(crop == "Pistachios") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
# visualize the data
ggplot(pistachios, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# pistachios ET time series
pistachios_ts <- ts(pistachios$ET.in, 
                start = c(2016,1), 
                frequency= 12) 

# decompose pistachios ET time series
pistachios_dec <- decompose(almond_ts)
# plot decomposition
plot(pistachios_dec)

# decompose the evapotranspiration time series for Fallow/Idle Cropland
# average fields for each month for Fallow/Idle Cropland
Fallow_Idle <- ETdat %>% 
  filter(crop == "Fallow/Idle Cropland") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
# visualize the data
ggplot(Fallow_Idle, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# Fallow/Idle Cropland ET time series
Fallow_Idle_ts <- ts(Fallow_Idle$ET.in, 
                    start = c(2016,1), 
                    frequency= 12) 

# decompose Fallow/Idle Cropland ET time series
Fallow_Idle_dec <- decompose(Fallow_Idle_ts)
# plot decomposition
plot(Fallow_Idle_dec)

# decompose the evapotranspiration time series for corn
# average fields for each month for corn
corn <- ETdat %>% 
  filter(crop == "Corn") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
# visualize the data
ggplot(corn, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# corn ET time series
corn_ts <- ts(corn$ET.in, 
                start = c(2016,1), 
                frequency= 12) 

# decompose corn ET time series
corn_dec <- decompose(corn_ts)
# plot decomposition
plot(corn_dec)

# decompose the evapotranspiration time series for grapes
# average fields for each month for grapes
grape <- ETdat %>% 
  filter(crop == "Grapes (Table/Raisin)") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
# visualize the data
ggplot(grape, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# grape ET time series
grape_ts <- ts(grape$ET.in, 
                start = c(2016,1), 
                frequency= 12) 

# decompose grape ET time series
grape_dec <- decompose(grape_ts)
# plot decomposition
plot(grape_dec)

# question 3
# autoregressive model for pistachios
pistachios_y <- na.omit(pistachios_ts)
model1 <- arima(pistachios_y , 
                order = c(1,0,0)) 
model1

# try running a 4th order AR
model4 <- arima(pistachios_y , 
                order = c(4,0,0)) 
model4

# calculate fit
AR_fit1 <- pistachios_y - residuals(model1) 
AR_fit4 <- pistachios_y - residuals(model4)
#plot data
plot(pistachios_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "blue", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","blue"),
       bty="n")

# forecast future pistachio evapotranspiration 
newPistachios <- forecast(model4)
newPistachios

#make dataframe for plotting
newPistachiosF <- data.frame(newPistachios)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistachiosF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = pistachios, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachios$date[1]),newPistachiosF$dateF[24])+  
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=newPistachiosF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

# autoregressive model for fallow/idle fields
Fallow_Idle_y <- na.omit(Fallow_Idle_ts)
model1a <- arima(Fallow_Idle_y , 
                order = c(1,0,0)) 
model1a

# try running a 4th order AR
model4a <- arima(Fallow_Idle_y , 
                order = c(6,0,0)) 
model4a

# calculate fit
AR_fit1a <- Fallow_Idle_y - residuals(model1a) 
AR_fit4a <- Fallow_Idle_y - residuals(model4a)
#plot data
plot(Fallow_Idle_y)
# plot fit
points(AR_fit1a, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4a, type = "l", col = "blue", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","blue"),
       bty="n")

# forecast future fallow/idle fields evapotranspiration 
newFallow_Idle <- forecast(model4a)
newFallow_Idle

#make dataframe for plotting
newFallow_IdleF <- data.frame(newFallow_Idle)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallow_IdleF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Fallow_Idle, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Fallow_Idle$date[1]), newFallow_IdleF$dateF[24])+  
  geom_line(data = newFallow_IdleF, aes(x = dateF, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=newFallow_IdleF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")