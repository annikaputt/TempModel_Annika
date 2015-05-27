############################
# TempModel.R
# TempModel_Annika.Rproj

# Basic time series modelling of fraser system data to remove
# seasonality and serial autocorrelation

# Created May 12, 2015
# A Putt
#############################

# Set your working directory to the location of the source file

source("TempModelLibraries.R")
source("DataUpload.R")

means <- read.csv("Data/means.csv",as.is=TRUE)
means$month <- as.factor(means$month)
means$week <- as.factor(means$week)
means$dateyear <- ymd(means$dateyear)
means$air7day <- rollapply(means$airtemp, 7, mean, fill=NA, align="right")
means$timestep <- 1:nrow(means)
head(means)
plot(means$dateyear,means$watertemp,type="l")

#===========================================
# Truncate the data to have enough ram on Annika's computer
#means <- subset(means,dateyear > "1998-01-01" & dateyear < "2000-01-01")
#plot(means$dateyear,means$watertemp,type="l")
#===========================================

# # Turn the water temp data into a time series for potential future analysis...
# meansts <- ts(means$watertemp,frequency=1)
# plot(meansts)

# Write a basic linear model with time, air temp, and water temperature
m1 <- lm(watertemp~timestep+air7day,data=means,na.action=na.exclude)
summary(m1)

# Look at residual patterns by month to see if we should account for seasonality
m1resids <- residuals(m1,na.action=na.exclude)
m1resid.month <- data.frame(resid=m1resids,month=means$month)
m1resid.month$month <- factor(m1resid.month$month,levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
boxplot(m1resid.month$resid~m1resid.month$month,las=1,ylab="resids",main="resids by month")
# We clearly have seasonal correlation, so we need to add a month term to our model

m2 <- lm(watertemp~timestep+month+air7day,data=means,na.action=na.exclude)
summary(m2)
# Check monthly residuals again
m2resids <- residuals(m2,na.action=na.exclude)
m2resid.month <- data.frame(resid=m2resids,month=means$month)
m2resid.month$month <- factor(m2resid.month$month,levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
boxplot(m2resid.month$resid~m2resid.month$month,las=1,ylab="resids",main="resids by month")
plot(means$dateyear,m2resids,type="l")

# Check residuals
Acf(residuals(m2))
Pacf(residuals(m2))
# We clearly still have residual issues, likely due to serial autocorrelation

rm(m2)
rm(m1)

# ##########
# # Remove the lag 1 correlation using a gls
# # memory.limit(size=25000) # temporarily changes size. Can only decrease by restarting R
# m3 <- gls(watertemp~timestep+month+air7day,correlation=corARMA(form =~timestep, p=2),method="REML",data=means,na.action=na.omit)
# m3resids <- residuals(m3,na.action=na.exclude)
# Acf(residuals(m3, type="normalized"))
# Pacf(residuals(m3, type="normalized"))
# 
# ##########
# # Try running an auto.arima model to get the autocorrelation structures narrowed down
# m.arima <- auto.arima(meansts,seasonal=TRUE,allowdrift=FALSE)
# summary(m.arima) 

##########
# Try including month as a random effect
m4 <- lme(watertemp~timestep+air7day, random = list( ~1|month,~1|week,~1|year),method="REML",data=means,na.action=na.exclude)
m4resids <- residuals(m4,na.action=na.exclude)
plot(means$dateyear,m4resids,type="l")
Acf(residuals(m4, type="normalized"))
Pacf(residuals(m4, type="normalized"))
m4predict <- predict(m4)

plot(means$dateyear,means$watertemp,type="l")
points(means$dateyear,m4predict,type="l",col="red")
mse.m4 <- mean((m4predict-means$watertemp)^2,na.rm=TRUE)
rmse.m4 <- sqrt(mse.m4)


# ##############################################
# # Model the data using harmonics to deal with seasonality
# # rather than the lm with month term
# 
# # Create a matrix of sin and cos terms
# SIN <- COS <- matrix(nr=length(means$dateindex), nc=10)
# # Set up harmonics to 10
# for (i in 1:10) {
#   COS[,i]<-cos(2*pi*i*means$dateindex/365)
#   SIN[,i]<-sin(2*pi*i*means$dateindex/365)
# }
# 
# m.har <-lm(watertemp ~ timestep +
#               COS[,1] + SIN[,1] + 
#               COS[,2] + SIN[,2] + 
#               COS[,3] + SIN[,3] + 
#               COS[,4] + SIN[,4] + 
#               COS[,5] + SIN[,5] +
#               COS[,6] + SIN[,6] +
#               COS[,7] + SIN[,7] +
#               COS[,8] + SIN[,8] +
#               COS[,9] + SIN[,9] +
#               COS[,10] + SIN[,10], 
#             data=means)
# summary(m.har)
# # It looks like only the first three to five terms are significant
# step(m.har)
# # Stepwise suggest something similar
# 
# m2.har <-lm(watertemp ~ timestep +
#               COS[,1] + SIN[,1] + 
#               COS[,2] + SIN[,2] +
#               COS[,3] + SIN[,3] +
#               COS[,4] + SIN[,4] +
#               COS[,5] + SIN[,5],
#               data=means)
# 
# summary(m2.har)
# # Look at residuals for serial autocorrelation
# m2.har.resids <- residuals(m2.har,na.action=na.exclude)
# Acf(m2.har.resids)
# Pacf(m2.har.resids)
# plot(means$dateyear,m2.har.resids)
# 
# # Looks like we have a lag of one that we need to account for 
# m3.har <- gls(watertemp~timestep+
#                 COS[,1] + SIN[,1] + 
#                 COS[,2] + SIN[,2] +
#                 COS[,3] + SIN[,3] +
#                 COS[,4] + SIN[,4] +
#                 COS[,5] + SIN[,5],
#                 correlation=corAR1(form = ~ timestep),method="REML",
#                 data=means,na.action=na.omit)
# 
# # Look at residuals again
# m3.har.resids <- residuals(m3.har,na.action=na.exclude)
# plot(means$dateyear,m3.har.resids)
# Acf(residuals(m3.har,type="normalized"))
# Pacf(residuals(m3.har,type="normalized"))