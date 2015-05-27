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
head(means)
plot(means$dateyear,means$watertemp,type="l")

#===========================================
# Trunate the data to have enough ram on Annika's computer
#means <- subset(means,dateyear > "1985-01-01" & dateyear < "1989-01-01")
#plot(means$dateyear,means$watertemp,type="l")
#===========================================

# Turn the water temp data into a time series for potential future analysis...
meansts <- ts(means$watertemp,frequency=1)
plot(meansts)

# Step 1:
# Write a basic linear model with time and water temperature
m1 <- lm(watertemp~dateyear,data=means,na.action=na.exclude)
summary(m1)
plot(means$dateyear,means$watertemp,type="l")
abline(m1,col="red")
# The slope is very significant, but may be because we are not accounting for seasonality

# Look at residual patterns by month to see if we should account for seasonality
m1resids <- residuals(m1,na.action=na.exclude)
m1resid.month <- data.frame(resid=m1resids,month=means$month)
m1resid.month$month <- factor(m1resid.month$month,levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
boxplot(m1resid.month$resid~m1resid.month$month,las=1,ylab="resids",main="resids by month")
# We clearly have seasonal correlation, so we need to add a month term to our model

m2 <- lm(watertemp~dateyear+month,data=means,na.action=na.exclude)
summary(m2)
# Check monthly residuals again
m2resids <- residuals(m2,na.action=na.exclude)
m2resid.month <- data.frame(resid=m2resids,month=means$month)
m2resid.month$month <- factor(m2resid.month$month,levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
boxplot(m2resid.month$resid~m2resid.month$month,las=1,ylab="resids",main="resids by month")

# Check residuals
Acf(residuals(m2))
Pacf(residuals(m2))
# We clearly still have residual issues, likely due to serial autocorrelation

# Remove the lag 1 correlation using a gls
# memory.limit(size=25000) # temporarily changes size. Can only decrease by restarting R
m3 <- gls(watertemp~dateindex+month,correlation=corARMA(form =~dateyear, p=1),method="REML",data=means,na.action=na.omit)
m3resids <- residuals(m3,na.action=na.exclude)
Acf(residuals(m3),type="normalized")
Pacf(residuals(m3),type="normalized")

#================
# Can't run the above code because I'm running into ram issues. Even
# when I change the max memmory allocation it's still problematic.
# Not really sure why it's happening.
#===================


##############################################
# Model the data using harmonics to deal with seasonality
# rather than the lm with month term

# Create a matrix of sin and cos terms
SIN <- COS <- matrix(nr=length(means$dateindex), nc=10)
# Set up harmonics to 10
for (i in 1:10) {
  COS[,i]<-cos(2*pi*i*means$dateindex/365)
  SIN[,i]<-sin(2*pi*i*means$dateindex/365)
}

m.har <-lm(watertemp ~ dateindex +
              COS[,1] + SIN[,1] + 
              COS[,2] + SIN[,2] + 
              COS[,3] + SIN[,3] + 
              COS[,4] + SIN[,4] + 
              COS[,5] + SIN[,5] +
              COS[,6] + SIN[,6] +
              COS[,7] + SIN[,7] +
              COS[,8] + SIN[,8] +
              COS[,9] + SIN[,9] +
              COS[,10] + SIN[,10], 
            data=means)
summary(m.har)
# It looks like only the first three to five terms are significant
step(m.har)
# Stepwise suggest something similar

m2.har <-lm(watertemp ~ dateyear +
              COS[,1] + SIN[,1] + 
              COS[,2] + SIN[,2] +
              COS[,3] + SIN[,3] +
              COS[,4] + SIN[,4] +
              COS[,5] + SIN[,5],
              data=means)

summary(m2.har)
# Look at residuals for serial autocorrelation
m2.har.resids <- residuals(m2.har,na.action=na.exclude)
Acf(m2.har.resids)
Pacf(m2.har.resids)

# Looks like we have a lag of one that we need to account for 
m3.har <- gls(watertemp~dateyear+
                COS[,1] + SIN[,1] + 
                COS[,2] + SIN[,2] +
                COS[,3] + SIN[,3] +
                COS[,4] + SIN[,4] +
                COS[,5] + SIN[,5],
                correlation=corAR1(form = ~ dateyear),method="REML",
                data=means,na.action=na.omit)

# Look at residuals again
m3.har.resids <- residuals(m3.har,na.action=na.exclude)
Acf(m3.har.resids,type="normalized")
Pacf(m3.har.resids,type="normalized")
