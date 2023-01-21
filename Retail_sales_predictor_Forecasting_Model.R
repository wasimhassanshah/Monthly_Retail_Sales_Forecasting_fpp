#install.packages("timetk")

#####################################

#We will use data to forecast the next two years of sales per day
 #1992 to 2020 data for retail sales

#####################################

#clear all variables in workspace

rm(list=ls()) # To remove objects in enviroment if any

#Load the forecasting package
library(fpp2)

library(tidyverse)

library(plotly)
# Load the data

library(tseries)


data <- read.csv("real_sales_per_day_92_18.csv") 

data <- data%>% na.omit()

# This is time series data : One variable is measured over time


#Declare this time series data

Y <- ts(data[ ,2], start = c(1992,1), frequency = 12) # declaring second column (value) of our data as time series
 #telling R that data starts from Jan 1992, and have 12 months in each year as frequency

#############################################
# Preliminary Analysis
#############################################

# Augmented Dickey-Fuller Test  (Hypothesis: if p is > 0.01 then series in stationary)


adf.test(Y, k=12)
 #here p is 0.495 so series is non stationary


#######
#Trend
######

# Timeplots (Plotting our data our time)

autoplot(Y) +
  ggtitle("Time Plot: Real US Retail Sales Per Day") +
  ylab("Millions of 2017 DOllars")




###Findings:
# So data has strong trend +ve trend then a dip at 2077-2008 period(recession) then again positive trend
#. Investigate transformation
# Transform data to get rid of deasonal trends

# Take the first difference of data to remove the trend
 # means change in sales from Jan 1991 to Feb 1991 next month next month and so on



DY1 <- diff(Y, differences = 1) # Difference can be 1,2,3.. until series become stationary (p<0.01)

adf.test(DY1, k=12) # Now p value is less than 0.
#Now p value is 0.02, still series is non stationary


DY <- diff(Y, differences = 2) 

adf.test(DY, k=12)
#Now p value is 0.01 so series is stationary



# Timeplots (Plotting our data our time) of differenced data

 timeplot<-autoplot(DY) +
  ggtitle("Time Plot of Differenced Data: Real US Retail Sales Per Day") +
  ylab("Millions of 2017 DOllars")
 
 ggplotly(timeplot)
 
 

#Now we have change from month to month
# Now we get rid of trend and now data is pretty flat
# Though there are big fluctuatons but there is no longer trend anymore
#So here the big negative values are happening in January as found in below seasonal plots

#############
#Seasonality
############

#Seasonal plots

#Now we have these fluctuations we want to see if they're happening in the same month  every year or
# if they are irregular that's going to make a difference for how we want to model the data and how we ant to make our forecast


# So, Series appears trend-stationary, use to investigate seasonality

seasonalplot.1 <-ggseasonplot(DY) +
  ggtitle("Seasonal Plot: Change in Daily Retail Sales") +
  ylab("Millions of 2017 DOllars")

 ggplotly(seasonalplot.1)
 

# In Seasonal plot  each year  gets its own line
# It plots one year at a time and plot it across each month

###Findings:

# So it looks like , the change from Dec to January is always a drop, always negative 
# Size of the drop may differ in different years, but the data has very large drop bw December and january
# So this is suggesting that we have regular seasonality

#And

#We also see clear seasonal patterns in December, where data is clearly growing from November to Dec for each year (1992 to 2020)
# So when we think about retail sales what happens in December : Christmas
# So there is a build up of sales before Christmas and once Christmas ends retal sales drop off and then slowly start build up throught the year again

# So, we see clear seasonal trends in our data

# Another seasonal plotsting way, the seasonal subseries plots 

 seasonalplot.2 <- ggsubseriesplot(DY)+
  ggtitle("Seasonal Plot: Change in Daily Retail Sales") +
  ylab("Millions of 2017 DOllars")


ggplotly(seasonalplot.2)



#So we can say in time plot all those big negative values are happening in January of every year as found in below seasonal plots

#And that will influence the type of model we want to use to forecast with


#############################################
#Our Series Y, has trend and seasonality
# To remove the Trend we take the first difference
#The first diffenreced series still has seasonality

#So, Let,s Forecast with various methods
#############################################



############

#1st Method: Use a benchmark method to forecast: Simple method, it can be influenced by seasonal patterns
#it says take the mean of the data and just forecast everymonth in the future will be the same as the in sample mean,
#for our data having seasonal patterns this will not work well, we take the mean of the Differenced data. it won't forecast very well
#because it is going to miss these big swings we have in Jan and Dec of each year
############

# 2nd Method: Seasonal Naive method as our benchmark:
#it says that the value of the data for ex: the value in Jan 1994 will be equal to the value in Jan 1993 + some random error
# so for Feb etc

#y_t = y_{t-s} + e_t

# Because we have strong seasonality this will be great method to use, be careful to use this method only with Differenced data
#(not with trand data as there is trend in that data with each passing year and same months are not equal so the propostion this method makes fails)


############


fit <- snaive(DY) #it produces a list

print(summary(fit))

#Residual sd (standard deviation): tells us our residual standard deviation that's measure of how well is our data is fitting, the smaller the numbers (closer to zero), the better
# We have "Residual sd: 10427.3529  (very high)
#Residual sd: is our benckmark , it says the values in current month is the same as the value in the previous year's same month
# it's value is the missing on average by roughly 10427.3 million dollars

checkresiduals(fit) # residual plots

#ACF is left over error term, the part of the data that the model can't explain
# We don't want any autocorrelation our time, so want all ACF bars withing those two blue dashed lines which are the 95% confidence interval
# So we have some autocorrelation left over in the residual of this model which is not a good thing, that means we are
# leaving infromation on the table ,
#Means the seasonal Naive model is not using the data as well as it could be using
#SO there is probably a better model out there to try


######## Residual sd: 10427.3529 

############
#One guide how good we forecast is the Standard deviation (SD) of residuals and how good the ACF fall within COnfidence Interval





############################################################
#3rd Method: ETS (Exponential Smoothing models)
############################################################
# So there is built in funtion in R that can try every possible exponential smoothing model and return the one that is the best
# the cool thing about the exponential smoothers is that we can  use  the data itself, so exponential smoothers can allow for trend
#SO if the computer thinks that there is a trend in the data based  on the test set it is doing , it can include that
# So we can use regular data itself rather than Differenced data

fit_ets <- ets(Y) #it produces a list

print(summary(fit_ets))

#ETS(M,A,M) is found to be the ETS model in this case
#  sigma:  0.0229 here is the same thing as the residuals SD # So smaller the number , the better the fit
# So low SD atleast shows the in sample ets is fitting better that  the seasonal naive
# Although in ACF plot , it looks like our bar crosses blue bar lines (95% Confidence interval
# which means that there's information in the data that the model is not using efficiently
# We need to use another model where information loss is minimum

 
checkresiduals(fit_ets)

############ Residual SD sigma:  0.0229



######################
#Methond 4: Simple Arima Model

######################

# ACF COrrealtion plots
# Choose closest to 0 and least value to keep things simple
Pacf(DY) # Pacf Plot to choose p value for ARIMA from plot, p=6

Acf(DY) # Acf plot to choose q value for ARIMA, q=6

ts_Model <- Arima(y= DY, order = c(7,2,6)) #2 represents the 2nd difference for timeserie

print(ts_Model)

armafcst <- forecast(ts_Model, h=12)

armafcst

autoplot(armafcst)


######################
# Method 5 : Auto ARIMA Model
# In this model, data needs to be stationary: For that, we can use differenced data to get rid of the trend and then tell ARIMA model that  there is seasonality going on
# Or with other methods
######################

#auto.arima() will try out a whole bunch of different ARIMA methods and return the model that fits the best

fit_arima <- auto.arima(Y, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) #Using regular data Y, though this data had a trend. So to manage that we can put d=1 which means we tell ARIMA model that before u fit the ARIMA Model take Difference of data first
                                     # We can also get rid of seasonality by taking first seasonal difference with putting D=1
# Making stepwise = FALSE, approximation = FALSE, make model fast ,
## approximation defines the best model byapproxcimating AIC instead of computing the exact AIC that saves time
# We not need to save much time as we are jsut considering one time series, so we turn approximation as FALSE
## stepwise instead of literally trying every possible combination of ARIMA model, it is going to try out onl a few,, so it will modigy them a tiny bit  and see if it fits better
#Again that will save a lot time , so wee not need to consider that as we have only one time seires so we set it as FALSE

##trace = TRUE is going to print out all the  models it is trying out


print(summary(fit_arima))

#sigma^2 this is squared error rather tha SD, for SD take sqrt(value of sigma^2 )
# it gives sqrt(77510399) = 9903 which is Residual SD


checkresiduals(fit_arima)
#Here autocorrelation is very low, it is best in the case of lowest autocorrelation


sqrt(37071926)

############ Residual SD = 9903



############ ############ ############ 
#Forecasting with Best Model
############ ############ ############ 

############ Using ARIMA MODEL to Forecast ############ 

fcst <- forecast(fit_arima, h=24) #h=24 tells how far to frecast, 24 means forecast for next 24 months (2 years ahead)

autoplot(fcst) # autoplot plots all the historical data as well as our forecast

autoplot(fcst, include=180) # include=180 means include only last 180 months (Last 15 years) to increase visibility of forecast

#In graph #Dark blue line is point forecast and medium blue shade shows how far these forecast might go (lower and upper 80% interval) and light blue shade is 95% confidence interval

print(summary(fcst)) #to print out all the forecast values, Jan 2021 to Dec 2022




############ Using ETS Model to Forecast ############ 





fcst2 <- forecast(fit_ets, h=24)

fcst2 

autoplot(fcst2, include=180)

print(summary(fcst2))













































































































































