#R script file for tutorial on a forecast accuracy study.
#The data are in a csv file called "Timeseries". The product information has been disguised.


#### Prep: Load R package; Set working directory; Load the data to R
# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# We next load some packages that are generally useful for data analytics.
#In each case, check first to see if a required package is installed. If not, then install it 
# and invoke it as a library.
if(!require(stats)){
  install.packages("stats")
  library(stats)
}

# Similarly, we need to change the working directory to the directory where we want to read and save data
# Define a variable, wd, with a string describing the path to the directory where we want to read and save data
# Change this to meet your needs.!!!!!!!!!
wd <- ""
# Set the working directory to this path
setwd(wd)
# Test by displaying the current working directory
getwd()

# read the data from the data file
result <- read.csv(file = "Timeseries.csv")
head(result)

####################### Forecasting in R Starts ######################################
# We will run different kinds of forecast for a specific product in the database

# Note that the datafile has provided the company's forecast in the column "forecast"
# We first plot the actual demand against the company's forecast for comparison
forecasts <- result$forecast
  actuals <- result$actual
  plot(actuals)
  # Connect the points with lines so the sequence is clear
  lines(actuals)
  # Now add the corporate forecasts as red circles
  points(forecasts,col=c("red"))
  # How good are the forecasts? Perfection in the next plot would be a straight line angled at 45 degrees
  # You could also plot the actual against forecast by uncomment the command below
  # plot(actuals,forecasts)
  # So, the forecasts are not good. 
  
# Let's develop a quantitative measure of forecast accuracy.
  errors <- forecasts-actuals
  # Take the absolute difference of forecasts versus actuals.
  abserrors <- abs(errors)
  # Compute the total of the absolute errors
  totalerror <- sum(abserrors)
  # Compute the total actual demand
  totalactual <- sum(actuals)
  # The ratio is the relative absolute error of forecast
  if (totalactual>0) {
      relativeabsoluteerror <- totalerror/totalactual
  } else {
    relativeabsoluteerror <- 0
  }
  # Practitioners multiply the relative absolute error this by 100 to get what they call the WAPE (Weighted Average Percent Error)
  WAPE <- relativeabsoluteerror*100
  WAPE
  # So, the Weighted Average Percent Error is about 49%. That is pretty high. 
  # And this is the largest selling product in the largest segment in the largest region!
  # The forecast errors for smaller selling products will likely be much worse.
####################### Grab data for a specific product Ends ###################################### 


# Next we try different forecasting methods in R
############################ Moving Average Forecasting Starts  ###################################################
  # The moving average technique takes a vector of weights over a certain number of periods. Let's do a moving average over three periods.
  weights <- rep(1/3,3)
  weights
  # There are many ways to implement moving average in R. The filter method is easy. 
  # The argument "Sides=1" says to just use data to the left of point to be forecast.
  maforecasts <- round(filter(actuals,weights,method="convolution",sides=1,circular=FALSE))
  # The first few fitted values are NA because there is insufficient history to calculate the moving average.
  # Omit NA entries.
  maforecasts <- na.omit(maforecasts)
  # The resulting structure is a time series. We just want the fitted values
  class(maforecasts)
  forecasts <- as.numeric(maforecasts)
  str(forecasts)
  
# Plot the data
  # Omit the first few entries in actuals which has not predictions
  actualsnew <- tail(actuals, length(forecasts))
  plot(actualsnew)
  # Connect the points with lines so the sequence is clear
  lines(actualsnew)
  # Now add the corporate forecasts as red circles
  points(forecasts,col=c("red"))
  title("Moving Average 3")

# Calculate the quantitative measure of forecast accuracy.
  errors <- forecasts-actualsnew
  # Take the absolute difference of forecasts versus actuals.
  abserrors <- abs(errors)
  # Compute the total of the absolute errors
  totalerror <- sum(abserrors)
  # Compute the total actual demand
  totalactual <- sum(actualsnew)
  
  # The ratio is the relative absolute error of forecast
  if (totalactual>0) {
      relativeabsoluteerror <- totalerror/totalactual
  } else {
    relativeabsoluteerror <- 0
  }
  # Practitioners multiply the relative absolute error this by 100 to get what they call the WAPE (Weighted Average Percent Error)
  WAPE <- relativeabsoluteerror*100
  WAPE
  # So, the Weighted Average Percent Error is about 22%. That is much better.
############################  Moving Average Forecasting Ends ###################################################

# We try another powerful technique Holt-Winters method.
############################  Holt-Winters method Starts ###################################################

################# Get read for Holt_Winters #################
  # Holt-Winters requires a time series structure. So we convert the actuals vector into a time series by specifying the frequency (in this case, the number of months per year)
  tsactuals <- ts(actuals,frequency=12)
  str(tsactuals)
  plot(tsactuals)

# Here we try three Holt-Winters methods
# We first try Holt-Winters with just exponential smoothing (beta=False,gamma=FALSE)
################# Holt-Winters with just exponential smoothing (beta=False,gamma=FALSE) #################
  hw <- HoltWinters(tsactuals, beta=FALSE,gamma=FALSE)
  # THe Holt Winters data structure is complicated. We want to extract the first column of the fitted values.
  hwfitted <- fitted(hw)[,1]
  # The fitted values are a time series so we convert it to numeric (and round them to whole numbers for simplicity).
  forecasts <- round(as.numeric(fitted(hw)[,1]))
    
# Plot the Holt-Winters forecasts
# The fitted values are missing the first entry, so we the first entries in actuals which has not predictions
  actualsnew <- tail(actuals, length(forecasts))
  plot(actualsnew)
  # Connect the points with lines so the sequence is clear
  lines(actualsnew)
  # Now add the corporate forecasts as red circles
  points(forecasts,col=c("red"))
  title("Exponential Smoothing")

# Calculate the quantitative measure of forecast accuracy.
  errors <- forecasts-actualsnew
  # Take the absolute difference of forecasts versus actuals.
  abserrors <- abs(errors)
  # Compute the total of the absolute errors
  totalerror <- sum(abserrors)
  # Compute the total actual demand
  totalactual <- sum(actualsnew)
  # The ratio is the relative absolute error of forecast
  if (totalactual>0) {
      relativeabsoluteerror <- totalerror/totalactual
  } else {
    relativeabsoluteerror <- 0
  }
  # Practitioners multiply the relative absolute error this by 100 to get what they call the WAPE (Weighted Average Percent Error)
  WAPE <- relativeabsoluteerror*100
  WAPE


# We secondly try Holt-Winters without seasonality (double exponential smoothing (gamma=FALSE)
################# Holt-Winters without seasonality (gamma=FALSE) #################
  # Now call the Holt-Winters function without seasonality (gamma=FALSE). This is equivalent to double exponential smoothing.
  hw <- HoltWinters(tsactuals, gamma=FALSE)
  # THe Holt Winters data structure is complicated. We want to extract the first column of the fitted values.
  hwfitted <- fitted(hw)[,1]
  # The fitted values are a time series so we convert it to numeric (and round them to whole numbers for simplicity).
  forecasts <- round(as.numeric(fitted(hw)[,1]))

####################Input your code for plotting and calculating WAPE ##########
  actualsnew <- tail(actuals, length(forecasts))
  plot(actualsnew)
  lines(actualsnew)
  points(forecasts,col=c("red"))
  title("Double Exponential Smoothing")
  
  # Calculate the quantitative measure of forecast accuracy.
  errors <- forecasts-actualsnew
  # Take the absolute difference of forecasts versus actuals.
  abserrors <- abs(errors)
  # Compute the total of the absolute errors
  totalerror <- sum(abserrors)
  # Compute the total actual demand
  totalactual <- sum(actualsnew)
  # The ratio is the relative absolute error of forecast
  if (totalactual>0) {
    relativeabsoluteerror <- totalerror/totalactual
  } else {
    relativeabsoluteerror <- 0
  }
  # Practitioners multiply the relative absolute error this by 100 to get what they call the WAPE (Weighted Average Percent Error)
  WAPE <- relativeabsoluteerror*100
  WAPE
  
# Finally we try the full Holt-Winters
################# Holt-Winters Forecasting #################
####################Input your code for forecasting using Holt-Winters ##############
  hw <- HoltWinters(tsactuals)
  hwfitted <- fitted(hw)[,1]
  forecasts <- round(as.numeric(fitted(hw)[,1]))
  
####################Input your code for plotting and calculating WAPE ##########
  # Plot the Holt-Winters forecasts
  # The fitted values are missing the first entry, so we the first entries in actuals which has not predictions
  actualsnew <- tail(actuals, length(forecasts))
  plot(actualsnew)
  # Connect the points with lines so the sequence is clear
  lines(actualsnew)
  # Now add the corporate forecasts as red circles
  points(forecasts,col=c("red"))
  title("Holt-Winters")
  
  # Calculate the quantitative measure of forecast accuracy.
  errors <- forecasts-actualsnew
  # Take the absolute difference of forecasts versus actuals.
  abserrors <- abs(errors)
  # Compute the total of the absolute errors
  totalerror <- sum(abserrors)
  # Compute the total actual demand
  totalactual <- sum(actualsnew)
  # The ratio is the relative absolute error of forecast
  if (totalactual>0) {
    relativeabsoluteerror <- totalerror/totalactual
  } else {
    relativeabsoluteerror <- 0
  }
  # Practitioners multiply the relative absolute error this by 100 to get what they call the WAPE (Weighted Average Percent Error)
  WAPE <- relativeabsoluteerror*100
  WAPE

###############################################################################
# Out of all the four forecasting methods above, which one gives the lowest WAPE? 
############################  Holt-Winters method Ends ###################################################

####################### Forecasting in R Ends ######################################

