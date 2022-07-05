# Set the working directory to the location of your data file
setwd()
# Read in the data from a comma-separated file
ccdata <-read.csv("cheddar-cheese.csv")
# Display the first few rows of the data
head(ccdata)
# Scatterplots of each pair of variables
plot(ccdata)
# Try a model with just Acetic
model <- Taste ~Acetic
result <-lm(model,ccdata)
summary(result)


# BREAK DOWN CODE
fit <- result$fitted.values
taste <- ccdata$Taste
r1 <- c(0,max(ccdata$Taste))
plot(fit,taste,xlim=r1,ylim=r1)
lines(r1,r1)
title("Actual vs. Fitted Values")

residuals <- taste-fit
plot(taste,residuals)
lines(r1,c(0,0))
residualfit <- lowess(taste,residuals,f=0.8)
lines(residualfit,col=c("red"))
title("Residuals vs. Actual Values")
