var <- lm( weight ~ height, data = women)
var
summary(var)
View(women)


str(cars)
windows(20,12)
par(mfrow=c(1,3))
scatter.smooth(x = cars$speed, y = cars$dist, main = "Dist ~ Speed")
boxplot(cars$speed)
boxplot(cars$dist)

View(cars)


install.packages("e1071")
library (e1071)

# divide graph area in 2 columns
windows(20,12)
par(mfrow = c(1,2))
# density plot for 'speed'
plot (density(cars$speed ),
      main = "Density Plot: Speed", 
      ylab = " Frequency", 
      sub = paste(" Skewness:",
      round(e1071::skewness( cars$speed ),2)))
polygon(density( cars$speed ), col = "blue")

# density plot for 'dist'
plot(density(cars$dist ), 
     main = "Density Plot: Distance", 
     ylab = " Frequency", 
     sub = paste(" Skewness:",
     round(e1071::skewness( cars$dist ), 2)))
polygon(density( cars$dist ), col = "red")
      
      
# calculate correlation between speed and distance
cor(cars$speed , cars$dist )

# build linear regression model on full data
linearMod <- lm( dist ~ speed, data = cars)
linearMod

###  Distance ~ -17.58 + 3.93 * Speed

# model summary
summary(linearMod)

AIC(linearMod) #AIC is a metric that is used to compare the fit of different regression models
              #AIC = 2K + n log(RSS/n)
              #most accurate model has the smallest AIC.
BIC(linearMod)  #Bayesian Information Criterion (BIC) is used in model selection for linear regression
                #BIC = n ln(SSE) − n ln(n) + ln(n)p




# sample chooses a random sample
# from 1:all records from cars, 80% of rows
no_of_records <- sample(1:nrow(cars), 0.8 * nrow (cars))

str(no_of_records)


# model training data
training_data <-   cars[ no_of_records,]
training_data

# test data
testing_data <- cars[-no_of_records,]
testing_data

# Build the model on training data
lr_model <- lm( dist ~ speed, data = training_data)

lr_model
summary(lr_model)

# Distance ~ -18.10 + 3.93 *Speed
# Distance ~ -18.10 + 3.93 * 10 = -18.10 +39.3 = 21.20


# predict distance from testing data
dist_predicted <- predict( lr_model , testing_data )

dist_predicted




# make actuals_predicteds dataframe
actuals_preds <- data.frame (cbind 
                          (actuals = testing_data$dist,
                          predicted = dist_predicted))
                  
                  
actuals_preds
head(actuals_preds)

#correlation_accuracy
attach(actuals_preds)
correlation_accuracy <- cor (actuals,predicted)
correlation_accuracy

correlation_accuracy_1 <- cor (actuals_preds)
correlation_accuracy_1


# Minmax accuracy
min_max_accuracy <-mean(apply( actuals_preds , 1, min)/
                         apply( actuals_preds , 1,max))
min_max_accuracy


# MAPE -- Mean Absolute Percentage Error
attach(actuals_preds)
mape <- mean(abs(predicted - actuals ) / actuals )
mape

# save a model
saveRDS(lr_model, "./cars_model.rds")

# load a model
lr_model_1 <- readRDS("./cars_model.rds")


# load the 




#K-fold cross validation

install.packages ("DAAG")
library (DAAG)

windows(20,10)
cvResults <- suppressWarnings (CVlm 
                          (data = cars, 
                          form.lm = dist ~ speed, 
                          m = 5, 
                          dots = FALSE , 
                          seed = 29, 
                          legend.pos = "topleft",
                          printit = FALSE , 
                          main = "Small symbols are 
                              predicted values while
                            bigger ones are actuals."))
cvResults
