var<-lm(weight ~ height,data=women)
var
summary(var)
View(women)
#depende=distance
#indep-Speed
#y=b0+b1*x
#weight~ -87.52+3.43*height

cars<-cars
View(cars)
summary(cars)

windows(20,12)
scatter.smooth(x=cars$speed,
               y=cars$dist,main="Dist~Speed")
windows(20,12)
par(mfrow=c(1,2))
#its for checking varience
boxplot(cars$speed,main="BOX PLOT- Speed")
boxplot(cars$dist,main="BOX Plot -Distance")
#here varience is not study..so no linear regression

#check Normality
windows(20,16)
par(mfrow=c(1,2))
plot(density(cars$speed),
     main="Density Plot:Speed",
     ylab="Frequency")
polygon(density(cars$speed),col="blue")

plot(density(cars$dist),
     main="Density Plot:Distance",
     ylab="Frequency")
polygon(density(cars$dist),col="red")

#Correlation
cor(cars$speed,cars$dist)
#correlation matrix
cor(cars)



#linear Regression
attach(cars)
linearmodel<-lm(dist ~ speed)
linearmodel


##Distance~ Independent~-17.58+3.932* Speed
##Model Summary
summary(linearmodel)

##compare the fit.AIC value smaller~
AIC(linearmodel)
##BIC value smaller~
BIC(linearmodel)
#Speed and Distance are linearly related



no_of_record<-sample(1:nrow(cars),0.8*nrow(cars))
str(no_of_record)


Training_data<-cars[no_of_record,]
Training_data
  

Testing_Data<-cars[-no_of_record,]
Testing_Data


lr_model<-lm(dist ~speed,data=Training_data )
lr_model

summary(lr_model)

#Distance~-13.84+3.70*speed
#
dist_predict<-predict(lr_model,Testing_Data)
dist_predict


#making Actual predicts dataframe
actuals_predict<-data.frame(cbind(actuals=Testing_Data$dist,
                                  predicted=dist_predict))
actuals_predict
attach(actuals_predict)
correlation_accuracy<-cor(actuals,predicted)
correlation_accuracy

cor(actuals_predict)


min_max_accuracy<-mean(apply(actuals_predict,1,min)/
                       apply(actuals_predict,1,max))
min_max_accuracy

#MAPE-Mean Absolute percentage error
mape<-mean(abs((actuals_predict$predicted - actuals_predict$actuals)) / actuals_predict$actuals)
mape

#K-fold cross Validation
install.packages("DAAG")
library(DAAG)
windows(20,16)
cvresults<-suppressWarnings(CVlm(data=cars,
                                 form.lm = dist~speed,
                                 m=5,
                                 dots = FALSE,
                                 seed = 29,
                                 legend.pos = "topleft",
                                 printit = FALSE,
                                 main="small symbols are predicted values while"))
cvresults

saveRDS(lr_model,"./cars_model.rds")

lr_modelss<-readRDS("./cars_model.rds")
lr_modelss
