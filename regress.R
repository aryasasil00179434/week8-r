var<-lm(weight ~ height,data=women)
var
summary(var)
View(women)
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
