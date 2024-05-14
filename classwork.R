
insurance_data<-read.csv("insurance.csv")
insurance_data
View(insurance_data)
insurance_data$sex<-factor(insurance_data$sex,levels=c("male","female"),ordered = FALSE)
is.factor(insurance_data$sex)
insurance_data$smoker<-factor(insurance_data$smoker,levels=c("yes","no"),ordered = FALSE)
is.factor(insurance_data$smoker)
insurance_data$region<-factor(insurance_data$region,levels=c("northeast","northwest","southeast","southwest"),ordered = FALSE)
is.factor(insurance_data$region)
str(insurance_data)

install.packages("psych")
library(psych)

windows(20,10)
pairs.panels(insurance_data,
             smooth = TRUE,
             scale=FALSE,
             density=TRUE,
             ellipses=TRUE,
             method="spearman",
             pch=21,
             lm=FALSE,
             cor=TRUE,
             jiggle=FALSE,
             factor=2,
             hist.col=4,
             stars=TRUE,
             ci=TRUE)
attach(insurance_data)
model<-lm(charges~
            age+
            sex+
            bmi+
            children+
            smoker+
            region)
model
summary(model)
saveRDS(model,"./insurance_model.rds")
#Equation
#charges ~ 11778.7+256.9*age+339.2*bmi+475.5*children -23848.5*smokerno-1035.0*regionsoutheast- 960.1*regionsouthwest


#Model 2
model_2<-lm(charges~
            age+
            bmi+
            children+
            smoker+
            region)
model_2
summary(model_2)
saveRDS(model_2,"./insurance_model2.rds")
#charges~11846.03+256.97*age+338.66*bmi+474.57*children-23836.30*smokerno-1034.36*regionsoutheast-958.37*regionsouthwest
AIC(model)
AIC(model_2)
BIC(model)
BIC(model_2)
paste('AIC of first model',round(AIC(model),2))
paste('AIC of second model',round(AIC(model_2),2))

paste('BIC of first model',round(BIC(model),2))
paste('BIC of second model',round(BIC(model_2),2))

##use format %f,%e,%g or %a for numeric objects
#else we can use sprintf() or printf() instead of paste


