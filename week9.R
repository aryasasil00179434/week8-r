states<-as.data.frame(state.x77)
str(states)

View(states)
# Rename column names
colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

windows(20,10)
pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
windows(20,16)
par(mfrow=c(4,2))
scatter.smooth(x=states$Population,
               y=states$Murder,
               xlab = "Population",
               ylab = "Murder %",
               main="correlation of Murder ~ Population")

scatter.smooth(x=states$Income,
               y=states$Murder,
               xlab = "Income ",
               ylab = "Murder %",
               main="correlation of Murder ~ Income ")

scatter.smooth(x=states$Illiteracy,
               y=states$Murder,
               xlab = "Illiteracy",
               ylab = "Murder %",
               main="correlation of Murder ~ Illiteracy")

scatter.smooth(x=states$Life_Exp,
               y=states$Murder,
               xlab = "Life_Exp",
               ylab = "Murder %",
               main="correlation of Murder ~ Life_Exp")

scatter.smooth(x=states$HS_Grad,
               y=states$Murder,
               xlab = "HS_Grad",
               ylab = "Murder %",
               main="correlation of Murder ~ HS_Grad")

scatter.smooth(x=states$Frost,
               y=states$Murder,
               xlab = "Frost",
               ylab = "Murder %",
               main="correlation of Murder ~ Frost")
scatter.smooth(x=states$Area,
               y=states$Murder,
               xlab = "Area",
               ylab = "Murder %",
               main="correlation of Murder ~ Area")

cor(states$Murder,states$Population)
cor(states$Murder,states$Income)
cor(states$Murder,states$Illiteracy)
cor(states$Murder,states$Life_Exp)
cor(states$Murder,states$HS_Grad)
cor(states$Murder,states$Frost)
cor(states$Murder,states$Area)
correlation_matrix<-cor(states)

#plot the matrix
correlation_matrix<-cor(states)
windows(20,16)
corPlot(correlation_matrix)










attach(states)
model<-lm(Murder~
            Population +
            Income +
            Illiteracy+
            Life_Exp +
            HS_Grad +
            Frost +
            Area)
model
summary(model)
saveRDS(model,"./model_states.rds")
