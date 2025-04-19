library(rgl)
library(olsrr)
library(car)

shr<-read.table(file="shr.txt",header=TRUE)  # ch 4 scotish hill race data
pairs(Time ~ Distance+Climb,data=shr, main="Simple Scatterplot Matrix")
plot3d(x = shr$Distance, y = shr$Climb, z = shr$Time)
model<-lm(formula = Time ~ Distance+Climb, data = shr)
summary(model)
plot(model)
hist(model$residuals)
plot(model$residuals, pch = 16, col = "red")
plot(cooks.distance(model), pch = 16, col = "blue") #Plot the Cooks Distances.
ols_plot_cooksd_bar(model) #olsrr package
ols_plot_dfbetas(model)
ols_plot_dffits(model)
ols_plot_resid_stand(model)
ols_plot_hadi(model)
ols_plot_resid_pot(model)
avPlots(model)  #car package
crPlots(model)

model<-lm(formula = Time ~ Distance+Climb, data = shr)
summary(model)
qqPlot(model)
shapiro.test(model$residuals)
ncvTest(model)
vif(model)
residualPlots(model)
outlierTest(model)
influencePlot(model)
influenceIndexPlot(model)

shr2 <- shr[-c(7, 18), ]
model<-lm(formula = Time ~ Distance+Climb, data = shr2)
summary(model)
qqPlot(model)
shapiro.test(model$residuals)
ncvTest(model)
vif(model)
residualPlots(model)
outlierTest(model)
influencePlot(model)
influenceIndexPlot(model)

shr3 <- shr[-c(7,18,33), ]
model<-lm(formula = Time ~ Distance+Climb, data = shr3)
summary(model)
qqPlot(model)
shapiro.test(model$residuals)
ncvTest(model)
vif(model)
residualPlots(model)
outlierTest(model)
influencePlot(model)
influenceIndexPlot(model)

shr3 <- shr[-c(7,11,18,33), ]
model<-lm(formula = Time ~ Distance+Climb, data = shr3)
summary(model)
qqPlot(model)
shapiro.test(model$residuals)
ncvTest(model)
vif(model)
residualPlots(model)
outlierTest(model)
influencePlot(model)
influenceIndexPlot(model)

hwh <-read.table(file="hwh.txt",header=TRUE) #ex 2.10 pg 51

cov(hwh$Husband,hwh$Wife)
cor(hwh$Husband,hwh$Wife)
hwh2 <- hwh*0.3937

cov(hwh2$Husband,hwh2$Wife)
cor(hwh2$Husband,hwh2$Wife)

plot(hwh$Husband,hwh$Wife)
model<-lm(formula = Wife ~ Husband, data = hwh)
summary(model)
qqPlot(model)
shapiro.test(model$residuals)
ncvTest(model)
vif(model)
residualPlots
ols_plot_resid_stand(model)
outlierTest(model)
influencePlot(model)
influenceIndexPlot(model)

exm <-read.table(file="exm.txt",header=TRUE) #ex3.3 pg 82
model<-lm(formula = F ~ P1, data = exm)
model<-lm(formula = F ~ P2, data = exm)
model<-lm(formula = F ~ P1+P2, data = exm)

summary(model)
qqPlot(model)
shapiro.test(model$residuals)
ncvTest(model)
vif(model)
residualPlots(model)
ols_plot_resid_stand(model)
outlierTest(model)
influencePlot(model)
influenceIndexPlot(model)

ex <-read.table(file="ex4_1.txt",header=TRUE)  #ex4.12 pg 127
model<-lm(formula = Y ~ X1+X2+X3+X4+X5+X6, data = ex)
summary(model)
qqPlot(model)
shapiro.test(model$residuals)
ncvTest(model)
vif(model)
residualPlots(model)
ols_plot_resid_stand(model)
outlierTest(model)
influencePlot(model)
influenceIndexPlot(model)

plot(model$residuals, pch = 16, col = "red")
plot(cooks.distance(model), pch = 16, col = "blue") #Plot the Cooks Distances.
ols_plot_cooksd_bar(model) #olsrr package
ols_plot_dfbetas(model)
ols_plot_dffits(model)
ols_plot_resid_stand(model)
ols_plot_hadi(model)
ols_plot_resid_pot(model)
avPlots(model)  #car package
crPlots(model)
