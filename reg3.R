library(rgl)
library(olsrr)
library(car)

ss<-read.table(file="sal_sur.txt",header=TRUE) #ch 5 salary survay data
E1 <- ifelse(ss$E == '1', 1, 0)
E2 <- ifelse(ss$E == '2', 1, 0)
ss1 <- data.frame(S = ss$S, X = ss$X, E1 = E1, E2 = E2, M = ss$M)
model<-lm(formula = S ~ X+E1+E2+M, data = ss1)
summary(model)
ols_plot_resid_stand(model)
residualPlots(model)

model<-lm(formula = S ~ X+E1+E2+M+E1*M+E2*M, data = ss1)
summary(model)
ols_plot_resid_stand(model)
residualPlots(model)

ss2 <- ss1[-c(33), ]
model<-lm(formula = S ~ X+E1+E2+M+E1*M+E2*M, data = ss2)
summary(model)
ols_plot_resid_stand(model)
residualPlots(model)
qqPlot(model)
shapiro.test(model$residuals)
ncvTest(model)
vif(model)
influencePlot(model)
influenceIndexPlot(model)





both_model <- step(model, direction = "both") # stepwise method
summary(both_model)
