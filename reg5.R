library(rgl)
library(olsrr)
library(car)

bd<-read.table(file="supervisor.txt",header=TRUE) #ch11 ex1
model<-lm(formula = Y ~ X1+X2+X3+X4+X5+X6, data = bd)
summary(model)
ols_plot_resid_stand(model)
pairs(Y ~ X1+X2+X3+X4+X5+X6,data=bd)
vif(model)


both_model <- step(model, direction = "forward")
summary(both_model)
both_model$anova

both_model <- step(model, direction = "both") # stepwise method
summary(both_model)
anova(both_model)

model1<-lm(formula = Y ~ X1+X2+X3+X4+X5+X6, data = bd)
model2<-lm(formula = Y ~ X1+X2+X3, data = bd)
ols_mallows_cp(model1, model2)


library(leaps)
regfit_full = regsubsets(Y~., data = bd,nvmax = 19, method = "backward")
summary(regfit_full)
plot(regfit_full, scale = "Cp")
plot(regfit_full, scale = "bic")
plot(regfit_full, scale = "adjr2")
coef(regfit_full, 3)


bd<-read.table(file="ch11ex1.txt",header=TRUE) #ch11 ex2
model<-lm(formula = H ~G+M+W, data = bd)
summary(model)
pairs(H ~G+M+W,data=bd)
vif(model)
both_model <- step(model, direction = "forward") # forward method
summary(both_model)
anova(both_model)
both_model <- step(model, direction = "backward") # backward method
summary(both_model)
anova(both_model)
both_model <- step(model, direction = "both") # stepwise method
summary(both_model)
anova(both_model)

cor(bd$H,bd$W)
