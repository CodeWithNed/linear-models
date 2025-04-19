library(rgl)
library(olsrr)
library(car)

bd<-read.table(file="bact.txt",header=TRUE) #ch6 bacteria data
plot(bd$t,bd$N)
model<-lm(formula = N ~ t, data = bd)
summary(model)
ols_plot_resid_stand(model)
residualPlots(model)
model<-lm(formula = log(N) ~ t, data = bd)
summary(model)
residualPlots(model)
ols_plot_resid_stand(model)
plot(model)

bd<-read.table(file="ch6supervisor.txt",header=TRUE) #ch6 supervisor data
plot(bd$x,bd$y)
model<-lm(formula = y ~ x, data = bd)
summary(model)
ols_plot_resid_stand(model)
residualPlots(model)
model<-lm(formula = log(y) ~ x, data = bd)
summary(model)
residualPlots(model)
ols_plot_resid_stand(model)

yp <- bd$y/bd$x
xp <- 1/bd$x
bd1 <- data.frame(x = xp, y = yp)
plot(bd1$x,bd1$y)
model<-lm(formula = y ~ x, data = bd1)
summary(model)
ols_plot_resid_stand(model)
residualPlots(model)

# ch 9
bd<-read.table(file="ch9ex1.txt",header=TRUE) #ch9 ex1
model<-lm(formula = ACHV ~ FAM+PEER+SCHOOL, data = bd)
summary(model)
ols_plot_resid_stand(model)
residualPlots(model)
vif(model)

pairs(ACHV ~ FAM+PEER+SCHOOL,data=bd)

model<-lm(formula = ACHV ~ SCHOOL, data = bd)
summary(model)
ols_plot_resid_stand(model)
residualPlots(model)

bd<-read.table(file="ch9ex2.txt",header=TRUE) #ch9 ex2
model<-lm(formula = IMPORT ~ DOPROD+STOCK+CONSUM, data = bd)
summary(model)
ols_plot_resid_stand(model)

pairs(IMPORT ~ DOPROD+STOCK+CONSUM,data=bd)
vif(model)

bd<-read.table(file="ch9ex21.txt",header=TRUE) #ch9 ex21
model<-lm(formula = IMPORT ~ DOPROD+STOCK+CONSUM, data = bd)
summary(model)
ols_plot_resid_stand(model)

pairs(IMPORT ~ DOPROD+STOCK+CONSUM,data=bd)
vif(model)

model<-lm(formula = IMPORT ~ STOCK+CONSUM, data = bd)
summary(model)
ols_plot_resid_stand(model)

model<-lm(formula = IMPORT ~ DOPROD+STOCK, data = bd)
summary(model)
ols_plot_resid_stand(model)

bd<-read.table(file="ch9ex3.txt",header=TRUE) #ch9 ex3
model<-lm(formula = S_t ~ A_t+P_t+E_t+A_t_1+P_t_1, data = bd)
summary(model)
ols_plot_resid_stand(model)

pairs(S_t ~ A_t+P_t+E_t+A_t_1+P_t_1,data=bd)
vif(model)

model<-lm(formula = S_t ~ P_t+E_t, data = bd)
summary(model)
ols_plot_resid_stand(model)

model<-lm(formula = A_t ~ P_t+A_t_1+P_t_1, data = bd)
summary(model)
ols_plot_resid_stand(model)

pairs(A_t ~ P_t+A_t_1+P_t_1,data=bd)
vif(model)
