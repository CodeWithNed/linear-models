milk<-read.table(file="milk.txt",header=TRUE)
model1<-lm(CurrMilk ~ Previous+Fat+Protein, data = milk)
summary(model1)
plot(milk$Previous,milk$CurrMilk)
abline(model1)

comprep<-read.table(file="comprep.txt",header=TRUE)
model2<-lm(Minutes ~ Units, data = comprep)
summary(model2)
plot(comprep$Units,comprep$Minutes)
abline(lm(Minutes ~ Units, data = comprep))

plot(model2$residuals, pch = 16, col = "red")
plot(cooks.distance(model2), pch = 16, col = "blue") #Plot the Cooks Distances.
comprep.aov <- aov(model2)
summary(comprep.aov)
coefficient <- cor.test(comprep$Minutes , comprep$Units)
coefficient$estimate
coefficient
plot(model2)


plot(model1$residuals, pch = 16, col = "red")
plot(cooks.distance(model1), pch = 16, col = "blue") #Plot the Cooks Distances.
milk.aov <- aov(model1)
summary(milk.aov)
plot(model1)

