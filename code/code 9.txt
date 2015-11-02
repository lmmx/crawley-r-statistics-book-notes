# analysis of covariance

compensation <- read.csv("c:\\temp\\ipomopsis.csv")
attach(compensation)
names(compensation)

plot(Root,Fruit,pch=16,col="blue")
plot(Grazing,Fruit,col="lightgreen")

# the wrong analysis (not controlling for initial size)

summary(aov(Fruit~Grazing))

# the correct anocova

model <- lm(Fruit~Root*Grazing)
summary.aov(model)

model <- lm(Fruit~Grazing*Root)
summary.aov(model)

model2 <- lm(Fruit~Grazing+Root)
anova(model,model2)
summary.lm(model2)

plot(Root,Fruit,pch=21,bg=(1+as.numeric(Grazing)))
legend(locator(1),c("grazed","ungrazed"),col=c(2,3),pch=16)
abline(-127.829,23.56,col="blue")
abline(-127.829+36.103,23.56,col="blue")









