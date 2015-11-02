#  age at death data

mortality <- read.csv("c:\\temp\\deaths.csv")
attach(mortality)
names(mortality)

tapply(death,treatment,mean)

tapply(death,treatment,var)

model <- glm(death~treatment,Gamma)
summary(model)

detach(mortality)

# survival analysis with censoring

library(survival)
sheep <- read.csv("c:\\temp\\sheep.deaths.csv")
attach(sheep)
names(sheep)

plot(survfit(Surv(death,status)~group),col=c(2,3,4),
xlab="Age at death (months)")

model <- survreg(Surv(death,status)~weight*group,dist="exponential")
summary(model)

model2 <- survreg(Surv(death,status)~weight+group,dist="exponential")
anova(model,model2,test="Chi")

model3 <- survreg(Surv(death,status)~group,dist="exponential")
anova(model2,model3,test="Chi")

model4 <- survreg(Surv(death,status)~1,dist="exponential")
anova(model3,model4,test="Chi")

summary(model3)

model3 <- survreg(Surv(death,status)~group,dist="exponential")
model4 <- survreg(Surv(death,status)~group,dist="extreme")
model5 <- survreg(Surv(death,status)~group,dist="gaussian")
model6 <- survreg(Surv(death,status)~group,dist="logistic")
anova(model3,model4,model5,model6)

tapply(predict(model3,type="response"),group,mean)

tapply(death,group,mean)


