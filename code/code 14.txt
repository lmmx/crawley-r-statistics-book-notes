# binary response variables

island <- read.csv("c:\\temp\\isolation.csv")
attach(island)
names(island)

[1] "incidence" "area"      "isolation"

model1 <- glm(incidence~area*isolation,binomial)

model2 <- glm(incidence~area+isolation,binomial)

anova(model1,model2,test="Chi")
summary(model2)

windows(7,4)
par(mfrow=c(1,2))
xv <- seq(0,9,0.01)

modela <- glm(incidence~area,binomial)
modeli <- glm(incidence~isolation,binomial)

yv <- predict(modela,list(area=xv),type="response")
plot(area,incidence,pch=21,bg="yellow")
lines(xv,yv,col="blue") 

xv2 <- seq(0,10,0.1)
yv2 <- predict(modeli,list(isolation=xv2),type="response")
plot(isolation,incidence,pch=21,bg="yellow")
lines(xv2,yv2,col="red")

ac <- cut(area,3)
ic <- cut(isolation,3)
tapply(incidence,ac,sum)
tapply(incidence,ic,sum)

table(ac)
table(ic)

tapply(incidence,ac,sum)/ table(ac)
tapply(incidence,ic,sum)/ table(ic)

xv <- seq(0,9,0.01)
yv <- predict(modela,list(area=xv),type="response")
plot(area,incidence,pch=21,bg="yellow")
lines(xv,yv,col="blue") 

d <- (max(area)-min(area))/3
left <- min(area)+d/2
mid <- left+d
right <- mid+d
xva <- c(left,mid,right)
pa <- as.vector(tapply(incidence,ac,sum)/ table(ac))
se <- sqrt(pa*(1-pa)/table(ac))

xv <- seq(0,9,0.01)
yv <- predict(modela,list(area=xv),type="response")
lines(xv,yv,col="blue") 

points(xva,pa,pch=16,col="red")
for (i in 1:3) lines(c(xva[i],xva[i]),
c(pa[i]+se[i],pa[i]-se[i]),col="red" )

xv2 <- seq(0,10,0.1)
yv2 <- predict(modeli,list(isolation=xv2),type="response")
plot(isolation,incidence,pch=21,bg="yellow")
lines(xv2,yv2,col="red") 

d <- (max(isolation)-min(isolation))/3
left <- min(isolation)+d/2
mid <- left+d
right <- mid+d
xvi <- c(left,mid,right)
pi <- as.vector(tapply(incidence,ic,sum)/ table(ic))
se <- sqrt(pi*(1-pi)/table(ic))

points(xvi,pi,pch=16,col="blue")
for (i in 1:3) lines(c(xvi[i],xvi[i]),
c(pi[i]+se[i],pi[i]-se[i]),col="blue" )

# binary ancova

infection <- read.csv("c:\\temp\\infection.csv")
attach(infection)
names(infection)

windows(7,4)
par(mfrow=c(1,2))
plot(infected,weight,xlab="Infection",ylab="Weight",col="lightblue")
plot(infected,age,xlab="Infection",ylab="Age", col="lightgreen")

table(infected,sex)

model <- glm(infected~age*weight*sex,family=binomial)
summary(model)

model2 <- step(model)

summary(model2)

model3 <- update(model2,~.-age:weight)
anova(model2,model3,test="Chi")

model4 <- update(model2,~.-age:sex)
anova(model2,model4,test="Chi")

model5 <- glm(infected~age+weight+sex,family=binomial)
summary(model5)

model6 <- glm(infected~age+weight+sex+I(weight^2)+I(age^2),family=binomial)
summary(model6)


library(mgcv)
model7 <- gam(infected~sex+s(age)+s(weight),family=binomial) 
plot.gam(model7)

model8 <- glm(infected~sex+age+I(age^2)+
I((weight-12)*(weight>12)),family=binomial)
summary(model8) 

model9 <- update(model8,~.-sex)
anova(model8,model9,test="Chi")

model10 <- update(model8,~.-I(age^2))
anova(model8,model10,test="Chi")

summary(model9)




