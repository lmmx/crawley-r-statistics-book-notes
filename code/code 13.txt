# proportion data


# logistic regression

numbers <- read.csv("c:\\temp\\sexratio.csv")
numbers

attach(numbers)
windows(7,4)
par(mfrow=c(1,2))
p <- males/(males+females)
plot(density,p,ylab="Proportion male")
plot(log(density),p,ylab="Proportion male")

y <- cbind(males,females)

model <- glm(y~density,binomial)
summary(model)

model <- glm(y~log(density),binomial)
summary(model)

xv <- seq(0,6,0.1)
plot(log(density),p,ylab="Proportion male",pch=21,bg="blue")
lines(xv,predict(model,list(density=exp(xv)),
type="response"),col="brown")

# catagorical explanatory variables

germination <- read.csv("c:\\temp\\germination.csv")
attach(germination)
names(germination)

y <- cbind(count , sample-count)

model <- glm(y ~ Orobanche * extract, binomial)
summary(model)

model <- glm(y ~ Orobanche * extract, quasibinomial)

model2 <- update(model, ~ . - Orobanche:extract)

anova(model,model2,test="F")

anova(model2,test="F")

model3 <- update(model2, ~ . - Orobanche)
anova(model2,model3,test="F")

coef(model3)

1/(1+1/(exp(-0.5122)))
1/(1+1/(exp(-0.5122+1.0574)))

tapply(predict(model3,type="response"),extract,mean)
     
p <- count/sample
tapply(p,extract,mean)

as.vector(tapply(count,extract,sum))/
as.vector(tapply(sample,extract,sum))

# ancova with proportion data

props <- read.csv("c:\\temp\\flowering.csv")
attach(props)
names(props)

y <- cbind(flowered,number-flowered)
pf <- flowered/number
pfc <- split(pf,variety)
dc <- split(dose,variety)

plot(dose,pf,type="n",ylab="Proportion flowered")
points(jitter(dc[[1]]),jitter(pfc[[1]]),pch=21,bg="red")
points(jitter(dc[[2]]),jitter(pfc[[2]]),pch=22,bg="blue")
points(jitter(dc[[3]]),jitter(pfc[[3]]),pch=23,bg="gray")
points(jitter(dc[[4]]),jitter(pfc[[4]]),pch=24,bg="green")
points(jitter(dc[[5]]),jitter(pfc[[5]]),pch=25,bg="yellow")

model1 <- glm(y~dose*variety,binomial)
summary(model1)

model2 <- glm(y~dose*variety,quasibinomial)
summary(model2)

model3 <- glm(y~dose+variety,quasibinomial)
anova(model2,model3,test="F")


xv <- seq(0,32,0.25)
length(xv)

yv <- predict(model3,list(dose=xv,variety=rep("A",129)),type="response")
lines(xv,yv,col="red")
yv <- predict(model3,list(dose=xv,variety=rep("B",129)),type="response")
lines(xv,yv,col="blue")
yv <- predict(model3,list(dose=xv,variety=rep("C",129)),type="response")
lines(xv,yv,col="gray")
yv <- predict(model3,list(dose=xv,variety=rep("D",129)),type="response")
lines(xv,yv,col="green")
yv <- predict(model3,list(dose=xv,variety=rep("E",129)),type="response")
lines(xv,yv,col="yellow")

