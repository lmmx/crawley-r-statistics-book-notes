
# one-way anova

oneway <- read.csv("c:\\temp\\oneway.csv")
attach(oneway)
names(oneway)

plot(1:20,ozone,ylim=c(0,8),ylab="y",xlab="order",pch=21,bg="red")

abline(h=mean(ozone),col="blue")
for(i in 1:20) lines(c(i,i),c(mean(ozone),ozone[i]),col="green")

plot(1:20,ozone,ylim=c(0,8),ylab="y",xlab="order",
pch=21,bg=as.numeric(garden))
abline(h=mean(ozone[garden=="A"]))
abline(h=mean(ozone[garden=="B"]),col="red")

index <- 1:length(ozone)
for (i in 1:length(index)){
if (garden[i] == "A" )
     lines(c(index[i],index[i]),c(mean(ozone[garden=="A"]),ozone[i]))
else 
     lines(c(index[i],index[i]),c(mean(ozone[garden=="B"]),ozone[i]), col="red")
}

SSY <- sum((ozone-mean(ozone))^2)
SSY

sum((ozone[garden=="A"]-mean(ozone[garden=="A"]))^2)
sum((ozone[garden=="B"]-mean(ozone[garden=="B"]))^2)

qf(0.95,1,18)
1-pf(15.0,1,18)

summary(aov(ozone~garden))

plot(aov(ozone~garden))

cbind(ozone[garden=="A"],ozone[garden=="B"])
tapply(ozone,garden,sum)

mean(ozone[garden=="A"])-mean(ozone)
mean(ozone[garden=="B"])-mean(ozone)
mean(ozone[garden=="A"])
mean(ozone[garden=="B"])-mean(ozone[garden=="A"])


# plots for anova

comp <- read.csv("c:\\temp\\competition.csv")
attach(comp)
names(comp)

plot(clipping,biomass,xlab="Competition treatment",
ylab="Biomass",col="lightgrey")

heights <- tapply(biomass,clipping,mean)
barplot(heights,col="green",ylim=c(0,700),
ylab="mean biomass",xlab="competition treatment")


# error bars

error.bars <- function(y,z) {
x <- barplot(y,plot=F)
n <- length(y)
for (i in 1:n) 
arrows(x[i],y[i]-z,x[i],y[i]+z,code=3,angle=90,length=0.15)
}

model <- aov(biomass~clipping)
summary(model)
table(clipping)

se <- rep(28.75,5)

error.bars(heights,se)

ci <- se*qt(.975,5)
barplot(heights,col="green",ylim=c(0,700),
ylab="mean biomass",xlab="competition treatment")
error.bars(heights,ci)

lsd <- qt(0.975,10)*sqrt(2*4961/6) 
lsdbars <- rep(lsd,5)/2

barplot(heights,col="green",ylim=c(0,700),
ylab="mean biomass",xlab="competition treatment")
error.bars(heights,lsdbars)


# fatorial experiments

weights <- read.csv("c:\\temp\\growth.csv")
attach(weights)

barplot(tapply(gain,list(diet,supplement),mean),beside=T)

labels <- levels(diet)
shade <- c(0.2,0.6,0.9)

barplot(tapply(gain,list(diet,supplement),mean),beside=T,
ylab="weight gain",xlab="supplement",ylim=c(0,30))

legend(locator(1),labels,gray(shade))

tapply(gain,list(diet,supplement),mean)
model <- aov(gain~diet*supplement)
summary(model)
tapply(gain,list(diet,supplement),length)

x <- as.vector(barplot(tapply(gain,list(diet,supplement),mean),
beside=T,ylim=c(0,30)))
y <- as.vector(tapply(gain,list(diet,supplement),mean))
z <- rep(0.656,length(x))
for( i in 1:length(x) ) 
arrows(x[i],y[i]-z[i],x[i],y[i]+z[i],length=0.05,code=3,angle=90)

legend(locator(1),labels,gray(shade))

model <- lm(gain~diet+supplement)
summary(model)

supp2 <- factor(supplement)
levels(supp2)

model2 <- lm(gain~diet+supp2)
anova(model,model2)

# split plot experiments

yields <- read.csv("c:\\temp\\splityield.csv")
attach(yields)
names(yields)

model <- aov(yield~irrigation*density*fertilizer+Error(block/irrigation/density))
summary(model)

interaction.plot(fertilizer,irrigation,yield)
interaction.plot(density,irrigation,yield)


# random effects and pseudoreplication

rats <- read.csv("c:\\temp\\rats.csv")
attach(rats)
names(rats)

Treatment <- factor(Treatment)
Rat <- factor(Rat)
Liver <- factor(Liver)

# this is the wrong way to do the analysis

model <- aov(Glycogen~Treatment)
summary(model)

# this is the right way to do the analysis

yv <- tapply(Glycogen,list(Treatment,Rat),mean)
( yv <- as.vector(yv) )

treatment <- factor(c(1,2,3,1,2,3))
model <- aov(yv~treatment)
summary(model)


# variance components analysis

model2 <- aov(Glycogen~Treatment+Error(Treatment/Rat/Liver))
summary(model2)



