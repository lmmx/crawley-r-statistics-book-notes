#  draw the figures illustrating maximum likelihood

x<-c(1,3,4,6,8,9,12)
y<-c(5,8,6,10,9,13,12)
windows(14,6)
par(mfrow=c(1,3))
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(0,0.6793,col="red")
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(8,0.6793,col="red")
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(lm(y~x))
abline(lm(y~x),col="blue")



plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(4.8273,1.5,col="red")
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(4.8273,0.2,col="red")
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(lm(y~x))
abline(lm(y~x),col="blue")


# randomizing treatments for experimental design

treatments <- c("aloprin","vitex","formixin","panto","allclear")

#   use sample to shuffle them for the active insects in dishes 1 to 5

sample(treatments)


# this produces a warning message because the same variable name
# appears in two attached dataframes

first.frame <- read.csv("c:\\temp\\test.pollute.csv")
second.frame <- read.csv("c:\\temp\\ozone.data.csv")
attach(first.frame)
attach(second.frame)

# this is how you should avoid this kind of problem

first.frame <- read.csv("c:\\temp\\test.pollute.csv")
second.frame <- read.csv("c:\\temp\\ozone.data.csv")
attach(first.frame)

# ........    
#  this is where you work on the information from first.frame. 
#  Then when you are finished
# ........   

detach(first.frame)
attach(second.frame)
