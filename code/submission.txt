worms <- read.csv("worms.csv")
names(worms)
attach(worms)
worms[,1:3]
worms[5:15,]
worms[Area>3 & Slope <3,]
worms[order(Area),]
worms[order(Area),c(2,3,5,7)]
worms[rev(order(worms[,5])),c(5,7)]
with(worms,tapply(Worm.density,Vegetation,mean))
aggregate(worms[,c(2,3,5,7)],list(Vegetation),mean)
aggregate(worms[,c(2,3,5,7)],list(Community=Vegetation),mean)
aggregate(worms[,c(2,3,5,7)],
list(Moisture=Damp,Community=Vegetation),mean)
with(worms,tapply(Slope,list(Damp,Vegetation),mean))
data <- read.csv("das.csv")
attach(data)
head(data)
which(y > 10)
y[50]
yields <- read.csv("fertyield.csv")
attach(yields)
head(yields)
table(treatment)
which(treatment == "nitogen")
data <- read.csv("scatter.csv")
attach(data)
head(data)
plot(x,y,pch=21,bg="red")
data <- read.csv("weather.data.csv")
attach(data)
head(data)
plot(factor(month),upper)
data <- read.csv("coplot.csv")
attach(data)
head(data)
windows(7,4)
par(mfrow=c(1,2))
plot(x,y)
plot(z,y)
windows(7,7)
coplot(y~x|z,pch=16,panel=panel.smooth)
data <- read.csv("np.csv")
attach(data)
head(data)
windows(7,4)
par(mfrow=c(1,2))
plot(nitrogen,yield,main="N")
plot(phosphorus,yield,main="P")
tapply(yield,list(nitrogen,phosphorus),mean)
barplot(tapply(yield,list(nitrogen,phosphorus),mean), beside=TRUE,xlab="phosphorus")
legend(locator(1),legend=c("no","yes"),title="nitrogen", fill=c("black","lightgrey"))

log (42/7.3)
log(16,2)
5+6+3+6+4+2+4+8+3+2+7
2+3; 5*7; 3-7
8^1/3
8^(1/3)
abs(5.7-6.8)/0.38
options(digits=3)
abs(5.7-6.8)/0.38
exp(1)
pi
options(digits=5)
exp(1)
pi
sin(pi/2)
cos(pi/2)
119%/%13
119%%13
9%%2
8%%2
15421%%7
15421%%7==0
X<-5
x<-5
x
x>7
x<7
floor(5.7)
ceiling(5.7)
rounded<-function(x) floor(x+0+5)
rounded(5.7)
rounded(5.4)
x
rounded<-function(x) floor(x+0.5)
rounded(5.7)
rounded(5.4)
3/0
-12/0
exp(-Inf)
0/Inf
(0:3)^Inf
0/0
Inf-Inf
Inf/Inf
is.finite(10)
is.infinite(10)
is.infinite(Inf)
x<-c(1:8,NA)
mean(x)
mean(x,na.rm=T)
(vmv<-c(1:6,NA,NA,9:12))
seq(along=vmv) [is.na(vmv)]
which(is.na(vmv))
(vmv[is.na(vmv)]<-0)
y<-10:16
y
y<-scan()
y<-c(10,11,12,13,14,15,16)
y<-scan(1)
y<-scan()
(counts<-c(25,12,7,4,6,2,1,0,2))
names(counts)<-0:8
counts
 0  1  2  3  4  5  6  7  8 
25 12  7  4  6  2  1  0  2 
(st<-table(rpois(2000,2.3)))
as.vector(st)
data<-read.csv("daphnia.csv")
data<-read.csv("daphnia.csv")
attach(data)
names(data)
tapply(Growth.rate,Detergent,mean)
tapply(Growth.rate,list(Water,Daphnia),median)
x<-0:10
sum(x)
sum(x<5)
sum(x[x<5])
x<5
1*(x<5)
x*(x<5)
sum(x*(x<5))
y<-c(98,3,5,7,6,6,8,9,2,3,9,4,10,4,11)
sort(y)
rev(sort(y))
rev(sort(y))[2]
rev(sort(y))[1:3]
rev(sort(y))
rev(sort(y))[2]
sum(rev(sort(y)) [1:3])
y
y<-c(8,3,5,7,6,6,8,9,2,3,9,4,10,4,11)
sort(y)
rev(sort(y))
rev(sort(y))[2]
sum(rev(sort(y)) [1:3])
y
which (y>5)
y[y>5]
length(y)
length(y[y>5])

x<-c(5,8,,6,7,1,5,3)
x<-c(5,8,6,7,1,5,3)
z<-x[-1]
z
trim.mean<-function(x)mean(sort(x)[-c(1,length(x))])
trim.mean(x)

rep(9,5)
rep(1:4,2)
rep(1:4,each=2)
rep(1:4, each=2, times=3)
rep(c(9,15,21,83),c(4,1,4,2))

gl(4,3)
gl(4,3,24)
gl(4,3,20)
gl(3,2,24,labels=c("A","B","C"))

seq(0,1.5,0.2)
seq(0,1.5,0,-0.2)
integer(0)
seq(1.5,0,-0.2)
x<-rnorm (18,10,2)
seq(88,50,along=x)

X<-matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
X
class(X)
attributes(X)
$dim

vector<-c(1,2,3,4,4,3,2,1)
V<-matrix(vector,byrow=T,nrow=2)
V
dim(vector)<-c(4,2)
is.matrix
function (x)  .Primitive("is.matrix")
is.matrix(vector)
(vector<-t(vector))
phrase<-"the quick brown fox jumps over the lazy dog"
q<-character(20)
for (i in 1:20) q[i]<-substr(phrase,1,i)
q
strsplit(phrase,split=character(0))
table(strsplit(phrase,split=character(0)))
words<-1+table(strsplit(phrase,split=character(0)))[1]
toupper(phrase)
tolower(toupper(phrase))

arithmetic.mean<-function(x) sum(x)/length(x)
y<-c(3,3,4,5,5,)
arithmetic.mean(y)
y<-c(3,3,4,5,5)
arithmetic.mean(y)
sort(y)[ceiling(length(y)/2)]
med<-function(x){
+ odd.even<-length(x)%%2
+ if (odd.even==0) (sort (x) [length(x)/2]+sort(x)[1+length(x)/2])/2
+ else sort (x) [ceiling(length(x)/2)]
+ }
med(y)
med(y[-1])

for(i in 1:5) print(i^2)
binary<-function(x) {
+ if(x==0) return (0)
+ i<-0
+ string<-numeric(32)
+ while(x>0) {
+ string[32-i]<-x%%2
+ x<-x%/%2
+ i<-i+1 }
+ first<-match(1,string)
+ string [first:32] }
sapply(15:17,binary)

z<-ifelse(y<0, -1,1)
(X<-matrix(1:24,nrow=4))
apply(X,1,sum)
apply(X,2,sum)
apply(X,1,sqrt)
apply(X,2,sqrt)

x<-sqrt(2)
x*x==2
x*x-2

as.numeric(factor(c("a","b","c")))
as.numeric(c(c("a","4","c")))
geometric<-function(x){
+ if(!is.numeric(x)) stop("Input must be numeric")
+ if (min(x)<=0) stop ("Input must be greater than zero")
+ exp(mean(log(x)))}
geometric(c(10,1000,10,1,1))
Sys.time()
class(Sys.time())
time.list<-as.POSIXlt(Sys.time())
class(time.list)
unlist(time.list)
time.list$wday
time.list$yday
data<-read.table
class(date)
y2<-as.POSIXlt("2018-10-22")
y1<-as.POSIXlt("2015-10-22")
y2-y1
y3<-as.POSIXlt("2015-10-22 09:30:59")
y4<-as.POSIXlt("2015-10-22 12:45:06")
y4-y3
y4>y3

x<-runif(23)
str(x)
basket<-list(rep("a",4),c("b0","b1","b2"),9:4,gl(5,3))
basket
str(basket)
xv<-seq(0,30)
yv<-2+0.5*xv+rnorm(31,0,2)
model<-lm(yv~xv+I(xv^2))
str(model)
