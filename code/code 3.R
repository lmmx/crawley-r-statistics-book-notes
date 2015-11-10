#  Chapter 3

yvals <- read.csv("c:\\temp\\yvalues.csv")
attach(yvals)
hist(y)


# arithmetic mean

total <- sum(y)
n <- length(y)

( ybar <- total/n )

arithmetic.mean <- function(x) sum(x)/length(x)

data <- c(3,4,6,7)
arithmetic.mean(data)

arithmetic.mean(y)

mean(y)


# median

sorted <- sort(y)
length(y)/2
ceiling(length(y)/2)
sorted[20]
sorted[ceiling(length(y)/2)]


sort(y)[ceiling(length(y)/2)]

y.even <- y[-1]
length(y.even)

sort(y.even)[19]
sort(y.even)[20]


(sort(y.even)[19]+sort(y.even)[20])/2

38%%2

39%%2


med <- function(x) {
modulo <- length(x)%%2
if (modulo == 0)  (sort(x)[ceiling(length(x)/2)]+sort(x)[ceiling(1+length(x)/2)])/2
else  sort(x)[ceiling(length(x)/2)]
}

med(y)
med(y.even)

median(y)
median(y.even)


#   geometric mean

100000^0.2
insects <- c(1,10,1000,10,1)
mean(insects)

exp(mean(log(insects)))


# harmonic mean

v <- c(1,2,4,1)
length(v)/sum(1/v)

1/mean(1/v)


