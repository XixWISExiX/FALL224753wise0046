2+2
mtbe <- read.csv("MTBE.csv")
mtbe
tab <- table(mtbe$WellClass, mtbe$Aquifier)
tab

mp <- c(-1,1)
mn <- mean(mtbe$pH)
t <- qt(1-0.05/2, length(mtbe$pH)-1)
ci <- mn + mp*t*sd(mtbe$pH)/sqrt(length(mtbe$pH))
ci
t.test(mtbe$pH)$conf.int

t.test(mtbe$pH, conf.level = 0.90)


x <- mtbe$pH
y <- mtbe$SpConduct
#yhat +/- t_{a/2} * s/sqrt(n)
# s/sqrt(n) is the std error
a <- 0.05
n <- 11
qt(1-a/2, n-1)
#yhat +/- t_{1-a/2} * s/sqrt(n)
qt(a/2, n-1)

#Paired
t.test(x,y, paired=TRUE)
#Independent
#variance equal
t.test(x,y, var.equal=TRUE)
#not variance equal
t.test(x,y, var.equal=FALSE)

# See if variance are equal
var.test(x,y)
t.test(x,y)

########################################################

s <- read.csv("SHALLOW.csv")
t.test(s$Actual, s$Predict, paired=TRUE, conf.level = 0.99)
cor(s$Actual, s$Predict)

m <- read.csv("mow-mow.csv")
var.test(m$y1, m$y2)
t.test(m$y1, m$y2, conf.level = 0.92, var.equal = FALSE)

v <- c(3,4,5)
t.test(v, conf.level = 0.8)
qt(1-0.2/2,2)

#10
t.test(x,y, var.equal=FALSE)

4+16+9*4
