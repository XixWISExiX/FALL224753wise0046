# Read in data
dird="~/Desktop/MainFolder/OuClasses/Spring 2023/Applied Statistical Methods/FALL224753wise0046/CourseData/Data-for-the-course/K25936_Downloads/Excel/"
library(readxl)

files = list.files(dird)

### Important Functions
myconvert = function(xl) {
  if(stringr::str_ends(xl, "XLS") | stringr::str_ends(xl, "xls")){
    v=try(readxl::read_xls(paste0(dird, xl)), silent = TRUE)
  }
  else{
    v = NA
  }
  v
}










# stuff 4/10/2023
set.seed(32)
y1 <- rnorm(30,20,5)
y2 <- rnorm(40,16,5)
y2 <- rnorm(40,18,5)
t.test(y1,y2, mu=0, var.equal = TRUE)
qt(1-0.05/2,68)
# qt < t value SO REJECT NULL HYPTOTHESIS

# stuff
py <- dbinom(x = 0:10, size = 10, prob = 1/2)
names(py) <- 0:10
barplot(py, col = rep(c("blue", "green", "blue", c(2,2))))


# In class 4/7/2023
qt(1-0.05/2, 30-1)
qchisq(1-0.05/2, 11-1)
qchisq(0.05/2, 11-1)
# End point corrections


# In class 4/3/2023
# Normal Distribution
12*2+3*12-4*12
(4+9+16)*9
(4+9+16)*9*4


# In class 3/29/2023
library(Intro2R)
L <- ddt$LENGTH
alpha <- 0.05
n <- length(L)
t <- qt(1-alpha/2,n-1)
mp <- c(-1,1)

ci <- mean(L) +mp*t*sd(L)/sqrt(n)
ci

# OR

# alpha = 0.10, only reading the interval "$conf.int"
t.test(x = L, conf.level = 0.90)$conf.int

set.seed(23);y1 <- rnorm(40, 20, 5)
y2 <- rnorm(30, 18, 5)
y1
y2

t.test(y1,y2, var.equal = TRUE)

set.seed(32)
y1 <- rnorm(40, 20, 5)
y2 <- rnorm(30, 25, 8)
var.test(y1,y2) # sigma_1^2/sigma_2^2
t.test(y1,y2,var.equal = FALSE) # difference in means, mu_1-mu_2
# Welch is when you assume var is false
# Is 0 in the interval?
# in this case yes, so it is plossible that mu_1=mu_2


# 3/29/2023
# mu = np
# var = np(1-p)
10*0.6
10*0.6*0.4
1-pnorm(6.5, 6, 1.55)
1-pbinom(6,10,0.6)


# 3/27/2023
# 1 TRUE
# 2 0.9431, ANSWER: 0.83
pnorm(6+0.5, 5, sqrt(10*0.5*0.5))
# 3 TRUE
# 4 TRUE




# mtbe <- read.csv("MTBE.csv")
# epagas <- read.csv("EPAGAS.csv")

# head(mtbe)


# eggs <- myconvert("EGGS.XLS")
# head(eggs)
