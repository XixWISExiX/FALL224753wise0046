# This is a little advanced at the beginning. Later is easy.
# To use all of this code you will need to install packages
# readxl, stringr, purrr, plotrix, qcc, ggplot2


dird = "D:/MATH4773-5773/DATA/Excel/"
library(readxl)

files = list.files(dird)
files

myconvert = function(xl) {
  if(stringr::str_ends(xl, "XLS") | stringr::str_ends(xl, "xls")){
  v=try(readxl::read_xls(paste0(dird, xl)), silent = TRUE)
  }
  else{
    v = NA
  }
  v
}

v  = purrr::map(files, ~myconvert(.x))
l <- stringr::str_length(files)
l
newnames <- stringr::str_sub(files,1,l-4)
newnames
names(v) <- newnames
v[1]

############ FATAL part 1

fatal <- myconvert("FATAL.XLS")
fatal$Cause

freq = table(fatal$Cause)
addmargins(freq)
prop <- prop.table(freq)
prop
addmargins(prop)
rbind(freq,prop)



## plots

barplot(freq)


pie(freq)

plotrix::pie3D(as.vector(freq),labels = names(freq),explode = 0.2,theta = 1)

windows()
qcc::pareto.chart(freq)

### FRECKLE

freckle <- myconvert("FRECKLE.xls")
freckle$`F-INDEX`
stem(freckle$`F-INDEX`)

#### FATAL part 2

fatal <- myconvert("FATAL.XLS")
obj = scale(fatal$Fatalities)
f<-abs(obj[,1])
coll = ifelse(f>3, "Red",ifelse(f>2, "Blue","Black"))
plot(f, type = "b", col = coll, pch =19, cex =2)
abline(h = 3)

## ggplot FATAL

# ggplot works on data frames
library(ggplot2)
df = as.data.frame(obj)
names(df) = "z"
g <- ggplot(df) + geom_point(aes(y=z, x=seq_along(z)), color = coll) + xlab("Index") + ylab("Z standardization")
g

# Leave one out stats

zloo <- function(x){ # x vector
  n <- length(x)
  
  X <- matrix(data = rep(x,n ), 
              nrow = n, 
              ncol = n, 
              byrow = FALSE)
  
  diag(X) <- NA # Place NA's on the diagonal
  
  xbarloo <- colMeans(x = X,
                    na.rm = TRUE)
  
  sdloo <- apply(X = X, 
                 MARGIN = 2,
                 sd, 
                 na.rm  = TRUE) # put options for the FUN in last place
  
  zloo <- (x-xbarloo)/sdloo
  
  z <- scale(x = x,
             center = TRUE,
             scale = TRUE)[,1] # vector not matrix
  
  zz <- matrix(c(x,z,zloo, xbarloo,sdloo), nrow = n, ncol = 5, byrow = FALSE)
  
  colnames(zz) <- c("x","z","zloo","xbarloo","sdloo")
  
  outloo <- zz[abs(zloo) > 3,]
  outz <- zz[abs(z) > 3,]
  
  newcol <- ifelse(test = abs(zloo) > 3, 
                   yes = "Red",
                   no = ifelse(test = abs(zloo) >=2 & abs(zloo) <= 3,
                               yes =  "Pink", 
                               no = "Green"))
  
  pairs(x = zz, pch = 21, bg = newcol, main = "Red outliers, loo")
  
  list(zz = zz, outloo = outloo, outz = outz )
}
#example
zloo(x = epg$MPG)


