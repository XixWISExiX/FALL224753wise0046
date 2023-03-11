## w method of sampling
rmyexp=function(n, ...){
  graphics.off()
w=runif(n,0,1)
y=-2*log(1-w)
h=hist(y,plot=FALSE, ...)
coll = rgb(h$density/max(h$density),.4,.1)
# windows()
hist(y,freq=FALSE,main="Uniform W",col = coll, ...)# col=rainbow(length(h$mids)),...)
curve( exp(-x/2)/2,add=TRUE,col="Blue",lwd=2)
text(10,.3, paste0("Simulation using\n w uniform method ", "n=",n,"\n ", "1/2exp(-x/2)"))
legend("topright", legend = c("Simulation", "Simulation","Truth"), fill=c(coll[1],coll[length(coll)], "Blue"))
dev.new(noRStudioGD = TRUE)
df=data.frame(y)
library(ggplot2)
g = ggplot(df, aes(x=y)) + geom_histogram(aes(fill=..density..), bins = 50)  + geom_density( col = "Red")
g = g + stat_function(fun = function(x) exp(-x/2)/2)
print(g)

}

rmyexp(100000, nclass = 40)

