myf <- function(x){
  dnorm(x, 10,5)
}

integrate(myf, 5,8)
pnorm(8,10,5)- pnorm(5,10,5)
