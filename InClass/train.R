# Late train MS 5.10
dtrain <- function(y){
  f <- ifelse(y > -5 & y < 5,3/500*(25-y^2),0) # vectorized
    return(f)
}

windows();curve(dtrain(x),
      xlim = c(-10,10),
      xlab = "y in minutes the train is late",
      ylab = "f(y)",
      main = "Density of train lateness",
      lwd = 2,
      col = "Blue")

ptrain <- function(y){
  myf <- function(x){
    3/500*(25-x^2)
  }
  pf <- ifelse(y > -5 & y < 5,
         integrate(myf,-5,y)$value,
         ifelse(y <= -5,
                0,
                1))
  pf
}

myf <- function(x){
  x^2 * 3/500 * (25-x^2)
}

integrate(myf, -5 , 5)


