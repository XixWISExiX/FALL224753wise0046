# on the way to completion
mynexample <- function(a,b,c,limsx){
  obj <- function(n){
    a*n^2 + b*n +c
  }
  
  layout(matrix(1:2,ncol =2))
  
  curve(obj(x), 
        xlim = limsx,
        main = "Quadratic roots")
  
  abline(h = 0)
  
  curve(abs(obj(x)), 
        xlim = limsx,
        main = "Minima")
  
  rt <- optimize(f = function(x) abs(obj(x)), 
                 lower = 1, 
                 upper = 3.5)
  
  list(min = rt$min, a = a, b = b, c = c)
}
mynexample(a = 1,b = -5,c = 6, limsx = c(1,4))


