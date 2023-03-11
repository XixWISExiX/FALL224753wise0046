x <- rnorm(50)
windows()
plot(x, pch = 21, bg = "Black")
ind <- which.min(x)
x[ind]
points(ind,x[ind], pch = 21, bg = "Red", cex = 2)


# 200 = qbinom(1-0.02,n,0.95)
qbinom(1-0.02,200:210,0.95)
