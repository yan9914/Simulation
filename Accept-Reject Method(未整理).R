# p1~p10 = (0.11,0.12,0.09,0.08,0.12,0.1,0.09,0.09,0.1,0.1)
# By Accept-Reject method
f <- function(r){
  t <- 0
  prob <- c(0.11,0.12,0.09,0.08,0.12,0.1,0.09,0.09,0.1,0.1)
  output <- NULL
  while (t < r) {
    u <- runif(r-t)
    y <- sample.int(10,r-t,TRUE)
    i <- u < prob[y]/0.12
    t <- t + sum(i)
    output <- c(output, y[i])
  }
  return(output)
}
table(f(1e5))/1e5


# Exercise 6.1  
# simulating value of X such that P(X=i) = P( Y=i | Y <= k)
# where Y ~ Poisson

## method 1 
## Essential of Conditional Probability
f <- function(r,lamb,k){
  t <- 0
  output <- NULL
  while(t < r){
    y <- rpois(r-t, lamb)
    i <- y <= k
    t <- t + sum(i)
    output <- c(output,y[i])
  }
  return(output)
}
table(f(1e7,5,10))/1e7
dpois(0:10,5)/sum(dpois(0:10,5))

## method 2
## Inverse transformation
f <- function(r,lamb,k){
  u <- runif(r)
  cumpb <- cumsum(dpois(0:k,lamb)/sum(dpois(0:k,lamb)))
  cut(u,breaks = c(0,cumpb),labels = 0:k)
}
tabulate(f(20,5,10))

## method 3
## Accept-Reject method
f <- function(r,lamb,k){
  t <- 0
  prob <- dpois(0:k,lamb)/sum(dpois(0:k,lamb))
  M <- max(prob)
  output <- NULL
  while (t < r) {
    u <- runif(r-t)
    y <- sample.int(k+1,r-t,TRUE)
    i <- u < prob[y]/M
    t <- t + sum(i)
    output <- c(output, y[i])
  }
  return(output)
}
table(f(20,5,10))

# Exercise 6.1
# simulating value of X such that P(X=i) = P( Y=i | Y <= k)
# where Y ~ Binomial
n <- 15; p <- 0.4; k <- 5
## (a)
(alpha <- 1-pbinom(k-1,n,p))
## (b)
(condprob <- dbinom(k:n,n,p)/alpha)
## (c)
f1 <- function(r){
  u <- runif(r)
  cdf <- cumsum(condprob)
  return(cut(u, c(0,cdf), labels = k:n))
}
f1(10)
## (d) Accept-Reject method
f2 <- function(r){
  t <- 0
  M <- max(condprob)
  output <- NULL
  while (t < r) {
    u <- runif(r-t)
    y <- sample(k:n,r-t,TRUE)
    i <- u < condprob[y-k+1]/M
    t <- t + sum(i)
    output <- c(output, y[i])
  }
  return(output)
}
f2(10)

# Example 19
f <- function(r){
  M <- optimize(function(x)dbeta(x,2,4),
                c(0,1),maximum = TRUE)$objective
  t <- 0
  output <- NULL
  while(t<r){
    y <- runif(r-t)
    i <- runif(r-t) <= dbeta(y,2,4)/M
    t <- t + sum(i)
    output <- c(output,y[i])
  }
  return(output)
}
mean(f(1e4)) # E(X) = 1/3

# Example 20
par(mfrow=c(1,2))
Nsim <- 2500
a <- 2.7;b <- 6.3
M <- optimize(function(x){dbeta(x,2.7,6.3)},c(0,1),
              maximum = TRUE)$objective
u <- runif(Nsim,max=M)
y <- runif(Nsim) 
x <- y[u<dbeta(y,a,b)]
w <- u[u<dbeta(y,a,b)]
plot(y,u,col = 'grey', pch = 20)
points(x,w,col = 'black', pch = 20)
q <- seq(0,1,length.out = length(x))
lines(q,dbeta(q,2.7,6.3),col = 'red', lwd = 2)
abline(h=M,col='dark green', lwd = 2)


Nsim <- 2500
a <- 2.7;b <- 6.3
M <- optimize(function(x){dbeta(x,2.7,6.3)/dbeta(x,2,6)},c(0,1),
              maximum = TRUE)$objective
u <- runif(Nsim,max=M)
y <- rbeta(Nsim,2,6) 
x <- y[u<dbeta(y,a,b)/dbeta(y,2,6)]
w <- u[u<dbeta(y,a,b)/dbeta(y,2,6)]
plot(y,u*dbeta(y,2,6),col = 'grey',pch = 20)
points(x,w*dbeta(x,2,6),col = 'black',pch = 20)
q <- seq(0,1,length.out = length(x))
lines(q,dbeta(q,2.7,6.3),col='red', lwd = 2)
lines(q,dbeta(q,2,6)*M,col='red', lwd = 2)
par(mfrow=c(1,1))

# Example 21
Nsim <- 2500
M <- optimize(function(x){dgamma(x,3/2,1)/dexp(x,2/3)},
              c(0,1000), maximum = TRUE)$objective
u <- runif(Nsim)
y <- rexp(Nsim,2/3)
x <- y[u < dgamma(y,3/2,1)/dexp(y,2/3)/M]
mean(x)

# Exercise 7 #1
## X|X<0.05 , X ~ Exp(1)
f <- function(r){
  M <- optimize(function(x){exp(-x)/(1-exp(-0.05))/20},
                c(0,0.05), maximum = TRUE)$objective
  t <- 0
  output <- NULL
  while(t<r){
    y <- runif(r-t,0,0.05)
    i <- runif(r-t) < exp(-y)/(1-exp(-0.05))/20/M
    t <- t + sum(i)
    output <- c(output, y[i])
  }
  return(output)
}
x <- f(1000)
mean(x)
## evaluate E(X|X<0.05)
install.packages('Ryacas')
library(Ryacas) 
x <- Sym('x')
z <- Integrate(x*exp(-x)/(1-exp(-0.05)), x = x, a = 0, b = 0.05)
eval(as.expression(z))

# Exercise 7 #2
f <- function(r){
  output <- NULL
  i <- .bincode(runif(r),c(0,1/3,2/3,1))
  output[i==1] <- runif(sum(i==1))
  output[i==2] <- rbeta(sum(i==2),3,1)
  output[i==3] <- rbeta(sum(i==3),5,1)
  return(output)
}
f(10)

# Example 22
M <- optimize(function(x){sqrt(2/pi)*exp(x-x^2/2)},
              c(0,100),maximum = TRUE)$objective
sqrt(2*exp(1)/pi)

f <- function(r){
  t <- 0
  output1 <- NULL
  output2 <- NULL
  while(t<r){
    y <- rexp(r-t,1)
    candidate <- rexp(r-t) - (y-1)^2/2
    i <- candidate >= 0
    t <- t + sum(i)
    output1 <- c(output1, y[i])
    output2 <- c(output2, candidate[i])
  }
  j <- runif(r)<=1/2
  output1[j] <- -output1[j]
  return(list(normal = output1, exponential = output2))
}

f(10)
mean(f(1e3)$normal)
mean(f(1e3)$exponential)

