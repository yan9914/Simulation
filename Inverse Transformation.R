## 1. 產生10個cdf為F(x)=x^3,0<x<1的隨機值
runif(10)^(1/3)

## 2. 產生10個Exp(lambda=5)的隨機值
-log(runif(10))/5

## 3. 產生10個Gamma(20,lambda=5)的隨機值
replicate(10, sum(-log(runif(20)))/5)

## 4. 產生10個Chi-square(6)的隨機值
replicate(10, sum(-log(runif(6/2)))*2)

## 5. 產生10個cdf為F(x)=(x^2+x)/2,0<x<1的隨機值
sapply(runif(10),
       function(u){
         uniroot(f = function(x){(x^2+x)/2-u},lower = 0, upper = 1)$root
       })

## 6. 產生10個logistic(2,3)的隨機值
sapply(runif(10),
       function(u){
         uniroot(f = function(x, mu=2, beta=3){1/(1+exp(-(x-mu)/beta))-u},
                 lower = -1e16, upper = 1e16)$root
       })

## 7. 產生10個Cauchy(2,3)的隨機值
sapply(runif(10),
       function(u){
         uniroot(f = function(x, mu=2, sigma=3){1/2+atan((x-mu)/sigma)/pi-u},
                 lower = -1e16, upper = 1e16)$root
       })

## 8. 產生10個pdf為f(x)=exp(x)/(exp(1)-1),0<x<1的隨機值
log((exp(1)-1)*runif(10)+1)

## 9. 產生10個pdf為f(x)=(x-2)/2,if 2<x<3 and (2-x/3)/2,if 3<x<6的隨機值
f <- function(u){
  if(u < 1/4){
    uniroot(function(x){1/4*x^2-x+1-u},c(2,3))$root
  }
  else{
    uniroot(function(x){-1/12*x^2+x-2-u},c(3,6))$root
  }
}
sapply(runif(10),f)

## 10. 產生10個(p1,p2,p3,p4) = (0.2,0.15,0.25,0.4), P(X = j) = Pj 的隨機值
f <- function(r){
  p <- c(0.2, 0.15, 0.25, 0.4)
  .bincode(runif(r), c(0,cumsum(p)))
}
f(10)

## 11. 產生1個Bin(5,0.3)的隨機值
f <- function(n,p){
  u <- runif(1)
  c <- p/(1-p)
  i <- 0
  pr <- (1-p)^n
  F <- pr
  while(u >= F){
    pr <- c*(n-i)/(i+1)*pr
    F <- F + pr
    i <- i+1
  }
  return(i)
}
f(5,0.3)

## 12. 產生1個Poisson(lambda=3)的隨機值
f <- function(lambda){
  u <- runif(1)
  p <- exp(-lambda)
  i <- 0
  F <- p
  while(u >= F){
    p <- lambda*p/(i+1)
    F <- F + p
    i <- i+1
  }
  return(i)
}
f(3)

## 13. 產生1個Geometric(0.3)的隨機值
f <- function(p){
  u <- runif(1)
  return(ceiling(log(u)/log(1-p)))
}
f(0.3)

## 14. 產生1個NB(5,0.3)的隨機值
f <- function(r,p){
  u <- runif(1)
  pr <- p^r
  i <- r
  F <- pr
  while(u >= F){
    pr <- i*(1-p)/(i+1-r)*pr
    F <- F + pr
    i <- i+1
  }
  return(i)
}
f(5,0.3)


## Composition Approach

## 15. 產生1個p1=...=p5=0.05, p6=...=p10=0.15的隨機值
f <- function(){
  u1 <- runif(1)
  u2 <- runif(1)
  if (u1 < 0.5) return(ceiling(u2*10))
  else          return(ceiling(u2*5)+5)
}
f()

## 16. 產生1個p5=p7=...=p13=0.11, p6=p8=...=p14=0.09的隨機值
f <- function(){
  u1 <- runif(1)
  u2 <- runif(1)
  if (u1 < 0.55) return(ceiling(u2*5)*2+3)
  else           return(ceiling(u2*5)*2+4)
} # 定義域為等差數列的Uniform隨機值 : ceiling(U*個數)*間距+修正
f()

## 17. 產生1個p1~p10 = (0.06,0.06,0.06,0.06,0.06,0.15,0.13,0.14,0.15,0.13)的隨機值
f <- function(){
  u1 <- runif(1)
  u2 <- runif(1)
  if      (u1 < 0.6)  return(ceiling(u2*10))
  else if (u1 < 0.78) return(ceiling(u2*2)*3+3)
  else if (u1 < 0.92) return(ceiling(u2*2)*3+4)
  else                return(8)
}
f()

## 18. 產生下列服從pmf的隨機值
#                                j-1
#                1  j+1    1    2
#   P(X = j) = ( — )   + ( — ) ————   , j = 1,2,...
#                2         2     j
#                               3
f <- function(r){
  u1 <- runif(r)
  u2 <- runif(r)
  v <- NULL
  x <- .bincode(u1,c(0,0.5,1))
  v[x==1] <- ceiling(log(u2[x==1])/log(1/2))  # Geo(1/2)
  v[x==2] <- ceiling(log(u2[x==2])/log(2/3))  # Geo(2/3)
  return(v)
}
f(10)
