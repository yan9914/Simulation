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

## 11. 產生10個Poisson(lambda)的隨機值

