## 1. ����10��cdf��F(x)=x^3,0<x<1���H����
runif(10)^(1/3)

## 2. ����10��Exp(lambda=5)���H����
-log(runif(10))/5

## 3. ����10��Gamma(20,lambda=5)���H����
replicate(10, sum(-log(runif(20)))/5)

## 4. ����10��Chi-square(6)���H����
replicate(10, sum(-log(runif(6/2)))*2)

## 5. ����10��cdf��F(x)=(x^2+x)/2,0<x<1���H����
sapply(runif(10),
       function(u){
         uniroot(f = function(x){(x^2+x)/2-u},lower = 0, upper = 1)$root
       })

## 6. ����10��logistic(2,3)���H����
sapply(runif(10),
       function(u){
         uniroot(f = function(x, mu=2, beta=3){1/(1+exp(-(x-mu)/beta))-u},
           lower = -1e16, upper = 1e16)$root
       })

## 7. ����10��Cauchy(2,3)���H����
sapply(runif(10),
       function(u){
         uniroot(f = function(x, mu=2, sigma=3){1/2+atan((x-mu)/sigma)/pi-u},
                 lower = -1e16, upper = 1e16)$root
       })

## 8. ����10��pdf��f(x)=exp(x)/(exp(1)-1),0<x<1���H����
log((exp(1)-1)*runif(10)+1)

## 9. ����10��pdf��f(x)=(x-2)/2,if 2<x<3 and (2-x/3)/2,if 3<x<6���H����
f <- function(u){
  if(u < 1/4){
    uniroot(function(x){1/4*x^2-x+1-u},c(2,3))$root
  }
  else{
    uniroot(function(x){-1/12*x^2+x-2-u},c(3,6))$root
  }
}
sapply(runif(10),f)

