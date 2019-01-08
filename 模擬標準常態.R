##  產生10個N(0,1)的隨機值
#   Box-Muller Algorithm
n <- 10/2
t <- 0
X <- NULL
Y <- NULL
while (t < n){
  V1 <- runif(n-t,-1,1)
  V2 <- runif(n-t,-1,1)
  S <- V1^2 + V2^2
  i <- which(S <= 1)
  X <- c(X, V1[i]*sqrt(-2*log(S[i])/S[i]))
  Y <- c(Y, V2[i]*sqrt(-2*log(S[i])/S[i]))
  t <- length(X)
}
c(X,Y)