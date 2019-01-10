## 1. 一副撲克牌52張抽5張, 一張Ace都沒有的機率

# 模擬
v <- NULL
for(i in 1 : 1e5){
  v[i] <- all(sample(52, 5) > 4)
}
mean(v)
# 實際
choose(4,0)*choose(48,5)/choose(52,5)

## 2. 產生10個Uniform(5,7)的隨機值
2*runif(10)+5

## 3. 獨立地擲10次公正骰子的結果, i.e. Discrete Uniform(1,2,...,6)
ceiling(6*runif(10))
sample.int(6, 10, replace = TRUE)

## 4. 產生10個Discrete Uniform(5,6,...15)的隨機值
ceiling(11*runif(10)) + 4
sample(5:15, 10, replace = TRUE)

## 5. 產生10個Bin(5, 0.3)的隨機值
replicate(10, sum(runif(5) < 0.3))
rbinom(10, 5, 0.3)

## 6. 1~100的牌,"第i次的時候抽到i"記為1 hit,總共幾hit?
f <- function(){
  x <- sample.int(100,100)
  return(sum(x == 1:100))
}
f()
