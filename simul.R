# uncoment to reinit
reslist=lapply(1:500, calcSinusApproximation)

approximations = sapply(reslist, \(x) x$combined[length(x$combined)])

realPrices = price[["Open"]][1:length(approximations) + 100 - 1]

stopifnot(length(realPrices) == length(approximations))

plot(price[["Open"]][1:200 + 100 - 1], t='l')
lines(approximations,  col="blue")

trades = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("dir", "oprice","cprice")) 
balances = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("closed","open")) 

tradeOpen = \(dir, price) trades[nrow(trades) + 1,] <<- c(dir,price,NA)   

tradesProfBClose = \(curprice) trades[which(trades[,'oprice'] < curprice & trades[,'dir'] =='b'), 'cprice'] <<- curprice
tradesProfSClose = \(curprice) trades[which(trades[,'oprice'] > curprice & trades[,'dir'] =='s'), 'cprice'] <<- curprice
tradesProfClose = \(dir, curprice) 
  if(dir == 'b') tradesProfBClose(curprice) else tradesProfSClose(curprice) 

balance = \(curprice) 
  c(
    which(!is.na(trades[,'cprice']) & trades[,'dir'] =='b') |> (\(x) as.numeric(trades[x,'cprice']) - as.numeric(trades[x,'oprice']))() |> sum() +
      which(!is.na(trades[,'cprice']) & trades[,'dir'] =='s') |> (\(x) - as.numeric(trades[x,'cprice']) + as.numeric(trades[x,'oprice']))() |> sum(),
    
    which(is.na(trades[,'cprice']) & trades[,'dir'] =='b') |> (\(x) curprice - as.numeric(trades[x,'oprice']))() |> sum() +
      which(is.na(trades[,'cprice']) & trades[,'dir'] =='s') |> (\(x) - curprice + as.numeric(trades[x,'oprice']))() |> sum()
  )

averageWin = \()
(
  which(!is.na(trades[, 'cprice']) &
          trades[, 'dir'] == 'b') |> (\(x) as.numeric(trades[x, 'cprice']) - as.numeric(trades[x, 'oprice']))() |> (\(x) c(sum(x), length(x)))() +
    which(!is.na(trades[, 'cprice']) &
            trades[, 'dir'] == 's') |> (\(x) - as.numeric(trades[x, 'cprice']) + as.numeric(trades[x, 'oprice']))() |> (\(x) c(sum(x), length(x)))()
) |> (\(x) x[1] / x[2])()

sharpee = \()
  c(which(!is.na(trades[, 'cprice']) &
          trades[, 'dir'] == 'b') |> (\(x) as.numeric(trades[x, 'cprice']) - as.numeric(trades[x, 'oprice']))(),
    which(!is.na(trades[, 'cprice']) &
            trades[, 'dir'] == 's') |> (\(x) - as.numeric(trades[x, 'cprice']) + as.numeric(trades[x, 'oprice']))()) |> ( \(x) mean(x) / sd(x) )()

sharpeeB = \()
  c(diff(balances[,1]), diff(balances[,2])) |> ( \(x) mean(x) / sd(x) )()
  


balancesAdd = \(curprice) balances[nrow(balances)+1,] <<- balance(curprice)
  
balances = balances[c(),]

for(i in 1: length(realPrices)){
  
  rp = realPrices[i]
  ap = approximations[i]
  if(rp < ap) {
    tradesProfSClose(rp)
    tradeOpen('b', rp)
  }
  if(rp > ap) {
    tradesProfBClose(rp)
    tradeOpen('s', rp)
  }
  
  balancesAdd(rp)
}

plot(balances[,1] + balances[,2], t='l')
