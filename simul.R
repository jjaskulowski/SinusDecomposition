# uncoment to reinit

#reslist=lapply(0:2430, \(x) calcSinusApproximation(x,7))



approximations = sapply(reslist, \(x) x$combined[length(x$combined)])

realPrices = price[["Open"]][1:length(approximations) + experimentLength - 1]
realPricesHigh = price[["High"]][1:length(approximations) + experimentLength - 1]
realPricesLow = price[["Low"]][1:length(approximations) + experimentLength - 1]
realPricesClose = c(realPrices[-1], tail(realPrices,1))

adx = ADX(cbind(High = realPricesHigh, Low = realPricesLow, Close = realPricesClose))
rsi = RSI(realPrices)
rsi[is.na(rsi)] = 50


stopifnot(length(realPrices) == length(approximations))

#plot(price[["Open"]][1:200 + 100 - 1], t='l')
#lines(approximations,  col="blue")

trades = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("dir", "oprice","cprice","size")) 
balances = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("closed","open")) 

tradeOpen = \(dir, price, amount = 1, size = 1) for(i in 1:1) {
  opposite = which(trades[,'dir'] != dir & is.na(trades[,'cprice']))
  #if (length(opposite) > 0) 
  #  trades[opposite[[1]], 'cprice'] <<- price
  #else
    trades[nrow(trades) + 1,] <<- c(dir,price,NA, size)   
} 

tradesProfBClose = \(curprice, hp) trades[which(as.numeric(trades[,'oprice']) < hp & trades[,'dir'] =='b'), 'cprice'] <<- curprice
tradesProfSClose = \(curprice, lp) trades[which(as.numeric(trades[,'oprice']) > lp & trades[,'dir'] =='s'), 'cprice'] <<- curprice
tradesProfClose = \(dir, curprice) 
  if(dir == 'b') tradesProfBClose(curprice) else tradesProfSClose(curprice) 

na = \(x,p) {
  x[is.na(x)] = p 
  x
}

suggestsize = \(dir,curprice) {
  if(nrow(balances) < 50) return(1)
  return(1)
  smoothbalance = loess(y ~ x, span=0.1, data = as.data.frame( cbind(x =1:min(nrow(balances),100), y= tail(balances[,2],100))))|> (\(x) predict(x, 1:min(nrow(balances),100)))()
  v = tail(diff(tail(smoothbalance))*10,1)
  
  return (if (v < 0) 1 else 0)
  #return(2^tail(diff(tail(smoothbalance))*10,1)^3)
  
  
  w = which(trades[,'dir'] == dir & !is.na(trades[,'cprice']))
  if(length(w) == 0) return(1)

  w = trades[w[length(w)],]
  if(w[,'dir'] == 'b' && as.numeric(w['cprice']) < curprice ||
     w[,'dir'] == 's' && as.numeric(w['cprice']) > curprice) 
    return((as.numeric(w[,'size']) * 0.5))
  return(1)
}



balance = \(curprice) 
  c(
    (which(!is.na(trades[,'cprice']) & trades[,'dir'] =='b') |> (\(x) (as.numeric(trades[x,'cprice']) - as.numeric(trades[x,'oprice'])) *  as.numeric(trades[x,'size']))() |> sum()) +
      (which(!is.na(trades[,'cprice']) & trades[,'dir'] =='s') |> (\(x) (- as.numeric(trades[x,'cprice']) + as.numeric(trades[x,'oprice'])) * as.numeric(trades[x,'size']))() |> sum()),
    
    (which(is.na(trades[,'cprice']) & trades[,'dir'] =='b') |> (\(x) (curprice - as.numeric(trades[x,'oprice'])) * as.numeric(trades[x,'size']))() |> sum()) +
      (which(is.na(trades[,'cprice']) & trades[,'dir'] =='s') |> (\(x) (- curprice + as.numeric(trades[x,'oprice'])) * as.numeric(trades[x,'size']))() |> sum())
  )
  
uncloses = \(dir) max(0,length(which(is.na(trades[,'cprice']) & trades[,'dir'] == dir)) -
  length(which(is.na(trades[,'cprice']) & trades[,'dir'] != dir)))
  

averageWin = \()
(
  which(!is.na(trades[, 'cprice']) &
          trades[, 'dir'] == 'b') |> (\(x) as.numeric(trades[x, 'cprice']) - as.numeric(trades[x, 'oprice']))() |> (\(x) c(sum(x), length(x)))() +
    which(!is.na(trades[, 'cprice']) &
            trades[, 'dir'] == 's') |> (\(x) - as.numeric(trades[x, 'cprice']) + as.numeric(trades[x, 'oprice']))() |> (\(x) c(sum(x), length(x)))()
) |> (\(x) x[1] / x[2])()

giveSeq = \(x) if(x < 1) c() else seq(x)



closePercentagePos = \(perc,curprice){
 
  
  b = which(is.na(trades[, 'cprice']) & trades[, 'oprice'] < curprice  & trades[, 'dir'] == 'b') |> (\(x) x[giveSeq(length(x) * perc)])()
  s = which(is.na(trades[, 'cprice']) & trades[, 'oprice'] > curprice  & trades[, 'dir'] == 's') |> (\(x) x[giveSeq(length(x) * perc)])()
  trades[c(b[!is.na(b)],s[!is.na(s)]), 'cprice'] <<- curprice

 
}

sharpee = \()
  c(which(!is.na(trades[, 'cprice']) &
          trades[, 'dir'] == 'b') |> (\(x) (as.numeric(trades[x, 'cprice']) - as.numeric(trades[x, 'oprice']))* as.numeric(trades[x,'size']))(),
    which(!is.na(trades[, 'cprice']) &
            trades[, 'dir'] == 's') |> (\(x) (- as.numeric(trades[x, 'cprice']) + as.numeric(trades[x, 'oprice']))* as.numeric(trades[x,'size']))()) |> ( \(x) mean(x) / sd(x) )()

sharpeeB = \()
  c(diff(balances[,1]), diff(balances[,2])) |> ( \(x) mean(x) / sd(x) )()



balancesAdd = \(curprice) balances[nrow(balances)+1,] <<- balance(curprice)
  

edge = 0.00001

for(i in 1: length(realPrices)){
  rp = realPrices[i]
  hp = realPricesHigh[i]
  lp = realPricesLow[i]
  #adx1 = adx[i-1,'DIp'] - adx[i-1,'DIn']
  
  
  if(nrow(balances) > 0)
    plot(balances[,1] + balances[,2], t='l')
  if(nrow(balances) >= 50){
    b = tail(balances[,1] + balances[,2],5)
    appr = mean(b)
    
    if(tail(appr,1) < tail(b,1)){
      #closePercentage(0.1, rp);
    }
    if(tail(appr,1) > tail(b,1)){
      #closePercentagePos(0.1, rp);
    }
  }
  
#  if(nrow(balances) > 20 && var(tail(balances[,2],20)) > var(head(tail(balances[,2],21),20))
#     && mean(tail(balances[,2],10)) < mean(head(tail(balances[,2],11),10))
#     ){
#    closePercentagePos(1, rp)
#  }

  
  ap = approximations[i]
  if(rp < ap && rsi[i] < 30) {
      tradesProfSClose(rp + 0.000034, lp)
      tradeOpen('b', rp, uncloses('s') + 1, suggestsize('b', rp))
  }
  if(rp > ap && rsi[i] > 70) {
      tradesProfBClose(rp - 0.000034, hp)
      tradeOpen('s', rp, uncloses('b') + 1, suggestsize('s', rp))
  }
  
  
  
  balancesAdd(rp)

}

plot(balances[,1] + balances[,2], t='l')

plot(realPrices[10:1600],t='l')
lines(approximations[10:1600], col=rgb(0.3,0.7,1), t='l')

sharpee()


