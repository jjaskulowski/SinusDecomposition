varPar = optim(c(8, 0), function(v)
  mean(abs(rescaleSin(selectedSin, v) - priceR) ^ 2))

rescaledSin = rescaleSin(selectedSin, varPar$par)


plot(priceR)
lines(scaledSin)
lines(rescaledSin, col='blue')

lines(priceRReminder)
lines(priceR - rescaledSin)


plot(priceR)
lines(sinWave(3.45,0.7)/100)

plot(sapply(0:1600/200, function(x) cor(priceR, sinWave(x,0.7)/100)))

optimizationSpace = expand.grid(0:800/100, 0:100/100);

values = apply(as.matrix(optimizationSpace), 1, function(x) cor(priceR, sinWave(x[1],x[2])))
sinWavePar = optimizationSpace[which(values == max(values))[1],]




persp(0:800/100, 0:100/100, matrix(values, ncol= 101) )

cor(priceR, sinWave(3.45,0.7)/100)



plot(res$combined, t='l')
lines(price[["Open"]][1:experimentLength], )


lines(res$fits[[1]] )
lines(res$fits[[1]] + res$fits[[2]])
lines(res$fits[[1]] + res$fits[[2]]  + res$fits[[3]])
lines(res$fits[[1]] + res$fits[[2]] + res$fits[[3]]  + res$fits[[4]])
lines(res$fits[[1]] + res$fits[[2]] + res$fits[[3]] + res$fits[[4]] + res$fits[[5]])
lines(res$fits[[1]] + res$fits[[2]] + res$fits[[3]] + res$fits[[4]] + res$fits[[5]] + res$fits[[6]])



