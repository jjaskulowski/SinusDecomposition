price <- read.csv(file = 'eur.csv')
head(price)

experimentLength = 200


calcSinusApproximation = function(shift) {
  optimizationSpace = expand.grid(0:600/100, 0:99/100);
  
  sinWave = function(f, s)
    sin((1:length(priceR) - 1) / length(priceR) * 2 * pi * 2 ^ (f - 1) - s *
          2 * pi)
  rescaleSin = function(sn, v)
    (min(priceR) + v[2] * priceRange + sn * priceRange * v[1])
  
  
  fits = list()
  prices = list()
  experimentXRange = 1:experimentLength + shift
  priceR = price[["Open"]][experimentXRange];
  
  for (iteration in 1:16) {
    #/// cor
    #sinWavePar = optim(c(1,0), function(v) cor(sinWave(v[1],v[2]), priceR), method = "L-BFGS-B", lower = c(-3,0), upper = c(5,1), control = list(fnscale = -1) )
    #sinWavePar = optim(c(1, 0), function(v)
    #  cor(sinWave(v[1], v[2]), priceR), control = list(fnscale = -1))
    
    values = apply(as.matrix(optimizationSpace), 1, function(x) cor(priceR, sinWave(x[1],x[2])))
    sinWavePar = as.numeric(optimizationSpace[which(values == max(values))[1],])
    
    #// sin
    #selectedSin = sinWave(sinWavePar$par[1], sinWavePar$par[2])
    selectedSin = sinWave(sinWavePar[1], sinWavePar[2])
    
    #// scale
    priceRange = max(priceR) - min(priceR)
    scaledSin = (selectedSin - min(selectedSin)) / (max(selectedSin - min(selectedSin))) * priceRange + min(priceR)
    
    #// minimize var
    
    #varPar = optim(c(1,0), function(v) { r = rescaleSin(selectedSin, v); m = median(abs(r - priceR)); print(c(v,m)); plot(r); m })
    varPar = optim(c(8, 0), function(v)
      mean(abs(rescaleSin(selectedSin, v) - priceR) ^ 2))
    
    rescaledSin = rescaleSin(selectedSin, varPar$par)
    priceRReminder = priceR - rescaledSin
    
    fits[[length(fits) + 1]] = rescaledSin
    prices[[length(prices) + 1]] = priceRReminder
    priceR = priceRReminder
    
  }
  
  #plot(price[["Open"]][experimentXRange])
  
  #for (it in 1:16) {
    #lines(apply(matrix(unlist(fits[1:it]), experimentLength), 1, function(x) sum(x)))
  #}
  
  combined = (apply(matrix(unlist(fits[1:16]), experimentLength), 1, function(x)
    sum(x)))
  
  list(combined = combined, fits = fits, prices = prices)
}

calcAndPlot = function(shift) {
  cc = calcSinusApproximation(shift)
  plot(price[["Open"]][1:experimentLength + shift])
  lines(cc)
}




