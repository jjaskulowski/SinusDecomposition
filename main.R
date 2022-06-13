#price <- read.csv(file = 'eur.csv')

price <- read.csv(file = 'EURUSD_Candlestick_1_M_BID_30.05.2022-04.06.2022.csv')

head(price)

experimentLength = 100
require(compiler)
enableJIT(3)
compilePKGS(T)
sinWave = (function(f, s)
  sin((1:experimentLength - 1) / experimentLength * 2 * pi * 2 ^ (f - 1) - s *
        2 * pi))

rescaleSin = (function(sn, v, priceR, priceRange)
  (min(priceR) + v[2] * priceRange + sn * priceRange * v[1]))

calcSinusApproximation = function(shift, noWaves = 6, priceR = NULL) {
 
  fits = list()
  prices = list()
  experimentXRange = 1:experimentLength + shift
  #if(is.null(priceR))
  priceR = price[["Open"]][experimentXRange];
  
  sinFreqFact = (log(experimentLength/3,2)*100) 

  
  if(!exists("optimizationSpace"))
    optimizationSpace <<- expand.grid(seq(0,sinFreqFact,2)/100, seq(0,99,2)/100);
  
  if(!exists("sins") || nrow(sins) != experimentLength)
  sins <<- apply(optimizationSpace, 1, function(x) sinWave(x[1],x[2]))
  
  

  
  for (iteration in 1:noWaves) {
    cat(iteration)
    #/// cor
    #sinWavePar = optim(c(1,0), function(v) cor(sinWave(v[1],v[2]), priceR), method = "L-BFGS-B", lower = c(-3,0), upper = c(5,1), control = list(fnscale = -1) )
    #sinWavePar = optim(c(1, 0), function(v)
    #  cor(sinWave(v[1], v[2]), priceR), control = list(fnscale = -1))
    
    #values = apply(as.matrix(optimizationSpace), 1, function(x) cor(priceR, sinWave(x[1],x[2])))
    #sinWavePar = as.numeric(optimizationSpace[which(values == max(values))[1],])
    values = cor(priceR, sins)[1,]
    #sinWavePar = optimizationSpace[which(values == max(values)),]
    
    #// sin
    #selectedSin = sinWave(sinWavePar$par[1], sinWavePar$par[2])
    #selectedSin = sinWave(sinWavePar[1], sinWavePar[2])
    wh = which(values == max(values))
    selectedSin = sins[,wh]
                       
#    #
    #sin2nd = (sins * selectedSin)
    #values = cor(priceR, sin2nd)[1,]
    #sinWavePar = optimizationSpace[which(values == max(values)),]
    #selectedSin = sin2nd[,which(values == max(values))]
#    #
                       
                   
    #// scale
    priceRange = max(priceR) - min(priceR)
    scaledSin = (selectedSin - min(selectedSin)) / (max(selectedSin - min(selectedSin))) * priceRange + min(priceR)
    
    #// minimize var
    
    #varPar = optim(c(1,0), function(v) { r = rescaleSin(selectedSin, v); m = median(abs(r - priceR)); print(c(v,m)); plot(r); m })
    #varPar = optim(c(8, 0), function(v)
    #  mean((rescaleSin(selectedSin, v, priceR, priceRange) - priceR) ^ 2))
    
    #rescaledSin = rescaleSin(selectedSin, varPar$par)
    rescaledSin = selectedSin
    #reshape = lowess(rescaledSin, priceR)
    #reshapedSin = splinefun(reshape$x,reshape$y)(rescaledSin)
    
    #reshapedSin = rescaledSin
    
    reshape = lowess(rescaledSin, priceR, f=2^(optimizationSpace[wh,1]-1))
    reshapedSin = approxfun(reshape$x,reshape$y)(rescaledSin)
    #plot(z2(rescaledSin))
    
    
    priceRReminder = priceR - reshapedSin
    
    fits[[length(fits) + 1]] = reshapedSin
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

calcSinusApproximation = cmpfun(calcSinusApproximation, options = list(optimize = 3))


