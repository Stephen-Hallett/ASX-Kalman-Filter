pairpath = 'I:\\CodingProjects\\AlgoTrading\\one_year_refined_ASX_significant_pairs.txt' #windows
pricespath = 'I:\\CodingProjects\\AlgoTrading\\newASXdata.csv' #windows

#pairpath = '/Volumes/EMTECC450/CodingProjects/AlgoTrading/ASX_significant_pairs.txt' #mac
#pricespath = '/Volumes/EMTECC450/CodingProjects/AlgoTrading/ASXdata.csv' #mac


pairs = read.table(pairpath, header=FALSE, sep = '\n')
prices = read.csv(pricespath, header = TRUE)
dates = prices['Date']
print(strsplit(pairs[1,1], ' +')[[1]])
for (i in 1:dim(pairs)[1]){
  splitpair = strsplit(pairs[i,1], ' +')[[1]]
  pairs[i,1] = splitpair[1]
  pairs[i,2] = splitpair[2]
}

returns = numeric(dim(pairs)[1])

for (i in 1:dim(pairs)[1]){
  print(i)
  pair = unlist(pairs[i,1:2])
  initialprices = c(prices[pair[1]][[1]][1], prices[pair[2]][[1]][1])
  largestindex = which(initialprices==max(initialprices))
  smallestindex = which(initialprices==min(initialprices))
  x = prices[pair[smallestindex]]
  y = prices[pair[largestindex]]

  x$int = rep(1, nrow(x))
  y = matrix(c(y[[1]]))
  x = matrix(c(x[[1]],x[[2]]),ncol=2)
  delta = 0.0001

  
  yhat = rep(0, nrow(y)) #measurement prediction
  e = rep(0,nrow(y)) # measurement prediction error
  Q = rep(0,nrow(y)) # measurement prediction error variance
  
  #we denote R(t|t) by P(t)
  R = matrix(rep(0,4),ncol=2)
  P = matrix(rep(0,4),ncol=2)
  beta = beta = matrix(rep(0,nrow(x)*2),nrow=2)
  Vw = delta/(1-delta) * diag(2)
  Ve = 0.001

  for(t in 1:nrow(y)){
    if (t > 1){
      beta[, t] = beta[, t-1] #state prediction
      R = P + Vw # state covariance prediction
    }
    temp_x = t(x[t,])
    yhat[t] = x[t, ] %*% beta[, t]
    Q[t] = temp_x %*% R %*% t(temp_x) + Ve # measurement variance prediction
    
    e[t] = y[t]-yhat[t]
    K = R %*% t(temp_x) / Q[t] #Kalman gain
    beta[,t] = beta[,t]+ K * e[t]
    P= R - K %*% temp_x %*% R
  }

  sqrtQ = 0.5*sqrt(Q)
  
  #plot(2:length(beta[1,]),beta[1,2:length(beta[1,])], type='l', main = 'Beta Slope')
  #plot(2:length(beta[2,]),beta[2,2:length(beta[2,])], type='l', main='Beta Intercept')
  
  #plot(10:length(e),e[10:length(e)], type='l', main='Error')
  #lines(10:length(Q),sqrtQ[10:length(sqrtQ)], type='l',col='blue')
  #lines(10:length(Q),-sqrtQ[10:length(sqrtQ)], type='l',col='blue')
  
  longentry = e < -sqrtQ
  longexit = e > -sqrtQ
  shortentry = e > sqrtQ
  shortexit = e < sqrtQ
  
  fillMissingData = function(prices){
    for (t in 2:length(prices)){
      missingdata = is.na(prices[t])
      if (missingdata){
        prices[t] = prices[t-1]
      }
    }
    return(prices)
  }
  
  numUnitsLong = rep(NA,length(y))
  numUnitsShort = rep(NA,length(y))
  
  #long entry/exit positions
  numUnitsLong[1] = 0
  numUnitsLong[longentry] = 1
  numUnitsLong[longexit] = 0
  numUnitsLong = fillMissingData(numUnitsLong)
  
  #short entry/exit posititons
  numUnitsShort[1] = 0
  numUnitsShort[shortentry] = -1
  numUnitsShort[shortexit] = 0
  numUnitsShort = fillMissingData(numUnitsShort)
  
  #total position history
  numUnits = numUnitsLong + numUnitsShort
  
  xypositions = matrix(c(numUnits, numUnits), ncol=2)
  xybeta = matrix(c(-beta[1,], rep(1,length(beta[1,]))),ncol=2)
  xy = matrix(c(prices[pair[1]][[1]],y),ncol=2)
  positions = xypositions * xybeta * xy
  
  lagxy = rbind(NaN, xy)[1:length(y),]
  lagpositions = rbind(NaN, xypositions)[1:length(y),]
  
  pnl = matrix(rowSums(lagpositions*(xy-lagxy)/lagxy))
  ret = pnl/rowSums(abs(lagpositions))
  ret[is.nan(ret)] = 0
  
  cumret = matrix(cumprod(1+ret)-1)
  plot(2:length(cumret),cumret[2:length(cumret)], type='l')
  returns[i] = cumret[dim(cumret)[1]]

}

print(mean(returns))
print(returns)


