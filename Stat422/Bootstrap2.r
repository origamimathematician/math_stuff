library(DAAG)

y = ais$pcBfat
x1 = ais$wt
x2 = ais$ssf
x3 = ais$sex == "f"
n = length(y)
X = cbind(rep(1,n),x1,x2,x3)

XTX = t(X)%*%X
XTXinv = solve(XTX)

betaHat = XTXinv%*%t(X)%*%y
variance = (t(y)%*%y-t(y)%*%X%*%betaHat)/(n-4)
beta1Hat = betaHat[2]
#beta1Col = X[,2]

varMatrix = variance[1]*XTXinv

varBeta1 = varMatrix[2,2]
t = qt(0.975,n-4)

tConfLower = beta1Hat - t*sqrt(varBeta1)
tConfUpper = beta1Hat + t*sqrt(varBeta1)

B = 10000
beta1Est = rep(NA,B)
 for(i in 1:B){
    idxs = sample(1:n,n,replace = TRUE)
    sampleResponse = y[idxs]
    samplePredict = X[idxs,]
    samXTX = t(samplePredict)%*%samplePredict
    samXTXinv = solve(samXTX)
    samBetaHat = samXTXinv%*%t(samplePredict)%*%sampleResponse
    beta1Est[i] = samBetaHat[2]
 }
 
 bootConf = quantile(p = c(.025,.975),beta1Est)
 
 
 
