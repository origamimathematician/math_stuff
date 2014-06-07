n = 100
B = 100000
onesCount = rep(NA,B)
for (i in 1:B){
    sampleSet = sample(1:n,n,replace= TRUE)
    onesCount[i] = length(which(sampleSet == 1))
}
frequencies = table(onesCount)/B
plot(frequencies)
