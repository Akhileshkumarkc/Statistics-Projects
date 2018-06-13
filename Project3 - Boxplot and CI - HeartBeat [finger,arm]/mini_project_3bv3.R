coverageCount = 0

#estimate Pestimate for the given size and probabilty

covergenceProbaility <- function(size,prob) {
  
  #repeat 200 times the binomial with a given size and prob
  rtimes = 1000
  a=replicate(rtimes,rbinom(size,1,prob))
  
  
  # repeat for the 1000 times finding the Confidence interval
  i=1
  repeat{
    if(i>rtimes){
      break
    }
    
    #estimate Pestimate for the given sample size n and Probability p
    Pestimate=c()
    pages = 1:size
    Pestimate[i]= length(pages[a[,i]==1])/size
    
    #find CI for the estimated Pestimate upper and lower limit.
    ci_upper = Pestimate[i] + (qnorm(0.975) * sqrt((Pestimate[i]*(1-Pestimate[i]))/size))
    ci_lower = Pestimate[i] - (qnorm(0.975) * sqrt((Pestimate[i]*(1-Pestimate[i]))/size))
    
    #find the coverage probability
    
    # if the prob is within with the range then update the count.
    if(ci_lower <= prob && prob <= ci_upper ){
      coverageCount=coverageCount+1
    }
    
    i=i+1
  }
  # coverage probabilty count to 100.
  coverageProbability = (coverageCount/rtimes)*100
  return(coverageProbability)
}

#list of sizes
sizes = c(5,10,30,50,60,70,80,90,100,110,120,130)
#list of probabilities
probs = c(0.05,0.1,0.25,0.3,0.5,0.7,0.75,0.9,0.95)

#prepare a dataframe
df = data.frame(cp=0,prob=0,size=0)

#compute data for all sizes and probs
for(size in sizes)
{
  for(prob in probs)
  {
    res <- covergenceProbaility(size,prob)
    df<-rbind(df,c(res,prob,size))
    
  }
}
# plot it side by side
par(mfrow=c(1,2))
plot(df$prob, df$cp,xlab="probability",ylab="coverage probability")
plot(df$size, df$cp,xlab="size",ylab="coverage probability")
par(mfrow=c(1,1))

# plot the big graph.
#plot(df)