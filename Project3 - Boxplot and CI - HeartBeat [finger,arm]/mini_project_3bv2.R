no_of_times = 500
count = 0

#estimate phat for the given n and p

cov_prob <- function(n,p) {
  
  a=replicate(no_of_times,rbinom(n,1,p))
  
  i=1
  repeat{
    if(i>no_of_times){
      break
    }
    
    #estimate phat for the given n and p
    Pestimate=c()
    pages = 1:n
    Pestimate[i]= length(pages[a[,i]==1])/n
    
    #find CI for the estimated phat
    ci_upper = Pestimate[i] + (qnorm(0.975) * sqrt((Pestimate[i]*(1-Pestimate[i]))/n))
    ci_lower = Pestimate[i] - (qnorm(0.975) * sqrt((Pestimate[i]*(1-Pestimate[i]))/n))
    
    
    
    #find the coverage probability
    if(ci_lower <= p && p <= ci_upper ){
      count=count+1
      
    }else{
    }
    
    

    i=i+1
  }
  print (count)
  coverage = (count/no_of_times)*100
  print(coverage)
  
  return(coverage)
}

np <- function(n1,p1, nvalues, pvalues, covprob) {
  cov = cov_prob(n1,p1)
  
  covprob <- c(covprob,cov)
  nvalues <- c(nvalues, n1)
  print(nvalues)
  
  pvalues <- c(pvalues, p1)
  
  result <- c(nvalues, pvalues, covprob)
  return(result)
  
}

covprob = nvalues = pvalues = NULL

res <- np(5, 0.05,nvalues,pvalues, covprob)
#print (res[1])

res <- np(10, 0.05,res[1],res[2], res[3])
n=res[1:2]
p=res[3:4]
cov=res[5:6]

res <- np(30, 0.5,n,p,cov)
n=res[1:3]
p=res[4:6]
cov=res[7:9]


res <- np(50, 0.95,n,p, cov)
n=res[1:4]
p=res[5:8]
cov=res[9:12]


res <- np(100, 0.05,n,p, cov)
n=res[1:5]
p=res[6:10]
cov=res[11:15]


res <- np(10, 0.25,n,p, cov)
n=res[1:6]
p=res[7:12]
cov=res[13:18]



res <- np(30, 0.5,n,p, cov)
n=res[1:7]
p=res[8:14]
cov=res[15:21]


res <- np(50, 0.95,n,p,cov)
n=res[1:8]
p=res[9:16]
cov=res[17:24]

res <- np(100, 0.05,n,p,cov)
n=res[1:9]
p=res[10:18]
cov=res[19:27]


res <- np(50, 0.95,n,p,cov)
n=res[1:10]
p=res[11:20]
cov=res[21:30]


par(mfrow=c(1,2))
plot(p, cov,xlab="p",ylab="coverage probability")
dev.copy(png,"p vs coverage prob.png",width=8,height=6,units="in",res=100)
dev.off()


plot(n, cov,xlab="n",ylab="coverage probability")
dev.copy(png,"n vs coverage prob.png",width=8,height=6,units="in",res=100)
dev.off()
par(mfrow=c(1,1))