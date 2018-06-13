# 1d simulating with 10,000 draws repatiting it five times
ListofSamples <-replicate(5,runif(n=10000,min=0,max=1)^(1/4))

ListofSamples <-replicate(5,rnorm(n=10000,p=10))

list = rnorm(1000, mean = 100, sd = 15)

qqnorm(list, main="Q-Q plot for HeartBeat Finger")
qqline(list)

sample = list[100]
mean(sample)

conf.int <- function(sample.size, p, alpha=0.05) {
  U <- runif(sample.size);
  X <- 1*(U<=p);
  phat<-mean(X);
  est.std.err <- sqrt(phat*(1-phat)/sample.size);
  ci <-phat + c(-1, 1) * qnorm(1 - (alpha/2)) * est.std.err;
  return(ci);
}

Accuracy_Ci<-function(nsim=1000,size, phat){
  acc=rep(0,5);
  for(i in 1:length(size)){
    cimat<-replicate(nsim,conf.int(size[i],phat));
    acc[i]<-mean((phat>=cimat[1,])*(phat<=cimat[2,]))*100;
    
  }
  return(acc);
}
#
#

#
#
#Take level of confidence to be 95% but use a variety of values for n and p, e.g., n = 5, 10,
#30, 50, 100, and p = 0.05, 0.1, 0.25, 0.5, 0.9, 0.95.
#
#95% CI for n=5,10,30,50,100,1000
list=c()

list = c((conf.int(5,0.1)))
list2 = Accuracy_Ci(5,list)

list[1]=c((conf.int(10,0.1)))
list[2]=c((conf.int(30,0.1)))          
list[3]=c((conf.int(50,0.1)))
list[4]=c((conf.int(100,0.1)))
list[5]=c((conf.int(500,0.1)))
list[6]=c((conf.int(1000,0.1)))
          


