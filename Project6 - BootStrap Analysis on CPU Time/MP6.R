library(boot)

#read values, distribution of random sample variable
cputimes <- scan(file="cputime.txt")

#Read 30 items
head(cputimes)
#[1] 70 36 43 69 82 48

# Given theta.hat = log(E(X)) and b=1000
b = 1000
theta.hat=log(mean(cputimes))
3.87605

##############################
##Bootstrap Initiallizations##
##############################

set.seed(12345)

##Calculating log of mean 
mean.npar=function(x, indices){
  result<-log(mean(x[indices]))
  return(result)
}

###############################
##   Resampling the sample   ##
###############################

##replicating b times
theta.hat.est<-replicate( b, mean(log(sample(cputimes,length(cputimes),replace = TRUE)))) 
#calculating the means of every 30 size sample, and finding their log values
#theta.hat.est

###############################
########  Question 1 ##########
###############################

#Bias = ((1/b)*sum(theta.hat.est))-theta.hat
bias<-(1/b)*sum(theta.hat.est)-theta.hat
bias
#[1] -0.1379195

#SE<- sqrt((1/(b-1))*sum())
mean1<-mean(theta.hat.est)
se<-sqrt((1/(b-1))*sum((theta.hat.est-mean1)^2))
se
#[1] 0.1018897

#Verifying with bootstrap
boot.values <- boot(cputimes, mean.npar, b , sim="ordinary", stype="i")
boot.values
#Bootstrap Statistics :
#  original      bias    std. error
#t1*  3.87605 -0.00559612  0.09878979

mean(boot.values$t)-boot.values$t0
#[1] -0.00559612
bias
#[1] -0.00559612

sd(boot.values$t)
#[1] 0.09878979

se
#[1] 0.1018897

###############################
########  Question 2 ##########
###############################

quantile(theta.hat , c(0.025,0.975))
#> quantile(theta.hat , c(0.025,0.975))
#2.5%   97.5% 
#3.87605 3.87605 


###############################
########  Question 3 ##########
###############################

quantile(theta.hat.est-theta.hat , c(0.025,0.975))
#> quantile(theta.hat.est-theta.hat , c(0.025,0.975))
#2.5%       97.5% 
#  -0.32768826  0.06735244 

###############################
########  Question 4 ##########
###############################

plot(boot.values)

#Calculating 95% CI for normal, basic and percentile using formulae
ci.norm<-c(theta.hat-bias-qnorm(0.975)*se , theta.hat-bias-qnorm(0.025)*se)
ci.norm
#[1] 3.81427 4.21367
ci.basic<-c((2*theta.hat)-sort(theta.hat.est)[c(b+1)*0.975] , (2*theta.hat)-sort(theta.hat.est)[c(b+1)*0.025])
ci.basic
#[1] 3.808742 4.204275
ci.percentile<-sort(theta.hat.est)[c((b+1)*0.025, (b+1)*0.975)]
ci.percentile
#[1] 3.547826 3.943359

#Verifyying using bootstrap

#Normal bootstrap
boot.ci(boot.values, conf=.95, type="norm" )
boot.ci(boot.values, conf=.25, type="norm" )

#Basic Bootstrap
boot.ci(boot.values, conf=.95, type="basic" )
boot.ci(boot.values, conf=.25, type="basic" )

#percentie Bootstrap
boot.ci(boot.values, conf=.95, type="perc" )
boot.ci(boot.values, conf=.25, type="perc" )