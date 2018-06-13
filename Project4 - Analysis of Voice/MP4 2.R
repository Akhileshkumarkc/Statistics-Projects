
#computing test static.
n=20
tstat = (9.02-10)/(2.22/sqrt(n))
#tstat
#-1.974186
pval = (1-pt(tstat,n-1))
#pval =0.9684606

#function to calculate p-value
pvalue = function(mu,n){
  x = rt(n,df=n-1)
  tstat= (mean(x)-10)/(2.22/sqrt(n))
  return (1-pt(tstat,n-1))
}

#monte-carlo simulation
rep_pval=replicate(1000,pvalue(10,20))
mean(rep_pval)

#direct method.

x = replicate(1000,rt(n,df=n-1))
t.test(x, alternative = "greater", mu = 10,parameter=n-1)


