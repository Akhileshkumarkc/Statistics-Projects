# Initializing the various data variables with the data provided in the problem.
nx = 400
ny=500
mu_x=2635
mu_y=2887
s_x=365
s_y=412
alpha = 0.05

# se is the standard error calculated using sample standard deviation of 2 samples.
se=sqrt(((s_x^2)/nx) + ((s_y^2)/ny))
#25.93358


# The 95% confidence interval for the 2 sample problem assuming normality as sample sizes are large.
cint = (mu_x-mu_y)+c(-1,1)*qnorm(1-alpha/2)*se      
#[1] -302.8289 -201.1711

# 5% level alpha test, z-test 
zstat = (mu_x-mu_y)/se  
#-9.717132
pval = pnorm(zstat)      
#1.274297e-22
#Since pval <0.05 reject the null hypothesis . 