singerdata <- read.csv("singer.txt", header = T, sep = ",")
list = unique(singerdata$voice)
#
#[1] Soprano Alto    Tenor   Bass   
#

#get the subset of data.
Bassitem = subset(singerdata,singerdata$voice.part=='Bass')
Tenoritem = subset(singerdata,singerdata$voice.part=='Tenor')
Altoitem = subset(singerdata,singerdata$voice.part=='Alto')
Sopranoitem = subset(singerdata,singerdata$voice.part=='Soprano')

#side by side boxplot
boxplot(Bassitem$height,Tenoritem$height,Altoitem$height,Sopranoitem$height, names=c('Bass','Tenor','Alto','Soprano'))

#Histogram plot
par(mfrow = c(1, 4))
hist(Bassitem$height,main="Bass")
hist(Tenoritem$height,main="Tenor")
hist(Altoitem$height,main="Alto")
hist(Sopranoitem$height,main="Soprano")
par(mfrow = c(1, 1))

#qq plot
par(mfrow = c(1, 4))
qqnorm(Bassitem$height, main="Q-Q plot for Baseitem")
qqline(Bassitem$height)
qqnorm(Tenoritem$height, main="Q-Q plot for Tenoritem")
qqline(Tenoritem$height)
qqnorm(Altoitem$height, main="Q-Q plot for Altoitem")
qqline(Altoitem$height)
qqnorm(Sopranoitem$height, main="Q-Q plot for Sopranoitem")
qqline(Sopranoitem$height)
par(mfrow = c(1, 1))
#summary of lists
summary(Bassitem$height)
summary(Tenoritem$height)
summary(Altoitem$height)
summary(Sopranoitem$height)

###############################################################
#> summary(Sopranoitem$height)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#60.00   62.00   65.00   64.12   65.00   70.00 
#> summary(Bassitem$height)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#66.00   69.00   71.00   70.98   72.00   75.00 
#> summary(Tenoritem$height)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#64.0    68.0    69.0    69.4    71.0    76.0 
#> summary(Altoitem$height)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#60.00   64.00   65.00   65.39   67.00   72.00 
#> summary(Sopranoitem$height)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#60.00   62.00   65.00   64.12   65.00   70.00 

# sample sizes of Bass and Tenor
nb = length(Bassitem$height)
#65
nt = length(Tenoritem$height)
#42

# sample mean of Bass and Tenor
smeanb =mean(Bassitem$height)
#70.984
smeant =mean(Tenoritem$height)
#69.40476

#sample varience.
svarb = var(Bassitem$height)
#6.327885
svart = var(Tenoritem$height)
#7.759001

#pooled sd 
se=sqrt(svarb/(nb) +svart/(nt))

# Z-test.
zstat = (smeanb - smeant)/se
#2.974561

#pval right tailed.
pval = 1- pnorm(zstat)
#0.001467043

#Since Pval is less than 0.05 we reject the Null hypothesis.

