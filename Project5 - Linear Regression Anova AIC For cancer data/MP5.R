# Get the Cancer data.

cancerdata <- read.table("prostate_cancer.csv", header = T, sep = ",")
str(cancerdata)

########################################################
#'data.frame':	97 obs. of  9 variables:
#  $ subject  : int  1 2 3 4 5 6 7 8 9 10 ...
#(T)$ psa      : num  0.651 0.852 0.852 0.852 1.448 ...
#(Quantative)$ cancervol: num  0.56 0.372 0.601 0.301 2.117 ...
#(Quantative)$ weight   : num  16 27.7 14.7 26.6 30.9 ...
#(Quantative)$ age      : int  50 58 74 58 62 50 64 58 47 63 ...
#(Quantative)$ benpros  : num  0 0 0 0 0 ...
#(Qualatative)$ vesinv   : int  0 0 0 0 0 0 0 0 0 0 ...
#(Quantative)$ capspen  : num  0 0 0 0 0 0 0 0 0 0 ...
#(Qualatative)$ gleason  : int  6 7 7 6 6 6 6 6 7 6 ...

###############################
#columns: subject   psa cancervol weight age benpros vesinv capspen gleason
#####################################

###################################
#Table values. - get used to the values.
#table(cancerdata$psa)
#table(cancerdata$weight)
#table(cancerdata$benpros)
#table(cancerdata$capspen)
#table(cancerdata$gleason)
#6  7  8 
#33 43 21 
#table(cancerdata$vesinv)
#0  1 
#76 21
###########################
# target  - psa
# quantative - cancervol weight age benpros capspen
# qualatative - vesinv gleason
# Subject -> key.

# Boxplot of PSA.

par(mfrow = c(2, 3))
boxplot(cancerdata$psa,	 ylab="PSA")
boxplot((logb(1+cancerdata$psa,2)),ylab="log 2 (1+PSA)")
boxplot(cancerdata$capspen, ylab="capspen")
boxplot(logb(1+cancerdata$capspen,2), ylab="log2 (1+capspen)")
boxplot(cancerdata$cancervol, ylab="cancervol")
boxplot(logb(1+cancerdata$cancervol,2), ylab=" log2 (1 + cancervol)")
par(mfrow = c(1,1))

# Log [ 1 + ( PSA ) ] (base 2)  has less outliers compared to direct data.
# log cancervol has less outlier.
# log capspen has less outlier
#benpros
boxplot(cancerdata$benpros, ylab="benpros")


# ScatterPlot:
par(mfrow = c(2, 4))
plot(cancerdata$cancervol,cancerdata$psa,xlab ="Cancer volume (cc)", ylab = "PSA Level(mg/ml)")
plot(cancerdata$weight,cancerdata$psa,xlab ="Weight(gm)", ylab = "PSA Level(mg/ml)")
plot(cancerdata$age,cancerdata$psa,xlab ="age (years)", ylab = "PSA Level(mg/ml)")
plot(cancerdata$benpros,cancerdata$psa,xlab ="Benign prostatic hyperplasia (cm2)", ylab = "PSA Level(mg/ml)")
plot(cancerdata$vesinv,cancerdata$psa,xlab ="Seminal vesicle invasion (1 or 0)", ylab = "PSA Level(mg/ml)")
plot(cancerdata$capspen,cancerdata$psa,xlab ="Capsular penetration", ylab = "PSA Level(mg/ml)")
plot(cancerdata$gleason,cancerdata$psa,xlab ="Gleason score", ylab = "PSA Level(mg/ml)")
par(mfrow = c(1, 1))

#scaled scatterplot
par(mfrow = c(2, 4))
plot(cancerdata$cancervol,cancerdata$psa,xlab ="Cancer volume (cc)", ylab = " PSA Level(mg/ml)")
plot(logb(1+cancerdata$cancervol,2),cancerdata$psa,xlab ="log Cancer volume (cc)", ylab = " PSA Level(mg/ml)")
plot(cancerdata$capspen,cancerdata$psa,xlab ="Capsular penetration", ylab = "PSA Level(mg/ml)")
plot(logb(1+cancerdata$capspen,2),cancerdata$psa,xlab ="log Capsular penetration", ylab = "PSA Level(mg/ml)")
plot(cancerdata$cancervol,(logb(1+cancerdata$psa,2)),xlab ="Cancer volume (cc)", ylab = "log PSA Level(mg/ml)")
plot(logb(1+cancerdata$cancervol,2),(logb(1+cancerdata$psa,2)),xlab ="log Cancer volume (cc)", ylab = "log PSA Level(mg/ml)")
plot(cancerdata$capspen,(logb(1+cancerdata$psa,2)),xlab ="Capsular penetration", ylab = "log PSA Level(mg/ml)")
plot(logb(1+cancerdata$capspen,2),(logb(1+cancerdata$psa,2)),xlab ="log Capsular penetration", ylab = " log PSA Level(mg/ml)")
par(mfrow = c(1, 1))



### getting a feel for data for other variables.
# weight has one outlier at 400.
# age seems to have no effect on PSA levels.but hugely clustered at 50 to 60.
# Benign hyperplasia has high effect on mg level.
# SV high value has few more datapoints .
# Capspen seems to have important relvance.

#lets try correlation of the data
cor(cancerdata)
#######################################################################################################################
#            subject         psa    cancervol       weight        age     benpros       vesinv      capspen     gleason
#subject   1.0000000  0.60268375  0.620997842  0.113741022 0.19655569  0.16500536  0.566780347  0.476752459  0.53792405
#psa       0.6026837  1.00000000  0.624150588  0.026213430 0.01719938 -0.01648649  0.528618785  0.550792517  0.42957975
#cancervol 0.6209978  0.62415059  1.000000000  0.005107148 0.03909442 -0.13320943  0.581741687  0.692896688  0.48143840
#weight    0.1137410  0.02621343  0.005107148  1.000000000 0.16432371  0.32184875 -0.002410475  0.001578905 -0.02420693
#age       0.1965557  0.01719938  0.039094423  0.164323714 1.00000000  0.36634121  0.117658038  0.099555351  0.22585181
#benpros   0.1650054 -0.01648649 -0.133209431  0.321848748 0.36634121  1.00000000 -0.119553192 -0.083008649  0.02682555
#vesinv    0.5667803  0.52861878  0.581741687 -0.002410475 0.11765804 -0.11955319  1.000000000  0.680284092  0.42857348
#capspen   0.4767525  0.55079252  0.692896688  0.001578905 0.09955535 -0.08300865  0.680284092  1.000000000  0.46156590
#gleason   0.5379241  0.42957975  0.481438397 -0.024206925 0.22585181  0.02682555  0.428573479  0.461565896  1.00000000

#important is this factor.
#            subject         psa    cancervol       weight        age     benpros       vesinv      capspen     gleason
#psa       0.6026837  1.00000000  0.624150588  0.026213430 0.01719938 -0.01648649  0.528618785  0.550792517  0.42957975

# high correlation is cancervol and capsen vesubv and gleason.
# lower corelation of weight and age might indicate they are not good factors.(Let's test)


# Fitting Cancervol with psa
plot(logb(1+cancerdata$cancervol,2),(logb(1+cancerdata$psa,2)),xlab ="log Cancer volume (cc)", ylab = "log PSA Level(mg/ml)")
fit1_cv <- lm((logb(1+psa,2)~logb(1+cancervol,2)),data=cancerdata)
summary(fit1_cv)
abline(fit1_cv)

# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + cancervol, 2)), data = cancerdata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.18380 -0.52724  0.01907  0.55417  2.60699 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.67328    0.22437   7.458 4.11e-11 ***
#   logb(1 + cancervol, 2)  0.85780    0.08172  10.497  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.004 on 95 degrees of freedom
# Multiple R-squared:  0.537,	Adjusted R-squared:  0.5322 
# F-statistic: 110.2 on 1 and 95 DF,  p-value: < 2.2e-16

# The Graph shows a trend and also the T-test indicates the parameter is significant.

# Fitting capspen with psa
plot(logb(1+cancerdata$capspen,2),(logb(1+cancerdata$psa,2)),xlab ="log Capsular penetration", ylab = " log PSA Level(mg/ml)")
fit2_cv <- lm((logb(1+psa,2)~logb(1+capspen,2)),data=cancerdata)
summary(fit2_cv)
abline(fit2_cv)
# 
# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + capspen, 2)), data = cancerdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3791 -0.8224 -0.0732  0.9093  3.1807 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           3.10244    0.16101  19.269  < 2e-16 ***
#   logb(1 + capspen, 2)  0.64489    0.09855   6.544 3.02e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.225 on 95 degrees of freedom
# Multiple R-squared:  0.3107,	Adjusted R-squared:  0.3035 
# F-statistic: 42.82 on 1 and 95 DF,  p-value: 3.021e-09

# The Graph shows a trend and also the T-test indicates the parameter is significant.

# fitting with weight.

plot(cancerdata$weight,(logb(1+cancerdata$psa,2)),xlab ="weight", ylab = " log PSA Level(mg/ml)")
fit3_weight <- lm((logb(1+cancerdata$psa,2)~cancerdata$weight),data=cancerdata)
summary(fit3_weight)
abline(fit3_weight)

# Call:
#   lm(formula = (logb(1 + cancerdata$psa, 2) ~ cancerdata$weight), 
#      data = cancerdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.9426 -1.0177  0.1052  0.7435  4.2575 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       3.608909   0.210626  17.134   <2e-16 ***
#   cancerdata$weight 0.003573   0.003275   1.091    0.278    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.466 on 95 degrees of freedom
# Multiple R-squared:  0.01237,	Adjusted R-squared:  0.001978 
# F-statistic:  1.19 on 1 and 95 DF,  p-value: 0.278
# The Graph shows a trend and also the T-test indicates the parameter is significant.

# fitting with age       
plot(cancerdata$age,(logb(1+cancerdata$psa,2)),xlab ="age", ylab = " log PSA Level(mg/ml)")
fit4_age <- lm((logb(1+cancerdata$psa,2)~cancerdata$age),data=cancerdata)
summary(fit4_age)
abline(fit4_age)
# 
# Call:
#   lm(formula = (logb(1 + cancerdata$psa, 2) ~ cancerdata$age), 
#      data = cancerdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1946 -0.9997  0.0363  0.8015  4.1569 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)     1.80365    1.28461   1.404    0.164
# cancerdata$age  0.03081    0.01998   1.542    0.126
# 
# Residual standard error: 1.458 on 95 degrees of freedom
# Multiple R-squared:  0.02442,	Adjusted R-squared:  0.01415 
# F-statistic: 2.378 on 1 and 95 DF,  p-value: 0.1264

# The Graph doesnot shows a trend and also the T-test indicates the parameter is insignificant.

#fitting with benpros

plot(cancerdata$benpros,(logb(1+cancerdata$psa,2)),xlab ="benpros", ylab = " log PSA Level(mg/ml)")
fit5_benpros <- lm((logb(1+cancerdata$psa,2)~cancerdata$benpros),data=cancerdata)
summary(fit5_benpros)
abline(fit5_benpros)

# Call:
#   lm(formula = (logb(1 + cancerdata$psa, 2) ~ cancerdata$benpros), 
#      data = cancerdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.8803 -1.0000 -0.1049  0.7833  4.3493 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.60360    0.19389  18.586   <2e-16 ***
#   cancerdata$benpros  0.06621    0.04922   1.345    0.182    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.462 on 95 degrees of freedom
# Multiple R-squared:  0.01869,	Adjusted R-squared:  0.008365 
# F-statistic:  1.81 on 1 and 95 DF,  p-value: 0.1817

# The Graph doesnot shows a trend and also the T-test indicates the parameter is insignificant.

#Quantative data.

#vesinv    

plot(cancerdata$vesinv,(logb(1+cancerdata$psa,2)),xlab ="vesinv", ylab = " log PSA Level(mg/ml)")
fit6_vesinv <- lm((logb(1+cancerdata$psa,2)~factor(cancerdata$vesinv)),data=cancerdata)
summary(fit6_vesinv)
abline(fit6_vesinv)

# Call:
#   lm(formula = (logb(1 + cancerdata$psa, 2) ~ factor(cancerdata$vesinv)), 
#      data = cancerdata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.59594 -0.87646 -0.04561  0.81905  2.64786 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  3.3193     0.1368  24.266  < 2e-16 ***
#   factor(cancerdata$vesinv)1   2.0885     0.2940   7.104 2.21e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.192 on 95 degrees of freedom
# Multiple R-squared:  0.3469,	Adjusted R-squared:  0.3401 
# F-statistic: 50.47 on 1 and 95 DF,  p-value: 2.207e-10

#from t test this factor seems very important.  

#gleason
plot(cancerdata$gleason,(logb(1+cancerdata$psa,2)),xlab ="gleason", ylab = " log PSA Level(mg/ml)")
fit6_gleason <- lm((logb(1+cancerdata$psa,2)~factor(cancerdata$gleason)),data=cancerdata)
summary(fit6_gleason)
abline(fit6_gleason)

# Call:
#   lm(formula = (logb(1 + cancerdata$psa, 2) ~ factor(cancerdata$gleason)), 
#      data = cancerdata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.73989 -0.80546 -0.06498  0.72801  2.84467 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   2.9948     0.2110  14.195  < 2e-16 ***
#   factor(cancerdata$gleason)7   0.6342     0.2805   2.261   0.0261 *  
#   factor(cancerdata$gleason)8   2.2887     0.3383   6.765 1.12e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.212 on 94 degrees of freedom
# Multiple R-squared:  0.3325,	Adjusted R-squared:  0.3183 
# F-statistic: 23.42 on 2 and 94 DF,  p-value: 5.594e-09

# from t -test this seems like a very important factor.

# Multi-Linear Regression Lets fit using the Important factors first.
# Cancervol,capspen
fitml1_cv <- lm(( logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 )),data=cancerdata)
fitml2_cvcap <-lm(( logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 )+logb(1+capspen,2)),data=cancerdata)
summary(fitml2_cvcap)
anova(fitml2_cvcap,fitml1_cv)

# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + logb(1 + 
#                                                                    capspen, 2)), data = cancerdata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.14123 -0.59871  0.04475  0.54434  2.58724 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.73921    0.23889   7.280 1.00e-10 ***
#   logb(1 + cancervol, 2)  0.79138    0.11551   6.851 7.53e-10 ***
#   logb(1 + capspen, 2)    0.09304    0.11417   0.815    0.417    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.006 on 94 degrees of freedom
# Multiple R-squared:  0.5403,	Adjusted R-squared:  0.5305 
# F-statistic: 55.24 on 2 and 94 DF,  p-value: < 2.2e-16
# 
# > anova(fitml2_cvcap,fitml1_cv)
# Analysis of Variance Table
# 
# Model 1: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + logb(1 + capspen, 
#                                                           2)
# Model 2: logb(1 + psa, 2) ~ logb(1 + cancervol, 2)
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     94 95.099                           
# 2     95 95.771 -1  -0.67185 0.6641 0.4172

#capsen anova seems to have higher value.

#Lets add vesinv to fitml_cv
fitml3_cvves <- lm(( logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + factor(vesinv) ),data=cancerdata)
summary(fitml3_cvves)
anova(fitml3_cvves,fitml1_cv)

# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv)), 
#      data = cancerdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1546 -0.6887  0.1354  0.6085  2.2130 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.89338    0.22682   8.347 5.93e-13 ***
#   logb(1 + cancervol, 2)  0.68926    0.09577   7.197 1.49e-10 ***
#   factor(vesinv)1         0.88748    0.29012   3.059  0.00289 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9626 on 94 degrees of freedom
# Multiple R-squared:  0.5789,	Adjusted R-squared:   0.57 
# F-statistic: 64.62 on 2 and 94 DF,  p-value: < 2.2e-16
# 
# > anova(fitml3_cvves,fitml1_cv)
# Analysis of Variance Table
# 
# Model 1: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv)
# Model 2: logb(1 + psa, 2) ~ logb(1 + cancervol, 2)
# Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
# 1     94 87.100                                
# 2     95 95.771 -1   -8.6708 9.3577 0.002893 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#Since the significance is high valuer, the factor is important. fitml3_cvves is imp

#lets add gleason to this fitml3_cvves
fitml4_cvvesgle <- lm(( logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + factor(vesinv) + factor(gleason) ),data=cancerdata)
summary(fitml4_cvvesgle)
anova(fitml4_cvvesgle,fitml3_cvves)

# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv)), 
#      data = cancerdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1546 -0.6887  0.1354  0.6085  2.2130 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.89338    0.22682   8.347 5.93e-13 ***
#   logb(1 + cancervol, 2)  0.68926    0.09577   7.197 1.49e-10 ***
#   factor(vesinv)1         0.88748    0.29012   3.059  0.00289 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9626 on 94 degrees of freedom
# Multiple R-squared:  0.5789,	Adjusted R-squared:   0.57 
# F-statistic: 64.62 on 2 and 94 DF,  p-value: < 2.2e-16
# 
# > anova(fitml3_cvves,fitml1_cv)
# Analysis of Variance Table
# 
# Model 1: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv)
# Model 2: logb(1 + psa, 2) ~ logb(1 + cancervol, 2)
# Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
# 1     94 87.100                                
# 2     95 95.771 -1   -8.6708 9.3577 0.002893 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# indicates gleason with value 7 may not be signifcant, but other value 8 is significant.
# The F-test is rejected, so the factor is important.

# lets try other factors found to be not so important linear regression if they are important.
# weight,age,benpros together.

fitml5_cvvesgle_other <- lm(( logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + factor(vesinv) + factor(gleason)+ weight + age + benpros ),
                            data=cancerdata)
summary(fitml5_cvvesgle_other)
anova(fitml4_cvvesgle,fitml5_cvvesgle_other)
# 
# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#                   factor(gleason) + weight + age + benpros), data = cancerdata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.26631 -0.58056  0.00112  0.67042  2.27645 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2.694171   0.841237   3.203  0.00189 ** 
#   logb(1 + cancervol, 2)  0.574773   0.099009   5.805 9.75e-08 ***
#   factor(vesinv)1         0.871904   0.279923   3.115  0.00248 ** 
#   factor(gleason)7        0.191795   0.220953   0.868  0.38771    
# factor(gleason)8        0.860393   0.307856   2.795  0.00636 ** 
#   weight                  0.002102   0.002160   0.973  0.33318    
# age                    -0.017525   0.013776  -1.272  0.20662    
# benpros                 0.092710   0.034982   2.650  0.00952 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9011 on 89 degrees of freedom
# Multiple R-squared:  0.6506,	Adjusted R-squared:  0.6232 
# F-statistic: 23.68 on 7 and 89 DF,  p-value: < 2.2e-16
# 
# > anova(fitml4_cvvesgle,fitml5_cvvesgle_other)
# Analysis of Variance Table
# 
# Model 1: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason)
# Model 2: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason) + weight + age + benpros
# Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     92 80.803                              
# 2     89 72.269  3    8.5338 3.5031 0.01866 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# T-test : 0.01866 indicates the model can be improved.

fitml5_cvvesgle_other_noweight <-lm(( logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + factor(vesinv) + factor(gleason) + age + benpros )
                                    ,data=cancerdata)
summary(fitml5_cvvesgle_other_noweight)
anova(fitml5_cvvesgle_other,fitml5_cvvesgle_other_noweight)
# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#                   factor(gleason) + age + benpros), data = cancerdata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.31672 -0.53020  0.00633  0.64135  2.35584 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2.73139    0.84012   3.251  0.00162 ** 
#   logb(1 + cancervol, 2)  0.57061    0.09889   5.770 1.11e-07 ***
#   factor(vesinv)1         0.88790    0.27936   3.178  0.00203 ** 
#   factor(gleason)7        0.15922    0.21834   0.729  0.46776    
# factor(gleason)8        0.85356    0.30768   2.774  0.00673 ** 
#   age                    -0.01667    0.01374  -1.213  0.22842    
# benpros                 0.10304    0.03332   3.092  0.00264 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9009 on 90 degrees of freedom
# Multiple R-squared:  0.6469,	Adjusted R-squared:  0.6234 
# F-statistic: 27.48 on 6 and 90 DF,  p-value: < 2.2e-16
# 
# > anova(fitml5_cvvesgle_other,fitml5_cvvesgle_other_noweight)
# Analysis of Variance Table
# 
# Model 1: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason) + weight + age + benpros
# Model 2: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason) + age + benpros
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     89 72.269                           
# 2     90 73.038 -1   -0.7688 0.9468 0.3332

# T-test: 0.3332 indicates weight not needed.

fitml5_cvvesgle_other_noweightage <-lm(( logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + factor(vesinv) + factor(gleason) + benpros ),
                                       data=cancerdata)
summary(fitml5_cvvesgle_other_noweightage)
anova(fitml5_cvvesgle_other_noweight,fitml5_cvvesgle_other_noweightage)
# 
# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#                   factor(gleason) + benpros), data = cancerdata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.45942 -0.66670  0.03114  0.63097  2.18839 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.75598    0.24323   7.219 1.53e-10 ***
#   logb(1 + cancervol, 2)  0.56058    0.09880   5.674 1.64e-07 ***
#   factor(vesinv)1         0.87487    0.27987   3.126  0.00238 ** 
#   factor(gleason)7        0.12864    0.21744   0.592  0.55556    
# factor(gleason)8        0.80656    0.30602   2.636  0.00987 ** 
#   benpros                 0.08807    0.03103   2.838  0.00559 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9032 on 91 degrees of freedom
# Multiple R-squared:  0.6412,	Adjusted R-squared:  0.6214 
# F-statistic: 32.52 on 5 and 91 DF,  p-value: < 2.2e-16
# 
# > anova(fitml5_cvvesgle_other_noweight,fitml5_cvvesgle_other_noweightage)
# Analysis of Variance Table
# 
# Model 1: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason) + age + benpros
# Model 2: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason) + benpros
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     90 73.038                           
# 2     91 74.232 -1   -1.1935 1.4706 0.2284


# T-test: 0.2284 indicates age not needed.

# fitml4_cvvesgle is without benpros, compare it with current model.
anova(fitml5_cvvesgle_other_noweightage,fitml4_cvvesgle)

# Analysis of Variance Table
# 
# Model 1: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason) + benpros
# Model 2: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason)
# Res.Df    RSS Df Sum of Sq     F   Pr(>F)   
# 1     91 74.232                               
# 2     92 80.803 -1   -6.5715 8.056 0.005593 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# T-Test: 0.005593, indicates benpros is needed in the model.
# so the final model is fitml5_cvvesgle_other_noweightage

fitml5_Cv_ves_gle_benpros <-fitml5_cvvesgle_other_noweightage
summary(fitml5_Cv_ves_gle_benpros)
# Call:
#   lm(formula = (logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#                   factor(gleason) + benpros), data = cancerdata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.45942 -0.66670  0.03114  0.63097  2.18839 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.75598    0.24323   7.219 1.53e-10 ***
#   logb(1 + cancervol, 2)  0.56058    0.09880   5.674 1.64e-07 ***
#   factor(vesinv)1         0.87487    0.27987   3.126  0.00238 ** 
#   factor(gleason)7        0.12864    0.21744   0.592  0.55556    
# factor(gleason)8        0.80656    0.30602   2.636  0.00987 ** 
#   benpros                 0.08807    0.03103   2.838  0.00559 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9032 on 91 degrees of freedom
# Multiple R-squared:  0.6412,	Adjusted R-squared:  0.6214 
# F-statistic: 32.52 on 5 and 91 DF,  p-value: < 2.2e-16

#
#

fitall <-lm(logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + weight + age +
      benpros + factor(vesinv) + log( 1 + capspen, 2) + factor(gleason)  , data = cancerdata)
anova(fitml5_Cv_ves_gle_benpros,fitall)
# #Analysis of Variance Table
# 
# Model 1: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + factor(vesinv) + 
#   factor(gleason) + benpros
# Model 2: logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + weight + age + benpros + 
#   factor(vesinv) + log(1 + capspen, 2) + factor(gleason)
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     91 74.232                           
# 2     88 70.887  3     3.345 1.3842 0.2529

# Since the P-value is high, when we fit all parameters, indicates our model is good enough to fit the variable.

#checking the fitness
#all plots
par(mfrow=c(2,2))
plot(fitml5_Cv_ves_gle_benpros)
par(mfrow=c(1,1))
#residual plot
plot(fitted(fitml5_Cv_ves_gle_benpros),resid(fitml5_Cv_ves_gle_benpros),ylab = "fittedvalue", xlab ="fitted model" )
abline(h=0)

#qqplot
qqnorm(resid(fitml5_Cv_ves_gle_benpros))
qqline(resid(fitml5_Cv_ves_gle_benpros))
#time series plot
plot(resid(fitml5_Cv_ves_gle_benpros),type="l")
# shows the minimum and max are increasing.sn

# AIC PLOT
#columns: subject   psa cancervol weight age benpros vesinv capspen gleason
fit11.backward <- step(lm(logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + weight + age +
                        benpros + factor(vesinv) + logb(1 + capspen, 2 ) + factor(gleason)  , data = cancerdata), 
                       scope = list(lower = ~1), direction = "backward")

####################################################################################
# Start:  AIC=-12.42
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + weight + age + benpros + 
#   factor(vesinv) + logb(1 + capspen, 2) + factor(gleason)
# 
# Df Sum of Sq    RSS      AIC
# - weight                  1    0.7532 71.640 -13.3967
# - logb(1 + capspen, 2)    1    1.3827 72.269 -12.5482
# - age                     1    1.4572 72.344 -12.4483
# <none>                                70.887 -12.4220
# - benpros                 1    5.6231 76.510  -7.0173
# - factor(gleason)         2    7.6650 78.552  -6.4626
# - factor(vesinv)          1    9.2127 80.099  -2.5700
# - logb(1 + cancervol, 2)  1   27.1180 98.005  16.9994
# 
# Step:  AIC=-13.4
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + age + benpros + factor(vesinv) + 
#   logb(1 + capspen, 2) + factor(gleason)
# 
# Df Sum of Sq    RSS      AIC
# - age                     1    1.3324 72.972 -13.6093
# - logb(1 + capspen, 2)    1    1.3983 73.038 -13.5217
# <none>                                71.640 -13.3967
# - factor(gleason)         2    7.7402 79.380  -7.4450
# - benpros                 1    7.6454 79.285  -5.5609
# - factor(vesinv)          1    9.5388 81.179  -3.2717
# - logb(1 + cancervol, 2)  1   26.8328 98.473  15.4617
# 
# Step:  AIC=-13.61
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + benpros + factor(vesinv) + 
#   logb(1 + capspen, 2) + factor(gleason)
# 
# Df Sum of Sq    RSS      AIC
# - logb(1 + capspen, 2)    1    1.2594 74.232 -13.9495
# <none>                                72.972 -13.6093
# - factor(gleason)         2    7.1159 80.088  -8.5835
# - benpros                 1    6.3338 79.306  -7.5354
# - factor(vesinv)          1    9.1575 82.130  -4.1418
# - logb(1 + cancervol, 2)  1   25.9184 98.891  13.8725
# 
# Step:  AIC=-13.95
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + benpros + factor(vesinv) + 
#   factor(gleason)
# 
# Df Sum of Sq     RSS      AIC
# <none>                                 74.232 -13.9495
# - factor(gleason)         2    6.2702  80.502 -10.0838
# - benpros                 1    6.5715  80.803  -7.7214
# - factor(vesinv)          1    7.9710  82.203  -6.0558
# - logb(1 + cancervol, 2)  1   26.2633 100.495  13.4334



fit12.both <- step(lm(logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + weight + age +
                        benpros + factor(vesinv) + capspen + factor(gleason)  , data = cancerdata), 
                   scope = list(lower = ~1), direction = "both")

# Start:  AIC=-11.64
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + weight + age + benpros + 
#   factor(vesinv) + capspen + factor(gleason)
# 
# Df Sum of Sq    RSS      AIC
# - weight                  1    0.7968 72.260 -12.5606
# - capspen                 1    0.8061 72.269 -12.5482
# - age                     1    1.4190 72.882 -11.7289
# <none>                                71.463 -11.6361
# - factor(gleason)         2    7.2019 78.665  -6.3224
# - benpros                 1    5.6357 77.099  -6.2732
# - factor(vesinv)          1    8.4237 79.887  -2.8275
# - logb(1 + cancervol, 2)  1   26.7083 98.172  17.1646
# 
# Step:  AIC=-12.56
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + age + benpros + factor(vesinv) + 
#   capspen + factor(gleason)
# 
# Df Sum of Sq    RSS      AIC
# - capspen                 1    0.7781 73.038 -13.5217
# - age                     1    1.2898 73.550 -12.8445
# <none>                                72.260 -12.5606
# - factor(gleason)         2    7.2878 79.548  -7.2401
# - benpros                 1    7.7078 79.968  -4.7293
# - factor(vesinv)          1    8.6684 80.928  -3.5711
# - logb(1 + cancervol, 2)  1   26.3363 98.596  15.5833
# 
# Step:  AIC=-13.52
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + age + benpros + factor(vesinv) + 
#   factor(gleason)
# 
# Df Sum of Sq     RSS      AIC
# - age                     1    1.1935  74.232 -13.9495
# <none>                                 73.038 -13.5217
# - factor(gleason)         2    6.7766  79.815  -8.9152
# - benpros                 1    7.7608  80.799  -5.7265
# - factor(vesinv)          1    8.1982  81.236  -5.2028
# - logb(1 + cancervol, 2)  1   27.0210 100.059  15.0119
# 
# Step:  AIC=-13.95
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + benpros + factor(vesinv) + 
#   factor(gleason)
# 
# Df Sum of Sq     RSS      AIC
# <none>                                 74.232 -13.9495
# - factor(gleason)         2    6.2702  80.502 -10.0838
# - benpros                 1    6.5715  80.803  -7.7214
# - factor(vesinv)          1    7.9710  82.203  -6.0558
# - logb(1 + cancervol, 2)  1   26.2633 100.495  13.4334




fit12.forward <-step(lm(logb ( 1 + psa, 2 ) ~ logb ( 1 + cancervol, 2 ) + weight + age +
          benpros + factor(vesinv) + logb( 1 + capspen, 2) + factor(gleason)  , data = cancerdata), 
     scope = list(lower = ~1), direction = "forward")


# Start:  AIC=-12.42
# logb(1 + psa, 2) ~ logb(1 + cancervol, 2) + weight + age + benpros + 
#   factor(vesinv) + logb(1 + capspen, 2) + factor(gleason)


#Use the final model to predict the PSA level for a patient whose
#predictors are at the sample means of the variables.

fitml5_Cv_ves_gle_benpros <-fitml5_cvvesgle_other_noweightage
summary(fitml5_Cv_ves_gle_benpros)

#get the mean
cancervolmean = mean(cancerdata$cancervol)
benprosmean =mean(cancerdata$benpros)
vesinvmean = 0 #max categorical factor occurence.
gleasonmean = 7  # max categorical factor occurence
weightmean = mean(cancerdata$weight)
agemean = mean(cancerdata$age)
capsenmean = mean(cancerdata$capspen)
newdata = data.frame(cancervol=cancervolmean,vesinv=vesinvmean,gleason=gleasonmean,benpros=benprosmean,weight=weightmean
                     ,age=agemean,capspen=capsenmean)
newdata
#cancervol vesinv gleason  benpros
#1  6.998682      0       7 2.534725

y = predict(fitml5_Cv_ves_gle_benpros,newdata)

#1 
#3.789468

# y = logb(1+psa,2) =3.789468
# so psa = 2^y-1 /log(2)

psa = (2^(y)-1)
#> psa
#1 
#12.8275

psa = (2^(y)-1)/log(2)
#18.50617

#cancerdata
#     subject     psa     cancervol  weight age benpros vesinv capspen gleason
#43      43  10.278       1.7860     47.942  62  5.5290      0  0.6505       6
#44      44  10.697       5.8709     49.402  61  0.0000      0  2.2479       7
#45      45  12.429       4.4371     30.265  66  5.7546      0  0.6505       7
#46      46  12.807       5.2593     29.666  61  1.8589      0  0.0000       7
#47      47  13.066       15.3329    54.598  79  6.5535      1 14.2963       8
#48      48  13.066       3.1899     56.826  68  5.5290      0  0.6505       7


#Test
plot(fitml5_Cv_ves_gle_benpros)

