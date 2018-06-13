#Load the data
heartbeatdata <- read.table("bp.txt", header = T, sep = "\t")
# Get the  summary of the database
summary(heartbeatdata)
#     armsys         fingsys     
#Min.   : 79.0   Min.   : 60.0  
#1st Qu.:111.5   1st Qu.:118.0  
#Median :125.0   Median :130.0  
#Mean   :128.5   Mean   :132.8  
#3rd Qu.:140.0   3rd Qu.:146.5  
#Max.   :220.0   Max.   :228.0 
#

#standard Deviation
sd(heartbeatdata$armsys)
sd(heartbeatdata$fingsys)

#side by side box plot.
boxplot(heartbeatdata, main="Boxplot of Arm and Finger Method")
plot(heartbeatdata)


#get Histogram plot
par(mfrow=c(1,2))
hist(x = heartbeatdata$armsys, main="Arm method",xlab="armsys")
hist(x= heartbeatdata$fingsys, main="Finger method",xlab = "fingersys")
par(mfrow=c(1,1))

# get QQ plots
par(mfrow=c(1,2))
qqnorm(heartbeatdata$armsys, main="Q-Q plot for HeartBeat Arm")
qqline(heartbeatdata$armsys)

qqnorm(heartbeatdata$fingsys, main="Q-Q plot for HeartBeat Finger")
qqline(heartbeatdata$fingsys)
par(mfrow=c(1,1))
# difference of heartbeats and qq-plot
dif = (heartbeat$armsys-heartbeat$fingsys)
qqnorm(dif,main="difference in heartbeat")
qqline(dif)

#95% cofidence interval with mean and standard error
alpha = 0.05
mu = mean(dif)
se = sd(dif)/sqrt(200)
#calculate CI from mean and formula
confidenceinterval = mu +c(-1,1)*qnorm(1-alpha/2)*se
confidenceinterval
#[1] -6.316529 -2.273471

sample = rnorm(1000)
qqnorm(sample,main="norm")
qqline(sample)

sample = rexp(1000,rate=1)
qqnorm(sample,main="norm")
qqline(sample)