#Author Akhilesh, Vidya
#import the Libraries.

library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)
library(fiftystater)# library to import 50 state map data.

# Get a shape file of states in the US, Get including alaska

usaMap.df =fifty_states
#change the column names to State.
colnames(usaMap.df)[6] <- "State"

# the 50 states.
#usaMap.df <- map_data("state")
#colnames(usaMap.df)[5] <- "State"

# Get the data to be plotted
usaElection.dat <- read.table("us_2016_election_data.csv", header = T, sep = ",")

# Clean the Input data.
#1. Convert to lower case the state.
usaElection.dat$State <- tolower(usaElection.dat$State)
#2. Keep required columns.
usaElection.dat <- usaElection.dat[c("State","Clinton..","Trump..")]

# 3. changing the column name remove .. and Capitals.
colnames(usaElection.dat)[which(names(usaElection.dat) == "Clinton..")]<- "clinton"
colnames(usaElection.dat)[which(names(usaElection.dat) == "Trump..")]<- "trump"

#4.convert % values to numeric values.
usaElection.dat$clinton <-as.numeric(gsub("%","",usaElection.dat$clinton))
usaElection.dat$trump <-as.numeric(gsub("%","",usaElection.dat$trump))

#5.Create a Margin file with data.
usaElection.dat$margin = usaElection.dat$trump - usaElection.dat$clinton
usaMerge.df <- join(usaMap.df, usaElection.dat, by = "State", type = "inner")

# Abbreviations of states and where they should be plotted

states <- data.frame(state.center, state.abb) # centers of states and abbreviations
subset <- tolower(state.name) %in% usaMerge.df$State # incase there is no data.
states <- states[subset, ]

# Write a function that does the plotting (Feel free to play around 
# with the options)

p <- function(data, brks,lbls, title) {
  ggp <- ggplot() + 
    #		Draw borders of states
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill = margin), color = "black", size = 0.15) + 
    #Used to fill the gradient Gradient with low color and high color with 2 values.   
    scale_fill_gradient2(midpoint = 0, low ="#179ee0", high="#FF5D40", breaks =brks,guide = "colorbar"
                         ,labels= lbls)+
    
    #                      break = c(-80,-60,-40,-20,0,20,40,60,80),break = c(-80,-60,-40,-20,0,20,40,60,80), labels = c("Trump","0","10","20","Clinton"),
    #                    limits= c(-80,80)) +
    
    #		Add legend
    theme_nothing(base_size = 8, legend = TRUE) + labs(title = title, fill = "Trump(Orange) Clinton(Blue)") +
    #		Add state abbreviations		
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 1)
  return(ggp)
}


# Get the break points for different shades

#range(usa.dat$margin)
#[1] -86.8  46.3 


brks.to.use <- seq(-90, 90, by = 10) # give intervals sepearted by 30
# -90,-60,-30,0,30,60,90 

labls.to.use <-c("C (90)","C (80)","C(70)",
                 "C (60)","C (50)","C (40)",
                 "C (30)","C (20)","C (10)",
                 "C = T",
                 "T(10)","T(20)","T(30)",
                 "T(40)","T(50)","T(60)",
                 "T(70)","T(80)","T(90)")
#labls.to.use <- seq(-90, 90, by = 10) 
figure.title <- "Election Votes  in US 2016 Trump V/s Clinton Margin plot"

# Save the map to a file to viewing (you can plot on the screen also, but it takes
# much longer that way. The ratio of US height to width is 1:9.)
sample<-p(usaMerge.df, brks.to.use,labls.to.use, figure.title)
sample
ggsave(sample,height = 4, width = 8,
       file = "usa_Election_results.jpg")

#additional mean,sd,range.
#margin
range(usaMerge.df$margin)
mean(usaMerge.df$margin)
sd(usaMerge.df$margin)

#trump
range(usaMerge.df$trump)
mean(usaMerge.df$trump)
sd(usaMerge.df$trump)

#hillary
range(usaMerge.df$clinton)
mean(usaMerge.df$clinton)
sd(usaMerge.df$clinton)

boxplot(usaMerge.df$clinton,usaMerge.df$trump,usaMerge.df$margin)
boxplot(usaMerge.df$trump,usaMerge.df$clinton,usaMerge.df$margin)
boxplot(usaMerge.df$trump,usaMerge.df$clinton,usaMerge.df$margin)
# 
IQR(usaMerge.df$trump)
#[1] 16.25
IQR(usaMerge.df$clinton)
#[1] 15.05
IQR(usaMerge.df$margin)
#[1] 31.35
