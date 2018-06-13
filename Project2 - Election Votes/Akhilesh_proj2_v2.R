#import the Libraries.

library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)
library(fiftystater)# library to import 50 state map data.

# Get a shape file of states in the US
usa.df =fifty_states
colnames(usa.df)[6] <- "State"
#usa.df <- map_data("state")
#colnames(usa.df)[5] <- "State"

# Get the data to be plotted

usa.dat <- read.table("us_2016_election_data.csv", header = T, sep = ",")

# Filter the Input data.
#1. Convert to lower case the state.
usa.dat$State <- tolower(usa.dat$State)
#2. Keep required columns.
usa.dat <- usa.dat[c("State","Clinton..","Trump..")]

# 3. changing the column name remove .. and Capitals.
colnames(usa.dat)[which(names(usa.dat) == "Clinton..")]<- "clinton"
colnames(usa.dat)[which(names(usa.dat) == "Trump..")]<- "trump"

#4.convert % values to numeric values.
usa.dat$clinton <-as.numeric(gsub("%","",usa.dat$clinton))
usa.dat$trump <-as.numeric(gsub("%","",usa.dat$trump))

#5.Create a Margin file with data.
usa.dat$margin = usa.dat$trump - usa.dat$clinton
usa1.df <- join(usa.df, usa.dat, by = "State", type = "inner")

# Abbreviations of states and where they should be plotted

states <- data.frame(state.center, state.abb) # centers of states and abbreviations
subset <- tolower(state.name) %in% usa1.df$State # exclude Hawaii as there is no data for this state
states <- states[subset, ]

# Write a function that does the plotting (Feel free to play around 
# with the options)

p <- function(data, brks,lbls, title) {
  ggp <- ggplot() + 
    #		Draw borders of states
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill = margin), color = "black", size = 0.15) + 
    # 		Use shades of red for plotting; trans = "reverse" option 
    # 		makes the shades go from dark to light as the income share increases, 
    #		ensuring that darkest red = worst case scenario.
    #scale_fill_distiller(palette = "Reds", breaks = brks,
    #                     trans = "reverse") + 
#    scale_fill_gradient2(midpoint = 0, low ="orangered1" ,mid="aliceblue", high="deepskyblue3",
#                         break = c(-80,-60,-40,-20,0,20,40,60,80), labels = c("Trump","0","10","20","Clinton"),
#                         limits= c(-80,80)) +

    scale_fill_gradient2(midpoint = -30, low ="#179ee0", high="#FF5D40", breaks =brks,labels= lbls)+
    
      #                      break = c(-80,-60,-40,-20,0,20,40,60,80),break = c(-80,-60,-40,-20,0,20,40,60,80), labels = c("Trump","0","10","20","Clinton"),
      #                    limits= c(-80,80)) +
  
    #		Add legend
    theme_nothing(legend = TRUE) + labs(title = title, fill = "") +
    #		Add state abbreviations		
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)
  return(ggp)
}


# Get the break points for different shades

#range(usa.dat$margin)
#[1] -86.8  46.3 


brks.to.use <- seq(-90, 90, by = 30) # give intervals:
labels.to.use <-C("H1","h2","h3","T1","T2","T3")
# (-90,-60),(-60,-30),(-30,0),(0,30),(30,60),(60,90) 
figure.title <- "Election Votes in US"

# Save the map to a file to viewing (you can plot on the screen also, but it takes
# much longer that way. The ratio of US height to width is 1:9.)
sample<-p(usa1.df, brks.to.use,labels.to.use, figure.title)
sample
ggsave(sample,height = 4, width = 7.6,
       file = "usa_Election_results.jpg")
#additional mean,sd,range.
#margin
range(usa.dat$margin)
mean(usa.dat$margin)
sd(usa.dat$margin)

#trump
range(usa.dat$trump)
mean(usa.dat$trump)
sd(usa.dat$trump)

#hillary
range(usa.dat$clinton)
mean(usa.dat$clinton)
sd(usa.dat$clinton)
