#####################################################################################
# Making a map using R #
# Making a map esesntially involves three steps --- getting a shape file for map, 
#		getting the data, and plotting. As an example, we will make a map of
#		state level income share of the top 1% of income earners in 2012 in 
#		the US (see http://www.shsu.edu/eco_mwf/inequality.html for more
#		more information about the variable and the data.)
#		But the ideas are quite general. 
# Much of what you would see below has been learnt from:
# (a) http://www.kevjohnson.org/making-maps-in-r/ for a general introduction to map
#		making in R
# (b) https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf 
#		for an introduction to colors in R
# See also:
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
#		for an introduction to using Google maps in making maps using 
#		using the R package ggmap
#####################################################################################

# Use ?functionname to see help on any unfamiliar functions.
# If not already installed, use install.packages("packagename") to first install 
# a package before loading it with library command.


library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)

# Get a shape file of states in the US

usa.df <- map_data("state")
colnames(usa.df)[5] <- "State"

# Get the data to be plotted

usa.dat <- read.table("us_2016_election_data1.csv", header = T, sep = ",")
usa.dat$State <- tolower(usa.dat$State)
usa.dat <- usa.dat[c("State","Clinton..","Trump..")]

# changing the column name.
#colnames(usa.dat)[which(names(usa.dat) == "Clinton..")]<- "clinton"
#colnames(usa.dat)[which(names(usa.dat) == "Trump..")]<- "trump"
usa.dat$margin = usa.dat$Trump-usa.dat$Clinton

#convert lowers
#usa.dat$State <- tolower(usa.dat$State)
#usa.dat$clinton <-gsub("%","",usa.dat$clinton)
#usa.dat$trump <-gsub("%","",usa.dat$trump)
#
# Merge the data with the shape file

usa1.df <- join(usa.df, usa.dat, by = "State", type = "inner")

# Abbreviations of states and where thy should be plotted

states <- data.frame(state.center, state.abb) # centers of states and abbreviations
subset <- tolower(state.name) %in% usa1.df$State # exclude Hawaii as there is no data for this state
states <- states[subset, ]

# Write a function that does the plotting (Feel free to play around 
# with the options)

p <- function(data, brks, title) {
  ggp <- ggplot() + 
    #		Draw borders of states
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill = margin), color = "black", size = 0.15) + 
    # 		Use shades of red for plotting; trans = "reverse" option 
    # 		makes the shades go from dark to light as the income share increases, 
    #		ensuring that darkest red = worst case scenario.
    #scale_fill_distiller(palette = "Reds", breaks = brks,
    #                     trans = "reverse") + 
    
    scale_fill_gradient2(midpoint = 0, low ="orangered1" ,mid="aliceblue", high="deepskyblue3",
                         break = brks.to.use , labels <- seq(-90,90,by =20))
    
    
    #		Add legend
    theme_nothing(legend = TRUE) + labs(title = title, fill = "") +
    #		Add state abbreviations		
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)
  return(ggp)
}


# Get the break points for different shades

#range(usa.dat$margin)
#[1] -86.8  46.3 

brks.to.use <- seq(-90, 90, by = 20) # give intervals:  
# 10-14 (less than 14), 14-18, ..., 30-34 (>= 30)
figure.title <- "Election Votes in US"

# Save the map to a file to viewing (you can plot on the screen also, but it takes
# much longer that way. The ratio of US height to width is 1:9.)
sample<-p(usa1.df, brks.to.use, figure.title)
sample
ggsave(sample,height = 4, width = 7.6,
       file = "usa_123.jpg")


