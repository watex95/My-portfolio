
### Data Preparation
########################################################################
setwd("D:/KAZI/FREELANCER_COM_WORK/Data Science project/old exercises/Visualization GGPlot")

# install these packages if not yet installed
install.packages("ggplot2")
install.packages("reshape2")
install.packages("ggmap")

library(ggplot2)
library(reshape)
library(ggmap)

# read in data and transform factors for nicer plots
bike = read.csv("CapitalBikeShare.csv")
str(bike)
bike$season=factor(bike$season, levels=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))
bike$weekday=factor(bike$weekday, levels=c(0,1,2,3,4,5,6), labels=c("Sunday", "Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday"))
bike$weathersit=factor(bike$weathersit, levels=c(1,2,3), labels=c("Sunny", "Cloudy", "Rainy"))
bike$mnth=as.factor(bike$mnth)

options(scipen=999) # turns off scientific notation

### Basic GGplot 
########################################################################

library(ggplot2)

### Distributions
################

# create base layer with aesthetic mapping
plot = ggplot(bike, aes(x=cnt)) # with ggplot plots can stored!

plot + geom_histogram()
plot + geom_density()

### Scatterplots
################

# create base layer with aesthetic mapping
plot = ggplot(bike, aes(x = atemp, y =  cnt))   
plot

# scatterplot
plot + geom_point()

# lineplot
plot + geom_line() 

# boxplot
plot + geom_boxplot()

# Making scatterplot a bit nicer
plot = plot +   
  ggtitle("Temperatur vs. Bike Rentals") +
  labs(x="Temperatur", y="Total Bike Rentals") 
plot

plot + geom_point(color = "green4", size = 2)

# Pimping scatterplot - playing around with geom_object
plot + geom_point(color = "green4", size = 2, shape = 16, alpha=0.4)

#saving file
ggsave("ScatterplotWithTrend.png", width=12, height=8, units = "cm") #as png
ggsave("ScatterplotWithTrend.pdf", width=12, height=8, units = "cm") #as pdf


### More complex scatterplots
################

# Color the points by season (factor)
ggplot(bike, aes(x = atemp, y = cnt, color = season)) + 
  geom_point()

# Color the points by windspeed (numeric)
ggplot(bike, aes(x = atemp, y = cnt, color = windspeed)) + 
  geom_point() + 
  scale_colour_gradient(high = "darkblue", low = "lightblue")

# Create Bubble Plot
plot = ggplot(bike, aes(x = atemp, y = cnt, color=season, size=((casual/cnt)*100)))+ 
  geom_point(shape = 16, alpha=0.4)+
  ggtitle("Temperatur vs. Bike Rentals") +
  labs(x="Temperatur", y="Total Bike Rentals",size="Casual\nBikers (%)", color="Season") 
plot

# Add regression line 
plot + 
  stat_smooth(method = "lm", color="green4")

# add smoothed trend line
plot + 
  stat_smooth(method = "lm", se = FALSE, color="green4", size=1.5)+
  stat_smooth(method = "loess", se = FALSE, color="orange", size=1.5)


### Barplots 
################

# base plot
plot = ggplot(bike, aes(x = season, y =  cnt))+ 
  ggtitle("Bike Rentals per Season") +
  labs(x="Temperatur", y="Total Bike Rentals")

# add bar plot  
plot + geom_bar(stat="identity",fill="green4", alpha=0.4)

# create ranking: high to low and add color
plot.ordered = ggplot(bike, aes(x = reorder(season, -cnt), y =  cnt))+ 
  ggtitle("Bike Rentals per Season") +
  labs(x="Temperatur", y="Total Bike Rentals")

plot.ordered + geom_bar(stat="identity",fill="green4", alpha=0.4)

# reorder from low to high
plot.ordered = ggplot(bike, aes(x = reorder(season, cnt), y =  cnt))+ 
  ggtitle("Bike Rentals per Season") +
  labs(x="Temperatur", y="Total Bike Rentals")
plot.ordered + geom_bar(stat="identity",fill="green4", alpha=0.4)

# stacked and horizontal bars
plot.stacked = ggplot(bike, aes(x = reorder(season, cnt), y =  cnt, fill=weathersit))+
  ggtitle("Bike Rentals per Season") +
  labs(x="Temperatur", y="Total Bike Rentals")+
  coord_flip()
plot.stacked + geom_bar(stat="identity")

# stacked and horizontal bars
plot.stacked + geom_bar(stat="identity", position="fill")

### Lineplots
################

# we need to aggregate data for nice line plots
casual=tapply(bike$casual,bike$mnth, sum)
registered=tapply(bike$registered,bike$mnth, sum)
rentals = data.frame(casual,registered)

# one line
ggplot(rentals, aes(x = 1:12, y = (casual+registered)))+ 
  geom_line(size=1.5, color="green4", alpha=0.4)+
  scale_x_continuous(breaks=1:12)+
  ggtitle("Bike Rentals per Month") +
  labs(x="Month", y="Total Bike Rentals")

# need to transform data into "long format" for plotting more lines
library(reshape2)
rentals.long = t(rentals)
rentals.long = melt(rentals.long)
colnames(rentals.long) = c("CustomerType", "Month", "Rentals")

# create line plot with two lines
ggplot(rentals.long, aes(x = Month, y =  Rentals)) + 
  geom_line(aes(colour=CustomerType, group=CustomerType), size=1.5)+
  scale_x_continuous(breaks=1:12)+
  ggtitle("Bike Rentals per Month") +
  labs(x="Month", y="Total Bike Rentals")


### Create Heatmaps
########################################################################

ggplot(bike, aes(x=weekday, y=mnth))+
  geom_tile(aes(fill = cnt), color = "white")+
  scale_fill_gradient(name="Bike\nRentals", low="red", high="green", breaks=c(2500,5000,7500))+
  labs(title="Bike Rentals: Months vs. Weekdays", subtitle = "October Monday Bike Rental Fatigue", x = "Day of the Week", y = "Month of the Year")+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))


### Create "real" Heatmaps
########################################################################

library(ggmap)

#get data
trips=read.csv("2017-Q1-Trips-Location.csv")
str(trips)

# ggmap worked perfectly with google maps
# problem: google has changed the API settings; now it is required to have an API key (with activitated billing account)
# but: changes are not yet implemented in newest version of ggmaps
# workaround: we will use stamenmaps 

# defining the coordinates that determine the map used in the heatmap
wash <- c(left = -77.07, bottom = 38.87, right = -77.00, top = 38.926030)
# http://bboxfinder.com/ is useful tool for finding the box to be displayed

washmap <- get_stamenmap(wash, zoom=14, maptype="toner-lite")
ggmap(washmap)

# put "arrival zones" on map
ggmap(washmap) + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level..,alpha = ..level..),
                 bins = 30,  data = trips, geom = "polygon") + 
  scale_alpha(range = c(0.1, 0.5), guide = FALSE)+
  scale_fill_gradient(low = "orange", high = "red", name="Number of Bikes")+
  ggtitle("Where do Washingtonian go with their rented Bikes? Weekday vs. Workdays\n") + 
  facet_wrap(~ Weekday)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(2,"lines"))

# warning message basically says that there some data points beyond our zoom factor 
# that are cut off
