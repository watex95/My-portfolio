
# Load the packages

#devtools::install_github("dkahle/ggmap", ref = "tidyup")
require(devtools)
library(ggmap)
library(ggplot2)
library(ggmap)

# Get GOOGLE API
# You go to https://console.cloud.google.com/google/maps-apis 
# and enable all these APIs; geocoding, direction,direction,
# mapsEmbed,mapsStaticAPI then go to credentials and obtain  your
# API key and paste it here, then run the following code;

register_google(key = "") 

# Plot with location name (Geocoding)
nai_center = as.numeric(geocode(" Kenyatta International Convention Centre(KICC) Nairobi Kenya"))

NaiMap = ggmap(get_googlemap(center = nai_center,
            zoom=10), extent="normal")
NaiMap


# VISUALIZE HOSIPIATLS IN NAIROBI
# ---------------------------------------------------------------------------
# Load the datase
hosi=read.csv('hospitals.csv',header = T)
hosi$Name=as.character(hosi$Name)
head(hosi)

# Get data for geo points 
coordinates=geocode(hosi$Name)
coordinates=as.data.frame(coordinates)
coordinates$Name=hosi$Name
beds=c(27,89,100,250,100,16,360,70,45,60,27,89,100,
       250,100,16,360,70,45,60,100,16,360,70,45,60)
coordinates$beds=beds

type=c('private','public','public','public','private',
       'private','public','public','public','private',
       'private','public','public','public','private',
       'private','public','public','public','private',
       'private','public','public','public','private','public')
coordinates$type=type
head(coordinates)
coordinates$type=as.factor(coordinates$type)
str(coordinates)

#Add simple data points to map
map_data <-NaiMap +
  geom_point(data=coordinates,aes(x=lon, y=lat,size=beds,colour=type),
     alpha=0.5)
map_data



# VISUALIZE CHURCHES IN NAIROBI ON A MAP
# -----------------------------------------------------------------------------
church=read.csv('churches.csv',header = T)
str(church)
church$Name=as.character(church$Name)
head(church)
church_coord=geocode(church$Name)

church_coord$Name=church$Name
head(church_coord)
church_coord=as.data.frame(church_coord)

write.csv(church_coord,'D:/church_coordinates.csv')

NaiMap
#Add simple data points to map
attach(church_coord)

NaiMap
+geom_point(data=church_coord,aes(x=lon, y=lat),
             colour='purple',size=3,alpha=0.7)








