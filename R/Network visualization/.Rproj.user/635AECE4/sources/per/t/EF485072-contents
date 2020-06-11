
# Install and  Load the packages
install.packages("igraph") 
install.packages("network") 
install.packages("sna")
install.packages('coda')
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ggraph")

library(igraph)
library(network)
library(sna)
library(visNetwork)
library(threejs)
library(networkD3)
library(ggraph)

# 1.	Import two csv files that contains the links and nodes of a network
# of Facebook users. The files are called FB_Links and FB_Nodes

# Load  and inspect the datasets
nodes=read.csv("Fb_Nodes.csv",header = T)
head(nodes)

links=read.csv("FB_Links.csv",header = T)
head(links)



# 2.	Inspect the attributes of the network using the E and V function and
# use the as_data_frame function to describe the nodes and links.
# Plot the network using the default settings and describe the main
# problem with this plot in terms of its readability

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net

class(net)

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object

V(net)$group # Vertex attribute "group"

# Find nodes and edges by attribute:
# (that returns oblects of type vertex sequence/edge sequence)
V(net)[group=="B"]

# Data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")
# Plot the network


plot(net,vertex.label.cex=0.7,asp=0.3,label.dist=2,
     vertex.label.color="black",asp=0.6,edge.arrow.size=0.5)

# --------------------------------------------------------------------------------
# 3.	Plot the network again using the following attributes settings; the colour
# of the nodes should be blue for male Facebook users and red for female
# (hint: use the ifelse function - see p23 of the R Studio Student Guide on Moodle),
# the node size should be set to value of the friend_count attribute divided by 70
# and the arrow.size attribute should be reset to an appropriate value that improves
# the clarity of the plot

# Generate colors based on media type:

V(net)$color <- ifelse(V(net)$sex=="male","blue","red")

# We use the friend count to set node size
V(net)$size <- V(net)$friend_count/70

#change arrow size 
E(net)$arrow.size <- 0.2

plot(net,layout=layout_with_fr,vertex.label.cex=0.6,asp=0.4)
legend("topright", legend = paste( c('MALE','FEMALE')),
       pch = 15, inset=c(0.8,-0.1),
       col = c("blue","red"), cex=0.6)



# 4. You should plot a total of 8 networks for this task - one network plot for
# each group. You should adjust the node size to a more appropriate value by 
# dividing the friend_count value with a smaller number than one used in task 3.
# Each plot should be displayed using the layout style of layout_with_fr.

# ---------------------------------------------------------------------
# Group "B"

library(dplyr)
# Filter the nodes and links from group B
node_B =nodes%>% 
  group_by(group) %>% 
  filter("B" %in% group)

names_B=as.character(node_B$id)
links_B=links[(links$to %in% c(names_B) & links$from %in% c(names_B)), ]

net3 <- graph_from_data_frame(d=links_B, vertices=node_B, directed=T) 
net3

# Data frames describing nodes and edges:
as_data_frame(net3, what="edges")
as_data_frame(net3, what="vertices")

# Male to have color blue and female to have color red
V(net3)$color <- ifelse(V(net3)$sex=="male","blue","red")
# We use the friend count to set node size
V(net3)$size <- V(net3)$friend_count/10
#change arrow size 
E(net3)$arrow.size <- 1

# We set the network layout:
graph_attr(net3, "layout") <- layout_with_fr
# Plot the network 
plot(net3,vertex.label.cex=0.8,asp=0.3)
legend("topright", legend = paste( c('MALE','FEMALE')),
       pch = 15, inset=c(0.8,-0.1),
       col = c("blue","red"), cex=0.7)


# --------------------------------------------------------------------------------------
# Group "C"
library(dplyr)
# Filter the nodes and links from group C
node_C =nodes%>% 
  group_by(group) %>% 
  filter("C" %in% group)

names_C<-as.character(node_C$id)
links_C<-links[(links$to %in% c(names_C) & links$from %in% c(names_C)), ]
net4 <- graph_from_data_frame(d=links_C, vertices=node_C, directed=T) 
net4

# Data frames describing nodes and edges:
as_data_frame(net4, what="edges")
as_data_frame(net4, what="vertices")

# Male to have color blue and female to have color red
V(net4)$color <- ifelse(V(net4)$sex=="male","blue","red")
# We use the friend count to set node size
V(net4)$size <- V(net4)$friend_count/10
#change arrow size 
E(net4)$arrow.size <- 0.5

# We set the network layout:
graph_attr(net4, "layout") <- layout_with_fr
# Plot the network 

plot(net4,layout=layout_with_fr,vertex.label.cex=0.8,vertex.label.dist=2, vertex.label.color="black",asp=0.6)
legend("topright", legend = paste(c('MALE','FEMALE')),title = "SEX",
       pch = 15, inset=c(0.8,-0.1),
       col = c("blue","red"), cex=0.8)



# --------------------------------------------------------------------------------------
# Group "F"
library(dplyr)

# Filter the nodes and links from group F
node_F =nodes%>% 
  group_by(group) %>% 
  filter("F" %in% group)

names_F=as.character(node_F$id)

links_F=links[(links$to %in% c(names_F)
               & links$from %in% c(names_F)), ]

net5 <- graph_from_data_frame(d=links_F, vertices=node_F, directed=T) 
net5

# Data frames describing nodes and edges:
as_data_frame(net5, what="edges")
as_data_frame(net5, what="vertices")

# Male to have color blue and female to have color red
V(net5)$color <- ifelse(V(net5)$sex=="male","blue","red")
# We use the friend count to set node size
V(net5)$size <- V(net5)$friend_count/30
#change arrow size 
E(net5)$arrow.size <- 0.5

# We set the network layout:
graph_attr(net5, "layout") <- layout_with_fr

# Plot the network 
plot(net5,vertex.label.cex=0.7,vertex.label.color="black",asp=0.5,dist=2)
legend("topright", legend = paste(c('MALE','FEMALE')),
       pch = 15, inset=c(0.8,-0.2),
       col = c("blue","red"), cex=0.8)


# ---------------------------------------------------------------------------------
# Group "G"
library(dplyr)

# Filter the nodes and links from group G
node_G = nodes%>% 
  group_by(group) %>% 
  filter("G" %in% group)

names_G = as.character(node_G$id)

links_G=links[(links$to %in% c(names_G) & links$from %in% c(names_G)), ]

net6 <- graph_from_data_frame(d=links_G, vertices=node_G, directed=T) 
net6

# Data frames describing nodes and edges:
as_data_frame(net6, what="edges")
as_data_frame(net6, what="vertices")

# Male to have color blue and female to have color red
V(net6)$color <- ifelse(V(net6)$sex=="male","blue","red")
# We use the friend count to set node size
V(net6)$size <- V(net6)$friend_count/10
#change arrow size 
E(net6)$arrow.size <- 0.5

# We set the network layout:
graph_attr(net6, "layout") <- layout_with_fr

# Plot the network 
plot(net6,vertex.label.cex=0.6,vertex.label.dist=2, vertex.label.color="black",asp=0.6)
legend("topright", legend = paste(c('MALE','FEMALE')),title = "SEX",
       pch = 15, inset=c(0.8,-0.2),
       col = c("blue","red"), cex=0.7)

# ---------------------------------------------------------------------------------------

# Group "H"
library(dplyr)

# Filter the nodes and links from group H
node_H = nodes%>% 
  group_by(group) %>% 
  filter("H" %in% group)

names_H = as.character(node_H$id)

links_H=links[(links$to %in% c(names_H) & links$from %in% c(names_H)), ]

net7 <- graph_from_data_frame(d=links_H, vertices=node_H, directed=T) 
net7

# Data frames describing nodes and edges:
as_data_frame(net7, what="edges")
as_data_frame(net7, what="vertices")

# Male to have color blue and female to have color red
V(net7)$color <- ifelse(V(net7)$sex=="male","blue","red")
# We use the friend count to set node size
V(net7)$size <- V(net7)$friend_count/10
#change arrow size 
E(net7)$arrow.size <- 0.5

# We set the network layout:
graph_attr(net7, "layout") <- layout_with_fr

# Plot the network 
plot(net7,vertex.label.cex=1.2,vertex.label.dist=2, vertex.label.color="black",asp=0.6)
legend("topright", legend = paste(c('MALE','FEMALE')),
       pch = 15, inset=c(0.7,-0.2),
       col = c("blue","red"), cex=0.7)


# --------------------------------------------------------------------------------------

# Group "M"
library(dplyr)

# Filter the nodes and links from group M
node_M = nodes%>% 
  group_by(group) %>% 
  filter("M" %in% group)

names_M = as.character(node_M$id)

links_M=links[(links$to %in% c(names_M) & links$from %in% c(names_M)), ]

net8 <- graph_from_data_frame(d=links_M, vertices=node_M, directed=T) 
net8

# Data frames describing nodes and edges:
as_data_frame(net8, what="edges")
as_data_frame(net8, what="vertices")

# Male to have color blue and female to have color red
V(net8)$color <- ifelse(V(net8)$sex=="male","blue","red")
# We use the friend count to set node size
V(net8)$size <- V(net8)$friend_count/30
#change arrow size 
E(net8)$arrow.size <- 0.2

# We set the network layout:
graph_attr(net8, "layout") <- layout_with_fr

# Plot the network 
plot(net8,vertex.label.cex=0.8,vertex.label.dist=2, vertex.label.color="black",asp=0.6)
legend("topright", legend = paste(c('MALE','FEMALE')),title = "SEX",
       pch = 15, inset=c(0.7,-0.2),
       col = c("blue","red"), cex=0.7)


# -----------------------------------------------------------------------------------

# Group "S"
library(dplyr)

# Filter the nodes and links from group S
node_S = nodes%>% 
  group_by(group) %>% 
  filter("S" %in% group)

names_S = as.character(node_S$id)

links_S=links[(links$to %in% c(names_S) & links$from %in% c(names_S)), ]

net9 <- graph_from_data_frame(d=links_S, vertices=node_S, directed=T) 
net9

# Data frames describing nodes and edges:
as_data_frame(net9, what="edges")
as_data_frame(net9, what="vertices")

# Male to have color blue and female to have color red
V(net9)$color <- ifelse(V(net9)$sex=="male","blue","red")
# We use the friend count to set node size
V(net9)$size <- V(net9)$friend_count/10
#change arrow size 
E(net9)$arrow.size <- 0.2

# We set the network layout:
graph_attr(net9, "layout") <- layout_with_fr

# Plot the network 
plot(net9,vertex.label.cex=0.9,vertex.label.dist=3, vertex.label.color="black",asp=0.6)
legend("topright", legend = paste( c('MALE','FEMALE')),
       pch = 15, inset=c(0.7,-0.2),
       col = c("blue","red"), cex=0.8)

# ------------------------------------------------------------------------------------

# Group "W"
library(dplyr)

# Filter the nodes and links from group S
node_W = nodes%>% 
  group_by(group) %>% 
  filter("W" %in% group)

names_W = as.character(node_W$id)

links_W=links[(links$to %in% c(names_W) & links$from %in% c(names_W)), ]

net10 <- graph_from_data_frame(d=links_W, vertices=node_W, directed=T) 
net10

# Data frames describing nodes and edges:
as_data_frame(net10, what="edges")
as_data_frame(net10, what="vertices")

# Male to have color blue and female to have color red
V(net10)$color <- ifelse(V(net10)$sex=="male","blue","red")
# We use the friend count to set node size

V(net10)$size <- V(net10)$friend_count/25
#change arrow size 
E(net10)$arrow.size <- 0.2

# We set the network layout:
graph_attr(net10, "layout") <- layout_with_fr

# Plot the network 
plot(net10,vertex.label.cex=0.6,vertex.label.dist=2, vertex.label.color="black",asp=0.6)
legend("topright", legend = paste(c('MALE','FEMALE')),
       pch = 15, inset=c(0.8,-0.1),
       col = c("blue","red"), cex=0.8)


# ------------------------------------------------------------------------------------

# 5.	Re-plot the network containing group F Facebook users so that only 
# males within that group are displayed. Perform the same task again,
# but this time only plot the females in this group. Both plots should 
# be displayed using the layout style of layout_with_fr.

# Filter the nodes and links from sex "male"
node_F_male =node_F%>% 
  group_by(sex) %>% 
  filter("male" %in% sex)

names_F_male=as.character(node_F_male$id)

links_F_male=links[(links$to %in% c(names_F_male)
               & links$from %in% c(names_F_male)), ]

net.male <- graph_from_data_frame(d=links_F_male, vertices=node_F_male, directed=T) 
net.male

# Data frames describing nodes and edges:
as_data_frame(net.male, what="edges")
as_data_frame(net.male, what="vertices")

# We use the friend count to set node size
V(net.male)$size <- V(net.male)$friend_count/30
#change arrow size 
E(net.male)$arrow.size <- 0.5

# We set the network layout:
graph_attr(net.male, "layout") <- layout_with_fr

# Plot the network 
plot(net.male,vertex.label.cex=0.8,vertex.label.dist=2,
     vertex.label.color="black",asp=0.6)



# Filter the nodes and links from sex "female"
node_F_female =node_F%>% 
  group_by(sex) %>% 
  filter("female" %in% sex)

names_F_female=as.character(node_F_female$id)

links_F_female=links[(links$to %in% c(names_F_female)
                    & links$from %in% c(names_F_female)), ]

net.female <- graph_from_data_frame(d=links_F_female, vertices=node_F_female, directed=T) 
net.female

# Data frames describing nodes and edges:
as_data_frame(net.female, what="edges")
as_data_frame(net.female, what="vertices")

# We use the friend count to set node size
V(net.female)$size <- V(net.female)$friend_count/20
#change arrow size 
E(net.female)$arrow.size <- 0.2

# We set the network layout:
graph_attr(net.female, "layout") <- layout_with_fr

# Plot the network 
plot(net.female,vertex.label.cex=0.8,vertex.label.dist=2,
     vertex.label.color="black",asp=0.6)


# 6.	Using the cluster_optimal function, detect the communities in each
# plot you created in task 5. Both plots should be displayed using the 
# layout style of layout_with_fr6.	Using the cluster_optimal function,
# detect the communities in each plot you created in task 5. Both plots
# should be displayed using the layout style of layout_with_fr

library(igraph)
oc <- cluster_optimal(net.male)
oc



oc$membership #the clusters that will be attached to the dataframe Node_F_male


node_F_male$membership = oc$membership

net.male.new <- graph_from_data_frame(d=links_F_male, vertices=node_F_male, directed=T) 
net.male.new

# Data frames describing nodes and edges:
head(as_data_frame(net.male.new, what="edges"))
as_data_frame(net.male.new, what="vertices")


# We use the friend count to set node size
V(net.male.new)$size <- V(net.male.new)$friend_count/20
V(net.male.new)$color <- ifelse(V(net.male.new)$membership==1,"green","purple")


#change arrow size 
E(net.male.new)$arrow.size <- 0.5

# We set the network layout:
graph_attr(net.male.new, "layout") <- layout_with_fr

# Plot the network 
plot(net.male.new,vertex.label.cex=0.9,vertex.label.dist=1.5,
     vertex.label.color="black",asp=0.5)
legend("topright", legend = paste('community', c('1','2')),
       pch = 15, inset=c(0.7,-0.2),
       col = c("green","purple"), cex=0.8)




# Female datasets
## The calculation only takes a couple of seconds
oc2 <- cluster_optimal(net.female)
oc2


oc2$membership #the clusters that will be attached to the dataframe Node_F_male


node_F_female$membership = oc2$membership

net.female.new <- graph_from_data_frame(d=links_F_female, vertices=node_F_female, directed=T) 
net.female.new

# Data frames describing nodes and edges:
as_data_frame(net.female.new, what="edges")
as_data_frame(net.female.new, what="vertices")


# We use the friend count to set node size
V(net.female.new)$size <- V(net.female.new)$friend_count/30
V(net.female.new)$color <- ifelse(V(net.female.new)$membership==1,"green","purple")


#change arrow size 
E(net.female.new)$arrow.size <- 0.5

# We set the network layout:
graph_attr(net.female.new, "layout") <- layout_with_fr

# Plot the network 

plot(net.female.new,vertex.label.cex=0.8,vertex.label.dist=2, 
     vertex.label.color="black",asp=0.6)
legend("topright", legend = paste('community', c('1','2')),
       pch = 15, inset=c(0.8,-0.2),
       col = c("green","purple"), cex=0.7)

# 7.	Simplify the original network containing all Facebook users so that
# only those nodes with a degree of greater than 10 are plotted. In this
# simplified network use the hub_score and authority_score functions to
# calculate these scores
# Count the number of degree for each node:

# The node size in each of these plots should be 10 times the value
# of the hub and authority scores. Your plots should be appropriately
# labelled and displayed using the layout style of layout_with_fr.


# The hub score and plots
hub=hub_score(net5)
hub

graph_attr(net5, "layout") <- layout_with_fr #set the layout

V(net5)$size<-hub$vector*10 #adjust the node size to be same as the hub score
#change arrow size 
E(net5)$arrow.size <- 0.2


plot(net5,layout=layout_with_fr,vertex.label.cex=0.8,
     vertex.label.dist=1.5,vertex.label.color="black",asp=0.6)
legend("topright", legend = paste(c('MALE','FEMALE')),
       pch = 15, inset=c(0.7,-0.2),
       col = c("blue","red"), cex=0.7)


# The Authorities score and plot
authority=authority_score(net5)
authority
graph_attr(net5, "layout") <- layout_with_fr #set the layout

V(net5)$size<-authority$vector*10 #adjust the node size to be same as the hub score
#change arrow size 
E(net5)$arrow.size <- 0.5

plot(net5,layout=layout_with_fr)

plot(net5,layout=layout_with_fr,vertex.label.cex=0.8,
     vertex.label.dist=1.5,vertex.label.color="black",asp=0.6)
legend("topright", legend = paste( c('MALE','FEMALE')),
       pch = 15, inset=c(0.8,-0.2),
       col = c("blue","red"), cex=0.7)



# 8.	For the simplified network created in task 7, calculate the betweeness
# (ignoring loops) of each node and edge density value for the network.
# Identify the node with the highest betweeness value in this network


deg <- centralization.degree(net5)$res
deg






