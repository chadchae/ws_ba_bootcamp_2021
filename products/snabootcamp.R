# Preparation
## Package Install
Packages = c(
    'tidyverse',
    'sna',
    'igraph',
    'intergraph',
    'psych'
)

for(p in Packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}


## System
set.seed(12345)
Sys.info()
sessionInfo()
Packages
Sys.time()

# Network Data
## Network Build
net_mat <- rbind(c(0,1,1,0,0,0,0),
                 c(1,0,1,0,1,0,0),
                 c(1,1,0,1,1,0,0),
                 c(0,0,1,0,0,0,0),
                 c(0,1,1,0,0,1,1),
                 c(0,0,0,0,1,0,0),
                 c(0,0,0,0,1,0,0))

rownames(net_mat) <- c("Chad","June","Tony","Jason","Yuliana","Hao","Ernest") 
colnames(net_mat) <- c("Chad","June","Tony","Jason","Yuliana","Hao","Ernest") 
net_net <- network(net_mat, matrix.type="adjacency", directed = FALSE)

## Convert as igraph object
inet<-asIgraph(net_net)

## Set node attribute
V(inet)$vertex.names <- c("Chad","June","Tony","Jason","Yuliana","Hao","Ernest") 
V(inet)$label <- V(inet)$vertex.names 
V(inet)$gender <- c('M','F','M','M','F','M','M')
V(inet)$gender_color <- ifelse(V(inet)$gender == "F", "red", "blue")
V(inet)$mj <- c("BA","MK","FN","CS","AC","FN","FN") 
V(inet)$role <- c("IN","ST","TA","ST","TA","ST","ST") 
V(inet)$dc <- igraph::degree(inet)
V(inet)$bc <- round(igraph::betweenness(inet)/34, 2)
V(inet)$cc <- round(igraph::closeness(inet),2)

## Graph layout
l<-layout.fruchterman.reingold(inet)

## Basic Plot
plot(inet)

# Network Level Analysis
## Network size (number of ties and nodes)
ecount(inet)
vcount(inet)

## Density (how much densly connected)
(2*8)/(7*6) # by hand 2L/V(V-1), L = n of links, V = n of nodes
graph.density(inet) # by igraph function
## Centralization (how they are structured, close to 1 -> star shape)
centralization.degree(inet)$centralization

## Diameter (how many connections are the longest path)
diameter(inet)
get_diameter(inet)

## Component (how many graphs are unbreak in this network dataset )
igraph::components(inet)$no
igraph::components(inet)$csize

## Clustering coefficient (How crumbly will this network be split?
transitivity(inet, type="global")

## Average shortest path (in general, how easily they can be connected?)
average.path.length(inet, directed=F)


# Individual Level

## Degree centrality
igraph::degree(inet)
plot(inet, 
     layout=l, 
     vertex.color= adjustcolor(V(inet)$gender_color, alpha.f = .5),
     vertex.size = V(inet)$dc * 5,
     #vertex.size = rescale(dc, min(V(inet)$dc), max(V(inet)$dc)),
     vertex.label =  paste(V(inet)$vertex.names, V(inet)$dc, sep=" "),
     label.csx = 5,
     vertex.label.dist=4,
     displaylabels = TRUE,
     edge.arrow.size=.5, 
     main="Degree Centraility")

## Betweenness centrality
round(igraph::betweenness(inet)/34, 2)
plot(inet, 
     layout=l, 
     vertex.color= adjustcolor(V(inet)$gender_color, alpha.f = .5),
     vertex.size = V(inet)$bc * 100,
     #vertex.size = rescale(dc, min(V(inet)$dc), max(V(inet)$dc)),
     vertex.label =  paste(V(inet)$vertex.names, V(inet)$bc, sep=" "),
     label.csx = 5,
     vertex.label.dist=4,
     displaylabels = TRUE,
     edge.arrow.size=.5, 
     main="Betweenness centrality")

## Closeness centrality
round(igraph::closeness(inet),2)
plot(inet, 
     layout=l, 
     vertex.color= adjustcolor(V(inet)$gender_color, alpha.f = .5),
     vertex.size = V(inet)$cc * 100,
     #vertex.size = rescale(dc, min(V(inet)$dc), max(V(inet)$dc)),
     vertex.label =  paste(V(inet)$vertex.names, V(inet)$cc, sep=" "),
     label.csx = 5,
     vertex.label.dist=4,
     displaylabels = TRUE,
     edge.arrow.size=.5, 
     main="Closeness centrality")

# Clustering (Community Detection)
## by gender
plot(inet, 
     layout=l, 
     vertex.color= adjustcolor(V(inet)$gender_color, alpha.f = .5),
     #vertex.size = V(inet)$cc * 100,
     #vertex.size = rescale(dc, min(V(inet)$dc), max(V(inet)$dc)),
     #vertex.label =  paste(V(inet)$vertex.names, V(inet)$cc, sep=" "),
     label.csx = 5,
     vertex.label.dist=4,
     displaylabels = TRUE,
     edge.arrow.size=.5, 
     main="Color by gender")

## by role
plot(inet, 
     layout=l, 
     vertex.color= adjustcolor(as.numeric(as.factor(V(inet)$role)), alpha.f = .5),
     #vertex.size = V(inet)$cc * 100,
     #vertex.size = rescale(dc, min(V(inet)$dc), max(V(inet)$dc)),
     #vertex.label =  paste(V(inet)$vertex.names, V(inet)$cc, sep=" "),
     label.csx = 5,
     vertex.label.dist=4,
     displaylabels = TRUE,
     edge.arrow.size=.5, 
     main="Color by role")

## By major
plot(inet, 
     layout=l, 
     vertex.color= adjustcolor(as.numeric(as.factor(V(inet)$mj)), alpha.f = .5),
     #vertex.size = V(inet)$cc * 100,
     #vertex.size = rescale(dc, min(V(inet)$dc), max(V(inet)$dc)),
     #vertex.label =  paste(V(inet)$vertex.names, V(inet)$cc, sep=" "),
     label.csx = 5,
     vertex.label.dist=4,
     displaylabels = TRUE,
     edge.arrow.size=.5, 
     main="Color by Major")

## by connection
c4 <- cluster_fast_greedy(inet)
plot(c4, inet, vertex.color=membership(c4))

