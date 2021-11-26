require(packrat)
require(devtools) 
require(UserNetR)
require(statnet)
require(igraph)
require(statnet)
require(intergraph)
require(visNetwork) 
require(networkD3) 
require(sna)

# Read Data
data(Moreno)
gender <- Moreno %v% "gender"
plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)

# Basic Description
network.size(Moreno)
summary(Moreno,print.adj=FALSE)

## Density
den_hand <- 2*46/(33*32)
den_hand
gden(Moreno)

## Components
sna::components(Moreno)

## Diameter
lgc <- component.largest(Moreno,result="graph") 
gd <- geodist(lgc)
max(gd$gdist)

## Clustering Coefficient
gtrans(Moreno,mode="graph")

## Degree centrality
sna::degree(Moreno)
sna::betweenness(Moreno)
sna::closeness(Moreno)


# Network data management
## Data create from scratch
netmat1 <- rbind(c(0,1,1,0,0), 
                 c(0,0,1,1,0), 
                 c(0,1,0,0,0), 
                 c(0,0,0,0,0), 
                 c(0,0,1,0,0))
rownames(netmat1) <- c("A","B","C","D","E") 
colnames(netmat1) <- c("A","B","C","D","E")
net1 <- network(netmat1,matrix.type="adjacency") 
class(net1)
summary(net1)
gplot(net1, vertex.col = 2, displaylabels = TRUE)


netmat2 <- rbind(c(1,2),
                 c(1,3), 
                 c(2,3), 
                 c(2,4), 
                 c(3,2), 
                 c(5,3))
net2 <- network(netmat2,matrix.type="edgelist") 
network.vertex.names(net2) <- c("A","B","C","D","E") 
summary(net2)


as.sociomatrix(net1)
as.matrix(net1,matrix.type = "edgelist")




## Node attributes

network::set.vertex.attribute(net1, "gender", c("F", "F", "M", "F", "M"))
net1 %v% "alldeg" <- sna::degree(net1)
network::list.vertex.attributes(net1)
summary(net1)
network::get.vertex.attribute(net1, "gender")
net1 %v% "alldeg"

## List attributes
network::list.edge.attributes(net1)
network::set.edge.attribute(net1,"rndval", runif(network.size(net1),0,1))
network::list.edge.attributes(net1)
summary(net1 %e% "rndval")
summary(network::get.edge.attribute(net1,"rndval"))

netval1 <- rbind(c(0,2,3,0,0), 
                 c(0,0,3,1,0), 
                 c(0,1,0,0,0), 
                 c(0,0,0,0,0), 
                 c(0,0,2,0,0))
netval1 <- network(netval1,matrix.type="adjacency", 
                   ignore.eval=FALSE,names.eval="like") 
network.vertex.names(netval1) <- c("A","B","C","D","E")
network::set.vertex.attribute(netval1, "label", 
                              c("Sylvia", 
                                "Aida", 
                                "Roger", 
                                "Tracy", 
                                "Leonard"))
network::set.vertex.attribute(netval1, "gender", c("F", "F", "M", "F", "M"))
network::list.edge.attributes(netval1)

network::get.edge.attribute(netval1, "like")

as.sociomatrix(netval1)
as.sociomatrix(netval1,"like")

inetval1<-asIgraph(netval1)
plot(inetval1)

write.graph(inetval1, "inetval1.graphml", format = "graphml")





el<-read.csv("./data/relation.csv")
nl<-read.csv("./data/actors.csv")

el<-el[,-1]
el

nl<-nl[,-1]
nl

g <- graph_from_data_frame(el, directed=TRUE, vertices=nl)
print(g, e=TRUE, v=TRUE)

plot(g)

as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")
as_edgelist(g)
as_adjacency_matrix(g)
as_data_frame(g)
plot(g)


a<-as.matrix(as_adjacency_matrix(g))
a

gplot(a,vertex.cex=2, gmode="graph", vertex.col="blue",edgelwd=3)

l<-layout.fruchterman.reingold(g)
a<-igraph::degree(g)
a

plot(g, layout=l, vertex.size=a*5, edge.arrow.size=.2, main="Degree")
plot(g, layout=l, vertex.size=a*5, edge.arrow.size=.15, main="Gender Difference", vertex.color=nl$gender)
V(g)$gender_color <- ifelse(V(g)$gender == "F", "red", "blue")
V(g)$gender_color 

plot(g, layout=l, vertex.size=a*5, edge.arrow.size=.15, main="Network", vertex.color=V(g)$gender_color)

#==Visualization
#Node
data(Bali)
par(mfrow=c(2,3))
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='circle',main="circle") 
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='eigen',main="eigen") 
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='random',main="random") 
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='spring',main="spring") 
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='fruchtermanreingold',main='fruchtermanreingold') 
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='kamadakawai',main='kamadakawai')
par(mfrow=c(1,1))


data(Bali) 
gplot(Bali,vertex.col="slateblue2",gmode="graph")
col2rgb('slateblue2') 
gplot(Bali,vertex.col=rgb(122,103,238,maxColorValue=255),gmode="graph") 
gplot(Bali,vertex.col="#7A67EE",gmode="graph")


ndum <- rgraph(300,tprob=0.025,mode="graph") 
par(mfrow=c(1,2)) 
gplot(ndum,gmode="graph",vertex.cex=2,
      vertex.col=rgb(0,0,139,maxColorValue=255), 
      edge.col="grey80",edge.lwd=0.5, main="Fully opaque")

gplot(ndum,gmode="graph",vertex.cex=2, 
      vertex.col=rgb(0,0,139,alpha=80,maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5,
      main="Partly transparent")
par(mfrow=c(1,1)) 

rolelab <- network::get.vertex.attribute(Bali,"role")
plot(Bali,usearrows=FALSE,vertex.cex=1.5,label=rolelab, displaylabels=T,vertex.col="role") 
palette()

plot(Bali,vertex.cex=0.5,main="Too small") 
plot(Bali,vertex.cex=2,main="Just right") 
plot(Bali,vertex.cex=6,main="Too large")


deg <- sna::degree(Bali,gmode="graph") 
deg

cls <- sna::closeness(Bali,gmode="graph") 
cls

bet <- sna::betweenness(Bali,gmode="graph") 
bet

plot(Bali,usearrows=T,vertex.cex=deg,main="Raw") 
plot(Bali,usearrows=FALSE,vertex.cex=log(deg),main="Adjusted")

plot(Bali,usearrows=T,vertex.cex=cls,main="Raw") 
plot(Bali,usearrows=FALSE,vertex.cex=4*cls,main="Adjusted")

plot(Bali,usearrows=T,vertex.cex=bet,main="Raw") 
plot(Bali,usearrows=FALSE,vertex.cex=sqrt(bet+1),main="Adjusted")


rescale <- function(nchar,low,high) {
    min_d <- min(nchar)
    max_d <- max(nchar)
    rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low 
    rscl
}
plot(Bali,vertex.cex=rescale(deg,1,6),main="Adjusted node sizes with rescale function.")


network::get.vertex.attribute(Bali,"vertex.names")

plot(Bali,displaylabels=TRUE,label.cex=0.8,
     pad=0.4,label.col="darkblue")

IClevel <- Bali %e% "IC" 
plot(Bali,vertex.cex=1.5,edge.lwd=1.5*IClevel)

n_edge <- network.edgecount(Bali) 
edge_cat <- sample(1:3,n_edge,replace=T) 
linecol_pal <- c("blue","red","green")
plot(Bali,vertex.cex=1.5,vertex.col="grey25", edge.col=linecol_pal[edge_cat],edge.lwd=2)


n_edge <- network.edgecount(Bali) 
edge_cat <- sample(1:3,n_edge,replace=T) 
line_pal <- c(2,3,4)
gplot(Bali,vertex.cex=0.8,gmode="graph", vertex.col="gray50",edge.lwd=1.5, edge.lty=line_pal[edge_cat])

# Load data
NODES<-read.csv("Script/Extra/data/KSNODES.csv")
K1M<-read.csv("Script/Extra/data/KSEDGES.csv")

## Transforming relation data into matrix data form
rownames(K1M)<-K1M$X
K1M<-K1M[,-1]
K1M<-as.matrix(K1M)

## Building Network from Data
K1N<-graph_from_adjacency_matrix(K1M, mode="directed")
V(K1N)$Name<-NODES$Name
V(K1N)$Sex<-NODES$Sex
V(K1N)$Year<-NODES$Year

# Summary and plot network 
summary(K1N)
plot(K1N, vertex.cex=3,
     edge.arrow.size=.2, 
     vertex.label.color="black", 
     vertex.label.dist=1.5)

# Analysis
## Network Level Analysis
### Number of nodes and edges
length(V(K1N))
length(E(K1N))


### Density
graph.density(K1N)

### Centralization
centralization.degree(K1N)$centralization
centralization.betweenness(K1N)$centralization
centralization.closeness(K1N)$centralization

### Diameter
diameter(K1N)
get_diameter(K1N)

### Average path length
average.path.length(K1N, directed=F)

### Clustering Coefficient
transitivity(K1N, type="global")

## Node Level Analysis
degree <- sna::degree(K1M)
degree
between <- sna::betweenness(K1M)
between
close <- sna::closeness(K1M)
close


## Ego-centric network
V(K1N)$name
ego(K1N, 1)

ego<-make_ego_graph(K1N, 1)
ego_Stella<-ego[[1]]
plot(ego_Stella)
graph.density(ego_Stella)
centralization.degree(ego_Stella)$centralization

ego_Grace<-ego[[2]]
plot(ego_Grace)
graph.density(ego_Grace)
centralization.degree(ego_Grace)$centralization

graph.density(ego_Stella)
graph.density(ego_Grace)

#----------------------
nodes <- read.csv("Script/Extra/data/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Script/Extra/data/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

# Examine the data:
head(nodes)
head(links)

nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))
nrow(unique(links[,c("from", "to", "type")]))

# Collapse multiple links of the same type between the same two nodes
# by summing their weights, using aggregate() by "from", "to", & "type":
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

nrow(links); nrow(unique(links[,c("from", "to")]))



# --DATASET 2: matrix--

nodes2 <- read.csv("Script/Extra/data/Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Script/Extra/data/Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

# Examine the data:
head(nodes2)
head(links2)

# links2 is a matrix for a two-mode network:
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)



library("igraph")


net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 


class(net)
net 


E(net)
V(net)
E(net)$type
V(net)$media

V(net)[media=="BBC"]
E(net)[type=="mention"]


as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")


as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")



net[1,]
net[5,7]


plot(net) # not pretty!


net <- simplify(net, remove.multiple = F, remove.loops = T) 


plot(net, edge.arrow.size=.4,vertex.label=NA)



# DATASET 2 


head(nodes2)
head(links2)


net2 <- graph_from_incidence_matrix(links2)


table(V(net2)$type)

plot(net2,vertex.label=NA)


class(net2)
net2 




plot(net, edge.arrow.size=.4, edge.curved=.1)

plot(net, edge.arrow.size=.4, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 


colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

V(net)$size <- V(net)$audience.size*0.6

V(net)$label.color <- "black"
V(net)$label <- NA


E(net)$width <- E(net)$weight/6


E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

plot(net) 



legend(x=-1.1, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=3)



plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=1.2, edge.color="gray90")


edge.start <- ends(net, es=E(net), names=F)[,1] # get the "from" node
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)




net.bg <- sample_pa(80, 1.2) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

plot(net.bg, layout=layout_randomly)

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

l
l <- cbind(1:vcount(net.bg), c(1, vcount(net.bg):2))
plot(net.bg, layout=l)

l <- layout_randomly(net.bg)
plot(net.bg, layout=l)

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

l <- layout_on_sphere(net.bg)
plot(net.bg, layout=l)

l <- layout_with_fr(net.bg)
plot(net.bg, layout=l)

par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=l)
plot(net.bg, layout=l)

dev.off()

l <- layout_with_fr(net.bg)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(net.bg, rescale=F, layout=l*0.4)
plot(net.bg, rescale=F, layout=l*0.8)
plot(net.bg, rescale=F, layout=l*1.2)
plot(net.bg, rescale=F, layout=l*1.6)

dev.off()

l <- layout_with_kk(net.bg)
plot(net.bg, layout=l)

plot(net.bg, layout=layout_with_mds)

plot(net.bg, layout=layout_with_lgl)


layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 

layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))

for (layout in layouts) {
    print(layout)
    l <- do.call(layout, list(net)) 
    plot(net, edge.arrow.mode=0, layout=l, main=layout) }

dev.off()

plot(net)


hist(links$weight)
mean(links$weight)
sd(links$weight)

cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off])
plot(net.sp) 


clp <- cluster_label_prop(net)
class(clp)
clp$membership

plot(clp, net)

V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])





# ------->> Interactive plotting with tkplot -------- 

tkid <- tkplot(net) 
l <- tkplot.getcoords(tkid) 
plot(net, layout=l)


# Heatmap
netm <- as_adjacency_matrix(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 


heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(20), 
        scale="none", margins=c(10,10) )



library("visNetwork") 

head(links)
head(nodes)

visNetwork(nodes, links)
visNetwork(nodes, links, height="600px", width="100%", main="Network!")

nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <- nodes$media # Text on click
nodes$label <- nodes$type.label # Node label
nodes$size <- nodes$audience.size # Node size
nodes$borderWidth <- 2 # Node border width
nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"

visNetwork(nodes, links)

links$width <- 1+links$weight/8 # line width
links$color <- "gray"    # line color  
links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- FALSE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow

visNetwork(nodes, links)

# Remove the arrows and set the edge width to 1:
links$arrows <- "" 
links$width  <- 1



visNetwork(nodes, links) %>%
    visOptions(highlightNearest = TRUE, 
               selectedBy = "type.label")


el <- data.frame(from=as.numeric(factor(links$from))-1, 
                 to=as.numeric(factor(links$to))-1 )

nl <- cbind(idn=factor(nodes$media, levels=nodes$media), nodes) 

forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "idn", Group = "type.label",linkWidth = 1,
             linkColour = "#afafaf", fontSize=12, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 600, height = 600)



