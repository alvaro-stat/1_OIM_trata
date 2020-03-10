rm(list=ls())
###################################################
library(readxl)
library(igraph)
library(sna)
###################################################
setwd("/Users/iMac6/Desktop/_ARU2020/1_OIM_trata")
setwd("~/Downloads/netscix2016")
###################################################
oim_v<-read_excel("entidades.xlsx",1,skip=1)
oim_l<-read_excel("entidades.xlsx",2)

nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T) 
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(links)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
netmap <- graph_from_data_frame(d=oim_l, vertices=oim_v, directed=T)

colrs <- c("gray50", "tomato", "gold","orange")
ltent=c("Cooperacción","Pública","Sociedad Civil","ONG")

V(netmap)$color <- colrs[V(netmap)$itent]

V(netmap)$tentidad

l <- layout_in_circle(netmap)

plot(netmap,vertex.label.color="black",vertex.size=V(netmap)$w*20,
     vertex.label=V(netmap)$id,vertex.frame.color=T,
     vertex.color=V(netmap)$color,
     edge.curved=.1,layout=layout_randomly)

plot(netmap,vertex.label.color="black",vertex.size=V(netmap)$w*20,
     vertex.label=V(netmap)$id,vertex.frame.color=T,
     vertex.color=V(netmap)$color,
     edge.curved=.1,layout=l)

legend("topright", ltent, pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#manual layout
tkid <- tkplot(netmap)
l <- tkplot.getcoords(tkid)
plot(netmap, layout=l)

ceb <- cluster_edge_betweenness(netmap) 
dendPlot(ceb, mode="hclust")
plot(ceb, netmap)
clp <- cluster_label_prop(netmap) 
plot(clp, netmap)

V(net2)$shape <- c("square", "circle")[V(net2)$type+1]

plot(net, edge.arrow.size=.4,vertex.label=V(net)$media)

net <- simplify(net, remove.multiple = F, remove.loops = T)
as_edgelist(net, names=T)
as.network(net)

library(igraph)
df<-data.frame("from" = c("Lyon", "Toulouse", "Paris", "Marseille"), 
               "to"= c("Paris", "Paris", "Marseille", "Toulouse"))
meta <- data.frame("name"=c("Lyon", "Toulouse", "Paris", "Marseille"), 
                   "lon"=c(-4.850000, 1.444209, 2.352222, 5.36978),  
                   "lat"=c(45.750000, 43.604652, 48.856614, 43.296482))

g <- graph.data.frame(df, directed=FALSE, vertices=meta)
lo <- layout.norm(as.matrix(meta[,2:3]))
plot.igraph(g, layout=lo,vertex.size = 60,
            vertex.color="red",
            vertex.frame.color= "white",
            vertex.label.color = "white",
            vertex.label.family = "sans",
            edge.width=2,  
            edge.color="black")

library(sp)
gg<-get.data.frame(netmap, "both")
vert <- gg$vertices
coordinates(vert) <- ~long+lat
edges <- gg$edges
edges <- lapply(1:nrow(edges), function(i) {
        as(rbind(vert[vert$name == edges[i, "from"], ], 
                 vert[vert$name == edges[i, "to"], ]), 
           "SpatialLines")
})
for (i in seq_along(edges)) {
        edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
}
edges <- do.call(rbind, edges)
library(leaflet)
leaflet(vert) %>% addTiles() %>% addMarkers(data = vert) %>% addPolylines(data = edges)


gg <- get.data.frame(g, "both")
vert <- gg$vertices
coordinates(vert) <- ~lon+lat







