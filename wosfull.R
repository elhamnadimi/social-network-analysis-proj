load('/content/CISPRINWOSmatrices.RData')
load('/content/CISPRINWOS_attributes.RData')
library("igraphdata")
library("igraph")
library("network")
library("statnet")
library("intergraph")
library("dplyr")
library("stringr")
library("RColorBrewer")
library("sand")



nodeWOS <- data.frame(WOS)
#nodePRIN
relationsp <- as.matrix(WOSFull)

WOS_graph = graph_from_adjacency_matrix(relationsp, mode="undirected",weighted = TRUE)
WOS_graph



allcodes_vec <- as.vector(V(WOS_graph)$name)#vector of all names of nodes in the PrinFull
External_authors<-str_subset(allcodes_vec, "A")
#External_authors
#Also we subset all codes that start from S-Italian authors from matrix with all names of nodes in the CISFull and save them as InternalCodes
InternalCodes<-str_subset(allcodes_vec, "S")
#InternalCodes

#Here we found what authors are representes in both tables CIS and CISFull and we will assign to them all given attributes.
Italiancodes_vec <- as.vector(nodeWOS$Code)#choose all codes from CIS dataframe-table of Italian statisticians.
Italian_authors <- intersect(InternalCodes,Italiancodes_vec)#intersection of Italian authors code from the previous table with the full matrix of egdes between codes. 
#Italian_authors


#Here we found what authors are representes in both tables CIS and CISFull and we will assign to them all given attributes.
Italiancodes_vec <- as.vector(nodeWOS$Code)#choose all codes from CIS dataframe-table of Italian statisticians.
Italian_authors <- intersect(InternalCodes,Italiancodes_vec)#intersection of Italian authors code from the previous table with the full matrix of egdes between codes. 
#Italian_authors
#Here we found what authors are presented in relationship table CISFull, but not in attributes table CIS with codes similar to Italian statistitians, starting from S.
Unknown_italian_authors <- setdiff(InternalCodes,Italiancodes_vec)
#Unknown_italian_authors



#Set of nodes attributes - Author names
vertex_attr(WOS_graph, "Author",index=V(WOS_graph)$name[V(WOS_graph)$name %in% Italian_authors]) <- as.character(nodeWOS$AUTHOR)
vertex_attr(WOS_graph, "Author",index=V(WOS_graph)$name[V(WOS_graph)$name %in% External_authors]) <- "External"
vertex_attr(WOS_graph, "Author",index=V(WOS_graph)$name[V(WOS_graph)$name %in% Unknown_italian_authors]) <- "Unknown"
#Set other attributes
vertex_attr(WOS_graph, "Sector",index=V(WOS_graph)$name[V(WOS_graph)$name %in% Italian_authors]) <- as.character(nodeWOS$Sector)
vertex_attr(WOS_graph, "H.Index",index=V(WOS_graph)$name[V(WOS_graph)$name %in% Italian_authors]) <- nodeWOS$H.Index
vertex_attr(WOS_graph, "NumDocs",index=V(WOS_graph)$name[V(WOS_graph)$name %in% Italian_authors]) <- nodeWOS$NumDocs
vertex_attr(WOS_graph, "References",index=V(WOS_graph)$name[V(WOS_graph)$name %in% Italian_authors]) <- nodeWOS$References
vertex_attr(WOS_graph, "Citations",index=V(WOS_graph)$name[V(WOS_graph)$name %in% Italian_authors]) <- nodeWOS$Citations
#set additional attribute type of authors-foreign/italian for futher plotting
vertex_attr(WOS_graph, "Author.Type",index=V(WOS_graph)$name[V(WOS_graph)$name %in% InternalCodes]) <- "Italian_researchers"
vertex_attr(WOS_graph, "Author.Type",index=V(WOS_graph)$name[V(WOS_graph)$name %in% External_authors]) <- "Foreign_researchers"

vertex.attributes(WOS_graph)
summary(WOS_graph)




#transforming from Igraph to Network
WOS_NET<-asNetwork(WOS_graph)
WOS_NET

#this doesnt work on wosfull .
#visualizing the Large network and as we can see the contrality is not in the center of graph.
my_pal <- brewer.pal(11,"Set3") #for color the plot

rolecat <- as.factor(get.vertex.attribute(WOS_NET,"Author.Type"))  # color based on Author.Type
par(mar=c(0,0,2,0))


sidenum <- 3:7 #shape based on Author.Type
gplot(WOS_NET,vertex.cex=1,
      edge.col="grey80",edge.lwd=0.02,
      thresh=0.01,jitter=TRUE,
      vertex.sides=sidenum[rolecat]
      ,cex.main=1.5,layout.fruchterman.reingold,
      vertex.col=my_pal[rolecat],main="Current Index to Statistics Database Network-WOSFull")

legend("bottomleft",legend=c("Italian_researchers","Foreign_researchers"),
       col=my_pal[rolecat],pch=19,pt.cex=1.5,bty="n",
       title="Authors")




#visualizing the Large network and as we can see the contrality is not in the center of graph.
my_pal <- brewer.pal(11,"Set3")
rolecat <- my_pal[as.numeric(as.factor(V(WOS_graph)$Author.Type))]

par(mar=c(0,0,2,0))
deg <- degree(WOS_NET,rescale=TRUE)
#V(WOS_graph)$size <- deg*3000
#V(WOS_graph)$color <-ifelse(V(WOS_graph)$Author.Type=="Italian_researchers","red", "blue")
l=layout.lgl(WOS_graph)
plot(WOS_graph,edge.color="gray60"
     ,pad=0.4,vertex.label=NA,layout=l,vertex.size=2,vertex.color= my_color)
title(main="Current Index to Statistics Database Network-WOSFull",
      sub="Large Graph Layout", cex.main=2)
legend("bottomleft",legend=c("Italian_researchers","Foreign_researchers"),
       col=my_pal[rolecat],pch=19,pt.cex=1.5,bty="n",
       title="Authors")




#lests plot high degree nodes

my_pal <- brewer.pal(11,"Set3")
rolecat <- my_pal[as.numeric(as.factor(V(WOS_graph)$Author.Type))]


CIS_CI<- igraph::degree(WOS_graph)
sorted<-sort(CIS_CI, decreasing = FALSE)
max_citation<-sorted[c(5291,5290,5289,5288,5287,5286,5285)] #HIGHEST degree that I used to plot in the next code.
max_citation

V(WOS_graph)$Author[CIS_CI %in% max_citation]#authors that have the most degrees

#IN WHICH SECTOR DO AUTHORS WITH HIGH CITATION BELONG? they are mostly belong to sector 1.
table(V(WOS_graph)$Author[CIS_CI %in% max_citation],V(WOS_graph)$Sector[CIS_CI%in% max_citation])


#Plot the graph with node size equal to node degree
V(WOS_graph)$size <- CIS_CI*0.009
plot(WOS_graph,layout=layout.random,
     vertex.frame.color=NA,vertex.label.color="black",
     vertex.color= rolecat,vertex.label = ifelse(CIS_CI %in% max_citation ,
                                                 V(WOS_graph)$Author, NA),vertex.label.dist=0)

legend("bottomleft", legend=levels(as.factor(V(WOS_graph)$Author.Type)),
       col = my_pal, bty = 'n', pch=20 , pt.cex = 3, cex = 1, text.col='black' ,
       horiz = FALSE)

legend("right", legend=as.character(V(WOS_graph)$Author[CIS_CI %in% max_citation]), 
       bty = 'n', pch=NA_integer_ , pt.cex = 6, cex = 1, text.col='black' , 
       horiz = FALSE)

title(main="WOS Network-High degree centrality", cex.main=2)



table(degree(WOS_NET,gmode="graph")) #there is 26 isolated node and 
#there is 325 node with degree 886! also we can plot them

it <- which(V(WOS_graph)$name %in% Italiancodes_vec) 
# Extract subgraph of italian researchers only
ll <- induced.subgraph(graph=WOS_graph,vids=it)
ll
l<-asNetwork(ll)
l


#vidualizing based on the italian researchers and the size of node is proportional to degree.
my_pal <- brewer.pal(11,"Set3")

par(mar=c(0,0,2,0))

deg <- degree(l,rescale=TRUE)
V(ll)$size <- deg*1700  #scaling degree

rolecat <- as.factor(get.vertex.attribute(l,"Sector"))
V(ll)$color<- my_pal[rolecat]

plot(ll,edge.color="gray60"
     ,pad=0.4,vertex.label=NA,edge.curved=0.3)
title(main="Italian researchers collaboration-WOS", cex.main=2)

legend("bottomleft",legend=c("SEC1","SEC2","SEC3","SEC4","SEC5"),
       col=my_pal,pch=19,pt.cex=1.5,bty="n",
       title="SECTORS")



#NOTE:
# I couldnt run commented code on this data as it more big

#Basic Description
network.size(WOS_NET) #the # of authors
network.size(l) #the # of italian authors

gden(WOS_NET)
#components(WOS_NET) # split into various subgroups

#In the following code the largest component is extracted into a new matrix.
#The geodesics (shortest paths) are then calculated for each pair of nodes using the
#geodist() function. The maximum geodesic is then extracted,
# which is the diameter for this component.
#lgc <- component.largest(WOS_NET,result="graph")
#gd <- geodist(lgc)
#max(gd$gdist)

#count how many isolates there are:from this output.
#table(component.dist(WOS_NET)$csize)
#As we can see the number of isoletd node is 7.

length(V(WOS_graph)$name[igraph::degree(WOS_graph)==0]) #finding isolated
diameter(WOS_graph)#The longest shortest path




centralization(WOS_NET,degree, mode= "graph") 

#Closeness centrality-centrality based on distance to others in the graph
centr_clo(WOS_graph, mode="all", normalized=T)

#Eigen centrality-centrality proportional to the sum of connection centralities
centr_eigen(WOS_graph, directed=F, normalized=T)

#Betweenness centrality-centrality based on a broker position connecting others
centralization(WOS_NET,betweenness,  mode= "graph")

transitivity(WOS_graph,type = "global")#The global clustering coefficient is the number of closed triplets over the total number of triplets.



#degree  Distribution
d1 <- degree(WOS_NET,gmode="graph")
hist(d1,col="blue",
     xlab="Degree", ylab="Frequency",
     main="Degree Distribution-WOSFull", ylim=c(0, 5500),cex.main=2)

#Interpretation: While there is a substantial number 
#of nodes of quite low degree, there are also a trivial
#number of nodes with higher order of degree magnitudes.
#, a plot of average neighbor degree versus vertex degree
a.nn.deg <- graph.knn(WOS_graph,V(WOS_graph))$knn
plot(d1, a.nn.deg, log="xy",
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree-WOS"))
title(main="Average nighbor degrees-  WOSFull", cex.main=2)

#Interpretation: The plot "Log Average Neighbor Degree" suggests that while there
# is a tendency of nodes of higher degrees to link with similar nodes,
# nodes of lower degree tend to link with nodes of both lower and higher degrees.





#Degree Centrality 
str(degree(WOS_NET))
summary(degree(WOS_NET,gmode="graph"))

#correlations among a set of centrality measures
df.prom <- data.frame(
  deg = degree(WOS_NET,gmode="graph"),
  btw = betweenness(WOS_NET,gmode="graph")
)

cor(df.prom)


# Centralization: Network Level Indices of Centrality
centralization(WOS_NET,degree, mode= "graph") *100
#centralization(WOS_NET,betweenness,  mode= "graph")*100

#Subgroups
#clique.number() does not return the number of cliques, but the size of the largest clique
numclq<-clique.number(WOS_graph)
numclq
#maximal.cliques(PRIN_graph,min=10)
lrgclq<-largest.cliques(WOS_graph)
lrgclq

#italian resercher :
#there are 469 nodes (cliques
#of size one) and 399 edges (cliques of size two), followed by 129 triangles (cliques
#of size three).
table(sapply(cliques(ll), length))

#cliques(PRIN_graph)



#vidulizing the largest clique.
# Assign largest cliques output to object 'lc'
lc <- largest_cliques(WOS_graph)

# Create two new undirected subgraphs, each containing only the vertices of each largest clique.
gs1 <- as.undirected(subgraph(WOS_graph, lc[[1]]))

# Plot the  largest cliques 
par(mar=c(0,0,2,0))

plot(gs1,
     vertex.label = NA, 
     vertex.size = 2,
     edge.color = 'gray80',
     main = "Largest Clique",
     
)




#A k-core is a maximal subgraph where each vertex is
# connected to at least k other
#vertices in the subgraph. 
#It returns a vector listing the highest core 
#that each vertex belongs to in the network.
#The results tell us the k-cores range from 1 to 5 for italian Authors. 
coreness <- graph.coreness(ll)
table(coreness)
maxCoreness <- max(coreness) # THE NUMBER OF CORNESS IN GRAPH THAT IN THOS CASE IS 5
maxCoreness
f<- as.vector(unique(coreness))
f

#it shows us the nodes in k core
cores = graph.coreness(as.undirected(WOS_graph))
head(sort(cores, decreasing=TRUE), 10)




#Creat layout
CorenessLayout <- function(g) {
  coreness <- graph.coreness(g);
  xy <- array(NA, dim=c(length(coreness), 2));
  
  shells <- sort(unique(coreness));
  for(shell in shells) {
    v <- 1 - ((shell-1) / max(shells));
    nodes_in_shell <- sum(coreness==shell);
    angles <- seq(0,360,(360/nodes_in_shell));
    angles <- angles[-length(angles)]; # remove last element
    xy[coreness==shell, 1] <- sin(angles) * v;
    xy[coreness==shell, 2] <- cos(angles) * v;
  }
  return(xy);
}


# g is the network
# compute coreness
coreness <- graph.coreness(ll);
# assign colors
colbar <- rainbow(max(coreness));
# create layout
lay <- CorenessLayout(WOS_graph);
# plot
plot(ll,vertex.label=NA
     , layout=lay, vertex.size=2,
     vertex.color=colbar[coreness],
     vertex.frame.color=colbar[coreness], main='Coreness-WOSFull');
#This figure shows that the not center of the network is made up of the
#highest k-core. In this case, the 5-core is comprised  of the total nodes.





#lets plot coreness 3
coreness <- graph.coreness(ll)
#V(PRIN_graph)$name <- coreness
par(mar=c(0,0,2,0))

iD <- ll
iD <- induced.subgraph(ll,vids=which(coreness==3))

plot(iD,main="k-cores 3",vertex.color=rgb(0,0,139,alpha=80,maxColorValue=255),
     vertex.size=10,edge.color="gray", usecurve=TRUE,edge.lwd=0.02,edge.curved=0.3)




#To find the authors with maximum coreness.
kcores <- coreness(ll)
verticesHavingMaxCoreness <- which(kcores == max(kcores)) 
V(ll)$Author[verticesHavingMaxCoreness]
#To plot authors with maximum core. 
# Add edge attribute id values
E(ll)$id <- seq(ecount(ll))
# Extract subgraph
MaxCorenessPlot <- induced.subgraph(graph=ll,vids=verticesHavingMaxCoreness)
#Plot subgraph
plot(MaxCorenessPlot,vertex.color=rgb(0,0,139,alpha=50,maxColorValue=255),
     vertex.size=5,edge.color="gray10", usecurve=TRUE,edge.lwd=0.02,edge.curved=0.3,
     vertex.label.dist=1,
     pad=0.9,
     edge.arrow.size=.1,
     vertex.label.color ="gray10",
     vertex.label.cex=1,
     vertex.label=V(ll)$Author[verticesHavingMaxCoreness])
title(main="Maximun Coreness -CIS", cex.main=1)





#vidualizing when coreness is equall to 2
coreness <- graph.coreness(WOS_graph)
#V(PRIN_graph)$name <- coreness
par(mar=c(0,0,0,0))

iD <- WOS_graph
iD <- induced.subgraph(WOS_graph,vids=which(coreness==2))

plot(iD,main="k-cores 27-28",vertex.color=rgb(0,0,139,alpha=80,maxColorValue=255),
     vertex.size=4,edge.color="gray", usecurve=TRUE,edge.lwd=0.02,edge.curved=0.3,
     vertex.label=NA)




#---------------------community detection
COM <- decompose.graph(WOS_graph)
#The graph has 103  total component and
#A census of all connected components within this graph, however, shows that there
#clearly is a giant componentwith 4852 vertices, 
#This  component contains more than 90% of the vertices in the network.
table(sapply(COM, vcount))

COM.gc <- decompose.graph(WOS_graph)[[1]]
#e average path length in the giant component is barely greater than 5
average.path.length(COM.gc)[[1]]
#n the longest of paths
diameter(COM.gc)
#In the case of the giant component of the  network, the vertex and edge
#connectivity are both equal to one.
#Thus it requires the removal of only a single well-chosen vertex or edge in order to
#break this subgraph into additional components.
vertex.connectivity(COM.gc)
edge.connectivity(COM.gc)
#In the giant component of the data network,
#almost 10% of the vertices are cut vertices.(234) out of all nodes
P.cut.vertices <- articulation.points(COM.gc)
length(P.cut.vertices)




#Vidualizing 2  component. # 2
par(mfrow=c(1,1))
plot(COM[[2]], vertex.size=8, vertex.label=NA,vertex.color=rgb(0,0,139,alpha=80,maxColorValue=255),
     edge.color="gray", usecurve=TRUE,edge.lwd=0.02,edge.curved=0.3,main=" component 2")

#visualizing the component 4 
my_pal <- brewer.pal(11,"Set3")

par(mar=c(0,0,2,0))

deg <- degree(l,rescale=TRUE)
V(COM[[4]])$size <- deg*1700

rolecat <- as.factor(get.vertex.attribute(l,"Sector"))
V(COM[[4]])$color<- my_pal[rolecat]

plot(COM[[4]],edge.color="gray60"
     ,pad=0.4,vertex.label=NA,edge.curved=0.3)
title(main="Italian researchers collaboration-WOS", cex.main=1.6)

legend("bottomleft",legend=c("SEC1","SEC2","SEC3","SEC4","SEC5"),
       col=my_pal,pch=19,pt.cex=1.5,bty="n",
       title="SECTORS")



#From graph, I extract all clusters 

modules <- decompose.graph(WOS_graph)
out <- modules[order(sapply(modules, ecount), decreasing=T)]
length(out)
out
#min.vertices = 10


#a quick glance at plot reveals that this graph is not composed
# of a single connected component.
#total component is:103
#So 4852 of the nodes are in a single large component and the
# remaining  are in 102 small components. The 4852 nodes in
# the big component overwhelm the smaller components and the 102 
#small components acts as visual clutter for the big component.
# Let's separate the 26 small components.



vertexes <- character()
data_frames <- list()
for(i in 2:length(out)) {
  vertexes[i] <- list(vertex.attributes(out[[i]])$name)
  data_frames[[i]] <- get.data.frame(out[[i]])
}
sub_nodes = unlist(vertexes)
subv <- sub_nodes
g3 <- induced.subgraph(graph=WOS_graph,vids=subv)
g4<-asNetwork(g3)
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}





#I can calculate the betweenness of this  graph and display the outcome:
wc <- edge.betweenness.community(g3, weights = NULL
                                 ,directed = FALSE,bridges = TRUE)
#This splits up each sub cluster as a community.



## Let's separate the 103 small components.
plot(wc, g3, vertex.size=4,
     edge.color="gray80",
     vertex.label=NA)

title(main="102 small component WOS Network.",sub="cluster louvain Algorithm", cex.main=2)

#lets visualize giant comonent

vertexes <- character()
data_frames <- list()
for(i in 1:1) {
  vertexes[i] <- list(vertex.attributes(out[[i]])$name)
  data_frames[[i]] <- get.data.frame(out[[i]])
}
sub_nodes = unlist(vertexes)
subv <- sub_nodes
LargeComp <- induced.subgraph(graph=WOS_graph,vids=subv)
g6<-asNetwork(LargeComp)
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}


LC.gn.comm <- cluster_louvain(LargeComp)
max(LC.gn.comm$membership)


#we can position vertices in the same community group together and 
#make different communities stay further apart.
LC_Grouped = LargeComp
E(LC_Grouped)$weight = 1
for(i in unique(membership(LC.gn.comm))) {
  GroupV = which(membership(LC.gn.comm) == i)
  LC_Grouped = add_edges(LC_Grouped, combn(GroupV, 2), attr=list(weight=6))
} 

set.seed(1234)
LO = layout_with_fr(LC_Grouped)
colors <- rainbow(max(membership(LC.gn.comm)))
par(mar=c(0,0,2,0))
plot(LC.gn.comm, LargeComp, layout=LO,
     vertex.size = 6, 
     vertex.color=colors[membership(LC.gn.comm)], 
     vertex.label = NA, edge.width = 1,edge.color="gray60")
title(main="The giant component -WOS",sub="cluster louvain Algorithm", cex.main=2)





#Attach community labels as vertex attribute
V(WOS_graph)$L.cluster <- membership(LC.gn.comm)
V(WOS_graph)$Author[V(WOS_graph)$L.cluster==14]
#The 14th community has the maximum density over the dataset
sapply(unique(membership(LC.gn.comm)), function(gg) {
  subg1<-induced.subgraph(WOS_graph, which(membership(LC.gn.comm)==gg)) #membership id differs for each cluster
  ecount(subg1)/ecount(WOS_graph)
})





#Contract the Communities

#Just plot a single node for each community. Here,
# I make the area of each community vertex proportional
# to the number of members of that community 
#I colored the vertices using a coarse grouping based on their degrees.
GN.Comm = simplify(contract(LargeComp, membership(LC.gn.comm)))
D<- unname(igraph::degree(GN.Comm))

set.seed(123)
par(mar=c(0,0,2,0))
plot(GN.Comm, vertex.size=sqrt(sizes(LC.gn.comm))*1.2,
     vertex.label=1:23, vertex.cex = 2,
     vertex.color=round(log(D))+1,
     layout=layout.lgl,  vertex.label.cex=2,edge.curved=0.3, 
     vertex.label.color ="gray10",vertex.label.dist=1.5
)
title(main="Giant component with contracted node-WOS Network.",sub="cluster louvain Algorithm", cex.main=1.5)





#I run cluster_louvain of each subgraph by doing:
#Community Structure Detection Based On Edge Betweenness
#Many networks consist of modules which are densely connected 
#themselves but sparsely connected to other modules.

#Girvan-Newman produces 43 communities within this large component

betweenness_sub <- list()
for(i in 1:length(out)) {
  btws <-  cluster_louvain(out[[i]])
  print(btws)
  betweenness_sub[[i]] <- btws
}



#Whereas, if we consider the biggest cluster individually, we get:
#I can plot these individually by doing:
plot(betweenness_sub[[2]], out[[2]], vertex.size=4,
     edge.color="gray80",vertex.label=NA, layout=layout.kamada.kawai)



#It is possible to plot multiple clusters using the plotly package:

par(mfrow=c(2,2))
#par(mar = rep(2, 3))
for(i in 2:length(out)) {
  plot(betweenness_sub[[i]], out[[i]],vertex.size=6,
       edge.color="gray80",vertex.label=NA, layout=layout.kamada.kawai)
}



#--------------------------------ergm



#get italian reserchers as a subgraph who are in sector 1.
it <- which(V(WOS_graph)$name %in% Italiancodes_vec) 
# Extract subgraph of italian researchers only
l <- induced.subgraph(graph=WOS_graph,vids=it)

l3<-asNetwork(l)

l2=get.inducedSubgraph(l3,which(V(l)$Sector %in% c("SECS-S/02","SECS-S/03","SECS-S/04","SECS-S/05")))
l2
l3<-asIgraph(l3)



transitivity(l3)# the transivity opf this subgraph is 36% , meybe is not a good choice to model with this.
#ergm

#A null model includes only the edges term.,it  ensures that the simulated networks have the same number of edges
#as the observed network. This can be seen by taking the logistic transformation
#of the edges parameter, which gives the overall density of network. 


#Creating the NULL model and the results of fitting the model are stored 
#in a model object for further examination and analysis.

#A null model includes only the edges term.

k<- ergm(l2~edges) # fit model
summary(k) # look in more depth
k$coef #get all the coef inn the model

# We get a negative edge parameter since the network is rather sparse.
## The edge parameter here is the log of the edge odds, i.e. log(#dyads-w-edge/#dyads-no-edge)



gden(WOS_NET)
gden(l2)
plogis(coef(k))  # conditional probability of having a tie is equal to dencuty 

#Including Node Attributes
#Once a null model is obtained, more interesting models can be fit using a wide
#variety of predictors

scatter.smooth(l2 %v% 'Citations',
               degree(l2,gmode='graph'),
               xlab='Author Citations ',
               ylab='Degree',
               main="Basic association between Citations of Author and node degree")





#Testing node characteristic main effects
#Let's test whether the edge probabilities are a function of Citations:
#we want to see if that athors with a high # of citation are connected together

#there is a  estimation around 44% for isolated node. and also for citation the estime is positive, but very small number
k4<- ergm(l2 ~ edges + isolates)
summary(k4)

mcmc.diagnostics(k4)
#we are aiming for the trace to look like a hairy caterpillar. and 
#there was a burn-in  the plot.




k3<- ergm(l2 ~ edges + isolates+nodecov('Citations') )
summary(k3)

mcmc.diagnostics(k3)





#lets see that are ties more or less likely between network
#members who are similar to each other on some characteristic (homophily) or not
mesa <- l2
#par(mfrow=c(2,2))
par(mar=c(0,0,2,0))


#plot(mesa,main="Network in sector 1")
#plot(mesa, vertex.col='Citations',main="homophily based on Citations")
#legend('bottomleft',fill=7:12,legend=paste('Citations',7:12),cex=0.75)
#plot(mesa, vertex.col='Author.Type',main="plot based on Author.Type")
#plot(mesa, vertex.col='NumDocs',main="homophily based on NumDocs")
#plot(mesa, vertex.col='References',main="homophily based on References")
plot(mesa, vertex.col='Sector')

title(main="homophily based on Sector-WOSFull",cex.main=2)
plot(mesa, vertex.col="blue")
title(main="italian resercher in sector 2:5-WOSFull",cex.main=2)




# SIMULATION

#Once we estimated the model we define a prob. 
#distribution on all possible networks of a given size (= to the observed size). 

# If the model fits the data well we should expect that
# we can draw form this distribution network similar to the observed one (on average)

# For simulation we can use this function

k3.sim <- simulate(k3,nsim=10)
k3.sim 





#To get the first object in a list:

k3.sim[[1]]
l2
gden(l2)
gden(k3.sim[[1]])

par(mar=c(0,0,2,0))
gplot(k3.sim[[1]], gmode = "graph",edge.col="gray60",edge.lwd=0.02,
      thresh=0.01,jitter=TRUE)

#Ofc we should expect that differences between
# this drawn network and the observed is given by random fluctuations only
title(main="simulated network-WOSFull",cex.main=2)



#Goodness-of-fit 
mesamodel.02 <- ergm(l2~edges+ nodematch('Sector'))
mesamodel.02.gof <- gof(mesamodel.02~degree)
plot(mesamodel.02.gof)




  