
### Characterizing whole networks.

library(igraph)

## Example Dataset - campnet
# 18 individuals at a Summer camp nominating friendship preferences

mat <- as.matrix(readr::read_csv("data/campnet.csv")[,-1])
mat1 <- as.matrix(readr::read_csv("data/Camp92_wk2.csv")[,-1])
mat2 <- as.matrix(readr::read_csv("data/Camp92_wk3.csv")[,-1])

mat   # top 3 choices in week 3
mat1  # ranked choices week 2
mat2  # ranked choices week 3

# attributes
campattr <- readr::read_csv("data/campattr.csv")
campattr

# convert to igraph object

g <-  graph_from_adjacency_matrix(mat)
g


# add gender attribute
g <- set_vertex_attr(g,"gender",value=campattr$Gender)
g


# visualize
plot(g, edge.arrow.size=.2)


## sizes

gorder(g) # nodes
vcount(g) # also nodes

gsize(g) # edges
ecount(g) # also edges



### Measures of Cohesion ---

## All cohesion measures to some extent don't capture everthing about what a network's structure looks like.



## Graph Density --

# The probability of tie occurring between any 2 random nodes
# in valued networks, equates to average tie strength

graph.density(g)
edge_density(g, loops=F)  # almost always the default is to ignore self-loops
edge_density(g)  # a lot of igraph functions have '.' and '_' versions.

ecount(g)/(vcount(g)*(vcount(g)-1)) #equivalent to this in a directed network


# density is obviously influenced by e.g. size, making comparisons hard between varying networks.

# in this case use average degree of network.

#dhat = 2T / n  (T is number of edges)
degree(g)
mean(degree(g))
(ecount(g)*2)/vcount(g)


# can compare density within subgroups:
# count ties between members of same group, compute density just for those ties.
# e.g. campnet dataset males vs females.

g.female <- induced_subgraph(g, V(g)$gender==1)  #female-female
g.male <- induced_subgraph(g, V(g)$gender==2)  #male-male

g.female
g.male

graph.density(g.female)  #.36
graph.density(g.male) #.28



## Compare density of two graphs visually

set.seed(17)
gs1 <- sample_gnp(20,.1, directed = F)
gs2 <- sample_gnp(20,.25, directed = F)
par(mfrow=c(1,2))
plot(gs1, main=paste0("density = ",round(graph.density(gs1),3)))
plot(gs2, main=paste0("density = ",round(graph.density(gs2),3)))
par(mfrow=c(1,1))





## What is a 'whole' network anyway...

## components are individually connected parts of graphs
components(gs1) # number and size of components

# get components
decompose(gs1)



# Component Ratio --

# a measure of how well connected a graph is.
# CR =  c - 1 / n -1   (c is number of components, n is number of nodes in the graph)
# maximum is 1 when every node is an isolate.
# minimum of 0 when whole network is one component.
# an inverse measure of cohesion

gs3 <- sample_gnp(200,.015, directed = F)
plot(gs3, vertex.size=2, vertex.label=NA)

cno <- components(gs3)$no #11
(cno - 1) / (vcount(g) - 1) # [1] Component Ratio = 0.5882353




## Connectedness (also called fragmentation) --

# The proportion of pairs of nodes that can reach the other by a path of any length
# Boils down to the proportion of pairs of nodes that are located in the same component

# igraph and sna don't appear to have methods precisely for this.

net <- intergraph::asNetwork(g)
sna::connectedness(net) #1 connectedness treats networks as undirected.


# use case e.g. which nodes to remove from a network to maximally fragment the network.
# go through a network and remove each node and see change in fragmentation score.
# Albert R., Jeong H., Barabasi A. (2000) Error and attack tolerance of complex networks. Nature, 406:378-381.

library(keyplayer)
keyplayer::fragment(mat)
plot(g, edge.arrow.size=.05)
V(g)$name

keyplayer::fragment(mat) # notice John has lowest fragmentation, # removing Michael leads to more fragmentation






## Average path lengths, diameter etc.----

# another key measure of network connectedness revolves around paths in a network
# undirected network - either direction
# directed network - can only go via direction of edge

## walks, paths and trails (directed graphs)

# A path is a walk in which each other actor and each other relation 
#   in the graph may be used at most one time

# A trail between two actors is any walk that includes a given relation no more 
#   than once (the same other actors, however, can be part of a trail multiple times. 

# Example Dataset - outbreak of measles in Germany village (19th C)
measles <- read.csv("data/measles.csv")

# Get graph object
gm <- graph_from_data_frame(measles, directed = TRUE)

# Make a basic plot
plot(gm, 
     vertex.label.color = "black", 
     vertex.label.cex = .6,
     edge.color = 'gray42',
     vertex.size = 0,
     edge.arrow.size = 0.05,
     layout = layout_nicely(gm))


# diameter - standard measure of 'distance' in graph
diameter(gm, directed=T)
diameter(gm, directed=F) # if don't wish to consider edge direction

# Of all shortest paths between two nodes, what is the average length?
average.path.length(gm)
average.path.length(gm, directed = F) # if don't wish to consider edge direction

# another name for the same function
mean_distance(gm, directed = T)
mean_distance(gm, directed = FALSE)



# Is there an edge going from vertex 184 to vertex 178?
gm['184', '178']

# Is there an edge going from vertex 184 to vertex 178?
gm['178', '184']

# Show all edges going to or from vertex 184
incident(gm, '184', mode = c("all"))

# Show all edges going out from vertex 184
incident(gm, '184', mode = c("out"))

# Identify all neighbors of vertex 12 regardless of direction
neighbors(gm, '12', mode = c('all'))

# Identify other vertices that direct edges towards vertex 12
neighbors(gm, '12', mode = c('in'))

# Identify any vertices that receive an edge from vertex 42 and direct an edge to vertex 124
n1 <- neighbors(gm, '42', mode = c('out'))
n2 <- neighbors(gm, '124', mode = c('in'))
intersection(n1, n2)

#- Which two vertices are the furthest apart in the graph ?
farthest_vertices(gm) 

#- Shows the path sequence between two furthest apart vertices.
get_diameter(gm)  

# Identify vertices that are reachable within two connections from vertex 42
ego(gm, 2, '42', mode = c('out'))

# Identify vertices that can reach vertex 42 within two connections
ego(gm, 2, '42', mode = c('in'))





## Geodesic Distance

# Make an ego graph
g184 <- make_ego_graph(gm, diameter(gm), nodes = '184', mode = c("all"))[[1]]

# Get a vector of geodesic distances of all vertices from vertex 184 
dists <- distances(g184, "184")

# Create a color palette of length equal to the maximal geodesic distance plus one.
colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")

# Set color attribute to vertices of network g184.
V(g184)$color <- colors[dists+1]

# Visualize the network based on geodesic distance from vertex 184 (patient zero).
plot(g184, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05,
     main = "Geodesic Distances from Patient Zero"
)



#Get the diameter of the graph g
diameter(g, directed = FALSE)

#Get the average path length of the graph g





# Directed connectedness - what proportion of pairs of nodes can reach each other?:

plot(g,edge.arrow.size=.15)
sna::reachability(net)
V(g)$name

x<-sna::reachability(net)
nrow(x)
(sum(x)-nrow(x))/(nrow(x)*(nrow(x)-1)) #.67



## Shortest paths between all nodes:

distances(gm) # includes weights in assessment
distances(gm, weights=NA) # ignore weights





## Visualizing shortest paths.


# this is geodesic distance again:

set.seed(11)
gx <- sample_gnp(40,.06, directed=F)
gx
plot(gx, vertex.size=9,vertex.label.cex=.8)


#distance from node 33 to all others:
gx.dist <- distances(gx, v=33, to=V(gx), weights=NA)
gx.dist[gx.dist=="Inf"]<-NA
gx.dist

# Set colors
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(gx.dist,na.rm=T)+1)
col <- col[gx.dist+1]


plot(gx, vertex.color=col, edge.arrow.size=.5, vertex.size=19,
     vertex.label.cex=.8,vertex.label.color="white")




# Shortest Path visualization
path33.30 <- shortest_paths(gx, 
                            from = 33,
                            to  = 30,
                            output = "both")

path33.30

# Edge colors
ecol <- rep("gray80", ecount(gx))
ecol[unlist(path33.30$epath)] <- "orange"

# thicken edge of shortest path
ew <- rep(2, ecount(gx))
ew[unlist(path33.30$epath)] <- 4

# Nodes along path color:
vcol <- rep("gray40", vcount(gx))
vcol[unlist(path33.30$vpath)] <- "gold"


plot(gx, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0,
     vertex.size=9,vertex.label.cex=.8)




### Visualizing neighbors

inc.edges <- incident(gx, 33, mode="all")


# Set colors
ecol <- rep("gray80", ecount(gx))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(gx))
vcol[33] <- "gold"
ew <- rep(2, ecount(gx)) #reset edge widths

plot(gx, vertex.color=vcol, edge.color=ecol,
     edge.width=ew, edge.arrow.mode=0,
     vertex.size=9,vertex.label.cex=.8)


# Set colors to plot the neighbors:
neigh.nodes <- neighbors(gx, 33, mode="all")
vcol[neigh.nodes] <- "#ff9d00"
plot(gx, vertex.color=vcol)




## another measure of 'neighborhoods' is co-citation
# for any pair of nodes, how many shared nominations they have
  
cocitation(gx) #notice 5 & 18 share 2 nodes



## Compactness--
# A variation of connectedness, which weighs the paths of connecting nodes inversely by their length
# Accounts for geodesic distance in computation
# Better than average path length when dealing with disconnected networks

gy<- igraph::sample_gnp(100,.1)
plot(gy)
GNEL <- igraph.to.graphNEL(gy)
QuACN::compactness(GNEL)
# fragmentation is sometimes measured as 1-compactness (often called breadth)







####  Reciprocity and Transitivity ----


# Reciprocity examines dyadic ties in directed networks
# are they mutual/symmetric/bidrectional or asymmetric/unidirectional?

plot(g) #campnet graph again
 
## reciprocity
reciprocity(g) #[1] 0.7037037  70% ofties are reciprocated.

compete::rshps(mat) #'rshps' function from 'compete' package also gives information about dyad types

dyad_census(g) 

2*dyad_census(g)$mut/ecount(g) # this is how reciprocity is calculated



## Transitivity --

# transitivity is a local measure of connectedness.

# in undirected graphs, it's a measure of how closed each triangle in the network is

#undirected graph
plot(gx, vertex.color=vcol, edge.color=ecol,
     edge.width=ew, edge.arrow.mode=0,
     vertex.size=9,vertex.label.cex=.8)

transitivity(gx) #[1] 0.1079137   10% of triangles are 'closed' = transitive.
transitivity(gx, type="global")   # same as above

transitivity(gx, type="local")  # gives relative transitivity for each node.
# notice node  33 has one closed triangle out of  3 potential = 1/3 = .333


# In directed networks, we can learn a lot more about triangle structure. 
# there are many more potential triangle (see notes)

plot(g, edge.arrow.size=.5) # directed network

transitivity(g) #[1] 0.5046729   friendship network has 50% of triangles being 'closed' = transitive.
transitivity(g, type="global")   # same as above
transitivity(g, type="local")



# we can also examine the nature of triangles by taking a census:

triad_census(g) # for directed networks 

# these are the David & Leinhardt notation system:

# 003  A,B,C, the empty graph.
# 012  A->B, C, the graph with a single directed edge.
# 102  A<->B, C, the graph with a mutual connection between two vertices.
# 021D A<-B->C, the out-star.
# 021U A->B<-C, the in-star.
# 021C A->B->C, directed line.
# 111D A<->B<-C.
# 111U A<->B->C.
# 030T A->B<-C, A->C.
# 030C A<-B<-C, A->C.
# 201  A<->B<->C.
# 120D A<-B->C, A<->C.
# 120U A->B<-C, A<->C.
# 120C A->B->C, A<->C.
# 210 A->B<->C, A<->C.
# 300 A<->B<->C, A<->C, the complete graph.



# Show all triangles in the network.
matrix(triangles(g), nrow = 3)

# Count the number of triangles that vertex "HOLLY" is in.
count_triangles(g, vids = 'HOLLY')




# transitivity() is a measure of Watts & Strogatz clustering coefficient for undirected networks

# get indivi clustering of each node (density of ties in each ego network)
# then clustering coefficient is a weighted average of individual clustering coefficients across all nodes
# transitivity coefficient is same as weighted clustering coefficient
# high clustering may occur in networks that aren't globally cohesive (local vs global cohesion)


# Watts Strogatz - "small world networks" - real world human networks very clumpy and compact
# with surprisingly short paths between individuals
# human systems are paradoxically clumpy and short pathed
# just a few connections between clumps dramatically shortens path lengths
# test for 'small-world' network by comparing clustering coefficient to random graphs.


# (see triad census + concordance example)






## Centralization --

# Centralization reflects the extent to which a network is dominanted by a single node
# A maximally centralized graph is a star.

# star graph
gs <- make_star(10, mode = "out")
plot(gs)

# centralization for a graph is a measure of the 'starness' of a graph.
# Freeman's centralization
# calculate by subtracting the number of ties of each node from the max degree in the network
# this sum is then divided by the maximal possible (if the graph was a star)

library(magrittr) # for pipe
ge <- make_empty_graph(n = 5) %>%
  add_edges(c(1,3, 1,4, 2,3, 2,5, 4,3, 3,5)) %>%
  as.undirected()
ge
plot(ge)

centr_degree(ge)$centralization
centr_clo(ge, mode="all")$centralization
centr_eigen(ge, directed=FALSE)$centralization




### gaining confidence in measures via:

## bootstrapping
## jackknifing



# quick example - 

# let's look at using jackknifing to get confidence in transitivity of campnet data

plot(g, edge.arrow.size=.4)

transitivity(g)

newt<-vector('list',vcount(g))
for(i in 1:vcount(g)){
  vids <- setdiff(V(g)$name, V(g)$name[i])
  newt[[i]]<-transitivity(induced_subgraph(g, vids))
}
unlist(newt)
summary(unlist(newt))
quantile(unlist(newt),.05)
quantile(unlist(newt),.95)
transitivity(g)


