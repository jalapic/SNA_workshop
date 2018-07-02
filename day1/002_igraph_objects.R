
### Types of Network Data - brief introduction


## terminology apologies

# graph/network
# ties/edges/links/relationships
# nodes/vertices ... node/vertex



## Number of formats that data may be collected in

# Care has to be taken in collecting data, 
# but once collected data can be moved easily between
# edgelists - matrices - network objects


## lists --
# e.g. list of individuals at each time point


## edgelists--
# pairwise events 
# undirected or directed
# with time of event
# with on/off times
# weighted edgelists


## adjacency matrices (sociomatrices)--
# frequency of events
# comembership
# association indices
# measure of relatedness
# measure of distance
# nominations (ranks)
# rates of interaction

# thresholding

## group-by-individual matrices
# presence/absence of individual in groups


## network objects--
# many R packages have their own 'network' class objects



## Directed vs Undirected Graphs

## Weighted (valued) vs unweighted (binary) graphs.



# We'll have examples of all of these  - just some beginning examples:



library(igraph) # the package we'll use


###  Edgelist to Graph Object


# Inspect the first few rows of the dataframe 'friends'
friends <- read.csv("data/friends.csv")
head(friends)

# Convert friends dataframe to a matrix
friends.mat <- as.matrix(friends)

# Convert friends matrix to an igraph object
g <- graph.edgelist(friends.mat, directed = FALSE)  # denote if directed


g  # let's look at what the graph object is


# Look at four letters at the top:
#     D / U, Directed vs undirected graph
#     N      Named graph 
#     W      Weighted graph 
#     B      Bipartite (two-mode) graph 

# first number = number of nodes
# second number = number of edges
# details about attributes of nodes and edges


class(g)


# Make a very basic plot of the network
plot(g)



### Can make graph object straight from dataframe

friends
g <- graph_from_data_frame(friends, directed=F)
g


## Useful thing about 'graph_from_data_frame' is you can add edge attributes e.g.weight

friends_w <- read.csv("data/friends_w.csv")
gw <- graph_from_data_frame(friends_w, directed=F)
gw
plot(gw)


## This data may also be in an adjacency matrix

# binary matrix
friends_mat <- read.csv("data/friends_mat.csv")
friends_mat

gM <- graph_from_adjacency_matrix(as.matrix(friends_mat), mode=c("undirected"))
gM

# valued matrix
friends_w_mat <- read.csv("data/friends_w_mat.csv")
friends_w_mat

gWM <- graph_from_adjacency_matrix(as.matrix(friends_w_mat), mode=c("undirected"),weighted=T)
gWM





## Graph objects can also be converted to matrices, edgelists or dataframes

gw[]
get.adjacency(gw)
get.adjacency(gw,sparse = F)

as_adjacency_matrix(gw, attr="weight")

as_edgelist(gw, names=T)

as_data_frame(gw, what="edges")
as_data_frame(gw, what="vertices")

V(g)[[]] # gives attribute info too. 







# Subset vertices and edges
V(g)
E(g)

# Count number of edges
gsize(g)
ecount(g)

# Count number of vertices
gorder(g)
vcount(g)



## import some attributes
friends_attr <- read.csv("data/friends_attr.csv")

# Create new vertex attribute called 'gender'
g <- set_vertex_attr(g, "gender", value = friends_attr$gender)

# Create new vertex attribute called 'age'
g <- set_vertex_attr(g, "age", value = friends_attr$age)

# View all vertex attributes in a list
vertex_attr(g)


# View edge attributes of graph object
gw

edge_attr(gw)

# Create new edge attribute called 'hours'
g <- set_edge_attr(g, "hours", value = friends_w$weight)


# View graph attributes
graph_attr(gw)


## can also import node and edge dataframes at same time with all attributes in them
gg <- graph_from_data_frame(friends_w, directed = F, vertices = friends_attr)
gg





### subsetting graphs


# View attributes of first five vertices in a dataframe
V(g)[[1:5]] 


# Find all edges that include "Britt"
E(g)[[inc('Britt')]]  

# Find all pairs that spend 4 or more hours together per week
E(g)[[hours>=4]]  

# also directly create new attributes
# Plot network and color vertices by gender
V(g)$color <- ifelse(V(g)$gender == 1, "orange", "dodgerblue")
plot(g, vertex.label.color = "black")


# attributes directly
V(g)$age
E(g)$hours


# Single brackets to get rows/cols/cells of the network matrix:
g[1,]
g[,5]
g[5,7]




# Is the graph directed?
is.directed(g)

# Is the graph weighted?
is.weighted(g)
is.weighted(gw)

# Where does each edge originate from?
table(head_of(g, E(g)))




### Other ways of creating graphs in igraph:

rm(list = ls()) # Remove everything from environment
dev.off() # remove plots


## manually....

graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 
graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=T ) 


graph( c("A", "B", "B", "C", "C", "D")) # named vertices
graph( c("A", "B", "B", "C", "C", "D", "D", "E"), isolates=c("F", "G", "H", "I") )  


plot(graph_from_literal(a---b, b---c)) 
plot(graph_from_literal(a--+b, b+--c))
plot(graph_from_literal(a+-+b, b+-+c)) 
plot(graph_from_literal(a:b:c---c:d:e))
graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)



## graph models ...

# these are specific  types of network graphs  that igraph will build:


make_empty_graph(20)
make_full_graph(20)
make_star(20)
make_tree(20, children = 3, mode = "undirected")
make_ring(20)


sample_gnm(n=50, m=30) #erdos-renyi
sample_gnp(n=50, p=.1)
sample_smallworld(dim=2, size=10, nei=1, p=0.1)
sample_pa(n=100, power=1, m=1,  directed=F) #Barabasi-Albert preferential attachment model



## built in datasets....
zach <- graph("Zachary") # the Zachary carate club
plot(zach, vertex.size=10, vertex.label=NA)






## Very brief introduction to statnet objects (cover in more detail later)

# there are several other R packages for network analysis
# the other major family of packages are the 'statnet' / 'network' family including 'sna'.
# these do not play nicely with igraph, so...

detach(package:igraph)
library(sna)


## First, it is possible to switch between packages:
library(intergraph)

zach.net <- intergraph::asNetwork(zach) # converts 'zach' igraph object to a 'network' object
zach.net
class(zach.net)

# and back again:
intergraph::asIgraph(zach.net)

# intergraph can also be used to create dataframes of edges and vertexes
intergraph::asDF(zach.net)

detach(package:sna)


## Many other network package have built in datasets that we will be using.












