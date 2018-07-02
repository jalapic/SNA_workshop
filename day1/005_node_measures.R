#### Measures of Node Relative Importance


## What makes a node 'key' or 'central' to a network ?
# centrality is a family of concepts related to power/influence of each node.


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
plot(g, edge.arrow.size=.1)




### Degree Centrality

# the number of edges that an individual node has
# can be 'all', 'outgoing', or 'incoming'
# equivalent to summing rows/columns of adjacency matrix 
# for weighted networks equivalent to 'strength'

degree(g, mode="all")
degree(g, mode="out")
degree(g, mode="in")

# gives more info (including graph level info)
centr_degree(g, mode="all", normalized=T)
centr_degree(g, mode="out", normalized=T)
centr_degree(g, mode="in", normalized=T)

degree_distribution(g,mode="in") # relative frequency of vertices with degree 0,1,2,3,4,5....etc.

# View a summary of in-degree
table(degree(g, mode="in"))


# plot based on in-degree.

# Set colors 
blues <- colorRampPalette(c("cyan", "purple"))
col <- blues(max(degree(g,mode="in"))+1)
col <- col[degree(g, mode="in")+1]
col

plot(g, vertex.color=col, edge.arrow.size=.1, vertex.size=19,
     vertex.label.cex=.8,vertex.label.color="black")


## comparing subgroups - e.g. degree

gu <- as.undirected(g) #making an undirected campnet graph
plot(gu)

degree(gu)
degree(gu, normalized=T)

by(degree(gu), INDICES=V(gu)$gender, mean)  # compare by group

# can test these group differences by doing a permutation based test (see later).





### Eigenvector Centrality

# Bonacich 1972
# higher in individuals who are well-connected and connected to others who are also well-connected
# related to sum of centralitites of neighbors


eigen_centrality(g, directed=T, weights=NA)
eigen_centrality(g, directed=T, weights=NA)$vector
round(eigen_centrality(g, directed=T, weights=NA)$vector,2)

# includes graph level info
centr_eigen(g, directed=T, normalized=T) 
centr_eigen(g, directed=T, normalized=T)$vector
round(centr_eigen(g, directed=T, normalized=T)$vector,1)


# note: in disconnected networks, small component nodes will get ev.cent of 0.
# note: smaller scores can also be observed in nodes in e.g. bow-tie structured networks





###  Closeness centrality

# Freeman 1979
# the sum of geodesic distances from a node to all others
# how close to others in a network is each node
# this is essentially the inverse of a node's average geodesic distance to others in the network

closeness(g, mode="all", weights=NA) 

#includes graph level metrics - be careful with directed graphs
centr_clo(g, mode="all", normalized=T) 
centr_clo(g, mode="out", normalized=T) 
centr_clo(g, mode="in", normalized=T) 

#note: closeness centrality is problematic in disconnected networks as no path exists between pairs of nodes
#note: not particularly useful for directed networks due to fragmentation


### K-reach Centrality

# Borgatti 2006
# how many  nodes can be reached in K steps from each  node

# Function modified from those by Dai Shizuka
# this is for undirected graphs - could be modified for directed graphs
# could also be modified for weighted graphs

# Function for K-step reach
reachK<-function(x,K){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,K,nodes=i)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}

reachK(gu,3)  #proportion of nodes  that can be reached
V(gu)$name
plot(gu, edge.arrow.size=.3)




### Betweenness Centrality

# Freeman 1979
# related to how many shortest paths each node lies on when examining all pairs of nodes
# individuals who have high betweenness are bridges connecting nodes from different parts of the network
# potential for controlling flow through a network

betweenness(g, directed=T, weights=NA) # can also do directed=F for undirected networks

#gives graph level info
centr_betw(g, directed=T, normalized=T)

## As well as edges, each edge can also be assigned a betweenness score
edge_betweenness(g, directed=T, weights=NA)






### Hub Score and Authority Score

# Kleinberg defined hubs and authorities as follows:
# hubs provide many links to other vertices (many outgoing ties)
# authorities receive many incoming links

hub_score(g, weights=NA)
hub_score(g, weights=NA)$vector

authority_score(g, weights=NA)
authority_score(g, weights=NA)$vector




## Google Page Rank 

#  a measure of how influential a node is in directing flow through a network.

page_rank(g)
page_rank(g)$vector


## There are several others... we can discuss in time.




### Note about valued networks.-

# these metrics can vary according to edge weights
# many of the above functions have a parameter/argument



### Within network comparison of centrality measures ----

gg <- graph(edges=c("s","q","q","m", "m", "l","m","j","l","k","j","i","j","p",
              "j","k","j","o","j","n","k","p","o","n","p","i","i","h",
              "h","g","h","f","h","c","c","d","f","g","f","d","f","e",
              "g","e","g","b","g","d","f","a","d","a","d","b","d","e",
              "e","b","b","a","h","r"), directed=F ) 

gg
plot(gg, layout=layout.kamada.kawai(gg))

betw <- betweenness(gg)
ec <- evcent(gg)$vector
clo <- closeness(gg)
deg <- degree(gg)

mx <- as.matrix(cbind(betw, ec,clo,deg))
mx


# Inter-correlations
Hmisc::rcorr(mx)


# Correspondence analysis
library(ca)
fit <- ca(mx)
fit  # gives dimenion scores
summary(fit) # 2 dimensions ok

fit$rowcoord # each dimension against each node

plot(fit)
# dim1 seems highly related to betweenness
# dim2 is almost ec vs closeness
# individuals in middle are 'high' in both ec and closenes

# betw/closeness and ec aren't that related
# degree appears related to all.






### Gaining confidence in measures

## bootstrapping
## jackknifing

# It is possible to generate confidence in measures by jackknifing or bootstrapping
# Jackknifing can causes problems with some measures e.g. betweenness

# we'll discuss more in future tutorials.


# Uses of centrality measures:
# Use as IV in eg linear models.
# Use as a DV for eg hypotheses about attributes that make indivs influential

