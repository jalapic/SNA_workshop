### Assortatitivity in Animal Networks  (Assortative Mixing)

# assortativity = a preference for a network's nodes to attach 
#                 to others that are similar in some way.

# The assortativity coefficient range between -1 (fully disassorted) and +1 (fully assorted)
# In a fully assorted network all ties would be between individuals that share attributes
# In a fully disassorted network all ties would be between individuals that do not share attributes.
# Attributes may be categorical or continuous.

# Homophily: the tendency of nodes to connect to others who are similar on some variable.



## igraph functions:
# assortativity_nominal() is for categorical variables (labels)
# assortativity() is for ordinal and above variables
# assortativity_degree() checks assortativity in node degrees


## package 'assortnet()'
# Farine shows using weighted edges assortativity is much more robust to
# sampling error than those using binary edges or thresholding.

## Farine's functions in assortnet()
# assortment.discrete()
# assortment.continuous() 



##### Examples ----

library(assortnet)

# assortment.discrete() 
# Calculates the assortativity coefficient for weighted and unweighted graphs 
# with nominal/categorical vertex values

# for binarized graphs is equivalent to igraph::assortativity_nominal()


# Inputs:
# graph - adjacency matrix (weighted or binary)  NxN
# types - values on which to calculate assortment (vector of N labels)
# weighted flag  T/F
# SE - report jackknifed standard error
# M (if very large network, increase M to speed up process)
# M should not be less than one hundredth of the number of edges. 

# Returns:
# $r the assortativity coefficient 
# $SE the standard error 
# $mixing_matrix the mixing matrix with the distribution of edges or edge weights by category

# Values in cells of the mixing matrix represent the proportion of edges that correspond 
# to edges between different types of nodes;  ai are the row sums, bi the column sums.


### Undirected network example

birds1 <- readr::read_csv("data/birds1.csv")
birds1_sps <- readr::read_csv("data/birds1_sps.csv")

birds1
birds1_sps

birds1 <- as.matrix(birds1)
rownames(birds1)<-colnames(birds1)

birds1 # note the order of ids, has to match traits
traits <- as.numeric(factor(birds1_sps$species))  # ensure this is in same order as matrix


# visualize
detach(package:sna)
detach(package:statnet)
detach(package:igraph)
library(igraph)

g <- graph_from_adjacency_matrix(birds1, mode=c("undirected"), weighted=T)
g <- set.vertex.attribute(g, 'species', value = birds1_sps$species)
colors <- c("red","yellow","cyan","green")
g <- set.vertex.attribute(g, 'color', value = colors[traits])

plot(g)  # notice many edges do exist between groups as well as within


## binarize matrices for comparison with igraph
mat <- birds1
mat[mat>0]<-1
mat

g1 <- graph_from_adjacency_matrix(mat, mode=c("undirected"))
g1 <- set.vertex.attribute(g1, 'species', value = birds1_sps$species)
g1 <- set.vertex.attribute(g1, 'color', value = colors[traits])

plot(g1)  # notice many edges do exist between groups as well as within



## "Thresholding"

# assign relationships that fail to meet a given threshold to 0
# this has to be done with some insight into the social system under study
# can lead to loss of key information
# e.g. assign "2" as min value (remove edges == 1)


birds2 <- birds1
birds2[birds2<2] <- 0
birds2
#
g2 <- graph_from_adjacency_matrix(birds2, mode=c("undirected"),weighted=T)
g2 <- set.vertex.attribute(g2, 'species', value = birds1_sps$species)
g2 <- set.vertex.attribute(g2, 'color', value = colors[traits])

plot(g2)  # notice many edges do exist between groups as well as within

# you could also binarize the thresholded network.


# check directed networks
is.directed(g)
is.directed(g1)
is.directed(g2)

# side by side plot
par(mfrow=c(1,3))
plot(g, main="weighted")
plot(g1, main="binarized")
plot(g2, main="thresholded - weighted")
par(mfrow=c(1,1))


## assortativity


## igraph assortativity
assortativity_nominal(g1, traits, directed = F)  #.423


# Test for assortment as binary network
assortment.discrete(birds1,traits,weighted=FALSE)  # agrees with igraph, also get 'mixing matrix'.


# Test for assortment as weighted network
assortment.discrete(birds1,traits,weighted=TRUE)  #.797   !!!!  

# Test for assortment as weighted & thresholded network
assortment.discrete(birds2,traits,weighted=TRUE)  #.902   !!!!  


# To get jackknifed standard errors of assortativity coefficient.
assortment.discrete(birds1,traits, weighted=FALSE, SE=T)  
assortment.discrete(birds1,traits, weighted=TRUE, SE=T)  
assortment.discrete(birds2,traits, weighted=TRUE, SE=T)  




### Directed Network Example ----

anim1_grps <- readr::read_csv("data/anim1_grps.csv")
anim1 <- readr::read_csv("data/anim1.csv")

anim1 <- as.matrix(anim1)
rownames(anim1)<-colnames(anim1)

anim1 # note the order of ids, has to match traits
traits <- as.numeric(factor(anim1_grps$group))  # ensure this is in same order as matrix


# visualize
library(igraph)
g <- graph_from_adjacency_matrix(anim1, mode=c("directed"), weighted=T)
g <- set.vertex.attribute(g, 'group', value = anim1_grps$group)
colors <- c("red","yellow","cyan","green", "magenta","mistyrose","gray79")
g <- set.vertex.attribute(g, 'color', value = colors[traits])

plot(g, edge.arrow.size=.1) 



# binarize matrices for comparison with igraph
mat1 <- anim1
mat1[mat1>0]<-1
mat1

g1 <- graph_from_adjacency_matrix(mat1, mode=c("directed"))
g1 <- set.vertex.attribute(g1, 'group', value = anim1_grps$group)
g1 <- set.vertex.attribute(g1, 'color', value = colors[traits])

plot(g1, edge.arrow.size=.2)  # notice many edges do exist between groups as well as within


## "Thresholding"
# e.g. assign "2" as min value (remove edges == 1)
# in this case lose a lot of information

anim1
hist(reshape2::melt(anim1)[,3],10)  # most data is just 1 observed instance....

anim2 <- anim1
anim2[anim2<2] <- 0
anim2

g2 <- graph_from_adjacency_matrix(anim2, mode=c("directed"),weighted=T)
g2 <- set.vertex.attribute(g2, 'group', value = anim1_grps$group)
g2 <- set.vertex.attribute(g2, 'color', value = colors[traits])

plot(g2, edge.arrow.size=.2)  # patterns revealed much more easily


##

is.directed(g)
is.directed(g1)
is.directed(g2)


#  side by side plots
par(mfrow=c(1,3))
plot(g, main="weighted", edge.arrow.size=.1)
plot(g1, main="binarized", edge.arrow.size=.1)
plot(g2, main="thresholded - weighted", edge.arrow.size=.1)
par(mfrow=c(1,1))


## igraph assortativity - igraph, binary
assortativity_nominal(g1, traits, directed = T)  #.225

# Test for assortment as binary network
assortment.discrete(anim1,traits,weighted=FALSE)  # agrees with igraph, also get 'mixing matrix'.

# Test for assortment as weighted network
assortment.discrete(anim1,traits,weighted=TRUE)  #.332  

# Test for assortment as weighted & thresholded network
assortment.discrete(anim2,traits,weighted=TRUE)  #.619   

# To get jackknifed standard errors of assortativity coefficient.
assortment.discrete(anim1,traits, weighted=FALSE, SE=T)$se
assortment.discrete(anim1,traits, weighted=TRUE, SE=T)$se
assortment.discrete(anim2,traits, weighted=TRUE, SE=T)$se



#### Assortativity for Continuous Vertex Values ----

ages <- anim1_grps$age


## igraph assortativity - igraph, binary
assortativity(g1, ages, directed = T)  #.047

# Test for assortment as binary network
assortment.continuous(anim1, ages, weighted=FALSE)  #.047  - same as igraph

# Test for assortment as weighted network
assortment.continuous(anim1, ages, weighted=TRUE)  #.049

# Test for assortment as weighted & thresholded network
assortment.continuous(anim2, ages, weighted=TRUE)  #.000   

# To get jackknifed standard errors of assortativity coefficient.
assortment.continuous(anim1, ages, weighted=FALSE, SE=T)$se
assortment.continuous(anim1, ages, weighted=TRUE, SE=T)$se
assortment.continuous(anim2, ages, weighted=TRUE, SE=T)$se



### Assortativity by Degree ----

# essentially uses 'degree' (minus 1) as the numerical value
# tests if well-connected individuals are connected to other well-connected individuals

assortativity_degree(g, directed = T)  #.014

assortativity_degree(
  graph_from_adjacency_matrix(birds1, mode=c("undirected"), weighted=T),
  directed=F)  # -.04





#### Significance Testing ----


### Node Permutation Method

# Phenotypic values of nodes are randomized while maintaining the 
# edge structure of the network. 
# Calculate the proportion of randomized values that are larger 
# than the observed values.
# (If disassortment - proportion of values less than observed)

birds1 # note the order of ids, has to match traits
traits <- as.numeric(factor(birds1_sps$species))  # ensure this is in same order as matrix

# visualize
library(igraph)
g <- graph_from_adjacency_matrix(birds1, mode=c("undirected"), weighted=T)
g <- set.vertex.attribute(g, 'species', value = birds1_sps$species)
colors <- c("red","yellow","cyan","green")
g <- set.vertex.attribute(g, 'color', value = colors[traits])
plot(g)

# Test for assortment as weighted network
assortment.discrete(birds1, traits, weighted=TRUE)  #.797 

obs.r <- assortment.discrete(birds1, traits, weighted=TRUE)$r


# one permutation
set.seed(1)
traits.perm <-sample(traits)
assortment.discrete(birds1, traits.perm, weighted=TRUE)  # -.172

#  side by side plots
par(mfrow=c(1,2))
g.perm1 <- g
g.perm1 <- set.vertex.attribute(g.perm1, 'color', value = colors[traits.perm])
plot(g, main="original data")
text(1, 1, "r = .797")
plot(g.perm1, main="permuted data")
text(1, 1, "r = -.172")
par(mfrow=c(1,1))


# many permutations
nperms <- 5000
res <- vector('list',nperms)
for(i in 1:nperms){ res[[i]] <- assortment.discrete(birds1, sample(traits), weighted=TRUE)$r }
unlist(res)
hist(unlist(res), 30, xlim=c(-.5,1))
abline(v=obs.r, col='red',lty=3)

# p-value
sum(unlist(res)>=obs.r) / nperms # p = .000




### Other methods:

## 1. If sampling issues, could bootstrap the raw data. (covered elsewhere)



## 2. Use QAP to compare similarity matrix of group membership with edge weight sociomatrix.
# e.g. (Wey et al., 2008, Wiszniewski et al., 2010).
# has limitations when sampling is biased.

# create a comembership matrix from 2-column list
table(birds1_sps)
bg <- graph.incidence(table(birds1_sps))
pr <- bipartite.projection(bg) 
comem <- get.adjacency(pr$proj1,sparse=F,attr="weight")
comem

## Use 'sna' package to run QAP correlation and sig test
sna::gcor(birds1, comem)  # r=.637

padg.qap <- sna::qaptest(list(birds1, comem), sna::gcor, g1=1, g2=2, reps = 10000)
summary(padg.qap)
plot(padg.qap)  



### Try for yourself with the 'bird2' datasets.
readr::read_csv("data/birds2.csv")
readr::read_csv("data/birds2_sps.csv")

