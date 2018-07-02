### Structural Equivalence
###	Automorphic Equivalence
###	Regular Equivalence


## Structural Equivalence assesses similarity of actors in a network
# if two actors send and receive ties to the same third parties - they are structurally equivalent
# examines an actor's position in social space

# structurally equivalent actors tend to show homogeneity in attributes

# differs from cohesive subgroup, in subgroups actors tend to interact with each other
# structurally equivalent actors may not even know each other

# if two actors are completely structurally equivalent, impossible to tell (without labels) who is who in graph

# same logic for directed graphs, st.equiv if out-going/incoming ties from 3rd parties are similar
# same logic for valued graphs, weights of edges should be similar




### Profile Similarity ----

## Simplest way of considering structural equivalence

# rows of undirected networks
# rows and columns concatenated of directed networks
# being mindful of ith and jth entries! (reciprocal swapping is used)
# can then compare 'similarity' using a number of measures:
   # correlation
   # Euclidean distance
   # Hamming distance
   # Jacard's distance
   # etc.
# Repeat for every pair of actors and create a distance matrix.

library(igraph)



# Sampson's 1968 data on social relations in isolated monastery
# towards end of study several monks were expelled/resigned
# top 3 nominations for 'esteem' and 'disesteem'
# Gregory, Basil, Elias, Simplicius were expelled, John left voluntarily
# then Hugh, Boniface, Mark, Albert left, then Victor and Amand departed.
# then Romuald left.

library(amen)
disesteem <- amen::sampsonmonks[,,6]
esteem <- amen::sampsonmonks[,,5]

esteem.dat  <- network::network(esteem, matrix.type="adjacency", directed=T)
disesteem.dat  <- network::network(disesteem, matrix.type="adjacency", directed=T)

esteem.g <- intergraph::asIgraph(esteem.dat)
disesteem.g <- intergraph::asIgraph(disesteem.dat)
V(disesteem.g)$name <- V(esteem.g)$name <-rownames(esteem)

plot(esteem.g, edge.arrow.size=.4, 
     edge.color='gray22', 
     vertex.color = "dodgerblue",
     vertex.label.color = "black",
     layout = layout.kamada.kawai(esteem.g))


# Euclidian Distance.
# values close to 0 indicate structural equivalence

sna::sedist(esteem.dat, method = "euclidean") #euclidean: the euclidean distance
sna::sedist(esteem.dat, method = "hamming") #hamming: the Hamming distance
sna::sedist(esteem.dat, method = "gamma") #gamma: the gamma correlation
sna::sedist(esteem.dat, method = "correlation") #correlation: the product-moment correlation

gse <- sna::sedist(esteem.dat, method = "euclidean")

esteem.euc <-round(gse,2)
rownames(esteem.euc)<-colnames(esteem.euc)<-V(esteem.g)$name
esteem.euc


## multidimensional scaling
x <- cmdscale(as.dist(gse))
rownames(x) <- rownames(esteem)
colnames(x)<-c("var1","var2")
x
plot(x, cex=.4)
text(var2 ~ var1, labels=rownames(x),data=x, cex=0.7, font=2)



## hierarchical clustering
hc <- hclust(as.dist(gse), method="single")
plot(hc, labels = rownames(esteem))

clusterCut <- cutree(hc, 4)
clusterCut
split(rownames(esteem),clusterCut)


## this can be done in one step:
#Cluster based on structural equivalence
library(sna)
eq<-equiv.clust(esteem.dat)
plot(eq, labels = rownames(esteem))

eq<-equiv.clust(esteem.dat,
                method="euclidean",
                mode="digraph") #digraph directed, graph undirected
plot(eq, labels = rownames(esteem))



## pvclust
library(pvclust)
# calculates p-values for hierarchical clustering via multiscale bootstrap resampling. Hierarchical
#clustering is done for given data and p-values are computed for each of the clusters.

pvc <- pvclust(gse, method.hclust="average", nboot=1000, parallel=FALSE)
pvc
plot(pvc, labels = rownames(esteem))
pvrect(pvc, alpha = 0.95)

pvpick(pvc, alpha = 0.95)

#AU p-value (printed in red color in default) is the abbreviation of "approximately unbiased" p-value
#One can consider that clusters (edges) with high AU values (e.g. 95%) are strongly supported by data


#Correlation can be useful if data are valued as indicates similarity in strength, but problematic if density high/low






### Blockmodels ----

# Exploratory method to identify role similarity.

# Breiger et al 1975
#  "An Algorithm for Clustering Relational Data with Applications 
# to Social Network Analysis and Comparison with Multidimensional Scaling.
# Journal of Mathematical Psychology, 12: 328-383. 


# First step is to use an algorithm to determine block membership.

#library(concoR)
source("https://raw.githubusercontent.com/aslez/concoR/master/R/concoR.R")
diag(esteem)<-0
mat <- cor(esteem)

blks <- concor_hca(list(mat), p = 2) 
blks

# Visualize blockmodel
blk_mod <- sna::blockmodel(esteem.dat, blks$block) 

blk_mod   #relative densities within and between blocks
rownames(esteem)
sna::gden(esteem.dat) #.176 is average graph density

# if intra- or inter-block density is > graph average, some indication of 
# focus of ties between those blocks.

# e.g. block 4 mainly ties with itself-
# but some slight evidence of a little with block 2 (Victor/Mark)

# block 1 (opposition)  and block 3  (young turks) only direct esteem to themselves.

plot(blk_mod)
rownames(esteem) # to help id rows
#squares indicate ties to other individuals 
# - mostly should occur within blocks if good fit.


g <- igraph::graph_from_adjacency_matrix(esteem, mode=c("directed"), weighted=T)
igraph::V(g)$blocks <- blks$block
igraph::plot.igraph(g, vertex.color=igraph::V(g)$blocks, edge.arrow.size=.3) 


# Blockmodels are typically used to identify common characteristics of actors within a block
# or to describe individual positions/social groups

# e.g. here Amand, Basil, Elias and Simplius are outcasts.



## Other blockmodel algorithms -

# This algorithm is particularly powerful (may crash R though....)
#  ZIBERNA, Ales (2007): Generalized Blockmodeling of Valued Networks. Social Networks, Jan. 2007, vol. 29, no. 1, 105-126.

# force the alogrithm to put nodes into N blocks
# will find the best fit (based on one of several methods)
# can also tell algorithm to make complete blocks (or not) as much as possible

# library(blockmodeling)
# diag(esteem)<-NA
# 
# # A four block partition
# class4 <- optRandomParC(M=esteem, k=4, rep=10, approach="ss", blocks="com")
# 
# # A five block partition
# class5 <- optRandomParC(M=esteem, k=5, rep=10, approach="ss", blocks="com")
# 
# 
# # Visualize
# par(mfrow=c(1,2)) 
# plot(class4, main="")
# title("Four Block Partition")
# plot(class5, main="") 
# title("Five Block Partition")
# par(mfrow=c(1,1)) 


#############


### Relating Structural Equivalence to Outcome of Interest


# e.g. MR-QAP
# DV could be e.g. being expelled, adopting an innovation etc.
# DV could be structural equivalence classification itself.







#### Regular Equivalence ----

# here we are intereseted in finding individuals that have similar social roles
# which nodes are similar in the types and number of edges they have



# The REGE Algorithm
# only works on directed data
# must have at least one actor with either zero out-degree (a sink) or zero-indegree (a source)

# scores are difficult to interpret except 1 which equals perfect equivalence.

esteem1 <- esteem
esteem1[esteem1>=1]<-1 #dichotomize

g <- igraph::graph_from_adjacency_matrix(esteem1, mode=c("directed")) 
g
igraph::degree(g, mode=c("out"))
igraph::degree(g, mode=c("in"))
plot(g, edge.arrow.size=.3)


library(blockmodeling) # Reload the blockmodeling package

mat <- as.matrix(igraph::get.adjacency(g))

D<-REGE.for(M=mat)$E  #similarities
D<-REGD.for(M=mat)$E  #dissimilarities

plot.mat(mat, clu=cutree(hclust(d=as.dist(D),method="ward.D"),k=4))  

