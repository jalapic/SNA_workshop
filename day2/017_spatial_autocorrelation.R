### Spatial Autocorrelation

## As well as measuring assortativity coefficient, we can also assess spatial autocorrelation of nodes.


detach(package:statnet)
detach(package:sna)
detach(package:ergm.count)
detach(package:tergm)
detach(package:ergm.rank)
detach(package:ergm)
detach(package:networkDynamicData)
detach(package:networkDynamic)
detach(package:network)
library(igraph)

campnet <- as.matrix(read.csv("data/campnet.csv")[,-1])
campsex <- read.csv("data/campsex.csv")    
campattr <- read.csv("data/campattr.csv")   
colnames(campattr)[1]<-colnames(campsex)[1]<-"name"

camp.g <- igraph::graph_from_adjacency_matrix(campnet)
camp.g <- igraph::set_vertex_attr(graph = camp.g, name = "gender", value = campattr$Gender)
camp.g
vertex.attributes(camp.g)


### plot network
V(camp.g)$color <- V(camp.g)$gender #assign the gender attribute as the vertex color
V(camp.g)$color <- ifelse(V(camp.g)$color==1, "mistyrose", "dodgerblue") # assigning specific colors
plot.igraph(camp.g,vertex.label=NA,layout=layout.fruchterman.reingold, edge.arrow.size=.2,
            edge.color="gray42")


## A hypothesis we may have about these data - 
# Are there more ties between individuals of the same gender than we would expect by chance ?


## Diffusion:  dyad level --> node attributes  e.g. conservative beliefs propogate thru network
## Selection: node attributes --> dyad level e.g. individuals of same gender choose to become friends


# different methods used if node attribute is categorical or numerical.





#### Continuous Attributes.----


### Method 1:  Measure autocorrelation.

# Moran's I
# Geary's C
# these test whether the distribution of a node attribute is independent of spatial proximity or adjacency.


# Example: Do individuals with similar/dissimilar wealth levels form ties?
# Did Florentine marriages have anything to do with wealth ?


padgm <- as.matrix(read.csv("data/PADGETT.csv")[,-1])     
padgm.attr <- read.csv("data/PADGW.csv")
colnames(padgm.attr)[1]<-"name"

g <- igraph::graph_from_adjacency_matrix(padgm)
g <- igraph::set_vertex_attr(graph = g, name = "wealth", value = padgm.attr$WEALTH)
g

plot.igraph(g,layout=layout.fruchterman.reingold, edge.arrow.size=.2,
            edge.color="gray42", vertex.size = sqrt(V(g)$wealth)*3)


# maybe some negative autocorrelation - that wealthier families not marrying each other, but maybe marrying less wealthy families?


## Moran's I (ranges from -1 to 1 , similar to Pearson's r)
# -1 = negative autocorrelation
# +1 = positive autocorrelation
# 0 = no correlation (independent of each other)


x<-sna::nacf(padgm, V(g)$wealth, type="moran", mode="graph")
x[2]  #-0.3107353 this is Moran's I for a step/geodesic distance of "1" into the network
str(x)
plot(x,type="l")

# I = -0.31 therefore moderate negative autocorrelation.



## Signficance Testing is typically done with permutation testing.

# Node permutation: one permutation.
set.seed(10)
sna::nacf(padgm, sample(V(g)$wealth), type="moran", mode="graph")

set.seed(10)
sna::nacf(padgm, sample(V(g)$wealth), type="moran", mode="graph")[2]

# do this 1000 times
set.seed(10)
nperms <- 1000
results <- vector('list', nperms)
for(i in 1:nperms){
  results[[i]] <- sna::nacf(padgm, sample(V(g)$wealth), type="moran", mode="graph")[2]
}

results

#pvalue - 1-tailed interested in how often get value lower than observed
sum(unlist(results)<=x[2])  # x[2] was our obseved value.  95
sum(unlist(results)<=x[2])/nperms  #p = 0.095  


# plot results
ggplot(data.frame(res=unlist(results)), aes(x=res)) + 
  geom_histogram(color='white') + 
  geom_vline(xintercept = x[2], color='red', lty=2, lwd=1)







## Geary's C  ranges from 0 to 2.
# <1 = positive autocorrelation
# ~1 = no autocorrelation (network/attributes are independent)
# >1 = negative autocorrelation

x <- sna::nacf(padgm, V(g)$wealth, type="geary", mode="graph")
x[2] ## 1.683607
str(x)
plot(x,type="l")

# C = 1.68  therefore moderate negative autocorrelation.


## Exercise:  perform node permutation test for Geary's C.



