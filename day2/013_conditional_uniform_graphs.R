
## Conditional Uniform Graphs (CUG)

# How do we know when some network measure in our observed network
# e.g. reciprocity, transitivity, betweenness, out-degree centralization, etc.
# is unexpectedly high or low ?



# 1. Calculate an observed value of some network metric.
# 2. Generate many networks that share some characteristic in common with the original network.
# 3. Compare the observed network metric with the same metric recalculate over all generated networks.


# What characteristics to simulate networks on ?

# a) size (number of nodes)
# b) number of edges
# c) the distribution of dyads


# Note that we don't produce all possible permutations of the graph, 
# we randomly sample from a uniform distribution of graphs

## Data

padgm <- read.csv("data/PADGETT.csv")    # marriage
padgb <- read.csv("data/PADGETT2.csv")   # business

# remove first column
padgm <- as.matrix(padgm[,-1])
padgb <- as.matrix(padgb[,-1])



## Use 'cug.test' from 'sna'
detach(package:igraph)
library(sna)

padgm.net <- network::network(padgm)
padgb.net <- network::network(padgb)

padgm.net
padgb.net


# a) Condition on Size - compare density

gden(padgb.net) #.125
plot(padgb.net)
gtrans(padgb.net) # .42

cug.den <- cug.test(padgb.net, gden, cmode="size",reps=5000)
cug.den
plot(cug.den)

# given its size, how extreme is the observed transitivity?
cug.tr0 <- cug.test(padgb.net, gtrans, cmode="size")
cug.tr0
plot(cug.tr0)



# b) Condition on Number of Edges (i.e. Density) 

# This question is effectively, 
# is there greater transitivity in the observed network than expected given its density?

gtrans(padgb.net) # .42
plot(padgb.net)

cug.tr1 <- cug.test(padgb.net, gtrans, cmode="edges")  
cug.tr1
plot(cug.tr1)


# For this example, we're calculating out-degree centralization
# remember out-degree centralization is a measure of how concentrated/skewed out-degree distribution is in the network
# in 'sna' you have to supply extra arguments to centralization to be specific
# so these are put in the FUN.arg=list() command at the end.

centralization(padgb.net, degree, cmode="outdegree") # .22

cug.cent <- cug.test(padgb.net, centralization, cmode="edges", FUN.arg=list(FUN=degree, cmode="outdegree"))
cug.cent
plot(cug.cent)




# c) Condition on Dyad Census

# this essentially compares to networks with the same size, density and reciprocity.
# the generated models will have approximately the same number of dyads that are:
# mutual (reciprocally directionally connected), unidirectional/asymmetric, null.

# Given the size, density and reciprocity of the original network, 
# how extreme is its level of transitivity?

gtrans(padgb.net) # .42
cug.tr2 <- cug.test(padgb.net, gtrans, cmode="dyad") 
cug.tr2
plot(cug.tr2)



# Look at the object in a bit more detail:

str(cug.tr2)


# Summary of Results for Transitivity:
Tvty <- c(cug.tr0$obs.stat, cug.tr1$obs.stat, cug.tr2$obs.stat)
PctG <- c(cug.tr0$pgteobs, cug.tr1$pgteobs, cug.tr2$pgteobs)
PctL <- c(cug.tr0$plteobs, cug.tr1$plteobs, cug.tr2$plteobs)
Tvty <- cbind(Tvty, PctG, PctL)
rownames(Tvty) <- c("Size", "Edges", "Dyads")
round(Tvty,3)

# For a network of the given size, the degree of transitivity is actually surprisingly small.
# However, for a network of the given density, the transitivity is actually suprisingly high.
# For a network of the given size, density and reciprocity, the transitivity is surprisingly high.


par(mfrow=c(1,3))
plot(cug.tr0, main="Transitivity \nConditioned on Size" )
plot(cug.tr1, main="Transitivity \nConditioned on Edges" )
plot(cug.tr2, main="Transitivity \nConditioned on Dyads" )
par(mfrow=c(1,1))




### An Extension for CUG tests:

### Using CUG tests to compare relationships between nodes and attributes ----


# Do individuals with higher tenure (more experience) have higher in-degree centrality?

advice <- read.csv("data/krack_advice.csv")[,-1]
attrs <- read.csv("data/krack_attr.csv")
net.advice <- network(advice)

set.vertex.attribute(net.advice,names(attrs),attrs)

net.advice
plot(net.advice)


# function to estimate correlation between in-degree and node attribute
indegCor <-  function(net,attr){
    require(sna)
    cor(degree(net,cmode="indegree"),attr)
}

cor(degree(net.advice,cmode="indegree"),attrs$TENURE) #.54

cug.test(net.advice,indegCor,cmode=c("dyad"),reps=500, FUN.args=list(attr = attrs$TENURE))

out <- cug.test(net.advice,indegCor,cmode=c("dyad"),reps=500, FUN.args=list(attr = attrs$TENURE))

out

plot(out)




############################################################################################


### Exercise 1 - Do same as above but for 'betweenness' using Florentine marriage network.



### Exercise 2 - # Complete CUG tests on the 'advice' network, testing if transitivity is greater than expected by chance,






advice <- read.csv("data/krack_advice.csv")     
advice <- as.matrix(advice[,-1])
net.advice <- network::network(advice)

# for networks of a given size, density, dyad census.
# Vary the number of networks simulated using the 'reps' argument.
cug0 <- cug.test(net.advice,gtrans,cmode=c("edges"),reps=500)
cug0
plot(cug0)


