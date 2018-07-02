
### Network Graph Models - 1


# Reference: Chapter 5: Kolaczyk & Csardi 



## 1. Classical Random Graphs
## 2. Small World Graphs
## 3. Preferential Attachment Graphs




### 1. Classical Random Graph Models ----


# Erdos-Renyi - 

library(igraph)

# Bernoulli:
g.er <- erdos.renyi.game(100, .02) # probablity of .02 of an edge between each pair of vertices

plot(g.er, layout=layout.fruchterman.reingold(g.er), vertex.size=0)

# ER graphs have a large component
table(sapply(decompose.graph(g.er), vcount))

# degree distribution is typically Poisson
mean(degree(g.er))
var(degree(g.er))
hist(degree(g.er), col="lightblue", xlab="Degree", ylab="Frequency", main="")

# low average path length (few vertices on shortest paths)
average.path.length(g.er)
diameter(g.er)

# low clustering
transitivity(g.er)

# directed = T
g.er1 <- erdos.renyi.game(100, .02, directed=T) #
plot(g.er1, layout=layout.fruchterman.reingold(g.er), vertex.size=0, edge.arrow.size=.3)


# Another classical E-R: specify number of vertices and edges.
g.er2 <- erdos.renyi.game(100, 80, type = "gnm") #
plot(g.er2, layout=layout.fruchterman.reingold(g.er), vertex.size=0)






### Random graphs with fixed degree sequence -

# e.g. graph with 8 vertices, 4 having 2 edges, 4 having 3 edges
degs <- c(2,2,2,2,3,3,3,3)
g1 <- degree.sequence.game(degs, method="vl")
g2 <- degree.sequence.game(degs, method="vl")


plot(g1, layout=layout.fruchterman.reingold(g1), vertex.label=NA)
plot(g2, layout=layout.fruchterman.reingold(g2), vertex.label=NA)

graph.isomorphic(g1,g2) #FALSE - g1 and g2 are different graphs




## you may wish to generate graphs of a given size with specific degree sequences

## Exponential degree distribution
degs <- sample(1:100, 100, replace=TRUE, prob=exp(-0.5*(1:100)))
if (sum(degs) %% 2 != 0) { degs[1] <- degs[1] + 1 } # to make sure degree count is even
g3 <- sample_degseq(degs, method="vl")
plot(g3, layout=layout.fruchterman.reingold(g3), vertex.label=NA, vertex.size=6)


## Power-law degree distribution
degs <- sample(1:100, 100, replace=TRUE, prob=(1:100)^-2)
if (sum(degs) %% 2 != 0) { degs[1] <- degs[1] + 1 } # to make sure degree count is even
g4 <- sample_degseq(degs, method="vl")
plot(g4, layout=layout.fruchterman.reingold(g4), vertex.label=NA, vertex.size=6)



## Make a graph with same degree sequence as an observed graph.

library(igraphdata)
data(yeast)
yeast

degs<-degree(yeast)
fake.yeast <- degree.sequence.game(degs, method=c("vl"))
plot(fake.yeast, layout=layout.fruchterman.reingold(fake.yeast), vertex.label=NA, vertex.size=1) # ugh!

diameter(yeast)
diameter(fake.yeast)

transitivity(yeast)
transitivity(fake.yeast)  # seems that the simulated graph is much different in terms of clustering
                          # to the original raw graph







### 2. Small World Graph Models ----

# Watts & Strogatz - wanted high clustering and small distances beween nodes
# Classical ER models do not have this property
# this works via rewiring edges from an original structure
# one end of each edge is independently and with probability p rewired to another vertex
# care is made to avoid loops and multi-edges


g.ws <- sample_smallworld(dim = 1, size = 25, nei = 5, p = .05)
# 25 vertices, neighborhoods of size 5,  rewiring probability of .05
plot(g.ws, layout=layout.fruchterman.reingold(g.ws), vertex.label=NA, vertex.size=6)

diameter(g.ws)
average.path.length(g.ws)
transitivity(g.ws)




## note there is a simple way of rewiring graphs in igraph

rewire(g.ws, each_edge(p = .1, loops = FALSE))  #rewires edges with prob p
rewire(g.ws, with = keeping_degseq(niter = vcount(g.ws))) #niter is number of rewiring trials




### 3. Preferential Attachment Graph Models ----

# network grows over time with edges connecting to individuals preferentially
# e.g. well connected nodes are preferentially attached to
# discrete time step model 
# common in many large networks


# Barabasi-Albert model
g.ba <- sample_pa(100, directed = F) # 100 vertices
plot(g.ba, layout=layout.fruchterman.reingold(g.ba), vertex.label=NA, vertex.size=6)






#### Assessing Significance of Network Graph Characteristics using these Models: ----

# General aim is to produce 1000s of randomized graphs that share some
# fundamental characteristics of original graph
# then ask how surprising is observed network statistic compared to randomized graphs

# important to consider how well the randomized graphs model key features of original graph




## Example: Assessing the number of communities in a network.


data("karate")

#observed community membership
kc <- fastgreedy.community(karate)
length(kc)
sizes(kc)
membership(kc)
plot(kc,karate)
dendPlot(kc,mode="phylo")


nv <- vcount(karate)    #number of vertices
ne <- ecount(karate)    #number of edges
degs <- degree(karate)  #degree distribution

ntrials <- 1000

# 1000 graphs of same size and edge count
num.comm.rg <- numeric(ntrials)
for( i in 1:ntrials){
  g.rg <- erdos.renyi.game(nv,ne,type="gnm")
  c.rg <- fastgreedy.community(g.rg)
  num.comm.rg[i] <- length(c.rg)
}


# 1000 graphs of same degree distribution
num.comm.grg <- numeric(ntrials)
for( i in 1:ntrials){
  g.grg <- degree.sequence.game(degs, method="vl")
  c.grg <- fastgreedy.community(g.grg)
  num.comm.grg[i] <- length(c.grg)
}


# summarize results
rslts <- c(num.comm.rg, num.comm.grg)
indx <- c(rep(0,ntrials), rep(1,ntrials))
counts <- table(indx, rslts)/ntrials
barplot(counts, beside=T, col=c('blue','red'),
        ylim = c(0, .75),
        xlab="number of communities",
        ylab="relative frequency",
        legend=c("Fixed Size", "Fixed Degree Sequence"))


# pvalues - proportion of times simulated values fell below or equal to observed value
# one-tailed
sum(num.comm.rg<=3)/ntrials  #p = 0.008
sum(num.comm.grg<=3)/ntrials  #p = 0.011

# could conclude that there is something other than simply density/distribution of edges that explains why there are 3 communities in the original data.








