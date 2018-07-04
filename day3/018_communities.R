###   Community Detection Methods


## Robustness in community detection algorithms.

library(igraph)

g <- make_graph("Zachary")
g

## Community Detection Algorithms

# algorithms aim to identify groups consisting of densely connected nodes
# these groups have high density of connections within groups and fewer connections between groups.



## Built-in community detection algorithms in igraph

# Edge-betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially 
# Community membership calculated at each step
# measure of best-fit is modularity 


ceb <- cluster_edge_betweenness(g)
#ceb <- cluster_edge_betweenness(g, weights=NULL) #ignore weights

plot(ceb, g)

membership(ceb) # community membership 

modularity(ceb)                 
# gives modularity for graph partitioning

# high modularity = dense connections within, sparse connections between communities.

igraph::crossing(ceb, g) # if TRUE-  that node connects across communities



## Greedy Methods 
cfg <- cluster_fast_greedy(as.undirected(g))
cfg
plot(cfg, as.undirected(g))



# another way of visualizing
V(g)$community <- cfg$membership
colors <- adjustcolor( c("tomato", "khaki1", "yellowgreen"), alpha=.6)
plot(g, vertex.color=colors[V(g)$community])


## Louvain Method

clv <- cluster_louvain(as.undirected(g))
clv
colors <- adjustcolor( c("tomato", "khaki1", "yellowgreen","dodgerblue","pink"), alpha=.6)
V(g)$community <- clv$membership
plot(g, vertex.color=colors[V(g)$community])




# there are many other methods:

# cluster_edge_betweenness(g)
# cluster_fast_greedy(g) 
# cluster_louvain()
# cluster_spinglass(g) 
# cluster_label_prop(g)
# cluster_optimal(g)
# cluster_walktrap(g)
# cluster_infomap(g)


par(mfrow=c(2,4)) #2 rows, 4 cols
set.seed(11) # fix everything
layoutg <- layout.fruchterman.reingold(g) # set layout
colors <- adjustcolor( c('tomato', 'khaki1', 'yellowgreen','dodgerblue',
                         'grey35','orange'), alpha=.6) # possible colors


plot(g, vertex.color=colors[cluster_edge_betweenness(g)$membership],layout=layoutg,
     main = "edge_betweenness")
plot(g, vertex.color=colors[cluster_fast_greedy(g)$membership],layout=layoutg,
     main = "fast_greedy")
plot(g, vertex.color=colors[cluster_louvain(g)$membership],layout=layoutg,
     main = "louvain")
plot(g, vertex.color=colors[cluster_spinglass(g)$membership],layout=layoutg,
     main = "spinglass")
plot(g, vertex.color=colors[cluster_label_prop(g)$membership],layout=layoutg,
     main = "label_prop")
plot(g, vertex.color=colors[cluster_optimal(g)$membership],layout=layoutg,
     main = "optimal")
plot(g, vertex.color=colors[cluster_walktrap(g)$membership],layout=layoutg,
     main = "walktrap")
plot(g, vertex.color=colors[cluster_infomap(g)$membership],layout=layoutg,
     main = "infomap")

par(mfrow=c(1,1)) #back to normal


### A tangent:

# these colors are not uniform across nodes because each algorithm assigns 
# numbers to memberships in a random fashion. 

# we could identify nodes that are consistently in  the same group across
# all groups and then use that as the basis of colors across

# the below isn't perfect - but helps us get an idea...

# nodes 13/18 always in same group
# nodes 15/16 always in same group
# nodes 25/26 always in same group
# nodes 6/17 always in same group

# little helper function
getcolor<-function(x){
notx <- setdiff(1:6, c(x[13],x[15],x[25],x[6]))
mycolors <- c('tomato', 'khaki1', 'yellowgreen','dodgerblue','grey35','orange','purple')
com <- c(x[13],x[15],x[25],x[6],notx)
xdf <- data.frame(com = com, color = mycolors[1:length(com)])
return(as.character(xdf$color[match(x, xdf$com)]))
}


par(mfrow=c(2,4)) #2 rows, 4 cols
set.seed(11) # fix everything

plot(g, vertex.color=getcolor(cluster_edge_betweenness(g)$membership),layout=layoutg,
     main = "edge_betweenness")
plot(g, vertex.color=getcolor(cluster_fast_greedy(g)$membership),layout=layoutg,
     main = "fast_greedy")
plot(g, vertex.color=getcolor(cluster_louvain(g)$membership),layout=layoutg,
     main = "louvain")
plot(g, vertex.color=getcolor(cluster_spinglass(g)$membership),layout=layoutg,
     main = "spinglass")
plot(g, vertex.color=getcolor(cluster_label_prop(g)$membership),layout=layoutg,
     main = "label_prop")
plot(g, vertex.color=getcolor(cluster_optimal(g)$membership),layout=layoutg,
     main = "optimal")
plot(g, vertex.color=getcolor(cluster_walktrap(g)$membership),layout=layoutg,
     main = "walktrap")
plot(g, vertex.color=getcolor(cluster_infomap(g)$membership),layout=layoutg,
     main = "infomap")

par(mfrow=c(1,1)) #back to normal




### The 'modMax' library has 38 different alogrithms !
library(modMax)
modMax::spectralOptimization((igraph::get.adjacency(g)))
modMax::greedy((igraph::get.adjacency(g)))
modMax::louvain((igraph::get.adjacency(g))) #very good for very large networks




## Girvan-Newman Edge-Betweenness Algorithm

# a divisive algorithm
# at each step the edge with the highest betweenness is removed from the graph. 
# idea is that high edge betweenness edges are those that connect different communities
# Compute the modularity of the graph at each step. 
# Use graph with highest value of modularity.
# M Newman and M Girvan: Finding and evaluating community structure in networks, Physical Review E 69, 026113 (2004)

ceb <- cluster_edge_betweenness(g)

# can use this to look at modularity at each step of algorithm (should you really want to):
mods <- sapply(0:ecount(g), function(i){
  g2 <- delete.edges(g, ceb$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  modularity(g,cl)
})

plot(mods, pch=16, type="l")



#####  Testing in vs out degree ties across all communities

# one way to confirm the utility of community detection can be to look at 'internal' vs 'external' ties
# i.e. a Wilcoxon rank-sum test on the "internal" and "external" degrees  
# Null hypothesis is no difference between the number of "internal" and "external" edges 
#
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

# e.g. Louvain method
clv <- cluster_louvain(g)
plot(clv,g)

result<-vector('list',length(clv))
for (i in 1:length(clv)){
  vs <- (membership(clv)==i)
  result[[i]] <-community.significance.test(g,vs)
}

result




### Robustness of Network Detection ----

## Method 1. Bootstrapping


# Lusseau, D., Whitehead, H., and Gero, S. (2008).
# Incorporating uncertainty into the study of animal social networks. 
# Anim. Behav. 75, 1809-1815. doi: 10.1016/j.anbehav.2007.10.029



#Following (Lusseau et al., 2008), to assess confidence in community membership assignment 

# 1. Bootstrapped our original data with replacement N times.
# 2. Each replicate had the same total number of observations as the original data. 
# 3. For each bootstrap replicate, reassig community membership. 
# 4. Produce a community comembership matrix.
# 5. Use comembership matrix to determine groupings via NMDS, tSNE, cluster analysis, community detection etc.


library(igraph)
library(tidyverse)
library(plyr)
library(MASS)

# some data
events <- read_csv("data/events.csv")
head(events)


# make weighted edgelist
library(plyr)
df.sum <- ddply(events, .(id1=events$id1, id2=events$id2), nrow)
colnames(df.sum)[3]<-"weight"

# make weighted undirected graph object
gg <- graph.data.frame(as.matrix(df.sum),directed=F)
gg

# perform N-G community detection.
gg.ceb <- cluster_edge_betweenness(gg)
gg.ceb
gg.ceb$membership

cm <- gg.ceb$membership
names(cm)<-V(gg)$name
cm


colors <- adjustcolor( c('tomato', 'khaki1', 'yellowgreen','dodgerblue',
                         'pink','orange','grey88','purple','blue',
                         'green'), alpha=.6) # possible colors

plot(gg, vertex.color=colors[cm])


## threshold for better visualization ...
df.sum
df.sum1 <- df.sum[df.sum$weight>1,] #only include if more than 1 observation


# make weighted undirected graph object
gg1 <- graph.data.frame(as.matrix(df.sum1),directed=F)
gg1

# perform N-G community detection.
gg1.ceb <- cluster_edge_betweenness(gg1)
cm1 <- gg1.ceb$membership

#plot
plot(gg1, vertex.color=colors[cm1])  # how many communities?



## bootstrap data one time:

events[sample(1:nrow(events),nrow(events),T),] # sample WITH replacement.

dfX <- events[sample(1:nrow(events),nrow(events),T),] 
dfX.sum <- ddply(dfX, .(id1=dfX$id1, id2=dfX$id2), nrow)
colnames(dfX.sum)[3]<-"weight"
ggX <- graph.data.frame(as.matrix(dfX.sum),directed=F)
ggX.ceb <- cluster_edge_betweenness(ggX)
cmX <- ggX.ceb$membership
plot(ggX, vertex.color=colors[cmX])
names(cmX)<-V(ggX)$name
cmX


## redo bootstrapping 1000 times and store membership vector results
nboot<-1000
res <- vector('list', nboot)
res.mod <- vector('list', nboot)
for(i in 1:nboot){
dfX <- events[sample(1:nrow(events),nrow(events),T),] 
dfX.sum <- ddply(dfX, .(id1=dfX$id1, id2=dfX$id2), nrow)
colnames(dfX.sum)[3]<-"weight"
ggX <- graph.data.frame(as.matrix(dfX.sum),directed=F)
ggX.ceb <- cluster_edge_betweenness(ggX)
cmX <- ggX.ceb$membership
names(cmX)<-V(ggX)$name
res.mod[[i]] <- modularity(ggX.ceb)
res[[i]]<-cmX
}

# look at results of bootstrapping:
res
res.mod
psych::describe(unlist(res.mod))
hist(unlist(res.mod),breaks=20)
# create co-membership matrix for one of these....
res1 <- res[[1]]

bm <- table(names(res1),res1)
bg <- graph.incidence(bm)
pr <- bipartite.projection(bg) 
get.adjacency(pr$proj1,sparse=FALSE,attr="weight")

# do for all 1000
comem <- function(x){
bm <- table(names(x),x)
bg <- graph.incidence(bm)
pr <- bipartite.projection(bg) 
get.adjacency(pr$proj1,sparse=FALSE,attr="weight")
}

comem.l <- lapply(res, comem)

lapply(comem.l, dim) # checking dimensions of all matrices

# my little check to make sure all matrices have same dimensions
all.identical <- function(l) all(mapply(identical, head(l, 1), tail(l, -1)))
all.identical(comem.l) #FALSE - cannot just add

# Reduce('+', comem.l)   # if they were all identical - could add like this.


# melt matrices, sum up, then create comembership matrix
#do.call('rbind', Map(cbind, lapply(comem.l, reshape2::melt), boot = 1:nboot))

library(data.table)
comem.dt <- data.table::rbindlist(Map(cbind, lapply(comem.l, reshape2::melt), boot = 1:nboot))
comem.dt.sum <- comem.dt[, list(Total = sum(value)), by=list(Var1, Var2)]
  

comem.matrix <- reshape2::acast(comem.dt.sum, Var1~Var2, value.var="Total")
comem.matrix

# NMDS visualization of comembership matrix
library(MASS)
d <- dist(comem.matrix) 
fit <- isoMDS(d, k=20)
fit 

comem.df <- data.frame(x = fit$points[,1], y = fit$points[,2], name = row.names(comem.matrix))

library(ggrepel)
library(ggplot2)
ggplot(comem.df, aes(x,y,label=name)) + geom_text_repel()


# threshold matrix
mat <- comem.matrix
mat[mat<500]<-0 # do in this order !!!
mat[mat>=500]<-1
mat
mat.g <- graph_from_adjacency_matrix(mat, mode=c("undirected"))
mat.cb <- cluster_edge_betweenness(mat.g)
mat.v <- mat.cb$membership
names(mat.v) <- V(mat.g)$name
mat.v
plot(mat.cb)

# make visualization
comem.g <- graph_from_adjacency_matrix(comem.matrix, 
                                       mode=c("undirected"), 
                                       weighted=TRUE)


# for edges
edge_col <- function(x) {
  paste0("gray",ceiling(scales::rescale(x, to=c(99,1))))
}

edge_col(E(comem.g)$weight)

plot(comem.g, vertex.color=colors[mat.v],
     edge.color = edge_col(E(comem.g)$weight),
     edge.width = log(log(E(comem.g)$weight+1)+1)
)


### Sampling Issues

# Shizuka & Farine have raised issues about these methods when there
# is concern over sampling bias - i.e. when observations over all animals
# may not be uniform.

# robust community detection methods are suggested for that
