#### Cliques continued...----

# kcliques
# nclans
# kplexes


### Data
## Zachary's karate club
library(igraph)
g <- make_graph("Zachary")
g
V(g)$name <- 1:34

plot(g)
GNEL <- igraph::igraph.to.graphNEL(g)  #conver to graphNEL object (for Kcliques function)
GNEL


### N-cliques (sometimes K-cliques)   - Wasserman & Faust

# all individuals of a clique must connect to all other members within N steps
# so you may not connect directly to all members, but they are 'friends-of-friends'
# these intermediary friends may be or may not be in the clique themselves

# source("https://bioconductor.org/biocLite.R")
# biocLite("RBGL")
library(RBGL)


RBGL::kCliques(GNEL)  # will return list of cliques for different lengths of K.


# one way of visualizing first K-clique of 2....
library(igraph)
coords <- layout.auto(g)
cl <- RBGL::kCliques(igraph.to.graphNEL(g))
cl


k <- 2
clSel <- cl[[paste0(k, '-cliques')]][[1]]
all(print(distances(induced_subgraph(g, clSel))) <=k )  #check

plot(
  g, 
  layout = coords,
  vertex.shape = "none",
  vertex.label.color = ifelse(V(g) %in% clSel, "red", "darkgrey"), 
  edge.color = ifelse(tail_of(g, E(g)) %in% clSel & head_of(g, E(g)) %in% clSel, "orange", "#F0F0F099"),
  vertex.size = .5, 
  edge.curved = 1
)


# could extend this visualization to include distinct clusters...
cl
clSel1 <- cl[[paste0(k, '-cliques')]][[9]]  #just take this one as a quick example.

plot(
  g, 
  layout = coords,
  vertex.shape = "none",
  vertex.label.color = ifelse(V(g) %in% clSel, "red", 
                        ifelse(V(g) %in% clSel1, "blue",
                        "darkgrey")), 
  edge.color = ifelse(tail_of(g, E(g)) %in% clSel & head_of(g, E(g)) %in% clSel, "orange", 
                 ifelse(tail_of(g, E(g)) %in% clSel1 & head_of(g, E(g)) %in% clSel1, "dodgerblue", 
                    "#F0F0F099")),
  vertex.size = .5, 
  edge.curved = 1,
  main = "K-clique analysis of Zachary network (k=2)"
)


## overlap analysis on k=2...
# - it could be possible to then use these cliques for overlap analysis.



### N-clans

# in N-clans, friends-of-friends connections cannot go via nodes who are not members of the clique
# function to identify n-clans
# source: https://stackoverflow.com/questions/40088150/find-n-cliques-in-igraph

nclan <- function(g,n){
  g <- as.undirected(g)
  E(g)$weight <- 1 #just in case g has weights - does not modify original graph
  ncliques <- kCliques(ugraph(igraph.to.graphNEL(g))) #get cliques
  n.cand <- ncliques[[n]] #n-clique candidates to be an n-clan
  n.clan <- list() #initializes a list to store the n-clans
  n.clan.i <- 1 #initializes a list pointer
  for (n.cand.i in 1:length(n.cand)){ #loop over all of the candidates
    g.n.cand <- induced_subgraph(g,n.cand[[n.cand.i]]) #get the subgraph
    if (diameter(g.n.cand)<=n){ #check diameter of the subgraph
      n.clan[[n.clan.i]] <- n.cand[[n.cand.i]] #add n-clan to the list
      n.clan.i <- n.clan.i+1 #increment list pointer
    }
  }
  return(n.clan) #return the entire list
}

nclan(g,2) #  nclans for Zachary graph, for k=2  
cl$`2-cliques` # notice that cliques  2,5,7 are in ncliques but not nclans

layoutg<-layout.kamada.kawai(g)
plot(g, vertex.color="tomato", 
     edge.color="gray22", vertex.label.cex=.8, layout=layoutg)

V(g)$color<-ifelse(V(g)$name %in% cl$`2-cliques`[[7]], "gold", "tomato")
layoutg<-layout.kamada.kawai(g)
plot(g, 
     edge.color="gray22", vertex.label.cex=.8, layout=layoutg)



###  K-plexes    

# note - presented here but not sure function for code is working as it should...
# see: https://stackoverflow.com/questions/40088150/find-n-cliques-in-igraph


# another way of relaxing the maximal connected subgraph (clique) constraints
# consider individuals to be part of a clique if they inter-connect with N-k other individuals in the clique
# i.e. if 6 in a clique, a node could connect to 5 of the clique but not the 6th.

# ncliques/nclans tend to find long 'stringy' cliques
# k-plexes tends to find more numerous smaller connected cliques
# k-plexes tends to identify overlapping social circles

# g=graph, k=number of links not to have to connect to, m=clique.size

# note: if nodes have names switch k.cand rows.

kplex <- function(g,k,m){
  g.sym <- as.undirected(g) #to make sure that degree functions properly
  g.sym.degmk <- induced_subgraph(g.sym,igraph::degree(g.sym)>=(m-k)) #makes algorithm faster
  k.cand <- combn(V(g.sym.degmk)$name,m) #all candidate combinations with m members
  k.cand <- combn(V(g.sym.degmk),m) #all candidate combinations with m members
#  k.plex <- list() #initializes a list to store the k-plexes
  k.plex.i <- 1 #initializes a list pointer
  for (k.cand.i in 1:dim(k.cand)[2]){ #loop over all of the columns
    g.k.cand <- induced_subgraph(g.sym.degmk,k.cand[,k.cand.i]) #get the subgraph
    if (min(igraph::degree(g.k.cand))>=(m-k)){ #if minimum degree of sugraph is > m=k, k-plex!
      k.plex[[k.plex.i]] <- k.cand[,k.cand.i] #add k-plex to list
      k.plex.i <- k.plex.i+1 #increment list pointer
    }
  }
  return(k.plex) #return the entire list
}

plot(g)
k4 <- kplex(g, k=1, m=4) # may take a while
k4

#check:
k4[[11]]
subgraph.edges(g, eids=E(g)[inc(k4[[11]])], delete.vertices = TRUE)
plot(subgraph.edges(g, eids=E(g)[inc(k4[[11]])], delete.vertices = TRUE))

plot(g)




### K-cores   Seidman 1983 ----

#  A maximal subset of vertices such that each is connected to at least k others in the subset.

# i.e. in a 4 core, all members of that core are connected with at least four other members.

# the algorithm identifies the highest k-core and then will identify lower k-cores
# e.g. if maximal is 4, will then search for k-cores of 3, then 2...then 1....
# 

#  k-cores cannot overlap - but nodes can be members of e.g. k-core =4,3,2.
# but they will be assigned to their highest core level.

#sna way:
dat <- intergraph::asNetwork(g)
dat
sna::kcores(dat, mode = "graph")  # "digraph" for directed data

#igraph way:
igraph::coreness(g)
kc <-igraph::coreness(g)

library(sna)
gplot(dat,vertex.col=kc)
detach(package:sna)
detach(package:igraph)
library(igraph)
plot(g,vertex.color=kc)


## connections could be considered cohesive as long as individuals have at least n connections to the main groups
## kcore can be used to identify those individuals.


## cohesive subgroups may be present in higher level k-cores
## can use k-core procedure to identify the 'inner core' of a network, to then apply further subgroup analysis
## particularly useful when dealing with very large networks.




