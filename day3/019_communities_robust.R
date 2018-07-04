
### Shizuka & Farine's Robust Community Detection Method ----
# https://www.sciencedirect.com/science/article/pii/S0003347215004480

# 1. Perform community detection on original data (e.g. using fast-greedy method)
# 2. Calculate  modularity value Q of the proportion of edge weights that occurred within communities relative to random expectation
# 3. Bootstrap - resample observations of groups (or individual observations)
# 4. Build bootstrap replicate network using pairwise association indices
# 5. Apply community detection to replicate network
# 6. Construct n x n matrix M - a comembership matrix of community assignment
# 7. Construct a co-presence matrix C - 1 if both nodes present in replicate, 0 if both not present
# 8. Construct n x n co-membership matrix P where values are proportion of trials
#    where both i/j present that they were in the same matrix
# 9. Calculate rc: the assortativity of values of matrix P with the original assignment of communities.
 
# rc = 1 if all bootstrap replicates have same community assignments as original
# rc = 0 if bootstrap replicate community assignment is same as random networks
# rc < 0 if bootstrap replicate community assignment is anti-correlated with original network

# Shizuka & Farine recommend values of rc > .5 represent robust community structure


library(igraph)
library(asnipe)
library(assortnet)

# Data

# In "group-by-individual" format, where groups = rows, individuals = columns
# cell value is 1 if the individual is seen in the group and 0 otherwise. 
# A 'group' here is defined as a set of individuals observed in close proximity during a given observation data point.

# e.g. 'gbi' dataset is of different tits forming flocks at bird feeders
# 151 individuals of 5 passerine species in Wytham Woods, UK: 
# 78 blue tits (Cyanistes caeruleus)
# 7 coal tits (Periparus ater)
# 51 great tits (Parus major)
# 11 marsh tits (Poecile palustris)
# 3 nuthatches (Sitta europaea)
# 1 individual of unknown species.


data("group_by_individual")
head(gbi) 


 
# 1. Calculate network
network <- get_network(gbi,data_format="GBI", association_index="SRI") #asnipe format
  
# 2. Calculate community membership of the observed network
community.observed <- fastgreedy.community(graph.adjacency(network,mode="undirected",weighted=TRUE))
community.observed$membership
modularity(community.observed)

# quick plot:
range(community.observed$membership)
colfunc <- colorRampPalette(c("purple", "orange"))
g <- graph_from_adjacency_matrix(network, mode=c('undirected'), weighted=T)
V(g)$color <- adjustcolor(colfunc(4)[community.observed$membership], .5) #set color and alpha
plot(g, vertex.cex = .4, vertex.label.cex=.7, edge.color = 'gray88')



# 3. Main bootstrapping method: i) Bootstrap the observed data, ii) recalculate the network, 
#    iii) recalculate community membership, iv) check if both individuals are observed
  
n.bootstraps <- 500

# Create space to store results from bootstraps
network.community <- matrix(0,ncol(gbi),ncol(gbi))
network.present <- matrix(0,ncol(gbi),ncol(gbi))
network.modularity <- vector('list',n.bootstraps)
  


# To illustrate the bootstraps of the sampling periods
set.seed(10)
gbi.boot <- gbi[sample(1:nrow(gbi),nrow(gbi),replace=TRUE),]
table(row.names(gbi.boot))


  for (i in 1:n.bootstraps) {
    # This step bootstraps the sampling periods
    gbi.boot <- gbi[sample(1:nrow(gbi),nrow(gbi),replace=TRUE),]
    network.boot <- get_network(gbi.boot,data_format="GBI", association_index="SRI")
    
    # This step calculates the community membership from the bootstrapped network
    community.boot <- fastgreedy.community(graph.adjacency(network.boot,mode="undirected",weighted=TRUE))
    
    # store results of modularity
    network.modularity[[i]] <- modularity(community.boot)
    
    # This step adds 1 to any dyads in the same community (create matrix M)
    network.community <- network.community + outer(community.boot$membership, community.boot$membership,"==")
    
    # This step adds 1 to any dyads that are both present (in this case if they have at least 1 edge)
    # create matrix C
    network.present <- network.present + outer((rowSums(network.boot)>0),(rowSums(network.boot)>0),"*")
  }
  # End bootstrap
  

  # Calculate proportion of times observed in the same community
  P <- network.community/network.present
  P[!is.finite(P)] <- 0  # in case any cells are /0 leading to errors.
  
  # Calculate assortment from known community membership
  rc <- assortment.discrete(P,community.observed$membership)$r
  rc   #.99


# Modularity and CI for modularity  
nm <- unlist(network.modularity)
mean(nm)
Rmisc::CI(nm, ci = 0.95)



## Visualize Results  

diag(P) <- 0
P

g <- graph.adjacency(P, "undirected", weighted=T)
g

# for edges
edge_col <- function(x) {
  paste0("gray",ceiling(scales::rescale(x, to=c(99,1))))
}

edge_col(E(g)$weight)


plot(g, 
     edge.width=E(g)$weight, 
     edge.color = edge_col(E(g)$weight),
     vertex.label="", 
     vertex.size=5, 
     vertex.color=membership(community.observed),
     layout = layout_nicely(g))
  
    
    
    


### Example 2.   Golden-crowned Sparrows (Shizuka et al 2014)
sparrows3 <- readr::read_csv("data/Flock_Season3_Dryad.csv")

sparrows3 # we need to rework this data into GBI format.

d <- sparrows3[,c(4:13)]
table(d$Bird10)

unique(unlist(d))
length(unique(unlist(d))) #NA is not right either, so 27 animals in total.

d <- cbind(d,sample = 1:nrow(d))  #add in sample period id
d

library(tidyverse)
d <- d %>% gather(key,value,1:10) %>% dplyr::select(sample,value) %>% mutate(present = 1) %>% filter(value!="NA")
d %>% arrange(sample)

gbi.d <- reshape2::acast(d, sample~value, value.var = "present", fill=0)

network.d <- get_network(gbi.d,data_format="GBI", association_index="SRI") #asnipe format
community.observed.d <- fastgreedy.community(graph.adjacency(network.d,mode="undirected",weighted=TRUE))
community.observed.d$membership
modularity(community.observed.d)

# quick plot:
range(community.observed.d$membership)
colfunc <- colorRampPalette(c("purple", "orange"))
g.d <- graph_from_adjacency_matrix(network.d, mode=c('undirected'), weighted=T)
V(g.d)$color <- adjustcolor(colfunc(3)[community.observed.d$membership], .5) #set color and alpha
plot(g.d, vertex.cex = .4, vertex.label.cex=.7, edge.color = 'gray88')


n.bootstraps <- 500

# Create space to store results from bootstraps
network.community.d <- matrix(0,ncol(gbi.d),ncol(gbi.d))
network.present.d <- matrix(0,ncol(gbi.d),ncol(gbi.d))
network.modularity.d <- vector('list',n.bootstraps)


for (i in 1:n.bootstraps) {
  gbi.boot.d <- gbi.d[sample(1:nrow(gbi.d),nrow(gbi.d),replace=TRUE),]
  network.boot.d <- get_network(gbi.boot.d,data_format="GBI", association_index="SRI")
  community.boot.d <- fastgreedy.community(graph.adjacency(network.boot.d,mode="undirected",weighted=TRUE))
  network.modularity.d[[i]] <- modularity(community.boot.d)
  network.community.d <- network.community.d + outer(community.boot.d$membership, community.boot.d$membership,"==")
  network.present.d <- network.present.d + outer((rowSums(network.boot.d)>0),(rowSums(network.boot.d)>0),"*")
}

P.d <- network.community.d/network.present.d
P.d[!is.finite(P.d)] <- 0  

rc.d <- assortment.discrete(P.d,community.observed.d$membership)$r
rc.d   #.84


nm.d <- unlist(network.modularity.d)
mean(nm.d)
Rmisc::CI(nm.d, ci = 0.95)


diag(P.d) <- 0
g.d <- graph.adjacency(P.d, "undirected", weighted=T)

plot(g.d, 
     edge.width=E(g.d)$weight, 
     edge.color = edge_col(E(g.d)$weight),
     vertex.label="", 
     vertex.size=7, 
     vertex.color=membership(community.observed.d),
     layout = layout_nicely(g.d))




