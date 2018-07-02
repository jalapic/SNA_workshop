### Identifying Strong and Weak Components

# Components of a graph are sub-graphs that are connected within, but disconnected between sub-graphs
# in directed networks,  weak components are connected in either direction, strong components only via direct edges
# Component breakdowns are  often used to reduce large networks for further analysis.

library(igraph)

set.seed(10)
g <- igraph::random.graph.game(100, .0075, type=c("gnp"), directed=T)
#g <- sample_gnp(100, 1/100)
g
g <- set_graph_attr(g, "layout", layout_with_kk(g))
plot(g, edge.arrow.size=0.4, margin = -.3, vertex.size=0)

#igraph...
is.connected(g)

decompose(g)
dg <- decompose(g, mode=c("weak"), min.vertices = 3)


dg           # subnetworks

#plot
comps <- components(g)$membership
colbar <- rainbow(max(comps)+1)
V(g)$color <- colbar[comps+1]
plot(g, layout=layout_with_fr, vertex.size=5, vertex.label=NA,edge.arrow.size=0.4)


plot(dg[[1]],edge.arrow.size=0.4, margin = -.3, vertex.size=0)   # Plot first
dg[[1]] <- set_graph_attr(dg[[1]], "layout", layout_with_kk(dg[[1]]))  # remember we set the layout, reset.
plot(dg[[1]],edge.arrow.size=0.4, margin = -.3, vertex.size=0)   # Plot first


# examine attributes of components
sapply(dg, diameter) # diameter of all components

max.g <- which.max(sapply(dg, vcount)) # which is the largest component

plot(dg[[max.g]],edge.arrow.size=0.4, margin = -.3, vertex.size=0, 
     layout=layout_with_kk(dg[[max.g]]))   # Plot largest






### Blocks and Cut-points

# cutpoints = nodes, that when removed,lead to the network becoming disconnected into components.
# these individuals are often called 'brokers'
# sometimes called articulation points (as in igraph).

# maximal non-separable sub-graphs are called 'blocks' or 'bi-components'
# these are the groups of nodes that keep the graph connected.
# they do not contain cut-points (cut-vertices)


# bridges are edges whose removal increases the components in the network

#install.packages("ergm")
library(ergm)
data(florentine)
detach("package:ergm")

# The data contains two network objects - one with marital and another one
# with business relations between Florentine families.

flobusiness; flomarriage 
g<-intergraph::asIgraph(flomarriage)
g1<-intergraph::asIgraph(flobusiness)
plot(g)
V(g)$vertex.names

parties<-c("Medici", "Split", "Split","Oligarch","Oligarch","Medici","Oligarch",
           "Oligarch","Medici","Medici","Oligarch","Medici","Medici","Split",
           "Oligarch","Medici")

g<-set_vertex_attr(g, "party", value = parties)

V(g)$label<-V(g)$vertex.names

#color by party
V(g)$color <- V(g)$party
V(g)$color <- gsub("Medici","gold",V(g)$color)
V(g)$color <- gsub("Oligarch","green",V(g)$color)
V(g)$color <- gsub("Split","gray70",V(g)$color) 


plot(g, layout=layout.fruchterman.reingold, 
            vertex.label.cex=0.75)




# cutpoint analysis
articulation_points(g) #Articuation points or cut vertices are vertices whose removal increases the number of connected components in a graph.

V(g)$label[articulation_points(g)]


# plot by cutpoint
V(g)$color <- ifelse(V(g) %in% articulation_points(g), "orange", "lightgreen")
plot(g, vertex.label.cex=.75, edge.width = 5)


# clearly, some cutpoints are more interesting than others....


## Bicomponents (the proportion of the network that are disconnected by removal of an edge)

bc <- biconnected_components(g) #A graph is biconnected if the removal of any single vertex (and its adjacent edges) does not disconnect it.

bc
summary(bc)

V(g)$label
bc$tree_edges
bc$component_edges
bc$components




# largest_component

largest_component <- lapply(biconnected_components(g)$components, 
                            length) %>% which.max()

V(g)$color <- 
  ifelse(V(g) %in%
           biconnected_components(g)$components[[largest_component]],
         "salmon","lightblue")

plot(g)




## use sna to generate bicomponents and cutpoints


g2 <-  intergraph::asNetwork(g)
g2

sna::cutpoints(g2, mode="digraph")
sna::cutpoints(g2, mode="digraph", return.indicator = T)
igraph::articulation.points(g)

sna::bicomponent.dist(g2) 



### Brokerage Roles


## bokerage (Gould & Fernandez) define brokerage as the role played by an actor who
# mediates contact between two other nodes.

# when dealing with class memberships, we can define several 'roles'


# Coordinator:  actor brokers contact between two members of own group
# Itinerant broker:  actor brokers contact between two members of another different group (not actor's own group)
# Representative: actor mediates incoming tie from out-group indiv to an in-group indiv
# Gatekeeper: actor mediates tie from in group-member to an out-group member
# Liaison: actor mediates contact between two individuals from different groups neither of which actor belongs to.
# Total: cumulative brokerage role occupancy

# broker in middle:

# Coordinator (w_l):  A -> A -> A
# Itinerant broker (w_O):    A -> B -> A
# Representative (b_{IO}):   A -> B -> B
# Gatekeeper (b_{OI}):   A -> A -> B
# Liaison (b_O):   A -> B -> C


# brokerage needs graph and a vector of class memberships

library(sna)
party <- V(g)$party
sna::brokerage(g2, cl=party)  # could, of course, use all manner of group options


g2.brk <- sna::brokerage(g2, cl=party) 

g2.brk$raw.nli   # observed brokerage scores
g2.brk$z.nli   # standardized brokerage scores



### Some additional measures of 'influence' related to brokerage



## Reach Centrality

# reach refers to how many ties can be reached in n steps from a node
# a neighborhood is the set of these nodes
# subtracting degree from reach gives a measure of the number of 'weak ties' that a node has

ego(g, 2, V(g)) # number of vertices reached in 2 steps for each node (includes self)
unlist(lapply(ego(g, 2, V(g)), length))

igraph::degree(g) # degree of each (doesn't included self)

ego(g, 1, V(g)) # number of vertices reached in 2 steps for each node (includes self)
unlist(lapply(ego(g, 1, V(g)), length)) 

# to calculate weak ties
weak_ties <- function(g,N){ unlist(lapply(ego(g, N, V(g)), length))  - igraph::degree(g) - 1 }

weak_ties(g, 1)
weak_ties(g, 2)
weak_ties(g, 3)



## Edge Betweenness  - betweenness score for edges.

edge_betweenness(g, directed = FALSE)

E(g)$width <- edge_betweenness(g, directed = FALSE)

plot.igraph(g,
            edge.width = igraph::edge.betweenness(g, directed = FALSE)+1,
            edge.color = heat.colors(igraph::edge.betweenness(g, directed = FALSE)+1))  


#
el <- get.edgelist(g)
el <- as.data.frame(el)
el$EB <- edge_betweenness(g, directed = FALSE)
el



## Burt's Constraint
constraint(g) # lower constraint  = more brokerage potential
V(g)$label
plot(g, edge.width=1)












###################################################################################################

### Exercises 


## DATA SET 1.    - wood-processing facility strike network (Pajek)

strike_nodes <- read_csv("data/strike_nodes.csv")
strike_edges <- read_csv("data/strike_edges.csv",col_names = FALSE)
colnames(strike_edges)<-c("from","to")


g <- graph_from_data_frame(strike_edges, directed=TRUE, vertices=strike_nodes)
V(g)$color <- ifelse(strike_nodes$group==1, "khaki1", ifelse(strike_nodes$group==2,'lightgreen','mistyrose'))
plot(g, vertex.label.cex=.75, edge.arrow.size=.3, layout=layout_with_kk(g))

# Spanish-speaking group - khaki1
# English speaking young employees - green
# English speaking old employees - pink
# Alejandro most proficient in English, Bob most proficient in Spanish
# Ozzie is Karl's father
# Bob got his job thanks to Norm








## DATA SET 2.    - prison population

# The following network is reported in
# Kreager, Derek A., David R. Schaefer, Martin Bouchard, Dana L. Haynie, Sara Wakefield, Jacob T.N. Young, and Gary Zajac. â???oToward a Criminology of Inmate Networksâ???. Forthcoming in Justice Quarterly.
# The network is the connections among 19 inmates who were asked to 
# nominate those who they "get along with most". They were allowed 10 nominations.

detach(package:igraph)
library(sna)

get.along.data <- read.csv("data/prison.csv",as.is=T,header=T,row.names=1) # the network.
race.data      <- read.csv("data/prison_race.csv",as.is=T,header=T,row.names=1) # the race variable.
get.along.net <- network(get.along.data,directed=T)
set.vertex.attribute(get.along.net,"race", race.data[,1])
get.along.net








