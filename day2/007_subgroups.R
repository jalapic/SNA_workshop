#####  Subgroups 


## Maximally connected subgroups

## Cliques & Clique Overlap.----



## Roethlisberger & Dickson 1939 
## bank wiring games' room data.
wiring<-read.csv("https://raw.githubusercontent.com/jalapic/SNA_workshop/master/data/wiring_gam.csv")[,-1]
wiring <- read.csv("data/wiring_gam.csv")
rownames(wiring)<-colnames(wiring)
wiring


library(igraph)
g <- graph_from_adjacency_matrix(as.matrix(wiring))
g

plot(g, edge.arrow.size=.4)

clique.number(g) #warning is ok
clique_num(g)
cliques(g)
cliques(g, min = 4)

# Identify the largest cliques in the network
largest_cliques(g)
max_cliques(g)
max_cliques(g, min=4)


# Determine all maximal cliques in the network and assign to object 'clq'
clq <- max_cliques(g)

# Calculate the size of each maximal clique.
table(unlist(lapply(clq, length)))

# Assign largest cliques output to object 'lc'
lc <- largest_cliques(g)

# Create two new undirected subgraphs, each containing only the vertices of each largest clique.
gs1 <- as.undirected(subgraph(g, lc[[1]]))
gs2 <- as.undirected(subgraph(g, lc[[2]]))
gs3 <- as.undirected(subgraph(g, lc[[3]]))


# Plot the largest cliques side-by-side

par(mfrow=c(1,3)) # To plot plots side-by-side

plot(gs1,
     vertex.label.color = "black", 
     vertex.label.cex = 1.2,
     vertex.size = 0,
     edge.color = 'gray28',
     edge.arrow.size=.3,
     main = "Largest Clique 1",
     layout = layout.circle(gs1)
)

plot(gs2,
     vertex.label.color = "black", 
     vertex.label.cex = 1.2,
     vertex.size = 0,
     edge.color = 'gray28',
     edge.arrow.size=.3,
     main = "Largest Clique 2",
     layout = layout.circle(gs2)
)

plot(gs3,
     vertex.label.color = "black", 
     vertex.label.cex = 1.2,
     vertex.size = 0,
     edge.color = 'gray28',
     edge.arrow.size=.3,
     main = "Largest Clique 3",
     layout = layout.circle(gs3)
)

par(mfrow=c(1,1)) #reset



# find largest clique each node is in.
# to do this we make an ego graph of each individual node
g
make_ego_graph(g)
lapply(make_ego_graph(g), max_cliques)




## independent vertex sets

# an reciprocal notion to cliques is that of independent vertex sets
# A vertex set is called independent if there no edges between any two vertices in it.


ivs_size(g)
ivs(g, min=ivs_size(g))  # warning is ok
largest_ivs(g)





## CLIQUE OVERLAP METHOD ----

#- What are the non overlapping distinct cliques?

# Borgatti method
# proximity matrix of cliques (clique co-membership matrix)


mcg <- max_cliques(g, min=4) # using a  minimum clique size of 4
mcg


## this (long-winded) code helps make a clique co-membership matrix:

lapply(mcg, names) 
lapply(lapply(mcg, names),combn,2) # this doesn't include self-self

mat1 <- do.call('cbind',lapply(lapply(mcg, names), combn,2))
t(mat1) #list of comembership

tmp <- unlist(lapply(mcg, names))
cbind(tmp,tmp)
tmp1 <- rbind(t(mat1),cbind(tmp,tmp))

# notice isolates not in this list - need to add in
cbind(names(wiring),names(wiring))
tmp2 <- rbind(tmp1, cbind(names(wiring),names(wiring)))
tmp2
tmp3 <- table(tmp2[,1],tmp2[,2]) # still need to remove 1 from diagonal.
tmp3
diag(tmp3)<-diag(tmp3)-1

tmp4 <- tmp3+t(tmp3)# need to sum transpose, except diagnoal
diag(tmp4) <- diag(tmp3) #put correct diagonal back in
tmp4

# to be consistent with Borgatti p216
idorder<-c("I1","I3","W1","W2","W3","W4","W5","W6","W7","W8","W9","S1","S2","S4")
tmp4[idorder,idorder]



# can perform standard methods on these matrices, e.g. average-link hierarchical clustering.
# gives sets of non-overlapping nested clusters of actors

fit <- hclust(dist(tmp4),method="ward.D")
plot(fit)

fit1 <- kmeans(dist(tmp4),4)
fit1

# drop individuals in no clusters
tmp4
rowSums(tmp4)
colSums(tmp4)
rowSums(tmp4)+colSums(tmp4)
rowSums(tmp4)+colSums(tmp4)==0  #I3,S2
rowSums(tmp4)+colSums(tmp4)==0  #I3,S2
tmp5 <- tmp4[-c(2,4),-c(2,4)]

fit2 <- hclust(dist(tmp5),method="ward.D")
plot(fit2)

fit3 <- kmeans(dist(tmp5),2)
fit3


## but note - this method can be biased towards structures where lots of overlapping cliques
# their relationships will be inflated.



#### BIMODAL METHOD ----

#actors as rows, cliques as columns
mcg <- max_cliques(g, min=4) # using a  minimum clique size of 4
mcg

# e.g. if just create a binary 0/1 clique matrix
mat <- matrix(0,nrow = length(wiring), ncol = length(mcg))
rownames(mat)<- names(wiring)
colnames(mat)<-paste0("cl", 1:length(mcg))
mat

for(i in 1:length(mcg)){
mat[,i][which(rownames(mat) %in% lapply(mcg, names)[[i]])]<-1
}

mat


# but can also scale membership of other members...
# for each clique - calculate number of ties an actor has to other members
# then divide this number by number of  individuals in clique + 1
# essentially this is asking what proportion of ties to become a member of the clique do they have.
# if node is a member  of a clique they automatically get a score of 1.
# assumes symmetrical matrix

mat1<-matrix(nrow=nrow(wiring), ncol=length(mcg))
for (i in 1:14){
  for (j in 1:5){

c(names(mcg[[j]]), rownames(wiring)[i])  #get names in clique plus a.n.other
m <- wiring[c(names(mcg[[j]]), rownames(wiring)[i]),c(names(mcg[[j]]), rownames(wiring)[i])] #subset matrix
mat1[i,j] <- rowSums(m[rownames(wiring)[i],])/length(names(mcg[[j]])) #calculate proportion 

  }
}

mat1
rownames(mat1)<- names(wiring)
colnames(mat1)<-paste0("cl", 1:length(mcg))
mat1  # but  notice that some of the 1s have become 0.8 
mat1[mat==1]<-1
mat1  #Same as Borgatti page 218

mat1[mat1<.5]<-0  #thresholding example


# apply bipartite graph to this matrix

bg<-graph.incidence(mat1) # make bipartite graph
V(bg)$type  #check types
V(bg)$name # check  names


# Visualize clique membership using bipartite graph:

# define color and shape mappings.
col <- c("steelblue", "orange")
shape <- c("circle", "square")

plot(bg,
     vertex.color = col[as.numeric(V(bg)$type)+1],
     vertex.shape = shape[as.numeric(V(bg)$type)+1]
)


# Project bipartite network as a one-mode projection
# can project either as a co-membership network of nodes
# or project as a network of groups that share members. 

pr <- bipartite.projection(bg) 
pr

# look at co-memberhip network of nodes (projection 1):
get.adjacency(pr$proj1,sparse=FALSE,attr="weight")
plot(pr$proj1,edge.width=E(pr$proj1)$weight^2,edge.color="black",vertex.label=V(pr$proj1)$name)





# Correspondence Analysis ---

# typically a visualization method of frequency tables  (similar to MDS)


library(ca)
mat1
mat1[rowSums(mat1)>0,] #C.A. won't work on rows where all 0
mat1.ca <- ca::ca(mat1[rowSums(mat1)>0,])
mat1.ca
summary(mat1.ca)

#visualize
plot(mat1.ca)
plot(mat1.ca, arrows=c(TRUE, FALSE))

library(ggplot2)
library(ggrepel)

mat1.ca$rowcoord
df<-as.data.frame(mat1.ca$rowcoord)
df
ggplot(df, aes(x=Dim1, y=Dim2, label=rownames(df))) + geom_text_repel()






## Directed vs Undirected Networks
# In other words, we create an undirected graph G' with edges between u and v 
# if and only if there is are directed edges u -> v and v -> u. 
# Finding a clique in G' should find you the equivalent clique in G

# could consider clique only for mutually reciprocated ties.
# or, could be for any tie in either direction

## if symmetric valued data - best to use MDS or hierarchical clustering.


##  EXERCISE:  Use above two methods on Karate Club Example
