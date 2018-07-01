#### Triad Census Comparison using Concordance Analysis 

# example....

tri_mats <- readRDS('data/tri_mats.rds')

tri_mats
str(tri_mats)

g_l <- lapply(tri_mats, graph_from_adjacency_matrix)
g_l

par(mfrow=c(2,4))
lapply(g_l, function(x) plot(x, edge.arrow.size=.05))
par(mfrow=c(1,1))



tc_l <- lapply(g_l, triad_census)

tc_l

m <-  do.call('rbind',tc_l)
colnames(m) <- c("003","012","102","021D","021U","021C","111D","111U","030T","030C","201","120D","120U","120C","210","300")
rownames(m) <- paste0("mat",LETTERS[1:8])  

m # this is a group by variable matrix
# notice 102/111D/111U/120D/120U


m1 <- m/rowSums(m)

round(m1,3)


## Correspondence Analysis

# a graphic method of exploring the relationship between variables in a contingency table. 

library(ca)
fit <- ca(m1)

fit  # gives dimenion scores
summary(fit) # 2 dimensions ok

fit$rowcoord # each dimension against each matrix

plot(fit)  # red triangles reflect relationship of triad census types to each other
# seem a split between matA-D vs matE-H

plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) 

# appears to be triad types related to reciprocity that are closer to mat ABCD
# mat D also has higher levels of out- and in- stars 02X and 021C - directed line. a->b->c



recips <- unlist(lapply(g_l,  reciprocity))
recips

plot(fit$rowcoord[,1], unlist(lapply(g_l,  igraph::graph.density)), xlab="dimension 1", ylab="density")
plot(fit$rowcoord[,2], recips, xlab="dimension 2", ylab="reciprocity")


## for real example, see:

## Comparing Similarity of Local Structure Across Social Groups

# Katharine Faust, Comparing Social Networks: Size, Density, and Local Structure, Metodoloski zvezki, Vol. 3, No. 2, 2006, 185-216 

# https://www.stat-d.si/mz/mz3.1/faust.pdf

