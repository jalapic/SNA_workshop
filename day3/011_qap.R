
### QAP correlation

padgm <- read.csv("data/PADGETT.csv")    # marriage
padgb <- read.csv("data/PADGETT2.csv")   # business

# remove first column
padgm <- as.matrix(padgm[,-1])
padgb <- as.matrix(padgb[,-1])



# Visualize networks


# step by step
as.vector(padgm)
as.vector(padgb)

cor.test(as.vector(padgm), as.vector(padgb)) # .378
cor(c(padgm), c(padgb))


# Using SNA

library(sna)

sna::gcor(padgb, padgm)  # .372



# why are these different ?   .... think about it.....


padgm0<-padgm
padgb0<-padgb
diag(padgm0)<-NA
diag(padgb0)<-NA

cor(c(padgm0), c(padgb0), use="complete.obs") # .372



## what if your data can contain self-loops ?
cor(c(padgm), c(padgb), use="complete.obs") # .378
sna::gcor(padgb, padgm, diag=T)  # .378


# Directed or undirected graphs ?
sna::gcor(padgb, padgm, mode = "digraph")  # .372
sna::gcor(padgb, padgm, mode = "graph")  # .372



### Significance Testing.

## First a walk through of what's going on....

# let's call our matrices X and Y for ease of typing...

X <- padgb
Y <- padgm
diag(X)<-NA
diag(Y)<-NA


## 1. One permutation

set.seed(10)
Y
toswitch <- sample(ncol(Y),2)
toswitch
Y[, rev(toswitch)] <- Y[, toswitch] # reverse columns
Y[rev(toswitch),] <- Y[toswitch,] # reverse rows


# check....
padgm  # col 5 starts 001, col 9 starts 111;  row 5 starts 001, row 9 starts 111;
Y  # col 5 starts 111, col 9 starts 001;  row 5 starts 111, row 9 starts 001;  

# calculate a Pearsons's r for X and new Y
sna::gcor(X, Y)  # .17





# now do this for several iterations ....  e.g. let's do for 10....
set.seed(10)
nperms <- 10
results <- vector('list', nperms)
results

for (i in 1:nperms){
toswitch <- sample(ncol(Y),2)
Y[, rev(toswitch)] <- Y[, toswitch] # reverse columns
Y[rev(toswitch),] <- Y[toswitch,] # reverse rows
results[[i]] <- sna::gcor(X, Y)  # .17

}

unlist(results)


### Do for 10,000 iterations...
set.seed(10)
nperms <- 10000
results <- vector('list', nperms)
results

for (i in 1:nperms){
  toswitch <- sample(ncol(Y),2)
  Y[, rev(toswitch)] <- Y[, toswitch] # reverse columns
  Y[rev(toswitch),] <- Y[toswitch,] # reverse rows
  results[[i]] <- sna::gcor(X, Y)  #

}

unlist(results)

library(ggplot2)
obs <- cor(c(padgm), c(padgb))
ggplot(data.frame(res=unlist(results)), aes(x=res)) + geom_histogram() # why does it look like this ?
ggplot(data.frame(res=unlist(results)), aes(x=res)) + geom_density()
ggplot(data.frame(res=unlist(results)), aes(x=res)) + geom_density() + geom_vline(xintercept = obs, color='red', lty=2)

# p-value  (1 tailed)

sum(unlist(results)>=obs)         # 1
sum(unlist(results)>=obs)/nperms  # 1e-04    i.e. 0.0001




## Using 'sna' package
padg.qap <- qaptest(list(padgb, padgm), gcor, g1=1, g2=2, reps = 10000)
summary(padg.qap)
plot(padg.qap)  



##### Some extensions / possible uses of QAP:


### Example 1. 


##  e.g. Use QAP to compare similarity matrix of group membership with edge weight sociomatrix.
# e.g. (Wey et al., 2008, Wiszniewski et al., 2010).
# has limitations when sampling is biased.


birds1 <- readr::read_csv("data/birds1.csv")
birds1_sps <- readr::read_csv("data/birds1_sps.csv")

birds1
birds1_sps

birds1 <- as.matrix(birds1)
rownames(birds1)<-colnames(birds1)

birds1 # note the order of ids, has to match traits
traits <- as.numeric(factor(birds1_sps$species))  # ensure this is in same order as matrix


# visualize
detach(package:sna)
detach(package:statnet)
detach(package:igraph)
library(igraph)

g <- graph_from_adjacency_matrix(birds1, mode=c("undirected"), weighted=T)
g <- set.vertex.attribute(g, 'species', value = birds1_sps$species)
colors <- c("red","yellow","cyan","green")
g <- set.vertex.attribute(g, 'color', value = colors[traits])

plot(g)  # notice many edges do exist between groups as well as within

# create a comembership matrix from 2-column list
table(birds1_sps)
bg <- graph.incidence(table(birds1_sps))
pr <- bipartite.projection(bg) 
comem <- get.adjacency(pr$proj1,sparse=F,attr="weight")
comem

## Use 'sna' package to run QAP correlation and sig test
sna::gcor(birds1, comem)  # r=.637

padg.qap <- sna::qaptest(list(birds1, comem), sna::gcor, g1=1, g2=2, reps = 10000)
summary(padg.qap)
plot(padg.qap)  




### Example 2.


# e.g. Are individuals more likely to report to individuals who are older than them in a workplace?
# use the Krackhardt high-tech dataset - perhaps this is not true in high-tech companies?

reportto <- as.matrix(read.csv("data/krack_reports.csv")[,-1])
krack_attr <- read.csv("data/krack_attr.csv")

g <- igraph::graph_from_adjacency_matrix(reportto)
g <- igraph::set_vertex_attr(graph = g, name = "age", value = krack_attr$AGE)
g
vertex.attributes(g)

### plot network
plot.igraph(g,vertex.label=NA,layout=layout.fruchterman.reingold, edge.arrow.size=.2,
            edge.color="gray42", vertex.size=sqrt(V(g)$age))


# Create a matrix of age differences.
mat <- outer(V(g)$age,V(g)$age,`-`)
mat<- as.matrix(mat)
str(mat)
mat  # a negative number indicates that row i is younger than column j.

# look at sociomatrix
reportto  # 1 in a cell indicates that row i reports to column j.
mat  #

# so a negative correlation between matrices would indicate that younger people tend
# to report to older people.


# QAP correlation (network matrix vs age difference matrix):
out1 <- sna::qaptest(list(reportto, mat), sna::gcor, g1=1, g2=2, reps = 10000)
summary(out1)
plot(out1)  

# looks like our oberved value is slightly negative (very minorly)
# but overall we cannot reject hypothesis that this was observed by chance
# so no evidence of a significant negative correlation
# the high-tech firm does not appear to be a 'traditional bureaucracy'


## Potential confounds with this analysis in this case? 

# hard to prove no relationship (sample size)
# structure of network makes it hard for various combinations to occur:
# i.e. limited by how many people are reported to.
# solution is to put missing values for all cells when neither party reports to the other.

mat1 <- mat # create copy of original age difference matrix
reportto.symm <- reportto + t(reportto) # need to symmetrize to make sure know all 'reporting to' relationships
mat1[reportto.symm==0]<-NA # put missing data into age matrix when individuals don't report to each other

reportto
mat1

## Redo QAP test (you'll get warnings)  (sna::qaptest doesn't work so well with missing data)

out2 <- sna::qaptest(list(reportto, mat1), sna::gcor, g1=1, g2=2, reps = 10000)
summary(out2)


# the observed correlation is more negative.
# but sna qaptest won't work with missing data....
# so we have to do this ourselves...

# we can do the QAP permutation test ourselves:

# just to make less typing for ourselves:
X <- reportto
Y <- mat


## Do this many times - note the fuller shuffling of the matrix in the permutation step:

set.seed(10)
nperms <- 10000
results <- vector('list', nperms)
results

for (i in 1:nperms){
  # need to reset matrices each time
  X <- reportto
  Y <- mat
  toswitch <- sample(ncol(X)) #NOTE - we are reshuffling the entire matrix every time (not 2 cols/rows at a time)
  X[, rev(toswitch)] <- X[, toswitch] # reverse columns
  X[rev(toswitch),] <- X[toswitch,] # reverse rows
  X.symm <- X + t(X) # need to symmetrize to make sure know all 'reporting to' relationships
  Y[X.symm==0]<-NA # put missing data into age matrix when individuals don't report to each other
  results[[i]] <-  cor(c(X), c(Y), use = "complete.obs")  # -.15 is the new correlation
  
}

unlist(results)

library(ggplot2)
obs <- cor(c(reportto), c(mat1), use = "complete.obs") 
ggplot(data.frame(res=unlist(results)), aes(x=res)) + geom_density() + geom_vline(xintercept = obs, color='red', lty=2)

# p-value  (1 tailed)
sum(unlist(results)<=obs, na.rm=T)/nperms  # p = 0.15 - so still no evidence that this organization has a younger->older reporting structure










### Other measures of 'similarity/dissimilarity' between two matrices...

# Pearson Correlation
# Euclidean Distance
# Match Coef
# Jaccard Coef
# Goodman-Kruskal Gamma
# Hubert Gamma



#' The first column shows the values of five alternative measures of association.  
#' The Pearson correlation is a standard measure when both matrices have valued relations measured at the interval level.  
#' Gamma would be a reasonable choice if one or both relations were measured on an ordinal scale.  
#' Simple matching and the Jaccard coefficient are reasonable measures when both relations are binary; 
#' the Hamming distance is a measure of dissimilarity or distance between the scores in one matrix 
#' and the scores in the other (it is the number of values that differ, element-wise, from one matrix to the other).



