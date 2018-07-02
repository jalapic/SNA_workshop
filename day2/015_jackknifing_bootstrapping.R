### Summary Statistics


### REFERENCES:

#Tom A.B. Snijders & Stephen P. Borgatti, 1999, 
#Non-Parametric Standard Errors and Tests for Network Statistics, CONNECTIONS 22(2): 61-70

#Tom A.B.Snijders., Stephen P.Borgatti. 1999. Non-Parametric Standard Errors and Tests for
#Network Statistics.

# Hanneman, Robert A. and Mark Riddle.  2005.  Introduction to social network methods. Chapter 18.
# Riverside, CA:  University of California, Riverside 
# ( published in digital form at http://faculty.ucr.edu/~hanneman/ )


knoki <- as.matrix(read.csv("data/knok_info.csv")[,-1])


library(igraph)
g <- graph_from_adjacency_matrix(knoki)
plot(g, edge.arrow.size=.2)

is.directed(g)  # TRUE
reciprocity(g)  # .694




# Jackknifing: Frank and Snijders (1994) 

# Delete each vertex in turn and recalculate...
# repeat for every node to calculate the standard error

g.jack <- delete_vertices(g, 1)
reciprocity(g.jack) #.75

res<-vector('list',length(V(g)))
for(i in 1:length(V(g))){res[[i]]<-  reciprocity(delete_vertices(g, i))}
unlist(res)

zin <- mean(unlist(res))
unlist(res)-zin
(unlist(res)-zin)^2
sum((unlist(res)-zin)^2)
summed <-sum((unlist(res)-zin)^2)
N<-length(V(g))
sqrt(((N-2)/(2*N))*summed) #.13





# Bootstrapping -  there are many ways of potentially bootstrapping
# vertex bootstrapping, edge bootstrapping.


# vertex bootstrapping:

library(snowboot)  # the vertboot() function from snowboot produces a vertex bootstrap
g
nboot<-1000

m1 <- igraph::as_adjacency_matrix(g)  #vertboot shuffles nodes using the matrix.
m1 <- as.matrix(m1)
vertboot_out <- vertboot(m1,nboot)
vertboot_out

lapply(vertboot_out, graph_from_adjacency_matrix)

res <- lapply(vertboot_out, function(x) reciprocity(graph_from_adjacency_matrix(x)))

zin <- mean(unlist(res))
unlist(res)-zin
(unlist(res)-zin)^2
sum((unlist(res)-zin)^2)
summed <-sum((unlist(res)-zin)^2)
M<-nboot
sqrt( (1/(M-1)) * summed) #.13





######################################

# applying this method to test if a network's e.g. density is significantly different from X


# As an empirical example, consider the network of friendship ties among 67 prison inmates 
# collected by Gagnon in the 1950s, reported by MacRae (1960), and available as part of the 
# UCINET 5 software package (Borgatti, Everett and Freeman, 1999). 
# Let us assume that the theoretical "tipping point" that separates epidemic from 
# extinction occurs at density 3%. The observed density for this network is 0.0412. 


prison <- as.matrix(read.csv("data/prison1.csv")[,-1])

library(igraph)
g <- graph_from_adjacency_matrix(prison)
plot(g, edge.arrow.size=.2)
graph.density(g) # 0.04115785



# Calculate with Jackknife method, standard error:

res<-vector('list',length(V(g)))
for(i in 1:length(V(g))){res[[i]]<-  graph.density(delete_vertices(g, i))}
unlist(res)

zin <- mean(unlist(res))
unlist(res)-zin
(unlist(res)-zin)^2
sum((unlist(res)-zin)^2)
summed <-sum((unlist(res)-zin)^2)
N<-length(V(g))
sqrt(((N-2)/(2*N))*summed) #.0036

se.jack <-  sqrt(((N-2)/(2*N))*summed) 
obs <- graph.density(g) # 0.04115785



## Bootstrapping Method:

library(snowboot)  # the vertboot() function from snowboot produces a vertex bootstrap
g
nboot<-1000

m1 <- igraph::as_adjacency_matrix(g)  #vertboot shuffles nodes using the matrix.
m1 <- as.matrix(m1)
vertboot_out <- vertboot(m1,nboot)
res <- lapply(vertboot_out, function(x) graph.density(graph_from_adjacency_matrix(x)))

zin <- mean(unlist(res))
unlist(res)-zin
(unlist(res)-zin)^2
sum((unlist(res)-zin)^2)
summed <-sum((unlist(res)-zin)^2)
M<-nboot
se.boot <- sqrt( (1/(M-1)) * summed) #.0053


## Calculate t-statistic

# The standard error, as estimated by the bootstrap method with 1,000 samples, is 0.0053
# it is 0.0036 using the jackknife method. 

# If assume sampling distribution is approximately normal:
# Next convert to standard error units (observed - expected) / standard error of sampling distribution

se.jack #0.003602691
se.boot #0.005054256


theor.val <- 0.03

tval.jack <- (obs - theor.val)/se.jack
tval.boot <- (obs - theor.val)/se.boot
tval.jack #3.097
tval.boot #2.208
DF <- length(V(g))-1

1-pt(tval.jack, DF) # one-tailed p value;  p=0.001
1-pt(tval.boot, DF) # one-tailed p value;  p=0.015



####


# Alternative (better) p-value obtained from bootstrap samples 
# How surprising is our observed value?

# Bootstrap distribution is centered on (or near) the observed statistic, Z, 
# rather than the theoretical parameter, so to calculate p in this instance...

# substract mean of bootstrap sampling distribution from each bootstrap value 
# and add back in theoretical value (Noreen 1989)
unlist(res) - zin + theor.val

# proportion that are larger than observed value
vals <- unlist(res) - zin + theor.val
sum(vals>=obs)/length(vals) #p=0.017



