## LR-QAP in R

# logisitic regression QAP



## Example   Newcomb's Fraternity Data.


# 15 matrices
# 17 men attending U Michigan during Fall 1956
# all incoming students with no prior acquaintance
# weekly sociometric preference rankings.
# rows are nominations (1-16)

newc_data <- readRDS("data/newc_data.RDS")

newc_data[[1]] # ranked choices of preference


# are ties in week1 the result of reciprocity and/or transitivity of ties formed in week0? 

# Predict week1 - including week0 as a control variable.

# First  convert matrices, so if in top 3 choices get 1, otherwise get 0.


## Generate Matrices
top3matrix <- function(X){
  diag(X)<-NA
  X[X<=3]<-1
  X[X>3]<-0
  return(X)
}

newc_dataD <- lapply(newc_data, top3matrix)


# week0 and week1 raw matrices
newc0D <- newc_dataD[[1]]
newc1D <- newc_dataD[[2]]

# reciprocity matrix
newc0D_recip <- t(newc0D)  # if people match ties, a 1 in newc0D_recip will be matched by a 1 in newc0D

# friends-of-friends matrices - geodesic distances
igraph::graph_from_adjacency_matrix(newc0D)
plot(igraph::graph_from_adjacency_matrix(newc0D),edge.arrow.size=0.5)
igraph::distances(igraph::graph_from_adjacency_matrix(newc0D))

newc0D_trans <- igraph::distances(igraph::graph_from_adjacency_matrix(newc0D))
newc0D_trans[newc0D_trans<=2]<-1
newc0D_trans[newc0D_trans>2]<-0
newc0D_trans  


# run QAP-based logistic regression, using 
# newc1D as DV
# newc0D, newc0D_recip and newc0D_trans as predictors (IVs)


#Fit a netlogit model

# put 0s back in to diagnoal (sna doesn't like NA on diagonal)
diag(newc0D)<-0
diag(newc0D_recip)<-0
diag(newc0D_trans)<-0
diag(newc1D)<-0

x<-list(newc0D, newc0D_recip, newc0D_trans)
y<-newc1D

# fit model
nl<-netlogit(y,x,reps=10000)  # this will take a while (might want to do 1000).........
summary(nl)

# results:  newc0D is significant 
# (unlikely for social structure at time T+1 to be massively different from that at time T)
# reciprocity parameter is positive and significant (p=.029)
# indicating a greater-than-chance tendency to reciprocate ties over time
# transitivity parameter is not significant (p=.174) 
# not likely to become friends with friends of friends.



## Note: Data need to be square (there are methods for dealing with rectangular data though)
