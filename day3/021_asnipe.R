#### asnipe

# Farine 2013 
# Animal social network inference and permutations for ecologists in R using asnipe
# Methods Ecol Evol

# For many animal behavior datasets, traditional permutation methods may not be appropriate
# see Croft et al 2011 TREE
# animal networks may have high levels of uncertainty (e.g. about edge occurrence) which may violate assumptions of permutations
# e.g. that all individuals in the network can be observed at all times



library(asnipe)


### Data Structures ----

## 1. Group by Individual matrix

# each column an individual
# each row an observation period (sample period)

data('group_by_individual')
gbi


## 2. Stack of association matrices

# several matrices in a list
# each matrix is an n x n association matrix

mats <- readRDS('data/matl.rds')
mats




### Functions for generating data formats from raw data & switching between data formats


# e.g. dataframe of 2 columns with ID in one and 'group' or 'sample period' in another...

individuals <- data.frame(ID=c('C695905','H300253','H300253',
                          'H300283','H839876','F464557','H300296','H300253',
                          'F464557','H300296','C695905', 'H300283','H839876'),
                          GROUP=c(1,1,2,2,2,3,3,4,5,5,6,6,6))

individuals


# get group by individual matrix
get_group_by_individual(individuals, data_format='individuals')

# note - it has to be a data.frame - a tibble will not work.



# e.g. data in a list with each element being the ids of individuals seen in that period.

groups <- list(G1=c('C695905','H300253'),
               G2=c('H300253','H300283','H839876'),
               G3=c('F464557','H300296'),
               G4=c('H300253'),
               G5=c('F464557','H300296'),
               G6=c('C695905','H300283','H839876'))

groups


# get group by individual matrix
get_group_by_individual(groups, data_format='groups')



## Generate Sampling Periods  

# First specify when observations occured

#Add another column with time
individuals <- cbind(individuals,DAY=c(1,1,1,1,1,2,2,2,3,3,3,3,3))
individuals


# now get sampling periods

SPs <- get_sampling_periods(association_data = individuals[,c(1,2)], # the dataframe format
                     association_times =  individuals[,3], # this is the column to use as 'samples'
                     sampling_period = 1, # this has to be 1
                      data_format = "individuals")


# examine how the matrix has treated 'group' and 'day'
# group is used to make each association matrix
# association matrices are made by day
SPs[1,,]
SPs[2,,]
SPs[3,,]
individuals




## list of groups format - for 'groups' (the list) we create a vector
days <- c(1,1,2,2,3,3)
groups

SPs <- get_sampling_periods(association_data = groups, # the list format
                            association_times =  days, # the vector of times
                            sampling_period = 1, # this has to be 1
                            data_format = "groups")

SPs[1,,]
SPs[2,,]
SPs[3,,]

str(SPs)



## Can also get frequency table of occurrences
SPs <- get_sampling_periods(association_data = individuals[,c(1,2)], # the dataframe format
                            association_times =  individuals[,3], # this is the column to use as 'samples'
                            sampling_period = 1, # this has to be 1
                            data_format = "individuals",
                            return="occ")
SPs




## Note on using sampling period for permutations

# Using sampling period matrices, can create permuted association matrices within sample periods
# this controls for individual gregariousness (Whitehead 2008)

# Also sometimes needed that must control for e.g. spatial organization
# e.g. there may be location constraints such that animals can only exist in certain locations




### Generating networks

# e.g. data from Farine et al. (2012). - bird data

data('group_by_individual')
gbi
str(gbi) #151 columns (indivdiuals), 347 observation periods (groups)

# metadata
data("individuals")
inds
head(inds)
colnames(gbi) <- inds$RING.NUMBER


# The default method for determining an adjacency matrix from these data is to calculate
# the simple ratio index.

network <- get_network(gbi, data_format='GBI') # creates network on group_by_individual formatted data
str(network)  # a 151 X 151 adjacency matrix

network # each cell entry is the simple ratio (see Whitehead 2008, p95)

get_network(gbi, data_format='GBI',  association_index = "HWI") # use half-weight index instead.


# automatically subsetting networks.

# for example - add times
data('times')
times
str(times)  # vector of numbers (times)
length(times) #347 - same as the number of rows in 'gbi'. 
# each number in the vector refers to the time at which each sample (row of gbi) was obtained.

## Create a  2 ? N ? N array that will hold results
# index 1 = network number (1 or 2 for 1st half, 2nd half of time)
# index 2 = row of each association matrix
# index 3 = column of each association matrix

networks <- array(0, c(2, ncol(gbi), ncol(gbi)))
networks

## calculate network for first half of the time
networks[1,,] <- get_network(gbi,
                             data_format = 'GBI', 
                             times = times,
                             start_time = 0, 
                             end_time = max(times)/2)


## calculate network for second half of the time
networks[2,,] <- get_network(gbi,
                             data_format='GBI', 
                             times=times,
                             start_time=max(times)/2, #note odd times number so this ok
                             end_time=max(times))


networks[1,,] #time point 1
networks[2,,] #time point 2

# this does remove the names of the ids though as we're putting them in the array structure
# could put them back in if we wished.


## Use other packages to calculate network metrics...
  

library(igraph)

g <- graph_from_adjacency_matrix(networks[1,,], mode=c('undirected'), diag=FALSE, weighted=TRUE)
plot(g,vertex.label.size=.4)

deg_weighted <- graph.strength(g) # sums edge weights of adjacent edges for each vertex
deg_weighted
hist(deg_weighted, 40, main="igraph_method")

detach(package:igraph)


# 'sna' deals with matrix stacks easily...

library(sna)
deg_weighted <- degree(networks, gmode='graph',g=c(1,2)) #'graph' for undirected, 'digraph' for directed
deg_weighted
hist(deg_weighted[,1], 40, main="sna_method")
hist(deg_weighted[,2], 40)

detach(package:sna)



# e.g. test if time point 1 and time point 2 networks are associated (mantel test)
library(ape)
mantel.test(networks[1,,],networks[2,,])  #z = 55.445, p=0.001 (2-tailed) matrices are highly correlated
# z.stat	- the Z-statistic (sum of rows*columns of lower triangle) of the data matrices



## Visualize Network 1 (first-half) and Network 2 (second-half):

library(igraph)
g1 <- graph_from_adjacency_matrix(networks[1,,], mode=c('undirected'), diag=FALSE, weighted=TRUE)
g2 <- graph_from_adjacency_matrix(networks[2,,], mode=c('undirected'), diag=FALSE, weighted=TRUE)
layoutg <- layout.fruchterman.reingold(g1)

# for edges
edge_col <- function(x) {
  paste0("gray",ceiling(scales::rescale(x, to=c(99,1))))
}

edge_col(E(g1)$weight)


par(mfrow=c(1,3)) # 1 row, 3 cols

plot(g1, 
     vertex.size=3, 
     vertex.label=NA, 
     layout = layoutg, 
     edge.color = edge_col(E(g1)$weight),
     edge.width = log(log(E(g1)$weight+1)+1),
     main = "time 1"
)

plot(g2, 
     vertex.size=3, 
     vertex.label=NA, 
     layout = layoutg, 
     edge.color = edge_col(E(g2)$weight),
     edge.width = log(log(E(g2)$weight+1)+1),
     main = "time 2 - layout as time 1"
)

plot(g2, 
     vertex.size=3, 
     vertex.label=NA, 
     layout = layout.fruchterman.reingold(g2),
     edge.color = edge_col(E(g2)$weight),
     edge.width = log(log(E(g2)$weight+1)+1),
     main = "time 2 - layout as time 2"
)

par(mfrow=c(1,1)) #reset


detach(package:igraph)




########

## Network permutations of the data stream

# Typical randomization method is to randomize the association matrix (e.g. Manly 1997) 
# But this can lead to biases and overestimates of statistical significance (Bejder et al. 1998) in animal behavior studies.
# Therefore may need to perform randomziations on the data stream rather than on the association matrix. 


# 'asnipe' permutation method:

# swaps individuals between groups when using group by individual matrices
# swaps associations when using sampling periods
# after each swap, recalculate the network 
# output is a stack of p matrices where p is the number of permutations,
# each slice is an N ? N association matrix. 

# These swaps maintain the variance in individual gregariousness 
# and size of each group constant (Bejder et al. 1998).

# This function enables the swaps in the data stream to be limited between
# individuals that occur on the same day, in the same location, 
# or are of the same class (such as sex or age class).


## EXAMPLE:

# calculate degree of original networks
library(sna)

deg_weighted <- degree(networks, gmode='graph', g=c(1,2))


## perform the permutations constricting within hour of observation
# Though each class is labelled, the function is flexible. 
# Hence, days can represent any time period (months, hours, etc.).


## Some guidelines by Whitehead 2008
# sampling periods should not be too short so most relationships are 0
# or too long such that most relationships are observed (1)
# because of the nature of these permutations (flipping row/column), the total
# number of permtuations should be much larger than other permutation tests
# Whitehead recommends 10,000 - 20,000 as a minimum.
# Ideally, do several replicates and observe when p-values 'stabilize'.

set.seed(107)
nperms <- 1000 # number of permutations

network1_perm <- network_permutation(gbi, 
                                     data_format="GBI",
                                     association_matrix=networks[1,,], 
                                     times=times, 
                                     start_time=0, 
                                     end_time=max(times)/2, 
                                     days=floor(times/3600), #gives vector of 347 'hours' 
                                     within_day=TRUE,
                                     permutations=nperms)

network2_perm <- network_permutation(gbi, 
                                     data_format="GBI",
                                     association_matrix=networks[2,,], 
                                     times=times, 
                                     start_time=max(times)/2, 
                                     end_time=max(times), 
                                     days=floor(times/3600), 
                                     within_day=TRUE,
                                     permutations=nperms)


network1_perm
network2_perm

str(network1_perm)
network1_perm[1,,] #first permutation
network1_perm[nperms,,] #last permutation




## calculate the weighted degree for each permutation
deg_weighted_perm1 <- degree(network1_perm,gmode="graph", g=c(1:nperms))
deg_weighted_perm2 <- degree(network2_perm,gmode="graph", g=c(1:nperms))
detach(package:sna)

deg_weighted_perm1 # 151 rows (individuals) x 1000 columns (permutations)
# the degree of each individual in the network, with each column a permutation
deg_weighted_perm1[,c(1,900)]
colMeans(deg_weighted_perm1) # we can look at the average degree weight in each column/network like this.


# get p-value for each batch of permutations:

obs1 <- mean(deg_weighted[,1])  # 3.9
obs2 <- mean(deg_weighted[,2])  # 7.2

sum( colMeans(deg_weighted_perm1)/ncol(deg_weighted_perm1) > obs1 ) # p = 0
sum( colMeans(deg_weighted_perm2)/ncol(deg_weighted_perm2) > obs2 ) # p = 0



## plot the distribution of permutations with the original data overlaid
par(mfrow=c(1,2))

# first network
hist(colMeans(deg_weighted_perm1),
     breaks=100, 
     main=paste("P = ", sum( colMeans(deg_weighted_perm1)/ncol(deg_weighted_perm1) > obs1 )), 
     xlab="Weighted degree", 
     ylab="Probability",
     xlim=c(3.5,4)
     )
abline(v=mean(deg_weighted[,1]), col='red')


# second network
hist(colMeans(deg_weighted_perm2),
     breaks=100, 
     main=paste("P = ", sum( colMeans(deg_weighted_perm2)/ncol(deg_weighted_perm2) > obs2 )), 
     xlab="Weighted degree", 
     ylab="Probability",
     xlim=c(6.3,7.3)
    )
abline(v=mean(deg_weighted[,2]), col='red')

par(mfrow=c(1,1)) # reset

# strength of network (mean weighted degree) significantly higher than chance at both time points.
# strength of the network was signficantly higher at time point 2 than time point 1




### Alternative function:  network_swap()


# 
# 
# # calculate network for data based on morning associations
# network <- get_network(gbi, 
#                        association_index="SRI", 
#                        times=times, 
#                        start_time=0, 
#                        end_time=max(times)/2)
# 
# # this one keeps the node ids as only a n x n matrix
# 
# 
# 
# # a list with the i) adj matrix ii) gbi matrix for only morning times
# network_perm <- list(network,gbi[which(times <= max(times)/2),])
# 
# # use hours as a grouping category
# hours <- floor(times/3600)[which(times <= max(times)/2)]
# 
# # 1 swap - this asks for the original association matrix
# # as it saves memory to only compute differences that are permuted
# 
# 
# network_perm[[2]]
# network_perm[[2]][111:121,41:50] # take a tiny peak at subset
# 
# set.seed(55)
# swap1 <- network_swap(network_perm[[2]], 
#              swaps=100, 
#              association_matrix=network_perm[[1]], 
#              days=hours, 
#              within_day=TRUE)
# 
# 
# swap1[[2]][111:121,41:50]
# 
# 
# ### several swaps....
# # this method is slow than network_permutation()
# ds <- rep(NA,100)
# 
# for (i in 1:100) {
#   network_perm <- network_swap(network_perm[[2]], 
#                                swaps=100, 
#                                association_matrix=network_perm[[1]], 
#                                days=hours, 
#                                within_day=TRUE)
#   ds[i] <- igraph::transitivity(
#                           igraph::graph_from_adjacency_matrix(
#                             network_perm[[1]], mode = c("undirected"), weighted = T
#                                   ))
# }
# 
# # plot the results with the original network as a red dot
# plot(ds,pch=20,cex=0.5,ylim=c(0.7,0.85))
# points(0,
#        igraph::transitivity(
#        igraph::graph_from_adjacency_matrix(
#          network, mode = c("undirected"), weighted = T)),
#        cex=1,pch=20,col="red")
# 
# 



### Permutations with linear models

# Can use this permutation method to generate relevant null models to 
# compare observed effects against permutations

# e.g. compare the coefficient estimate for magnitude of slope for original data
# to those estimated from each permutation.

# Example:  morning vs afternoon weighted degree (strength)



# build dataset with all data in one column of a dataframe
input <- rbind(data.frame(Degree=deg_weighted[,1],
                          Time='MORNING'),
              data.frame(Degree=deg_weighted[,2],
                         Time='AFTERNOON')
              )

input$id <- colnames(gbi)
input

# visualize
library(ggplot2)
ggplot(input, aes(x=Time, y=Degree)) + geom_jitter(alpha=.5, width=.05)

ggplot(input, aes(x=Time, y=Degree, group=id)) + 
  geom_point(size=.5,alpha=.5) +
  geom_line(alpha=.3)
  

# build model of strength (weighted degree) of each individual as a function of time of day
mod1 <- lm(Degree~Time, data=input)
summary(mod1)
coef(summary(mod1))
e <- coef(summary(mod1))[2,1]
e   # get parameter estimate of slope  beta = 3.32


# get an estimate of the slope for each permutation

deg_weighted_perm1 # recall this is 151 rows (individuals) x 1000 columns (permutations)
# the degree of each individual in the network, with each column a permutation

e_perm <- rep(NA,nperms) #store coefficients here from permutations

for (i in 1:nperms) { 

input_perm <- rbind(
                   data.frame(
                     Degree=deg_weighted_perm1[,i],
                     Time='MORNING'),
                   data.frame(
                     Degree=deg_weighted_perm2[,i],
                     Time='AFTERNOON')
                   )
  
model_tmp <- lm(Degree~Time, data=input_perm)
  
e_perm[i] <- coef(summary(model_tmp))[2,1]

} # end loop


## Visualize Results
ggplot(as.data.frame(e_perm), aes(e_perm)) + 
  geom_histogram(color='white',binwidth=.01) +
  geom_vline(xintercept = e, col="red", lty=2)


## calculate P value from how many of the slopes estimated in the randomized data
sum(e_perm > e)/nperms  # p = 0
plot(e_perm, type="l")

# so a significant effect of time of day on association patterns.
# the pvalue here is 0 as no permutations had a parameter estimate as large as the observed.

# Recall Whitehead (2008), many more permutations should be used in reality.

# This approach can be used with GLM, GLMM methods too.




### Testing for differentiated (preferred and avoided) relationships ----
## Bejder et al 1998 test on weighted networks

# compare the coefficient of variation of observededge weights
# with the same measure taken from the randomized data after each swap.
# ensure that appropriate randomizations are done (e.g. within sample/location)


data("group_by_individual")
data("times")

# calculate network for data based on morning associations
network <- get_network(gbi, association_index="SRI",times=times, start_time=0, end_time=max(times)/2)

# perform 100 permutations and calculate the coefficient  (usually  do at least 1000)
# of variance after each permutation.
# note that the subsetting is done outside of the function

library(raster)
cvs <- rep(NA,100)
network_perm <- list(network,gbi[which(times <= max(times)/2),])
hours <- floor(times/3600)[which(times <= max(times)/2)]
for (i in 1:100) {
  network_perm <- network_swap(network_perm[[2]], swaps=1,
                               association_matrix=network_perm[[1]], days=hours,
                               within_day=TRUE)
  cvs[i] <- raster::cv(network_perm[[1]])
}

# plot the results with the original network as a red dot
plot(cvs,pch=20,cex=0.5)
points(0,raster::cv(network),cex=1,pch=20,col="red")

raster::cv(network) #observed value

sum(raster::cv(network)<=cvs)/100 # pvalue 




### Exercise:  use asnipe permutation method to calculate community membership using
#  dataset 'xxx'




