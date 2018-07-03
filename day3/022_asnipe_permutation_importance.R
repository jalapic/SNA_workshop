
### An example of Importance of Data Stream versus Node Based Permutations ----

library('asnipe')
data('group_by_individual')
gbi #151 columns (indivdiuals), 347 observation periods (groups)

# metadata
data("individuals")
inds
head(inds)
colnames(gbi) <- inds$RING.NUMBER

#make network
network <- get_network(gbi, data_format='GBI') # creates network on group_by_individual formatted data

network[1:6,1:6]

##  Determine if males and females differ in degree

# Measure weighted and binary degrees using sna library
library(sna)
deg <- sna::degree(network, gmode="graph")
deg_binary <- sna::degree(network, gmode="graph",ignore.eval=TRUE)
detach("package:sna")
deg[1:6]
deg_binary[1:6]


# Add degrees to attributes data
inds$DEGREE <- deg
inds$DEGREE_BINARY <- deg_binary
inds

# Visualize degrees by sex
inds$SEX[inds$SEX == ""] <- NA  #some missing sex data
inds$SEX <- factor(inds$SEX) # don't want to plot NAs

par(mfrow=c(1,3))
boxplot(DEGREE~SEX,data=na.omit(inds), col=c("red","blue"))
boxplot(DEGREE_BINARY~SEX,data=na.omit(inds), col=c("red","blue"))
plot(DEGREE~DEGREE_BINARY,data=na.omit(inds), col=c("red","blue")[SEX])
legend("bottomright",c("Female","Male"),col=c("red","blue"),pch=1)


# mean male and female edge weights
# for the edges that exit (i.e. non 0 edges)
network_male <- network[which(inds$SEX=="M"), which(inds$SEX=="M")]
mean(network_male[network_male>0])  #[1] 0.1481028

network_female <- network[which(inds$SEX=="F"), which(inds$SEX=="F")]
mean(network_female[network_female>0]) # [1] 0.1281252


# Calculate the observed statistic
network_male
network_female

t.test(network_female[network_female>0], network_male[network_male>0])

# Store
t_obs <- t.test(network_female[network_female>0], network_male[network_male>0])$statistic
t_obs  # -4.93739


## Permutations 1. Data-Stream (DS) Permutation

# Get 1000 networks from data-stream permutation
random_networks <- network_permutation(gbi,data_format = "GBI",
                                       association_matrix=network, permutations=1000)

# Create a 1D matrix to store results
t_rand_DS <- rep(0,1000)

# Get test statistic from the 1000 random networks
 for (i in c(1:1000)) {
  net_male_rand <- random_networks[i,which(inds$SEX=="M"), which(inds$SEX=="M")]
  net_female_rand <- random_networks[i,which(inds$SEX=="F"), which(inds$SEX=="F")]
  t_rand_DS[i] <- t.test(net_female_rand[net_female_rand>0],
  net_male_rand[net_male_rand>0])$statistic
   }


## Permutations 2. Node (NP) Permutation

# Get test statistic from 1000 node permutations
# using rmperm from SNA library
# Create a 1D matrix to store results
library(sna)
t_rand_NP <- rep(0,1000)
 for (i in c(1:1000)) {
  random_network <- rmperm(network)
  net_male_rand <- random_network[which(inds$SEX=="M"), which(inds$SEX=="M")]
  net_female_rand <- random_network[which(inds$SEX=="F"), which(inds$SEX=="F")]
  t_rand_NP[i] <- t.test(net_female_rand[net_female_rand>0],
                           net_male_rand[net_male_rand>0])$statistic
 }

detach("package:sna")


# Plot observed versus posterior distributions
par(mfrow=c(1,2))
hist(t_rand_DS,breaks=100)
abline(v=t_obs, col="red")
hist(t_rand_NP, breaks=100)
abline(v=t_obs, col="red")
par(mfrow=c(1,1))



# p-values
sum(abs(t_obs) < abs(t_rand_DS))/1000  # [1] 0.026
sum(abs(t_obs) < abs(t_rand_NP))/1000  # [1] 0.159

#nb more permutations than this are required to stabilize p-values.












