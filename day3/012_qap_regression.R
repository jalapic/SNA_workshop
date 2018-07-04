####    QAP Regression  (aka MR-QAP)

library(sna)
# Permutation tests can be used for linear regression
# Each variable in the model is a network

# Known as MRQAP (multiple regression QAP)

# These matrices can contain all manner of information
# e.g. - relation data, attribute dyadid covariate data, genetic relatedness, etc.


# Here are 3 random matrices with 10 actors.

x <- rgraph(10, 3) 
x[1,,] # matrix 1
x[2,,] # matrix 2
x[3,,] # matrix 3

str(x)

# Now let's construct an outcome matrix (y)
# this is a linear combination of the first two networks, 
# but has nothing to do with the third one ....

y <- 2*x[1,,] + 7*x[2,,] + 3
y


## Say we wanted to see if we can predict 'y' based on 'x1','x2' and 'x3'...

# we'll use the function 'netlm' from the 'sna' package.
# the DV should be a single matrix or network object.
# the IV should be a list of network objects or a stack of matrices.

net.mod.1 <- sna::netlm(y, x, reps=1000)
summary(net.mod.1)

# intercept is 3 (as above) in the model
# x1 and x2 have significant effects on y (as in model)
# x3 has nothing to do with y (not in model)



##  MRQAP with Double-Semi-Partialing (DSP) - Dekker et al (2007)

# Dekker, D., Krackhard, D., Snijders, T.A.B (2007) 
# Sensitivity of MRQAP tests to collinearity and autocorellation conditions. 
# Psychometrika 72(4): 563-581.

#  Tests whether y is related to x1 on y while controlling for x2 and x3. 
#  This differs from regular mrqap, where the dependent (y) value is randomised, 
#  testing for whether y is related to x1 and x2 together.


library('asnipe')

net.mod.2 <- asnipe::mrqap.dsp(y ~ x[1,,] + x[2,,] + x[3,,], randomisations=10000, directed="directed")
net.mod.2  #look at estimates for each matrix






## Example 1.   Krackhardt's Business Organization Data

advice <- read.csv("data/krack_advice.csv")     
report <- read.csv("data/krack_reports.csv")    
friend <- read.csv("data/krack_friends.csv")   


# remove first column
advice <- as.matrix(advice[,-1])
report <- as.matrix(report[,-1])
friend <- as.matrix(friend[,-1])

advice
report
friend





# Visualize three networks
library(igraph)

g.advice <- graph_from_adjacency_matrix(advice)
g.report <- graph_from_adjacency_matrix(report)
g.friend <- graph_from_adjacency_matrix(friend)

par(mfrow=c(1,3))
layoutg <- layout_nicely(g.advice)
plot(g.advice, layout=layoutg, edge.arrow.size=.1, main="advice")    
plot(g.report, layout=layoutg, edge.arrow.size=.1, main="report")    
plot(g.friend, layout=layoutg, edge.arrow.size=.1, main="friend")    
par(mfrow=c(1,1))
        

# Results of MR-QAP

net.mod.3 <- sna::netlm(advice, list(friend,report), reps=1000) # list of matrices is ok
summary(net.mod.3)

# R2 = 6.3%  - Not very high!
# report-to and friendship don't influence advice seeking very much

# 'reports to': beta is .472 and p <.001 so some effect 
# Means for any batch of 1000 dyads where i reports to j, 
# we expect to see about 472 more cases of advice-seeking than when i does not report to j.





#### MRQAP with Double-Semi-Partialing (DSP) ----

### Example - does species and sex predict social network position ?


library('asnipe')
data("individuals")
data("group_by_individual")
gbi
inds

# Generate network
network <- get_network(gbi)

# Create a species similarity matrix
species <- array(0,dim(network))

# Create a sex similarity matrix
sex <- array(0,dim(network))

# Fill each matrix with 1 (same) or 0 (different)
for (i in 1:nrow(network)) {
  species[,-i] <- as.numeric(inds$SPECIES[1] == inds$SPECIES[-i])
  sex[,-i] <- as.numeric(inds$SEX[1] == inds$SEX[-i])
}


# check
str(species)
str(sex)
species
sex

# Run mrqap.dsp
reg <- mrqap.dsp(network ~ species + sex, randomisations=1000) # will take a while

# Results
reg  # p values for each fixed factor control for the influence of other fixed effects
# species and sex do not appear to influence network relationships


####
