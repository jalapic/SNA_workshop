
## Permutation Biases ----

## How pre-network permutations can resolve issues of bias when using node-based permutations


# e.g. Sampling Bias, 
# Farine & Whitehead, Appendix 4, Box 5 code

# how observable individuals are can affect network composition
# Here, males and females do not vary in gregariousness 
# But females are observed with only 70% reliability
# Males are observed with 100% reliability (e.g. bright conspicuous birds)
# 20 individuals of each sex were consigned to one of two areas

# Load required libraries
library(igraph)
library(sna)
library(asnipe)
library(lme4)

sps <- readRDS('data/spsX.rds')    # matrix based on full observations
sps2 <- readRDS('data/spsX2.rds')  # matrix based on females having 70% observation rate
ids <- readr::read_csv('data/ids_spsX.csv')    # ids dataframe


sps
str(sps)   # 200 x 40 x 40
str(sps2)  # 200 x 40 x 40

# Generate original network
network_original <- get_network(sps,data_format="SP",association_index = "SRI")
rownames(network_original) <- ids$SEX
colnames(network_original) <- ids$SEX
network_original

# Generate subsampled network
network <- get_network(sps2,data_format="SP")
rownames(network) <- ids$SEX
colnames(network) <- ids$SEX
network


# Both males and females have the same average degree measured on the full set of associations

# Calculate degrees for original network - no difference
ids$DEGREE_ORIG <- degree(network_original,gmode="graph")
plot(ids$DEGREE_ORIG~factor(ids$SEX),xlab="Sex",
     ylab="Strength (weighted degree)",ylim=c(0,max(ids$DEGREE_ORIG)))

# But reducing the observation probability of females introduces a difference in their means 
ids$DEGREE <- degree(network,gmode="graph")
plot(ids$DEGREE~factor(ids$SEX),xlab="Sex",
     ylab="Strength (weighted degree)",ylim=c(0,max(ids$DEGREE)))



ids  # DEGREE is DV,  SEX is IV,  AREA is random effect.
#remember DEGREE was from the 'reduced' dataset, not the original.

# Fitting a GLMM model suggests that this difference was highly significant 
summary(lmer(DEGREE~SEX+(1|AREA),data=ids))
coef <- fixef(lmer(DEGREE~SEX+(1|AREA),data=ids))[2] # Calculate effect
coef


# when calculating the P value using randomizations 
# the observed coefficient value does not fall outside 
# the distribution of randomized coefficient values

sampling_periods <- 100 # here there were 100 sampling periods, over 2 areas.
# each area was sampled 100 times
areas <- 2              # there are 2 areas


# Create random networks, randomising within day and within area
networks_rand <- network_permutation(sps2,data_format="SP",
                                     association_matrix=network,
                                     days=rep(c(1:sampling_periods),2),
                                     within_day=TRUE,
                                     locations=rep(1:areas,each=sampling_periods),
                                     within_location=TRUE)

str(networks_rand) 
round(networks_rand[1,,],2)

# Calculate degree distribution for each network
deg_rand <- apply(networks_rand,1,function(x) { degree(x,gmode="graph")})
str(deg_rand)  #degrees of 40 individual observed (rows) in each of 1000 randomizations (cols)
deg_rand[1:10,1:10]

# Get coefficients for each randomisation
coefs <- apply(deg_rand,2,function(x) { fixef(lmer(x~SEX+(1|AREA),data=ids))[2] })


# Plot results
par(mfrow=c(1,3),cex.lab=1.5)

# Plot network
plot(ids$DEGREE_ORIG~factor(ids$SEX),xlab="Sex",
     ylab="Strength (weighted degree)",ylim=c(0,max(ids$DEGREE_ORIG)))

# Plot observed difference
plot(ids$DEGREE~factor(ids$SEX),xlab="Sex",
     ylab="Strength (weighted degree)",ylim=c(0,max(ids$DEGREE)))

# Plot resulting distribution
a <- hist(coefs,xlim=c(min(coefs),max(coefs)),col="dodgerblue",
          main="",xlab="Coefficient value",ylab="Frequency")
segments(coef,0,coef,max(a$counts),col="red")

par(mfrow=c(1,1),cex.lab=1) # reset plot area

# pvalues
sum(coefs >= coef)  # 123 (your number will vary)
sum(coefs >= coef) / 1000  #(Prand = 0.123)  (your number will vary)

# again, to generated stable p-values, rerun this many times.




