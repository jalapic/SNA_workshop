##### Dominance Hierarchy Analysis #####

#  Example Dataset
#  Group of 12 individuals
#  Wins and losses (and ties) are collected in real time by observers

# Get data
df <- read.csv("data/mousedata.csv", stringsAsFactors = F)
head(df)


# Data Cleanup

df$Notes[!is.na(df$Notes)]

which(df$Notes=="couldn't tell who won") # need to fix this to tie
df[951,]
df$Timestamp <- as.POSIXct(strptime(df$Timestamp,'%m/%d/%Y %H:%M:%S'))

df$day <- lubridate::yday(df$Timestamp) - min(lubridate::yday(df$Timestamp)) + 1
df$zhour <- lubridate::hour(df$Timestamp)-11


head(df)
tail(df)
unique(df$day)
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 21 22
# Notice that there are no observations for day=20 - this will be relevant later on when we plot data by day.


#Actor/Recipients
unique(df$Actor)
unique(df$Recipient)

# First of all, we can remove the 'Start' and 'End' rows. 
# These indicate when an observer starts and finishes their set observation.
# We need these when counting rates of behavior per hour, but we won't do that in this example.

df <- df[df$Actor!="Start",]
df <- df[df$Actor!="End",]

#The next thing to deal with is that some rows have multiple values in the 'Actor' or 'Recipient' columns. 
#This can be either:
  
# two individuals tie during a contest (then both Actor and Recipient will have same two animals in the entry)
# one Actor beats more than one Recipient simultaneously
# one Recipient is beaten by more than one Actor simultaneously (much less common)
# To simplify this process, I have stored a function - expandrows on GitHub that we can source. 

# To use it, you must have splitstackshape and data.table installed. 
# The former is a great package for easy management of google survey type data. 
# This function also adds a column 'score' with a 1 indicating a clear win for the 
# Actor vs the Recipient, and a 0.5 indicating a tie.

library(data.table)
library(splitstackshape)
source("day4/hierarchy_custom_fns.R")
df1<-expandrows(df)
head(df1)
nrow(df1)

# We can check our expanding rows worked like this:
  
  table(df1$Actor)
  table(df1$Recipient)
  table(df1$score) # In total we have 1031 agonistic interactions of which only 6 are ties.


#  Having got our data cleaned up, we can quickly look at a preliminary raw sociomatrix:
  
  xtabs(~ Actor + Recipient, df1)


  
  
####  Data Analysis ----
  
  
# Once our data is in this clean format we can proceed with the basic data analysis. 
# First of all we will consider all behaviors together 
# - i.e. we won't subset or weight by fighting, chasing, mounting or subordinate behaviors. 
# We may want to do this for more fine-grained analysis, but not for this example. 
# Also, we will consider all data collected over all days together to start with.

# We will use the compete package to create sociomatrices and calculate several metrics.
library(compete)

  
## Create sociomatrices

# First we only need the winner and loser variables. 
# Also, in this basic analysis, we will exclude ties. 
# We could consider ties to be a 0.5 win, but given their low prevalence, 
# it does not affect our results to exclude here. 
# (Note the get_wl_matrix and get_di_matrix functions in compete can also automatically remove ties).

# Also, because df1 is actually a data.table (produced by the expandrows function),
# we need to use data.table style indexing.

str(df1)

wldf <- df1[score==1][, c(2,4), with = FALSE] #data.table indexing
head(wldf)


## The get_wl_matrix creates a raw frequency sociomatrix of wins and losses. 
# This is similar to the table above. 
# Here, e.g. animal 6 'won' 13 times against animal 2, and animal 7 had 17 wins against animal 8.

wlmat <- get_wl_matrix(wldf)
wlmat


## For quick visualization purposes, we can use the org_matrix function. 
# There are three methods of displaying the table:
 # by alphanumeric name (alpha), 
 # by total wins (wins)
 # by David's Scores (a measure of relative dominance).

## Using the David's Scores Methods, it looks a lot cleareer that this social group has a hierarchical structure.

org_matrix(wlmat, method="ds")


## Another useful matrix to keep is the binarized win-loss matrix. 
# This can be done using the get_di_matrix function. 
# There are many ways in which a raw frequency sociomatrix could be converted 
# to a binary matrix involving how to deal with ties, how to deal with structural 
# zeros (unknown relationships where no observations were made between two individuals), 
# and what criteria to use to choose a 'clear' winner. 

# Here, we will use the simplest measure of assigning a 1 to individuals who win more 
# frequently than the opposing individual and a 0 to losers or if there are ties 
# (e.g. if both animals had 1 win against each other).

bimat <- get_di_matrix(wlmat)
org_matrix(bimat, method="ds")




#### Basic hierarchy Measures ----

### Descriptives

# From the above matrices we can calculate a number of descriptive statistics.

# The rshps function gives us the total relationships in the group, 
# the total number of unknown relationships (i.e. each animal recorded 0 wins against each other), 
# the number of relationships that are tied (i.e. no clear winner), 
# the number of twoways relationships (where both animals recorded at least one win) and 
# the number of oneways relationships (where only one animal recorded a win).

rshps(wlmat)



### Directional Consistency


# The Directional Consistency of the sociomatrix
# - 0 meaning no directional consistency and 1 indicating that all contests are won 
# by more dominant individuals over more subordinate individuals. 
# The skew-symmetry index determines if there is asymmetry in interactions in a sociomatrix. 
# 0 indicates complete reciprocity whereas 0.5 indicates complete unidirectionality.

# The dc_test function will run the statistical tests suggested by Leiva et al. 2009 
# to ascertain if the directional consistency and phi values are significant or not.

dc_test(wlmat)

# For quick access to the DC and phi values, the following functions can be run:
dci(wlmat)
phi(wlmat)



### De Vries' Modified h' value

# The most common measure of social hierarchy linearity is the modified h' value by De Vries 1995. 
# The devries function will provide the h' value of a sociomatrix and associated p-value
# from a randomization test.

devries(wlmat)

# It's also possible to plot the results of the randomization test:

devries(wlmat, plot=T)





### Triangle Transitivity

# The third measure of hierarchical organization is the triangle transitivity suggested by
# Shizuka & McDonald 2012. This measure calculate the proportion of transitive versus
# intransitive triads within the directed network. The index ranges between 0 and 1, with
# 1 indicating that all triads are transitive (i.e. there are no cycles). This function also
# provides output from a randomization test to assess if the proportion of transitive triads 
# is higher than expected (i.e. there is hierarchical organization).

ttri_test(wlmat)

# This value can be ascertained quickly using ttri:
  
ttri(wlmat)



### Steepness

# Another measure of hierarchies are how steep the individual differences in David's Scores are 
# - prosposed by De Vries et al 2006. The higher the steepness, the greater the differences 
# between individual ranks in their ratings. The scores range between 0 and 1, with 1 having 
# the greatest differences between ranks in scores. We can test this using the steepness package.

library(steepness)
steep <- steepness::steeptest(wlmat, rep=10000)
steep
steep$Stp #Steepness
sum( (steep$Stpsim > steep$Stp) / 10000 )  #pvalue based on node permutations



#### Individual Ranking Measures ----

# A proliferation of ranking methods exist to try and determine which individual is more
# 'dominant', 'powerful' or 'influential' in a group. Many of the more recent methods are 
# network based, others come from the sports data literature. 

# Here I will describe some standard methods used in animal behavior.



## David's Scores

# A simple measure is the David's Scores of each individual. 
# This can be considered to be the opponent-adjusted win proportion of each individual. 
# Individuals with positive David's Scores are more dominant. 
# Individuals with negative David's Scores are losing more fights.

ds(wlmat)
ds(wlmat,norm=T)
# Viewing this as a simple plot shows the steepness of David's Scores:
  
plot(1:12, rev(sort(ds(wlmat))), "l",
       xlab = "Rank",
       ylab = "David's Score",
       main = "David's Scores by Rank")
abline(h = 0, col = "red", lty = 3)




## I&SI Method

# A commonly used method is the linear ordering algorithm - the I&SI ranking method. 
# This attempts to shuffle the binary sociomatix such that 1s are all above the diagonal 
# and 0s are all below the diagonal. Further, it tries to get 1s under the diagonal as
# close to it as possible. This is computationally expensive - and I need to implement this 
# in C++ in the next version of the compete package. There are two versions of this algorithm 
# - isi98 is the original method proposed here and isi13 is the updated method proposed here. 
# I recommend using the former as the updated version is too slow for most matrices - 
# and the additional benefit in improved linear ordering isn't huge. Also, it is worth running
# this procedure a few times to find the optimal ranking as it uses randomization to try and 
# find the best matrix.

# The output gives the initial matrix order, the proposed 'best matrix', 
# the I (number of inconsistencies), the SI (the strength of inconsistencies), 
# and the 'best order' of ranks. It also gives 'rs' the correlation between the ranks 
# proposed by the I&SI method and the David's Scores. 

#Running this example several times, it appears as if the best solution is I=3, SI=10 and rs=0.88.

isi.out <-  isi98(wlmat)
isi.out





## Bonacich's Power Centrality ---

# Bonacich conceived as 'powerful' actors in a network in two ways.
# 1.those who have outgoing edges to many individuals who themselves have outgoing edges to others
# 2. those who receive edges from many individuals who are well connected to others who 
# themselves connect to others who solely rely on that individual for connections. (more "dependency")


# these opposite interpretations can be measured by modifying the "attenuation factor" 
# Positive attenuation factor (between zero and one), being connected to neighbors with
# more connections makes one powerful. 
# Negative values represent dependency.

# for dominance, we use positive attenuation factors.
# choose an attenuation factor that is 1/N.


g <- igraph::graph_from_adjacency_matrix(wlmat,mode = c("directed"),weighted = T)
bp <- igraph::bonpow(g, exponent=1/igraph::gorder(g),rescale = T)
bp
plot(1:12,bp[rev(order(bp))], type="l")





## Inter correlation of ranks ----

## let's look at how these compare.

bp # power centrality
bp[rev(order(bp))]
names(bp[rev(order(bp))])
bprank<- names(bp[rev(order(bp))])

isirank <- isi.out$best_order # isi method
isirank

dss <- ds(wlmat) # david's scores
dss[rev(order(dss))]
names(dss[rev(order(dss))])
dsrank <- names(dss[rev(order(dss))])


ranks <- as.matrix(
  cbind(
  bp = bprank,
  isi = isirank,
  ds = dsrank)
)

ranks

Hmisc::rcorr(ranks, type=c("spearman"))  # bonacich's power and ISI have strong agreement here.





### Color Sociomatrix ---

# We can also use the observed best ranking order of our choice to make a customized 
# raw sociomatrix that we could use in a publication. This is using a ggplot2 based 
# function I wrote called matrixplot. If you lookup the function, you can change 
# the color scheme if you wish.

matrixplot(wlmat, mylevs=isi.out$best_order)


# There is also a plot to color the binarized matrix based on the directional 
# consistency of each relationship. The redder a cell, the higher the proportion 
# of wins by that individual. This method is useful for quickly visualizing 
# the inconsistencies in the hierarchy.

matrixplot0(wlmat, mylevs=isi.out$best_order)





### Network Certainty ---

# Another valuable approach is to measure the certainty we have of each individual's ranking 
# using network certainty. With this method, we can get the ranking of each individual according
# to their network position (akin to how much power or influence over others an individual has) and
# determine how certain we are of that ranking by examining the consistency of indirect relationships.
# We can use the Perc R package to do this.
# Vandeleest et al 2016 https://peerj.com/articles/2394.pdf

library(Perc)
obsmat <- as.conflictmat(wldf)
DominanceProbability.obs <- conductance(obsmat, maxLength = 2)
s.rank.obs <- simRankOrder(DominanceProbability.obs$p.hat, num = 10, kmax = 10)
dfobs <- merge(individualDomProb(DominanceProbability.obs$p.hat), s.rank.obs$BestSimulatedRankOrder)
plot(dfobs$ranking, dfobs$Mean,
xlab="Rank", ylab="Dominance Certainty")



## Clearly we could also bootstrap or jackknife our data to evaluate consistency in rankings 
# from replicate datasets to also achieve consistency in rankings / david's scores / power centrality etc.




#### Temporal Based Methods ----


### Glicko Ratings

# There are a number of temporally based ratings methods that calculate dynamic changes 
# in ratings over time. Two of these methods - ELO & Glicko - are pairwise-contest models 
# where all individuals start with an initial rating. Without any other knowledge about the 
# individuals we assume they all have the same initial ratings. Individuals gain points 
# for each win and lose points for each loss. The magnitude of the gain/loss in points 
# is based on the ratings difference between contestants at a particular time. Each method 
# has a constant value that adjusts this calculation. I prefer the Glicko method because 
# it additionally has a standard deviation of ratings giving us a measure of how certain we 
# are that individuals differ. The Glicko also has a decay function meaning that the rating 
# uncertainty increases if individuals haven't competed in a while.

# We could calculate ratings at the end of each day. Most primate studies use this approach. 
# However, as we observe all animals all of the time, I prefer to recalculate ratings after 
# every observation and use a smaller constant value (a higher constant value makes the ratings 
# more volatile - they respond to changes more rapidly).

# We can use the PlayerRatings package to calculate the Glicko ratings like this:

library(PlayerRatings)
df1 <- df1[order(df1$Timestamp),] #ensure in date order
df1$event <- 1:nrow(df1)
glick.df <- df1[, c(11,2,4,10), with = FALSE] #need event, actor, recipient, score
gl <- glicko(glick.df, history=T, cval=2)
gl



# Of course, the choice of cval affects how volatile individual ratings are. 
# When recalculating ratings after every behavioral interaction, we have found that
# a lower value of cval provides stable rankings that most strongly reflect the rankings 
# of animals as determined by other methods.


# How Glicko ratings change over time can be calculated using the basic plot function:
plot(gl,npl=12)
str(gl)
# The above base r plot is not aesthetically pleasing. 
# I have created a default function that allows us to take a glicko object and
# convert this to a more attractive plot.
plotglicko(glick.df, cval=2, ylim1=1500, ylim2=3000, thetitle="Glicko Ratings over Time",linewd=.5)



# We can also plot the final glicko ratings by rank and show the deviations in ratings scores.
ggplot(gl$ratings, aes(x=1:12, y=Rating)) + 
geom_point(size=2) + 
scale_x_continuous(breaks=1:12, labels=gl$ratings$Player)+
geom_errorbar(aes(ymin=Rating-Deviation, ymax=Rating+Deviation),
width=.2,                    
position=position_dodge(.9),
size=.5) +
geom_hline(yintercept=2200, color='red', linetype='dotted')+
ylab("Glicko Rating (Mean +/- SD)") +
xlab("Animal ID") +
theme_classic() +
ggtitle("Final Glicko Ratings")






#### Temporal Social Dynamics ----

# Calculating indices of hierarchical organization using all data at once may not be suitable. 
# We may, for instance, wish to see how one metric changes over time. 
# Here, we will look at how triangle transitivity changes over time.

# Below, we split the raw data into a new dataframe for everyday containing data 
# from day 1 up to that day. Therefore there are 21 dataframes in total 
# (as there are 21 days' worth of data - although the last day of observations was 22, 
# we did not have any observations for day 20).

wlmat.days <- lapply(
Reduce(rbind, split(df1, df1$day), accumulate=TRUE),
function(x) get_wl_matrix(x[score==1][, c(2,4), with = FALSE])
)
wlmat.days
wlmat.days[[1]]

# The above matrix is the winner-loser sociomatrix based on only day 1 data.
# We can then plot how triangle transitivity changes across days:

plot(c(1:19,21:22), unlist(lapply(wlmat.days, function(x) ttri(x)$ttri)), "l", 
xlab = "Day",
ylab = "Triangle Transitivity",
main = "Change in Triangle Transitivity over Days")



# Of course, we could prefer to use a different temporal strategy. 
# For instance, we could use a sliding window approach looking at how triangle transitivity
# changes for e.g. 3 days at a time. So we would plot ttri for days 1-3, then days 2-4, 3-5 etc.

# Another extension of this approach is something we discussed in our Animal Behavior paper. 
# At the end of each day, we could only keep the last 'n' interactions between any pair of individuals. 
# This has the advantage of not including behavioral events that occurred long before the day of interest. 
# For example, if animal A had accrued 100 wins against B and B had never beaten A, then if that relationships changed on e.g. day 10 it could take several days to register in the ttri - because it could take a long time for B to get more than 100 wins against A to 'flip' that relationship. Only considering e.g. the last 3 interactions overcomes this issue.
# To do these calculations we can use a function I wrote that we have already sourced from GitHub above.

ttri_N <- lapply(
Reduce(rbind, split(df1, df1$day), accumulate=TRUE),
function(x) ttri_lastN(x, N=3)
)
plot(c(1:19,21:22), unlist(ttri_N), "l", 
xlab = "Day",
ylab = "Triangle Transitivity",
main = "Change in Triangle Transitivity \n using 3 most recent interactions")


