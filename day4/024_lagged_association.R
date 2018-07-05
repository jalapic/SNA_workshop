### Lagged Association Rates ----


# Classical approach - Calculate lagged association rate g(tau) from Whitehead (2008)

# the dyadic lagged association rate of lag T is the probability of association T time units
# after a previous association and is estimated by the ratio of the number of associations
# X has with Y T time units apart divided by the number of pairs of identifiations  T
# time units apart.

# average over all dyads is the population lagged association rate g(T)

# see Whitehead (1995, 2008)


library('asnipe')


data("group_by_individual")
data("times")
data("individuals")

gbi
inds


# Calculate lagged association rate for great tits

times
range(times)

lagged_assoc <- LAR(gbi,                    # a K group by N interaction matrix
                    times,                  # a vector of size K of the times of each group/event
                    timejump=3600,          # the step length for tau 
                    classes=inds$SPECIES,   # vector of size K of classes (to be used for subsetting)
                    which_classes='GRETI'   # which class to include in network
                    )

lagged_assoc
str(lagged_assoc)
# col 1 is the log(time)
# col 2 is the lagged association rate for each time period T

plot(lagged_assoc[,1],lagged_assoc[,2],type="l",
     xlab="Time (log)", ylab="Lagged Association Rate",
     main="LAR for Great Tits")



# The choice of the time interval T values should not be too small or too large
# This can be checked by iterating over a range of T values.

# Plotting g(T)/LAR against different T values. (Whitehead Fig 5.13)
# may want to make interval smaller, but this will take a while......
lagT <- c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,7500,10000,15000,20000)
lagT

res <- vector('list',length(lagT))
for(i in 1:length(lagT)){
 res[[i]]<- LAR(gbi,times,timejump=lagT[i],classes=inds$SPECIES,which_classes='GRETI')
}

res[[6]]

lagt.df <- as.data.frame(do.call('rbind', Map('cbind', res, tlag = lagT)))
head(lagt.df)

colnames(lagt.df)[1:2]<-c("time","lar")
library(ggplot2)

head(lagt.df)

# make times on actual time scale
lagt.df$newtime <- (exp(lagt.df$time) * lagt.df$tlag)-lagt.df$tlag


ggplot(lagt.df, aes(newtime,lar,color=factor(tlag),group=factor(tlag))) + 
  geom_point(size=.5) + geom_line()


ggplot(lagt.df, aes(log(newtime),lar,color=factor(tlag),group=factor(tlag))) + 
  geom_point(size=.5) + geom_line()




## Generate a measure of 'uncertainty' / 'precision' in these measures:

# Whitehead suggests that jackknifing the data is the most suitable for lagged ARs
# and that permutations are not that suitable.

#  Create subsets of the data, removing one or more observations and 
#  calculating the lagged association rate for each.
#  The standard error can be estimated from comparing all samples.


# let's pick a time interval of 2500
lagged_assoc <-    LAR(gbi,
                       times,
                       timejump=2500,
                       classes=inds$SPECIES,   
                       which_classes='GRETI'
                      ) 

lagged_assoc # have 12 rows of results


# create an empty variable to store results, and
# store the result after each group has been removed

lagged_assoc_perm <- matrix(NA, nrow=12, ncol=nrow(gbi))

# create a loop to run each simulation, and run on the
# dataset having removed one row at a time. Here we are
# only interested in the second column of the result.


 for(i in 1:nrow(gbi)){
  
 lagged_assoc_perm[,i] <- LAR(
                             gbi[-i,],     # remove each sample in turn
                             times[-i],    # remove that time from the vector also
                             2500, 
                             classes = inds$SPECIES,
                             which_classes='GRETI')[,2]
   }
  

lagged_assoc_perm
lagged_assoc_perm[1:12,1:3]

# calculate the standard error
N <- nrow(gbi)  #347
means <- rowMeans(lagged_assoc_perm)
se <- sqrt(((N-1)/N) * apply((means - lagged_assoc_perm)^2,1,sum))
se


colnames(lagged_assoc)<-c("time", "lar")
lagged_assoc <- data.frame(lagged_assoc)
lagged_assoc$se <- se


ggplot(data=lagged_assoc,
  aes(x=time,y=lar))+ 
  geom_line() +
  geom_errorbar(aes(ymin=lar-se, ymax=lar+se), width=0.05, size=.1)



  
  
  
