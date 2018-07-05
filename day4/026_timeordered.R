
# timeordered package R

# an extension to igraph for temporal network analysis


# Construction of time-ordered networks (temporal graphs). 
# Shortest-time and shortest-path-length analyses. 
# Resource spread calculations. 
# Data resampling and rarefaction for null model construction. 
# Reduction to time-aggregated networks with variable window sizes
# application of common descriptive statistics to these networks. 
# Vector clock latencies. 
# Plotting functionalities & full compatibility with all network methods in the igraph library.

## Example data come from Blonder & Dornhaus ant studies (Univ Arizona)

# Several hundred ants from four colonies were uniquely marked with paint. 
# Their in-nest activity was recorded and a complete set of pairwise interactions 
# between all individuals at all times was made. 

library(timeordered)
data('ants')
str(ants)
head(ants)
table(ants$VertexFrom)
table(ants$VertexTo)

# A data frame containing 1911 observations over 24 minutes.
# Each row is a unique interaction between two ants. 
# Each interaction is directed, indicating that the VertexFrom
# ant has initiated a contact with the VertexTo ant


## Generate a time-ordered network
g <- generatetonetwork(ants) # can add in missing vertices with a 2nd argument
g # a weighted directed network of class igraph


## Visualize a Time-Ordered Network
plottonet(g,edgecolor="black")  #x axis = vertices, y-axis = time  # !!!

# note - see 'musicnotationR package'.
# note - see 'timeline' function from ndtv package.


## Slice into time units
td100 <- generatetimedeltas(0,1500,100) #start time, stop time, size of window
td100  # a n x 2 matrix; rows 

ns100 <- generatenetworkslices(g, td100) 
ns100  # a list of n time-aggregated networks corresponding to the n time windows.


## Aggregate over time
plotnetworkslices(ns100, td100)  
# slices = A list of n time-aggregated networks
# timedeltas	 = A n x 2 matrix describing the start and stop times for each time-aggregated network

plotnetworkslices(ns100, td100, edge.arrow.size=.2)  # doesn't work - doesn't inherit from igraph.plot
# I actually modifed the source function so this now works (see below)


td500 <- generatetimedeltas(0,1500,500)
ns500 <- generatenetworkslices(g, td500)
plotnetworkslices(ns500, td500)


## modified plot functions:
plotnetworkslices1 <- function (slices, timedeltas, ...) 
{
  timestrings <- paste(timedeltas[, 1], " - ", timedeltas[, 
                                                          2])
  par(mfrow = c(ceiling(sqrt(length(slices))), ceiling(sqrt(length(slices)))))
  par(mar = c(1, 1, 1, 1))
  for (i in 1:length(slices)) {
    plottanet1(slices[[i]],...)
    text(0, 0, timestrings[i])
  }
}

plottanet1 <- function (timeaggregatednetwork, layout = layout.circle, vertex.label = V(timeaggregatednetwork)$name, 
                        vertex.size = 0, vertex.label.cex = 0.5, edge.arrow.size = 0.5, 
                        edge.width = E(timeaggregatednetwork)$Count/5, ...) 
{
  plot(timeaggregatednetwork, layout = layout, vertex.label = vertex.label, 
       vertex.size = vertex.size, vertex.label.cex = vertex.label.cex, 
       edge.arrow.size = edge.arrow.size, edge.width = edge.width, 
       vertex.label.family = "Helvetica", ...)
}


plotnetworkslices1(ns500, td500, edge.arrow.size = 0.2, layout=layout_nicely,
                   edge.color="tomato")
# changing this such that it fixes nodes all the way through the plot would be fun.

dev.off()




#### Network Descriptives over Time

# applynetworkfunction(slices, fun)
# Applies a function (typically a descriptive statistic) to multiple time-aggregated networks
# slices	 = A list of time-aggregated networks, of class igraph
# fun	 = The function to be applied; takes a single argument


md100 <- applynetworkfunction(ns100, function(x) {diameter(x)})
md100

plot(midpoints(td100),unlist(md100),type="l",col="blue",xlab="Time",ylab="Network diameter")


md500 <- applynetworkfunction(ns500, function(x) {diameter(x)})
lines(midpoints(td500),unlist(md500),col="red")

legend("topleft",c("100 window size", "500 window size"),bty="n",lwd=2,col=c("blue","red"))


# choice of window size can be very impactful !




### lagged time

tl100 <- generatetimelags(0,1500,100) # a matrix of increasingly large windows
tl100

nl100 <- generatenetworkslices(g, tl100)
ml100 <- applynetworkfunction(nl100, function(x){diameter(x)})
plot(maxpoints(tl100),unlist(ml100),type="l",xlab="Aggregation time",ylab="Diameter",col="blue")

# how network changes over time.




### Randomizing Data.

# randomizetimes()
# Produces a new event list from an existing event list with resampled event 
# times given certain constraints on randomization. 
# Effectively re-orders pairs of start/stop times between different vertices.

rt <- randomizetimes(raw=ants,withinvertexfrom=T,byvertexfrom=T,withreplacement=T)
rt  # randomized dataset
head(rt)

# note this fixes the frequency of each edge, but resamples with replacement the timestart/timestop
# no changes to durations of edges as timestart/timestop are kept as pairs
ants[ants$VertexTo=="GGW_",]
rt[rt$VertexTo=="GGW_",]


plottonet(generatetonetwork(rt),edgecolor="black") # still looks ugly



## randomize many times to check significance....

## e.g. the average path length different in the first 
#       half of the network compared to the second half?


range(ants$TimeStart)

td <- generatetimedeltas(0,1438,1438/2)
td

ns <- generatenetworkslices(g, td)
ns

md <- applynetworkfunction(ns, function(x) {average.path.length(x)})
md
#[[1]][1] 2.772973
#[[2]][1] 2.357626

obsdif <- md[[1]][1] - md[[2]][1]
obsdif # [1] 0.4153472


# bootstrap one time ...

rt1 <- randomizetimes(raw=ants,withinvertexfrom=T,byvertexfrom=T,withreplacement=T)
g1 <- generatetonetwork(rt1) 
ns1 <- generatenetworkslices(g1, td)
md1 <- applynetworkfunction(ns1, function(x) {average.path.length(x)})
dif <- md1[[1]][1] - md1[[2]][1]
dif # [1] 0.4403754


# bootstrap many times... (this is very slow..., might even be worth recoding the function)
nboot <- 500
dif <- vector('list',nboot)
for(i in 1:nboot){
  rt1 <- randomizetimes(raw=ants,withinvertexfrom=T,byvertexfrom=T,withreplacement=T)
  g1 <- generatetonetwork(rt1) 
  ns1 <- generatenetworkslices(g1, td)
  md1 <- applynetworkfunction(ns1, function(x) {average.path.length(x)})
  dif[[i]] <- md1[[1]][1] - md1[[2]][1]
}

unlist(dif)
dif.df <- data.frame(vals=unlist(dif))
head(dif.df)

# visualize
library(ggplot2)
ggplot(dif.df, aes(vals)) +
         geom_histogram(color='white')+
         geom_vline(xintercept = obsdif, lty=2, col='red')

# pvalue
sum(unlist(dif)>=obsdif) / nboot # p = .528






### Time latencies (vector-clock latencies)

# Vector clock latencies describe the minimum time delay between one 
# individual broadcasting a signal and another individual receiving it, 
# at a given time, through any causally permitted path in the time-ordered network. 
# Smaller values indicate individuals that are connected by shorter 
# causally-permitted paths at a given time.



l <- generatelatencies(ants,allindivs=union(ants$VertexFrom, ants$VertexTo))

str(l)

l  # A n x n x m array, where n is the number of vertices and m is the maximum start time in the raw event list. 


# The [i,j,k] entry of the array describes the latency from i to j at time k. 
# NA is returned if there is not causally permitted path between i and j by time k.

l[,,1000]   # time point t=1000
l[6,2,1000]  # W___ -> WBYG at t=1000


apply(l, c(1,2), mean, na.rm=T)  # average between pairs of nodes over all time points

apply(l, 3, mean, na.rm=T)  # average between pairs of nodes over all time points (shifted by 1)

plot(apply(l, 3, mean, na.rm=T), type="l")  # does this reflect time issues in calculating, or real processes?


# e.g. for each individual, for each time point, get:
# a) the total number of individuals they have or can influence in the future

infl <- plyr::aaply(l, c(1,3), function(x) length(na.omit(x)))  # average between pairs of nodes over all time points
str(infl)
infl[1:20, 1000:1010]

#plot
ggplot(as.data.frame(reshape2::melt(infl)), aes(x=X2, y=value,color=X1)) + geom_step()
# horrible graph - but something we could investigate further, is that appears that
# some ants are early 'influencers' but other ants later influence many others more.


# b) the average vector clock latency for each individual at each time point
#(note - if only influence one node this could be not that interesting)
vcl <- plyr::aaply(l, c(1,3), mean, na.rm=T)  
str(vcl)
vcl[1:20, 1000:1010]




## Shortest paths over time
# shortesttimepath() = Determines a path (shortest by the least time) between a vertex at a
# start time and another vertex at any later time.

stp <- shortesttimepath(g, "YYRB", 280, "WRRY")
stp

# + 1/6900 vertex, named, from 88440d7:
#   [1] WRRY 315

# output: A vertex list containing all the events on the shortest-time path 
# between the start vertex at the start time and the stop vertex at a later time.












