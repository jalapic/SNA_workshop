### tsna & networkDynamic tutorial

# comiled from tsna vignette
# https://cran.r-project.org/web/packages/tsna/vignettes/tsna_vignette.html

detach(package:timeordered)
detach(package:igraph)
library(tsna)
library(networkDynamicData)
library(sna)

# tsna functions accept as their input continuous- and discrete-time longitudinal networks 
# having vertex, edge, and attribute dynamics stored in the networkDynamic format


## Example Data, plotted just as an aggregated network (time ignored)

data(moodyContactSim)

moodyContactSim
as.data.frame(moodyContactSim)

plot(moodyContactSim,displaylabels = TRUE,main='aggregate network')





## Plot networks at specific time points
# this illustrates that path between 8-11-1 need to encounter time
# 1-11 path disappears long before 8-11 arrives

as.data.frame(moodyContactSim)

par(mfcol=c(1,2))
plot(network.extract(moodyContactSim,at=215),
     main='network at time 215',
     displaylabels=TRUE,
     label.cex=0.6,
     label.pos=5,
     vertex.col='white',
     vertex.cex=3)

plot(network.extract(moodyContactSim,at=750),
     main='network at time 750',
     displaylabels=TRUE,
     label.cex=0.6,
     label.pos=5,
     vertex.col='white',
     vertex.cex=3)
par(mfcol=c(1,1))



### Temporal Paths in Networks

# Xuan, et al (2002) call a permissable time-respecting path between a pair of vertices a journey. 
# We can search for the earliest arriving forward temporal path from vertex i to vertex j 
# this is NOT the same as geodesic distance.
# The forward reachable set (FRS) from vertex v at time t is the
# set of vertices reachable by forward temporal paths from v, begining at time t. 
# Also known as temporal out-component (Nicosia, et al 2013)


v10path<-tPath(moodyContactSim,v=10,start=0) # from vertex 10, starting time=0
v10path

# $tdist = indicates the time (elapsed from  start) at which each 
#    vertex is reached from the starting vertex v.

# $previous = for each vertex index, the vertex id of the previous vertex along path. 

# $gsteps = the number of steps (graph hops) in the path from v for each vertex. 


# plot tpath
plot(v10path, displaylabels=TRUE)

v1path<-tPath(moodyContactSim,v=1,start=0) # from vertex 1

par(mfcol=c(1,2)) # set up side-by-side plot
plotPaths(moodyContactSim,v10path, main='fwd path from v10')
plotPaths(moodyContactSim,v1path, main = 'fwd path from v1')
par(mfcol=c(1,1)) # turn off side-by-side plots

## nb. Notice that paths are directed (even if the underlying network is not) and not always symmetric.



## starting at different time points:

par(mfcol=1:2)
plot(tPath(moodyContactSim,v=1,start=0),
     main='tPath from v1 @ t=0')
plot(tPath(moodyContactSim,v=1,start=500),
     main='tPath from v1 @ t=500')
par(mfcol=c(1,1))



### Forward Reachable Sets ----

# comparing networks based on distribution and sizes of FRSs
# maximal reach over time is something that is valuable information in e.g. disease/information transmission

library(networkDynamicData)
data(concurrencyComparisonNets)

# three random networks generated via tergm models
# three networks with 100 time steps, same size and mean degree
# different degree distributions
base
monog
middle

mean(degree(as.network(base)))
mean(degree(as.network(monog)))
mean(degree(as.network(middle)))

# not very helpful visualizations
par(mfrow=c(1,3))
plot(base, main="base")
plot(monog, main="monog")
plot(middle, main="middle")
par(mfrow=c(1,1))

#tReach calculates FRS (here only doing for samples of 25 vertices 
baseTrees<-tReach(base,sample=25)
monogTrees<-tReach(monog,sample=25)
middleTrees<-tReach(middle,sample=25)

baseTrees
monogTrees
middleTrees

boxplot(cbind(baseTrees,monogTrees,middleTrees),
        main='fwd-reachable set size distributions for nets of varying concurrency')







### Rates of Change ----

# these functions count the number of edges being lost or formed over time
# return counts of the number of edges forming (edge onset at time point)
# and dissolving (edge terminus at time point).

# provide descriptive stats about momentary rates of change in the network.

tEdgeDissolution(base)
tEdgeFormation(base)


plot(tEdgeDissolution(base),main="Edge dissolution counts for network 'base'")
plot(tEdgeFormation(base), main="Edge formation counts for network 'base'")


# as proportions / also can change start/end times or intervals
tEdgeDissolution(base,result.type = 'fraction',time.interval = 10)
tEdgeFormation(base,result.type = 'fraction',time.interval = 10)



### Static graph metrics as time series ----
data(harry_potter_support)
harry_potter_support  #note the maximal time range
as.data.frame(harry_potter_support)
gplot(harry_potter_support)

# transitivity over all time points
tSnaStats(harry_potter_support,snafun='gtrans') # result is a  time series object

# compute triad census scores for each time point
tSnaStats(harry_potter_support,snafun='triad.census')

# compute betweenness
bet<-tSnaStats(harry_potter_support,snafun='betweenness')
bet  #betweenness scores over time

# mean betweenness over all time points (per individual)
colMeans(bet,na.rm = TRUE)

# mean betweenness over all individuals (per time)
rowMeans(bet)

# measure metrics over time intervals
base # Maximal time range observed: 2 until 102 
prestScores<-tSnaStats(base,'prestige',time.interval=25,rescale=TRUE) #indegree
dim(prestScores) # 5 time intervals,  1000 cols (nodes)
prestScores[1:5,1:10] 




#### Getting information about edges

edgeDuration(moodyContactSim) # the first edge was active for 32 time units, the second for 33, etc
summary(edgeDuration(moodyContactSim))
hist(edgeDuration(moodyContactSim))


hist(edgeDuration(base),ylim=c(0,800))
hist(edgeDuration(middle),ylim=c(0,800))
hist(edgeDuration(monog),ylim=c(0,800))
# possible to compare distributions of edge durations across networks using standard methods + randomizations.


# reoccurring edges  (number of times edges occur as opposed to durations)

which(edgeDuration(monog,mode='counts')>1)
which(edgeDuration(moodyContactSim,mode='counts')>1) # all edges in this sim only active once

# if want durations for specific edges at specific timepoints
edgeDuration(monog,e=valid.eids(monog)[105],subject='edges')
edgeDuration(monog,e=valid.eids(monog)[105],subject='spells')







# Connected time of vertices

# The McFarland classroom interaction dataset is a collection of time-stamped speech
# acts among teachers and students during 40 minutes of classroom time. 
# How much total talking does each person in the room do?

data(McFarland_cls33_10_16_96)
cls33_10_16_96
as.data.frame(cls33_10_16_96)


tiedDuration(cls33_10_16_96, mode='counts') # as durations of each act are 0, set to 'counts'
cls33_10_16_96%v%'type'

tiedDuration(cls33_10_16_96, mode='counts',neighborhood = 'in') # duration of being talked to

# ratio of being talked to vs talking to
plot(tiedDuration(cls33_10_16_96, mode='counts',neighborhood = 'out'),
     tiedDuration(cls33_10_16_96, mode='counts',neighborhood = 'in'),
     xlab='# speaking events',ylab='# spoken to events',main='McFarland classroom network, speaking vs. spoken to' )
text(tiedDuration(cls33_10_16_96, mode='counts',neighborhood = 'out'),
     tiedDuration(cls33_10_16_96, mode='counts',neighborhood = 'in'),
     label=cls33_10_16_96%v%'type',cex=0.8,pos=4)






### Density of activity ----

# a crude dynamic measure of graph density
# For all the dyads which are ever observed to have edges, 
# what fraction of the time are they tied?

tEdgeDensity(base)  #any edge in this network is likely to be active 19% of the time.

# for all possible dyads, what proportion of time are they actually tied?
tEdgeDensity(base,agg.unit = 'dyad')  # [1] 0.000766006
# so only an 8 in 10,000 chance that at any random time point there'll be an active edge at a random dyad




### Measures of sequence ----

# tsna has some built in functions for assessing motifs

## pShift - turn-taking sequences as defined by Gibson 2003

#Gibson, D.R. (2003) 'Participation Shifts: Order and Differentiation in Group Conversation'
#Social Forces 81 (4): 1335-1380
#http://sf.oxfordjournals.org/content/81/4/1335.short

library(relevent) #tsna uses some functions from this to determine patterns
data(McFarland_cls33_10_16_96)
pShiftCount(cls33_10_16_96)

# Turn Receiving:
# [1] AB->BA (Alex talks to Brett, then Brett replies)
# [2] AB->B0 (Alex talks to Brett, then Brett addresses the group)
# [3] AB->BY (Alex talks to Brett, then Brett talks to Yuki)
# 
# Turn Claiming:
# [4] A0->X0 (Alex talks to the group, then Xuan talks to the group)
# [5] A0->XA (Alex talks to the group, then Xuan talks to Alex)
# [6] A0->XY (Alex talks to the group, then Xuan talks to Yuki)
# 
# Turn Usurping:
# [7] AB->X0 (Alex talks to Brett, then Xuan talks to the group)
# [8] AB->XA (Alex talks to Brett, then Xuan talks to Alex)
# [9] AB->XB (Alex talks to Brett, then Xuan talks to Brett)
# [10] AB->XY (Alex talks to Brett, then Xuan talks to Yuki)
# 
# Turn Continuing:
#   
# [11] A0->AY (Alex talks to the group, then addresses Yuki)
# [12] AB->A0 (Alex talks to Brett, then makes remark to the group)
# [13] AB->AY (Alex talks to Brett, then to Yuki)

pShiftCount(cls33_10_16_96)
# AB-BA AB-B0 AB-BY A0-X0 A0-XA A0-XY AB-X0 AB-XA AB-XB AB-XY A0-AY AB-A0 AB-AY
#   247     2    45     3     2     5     4     7     8   155     0     1    29

# lots of turn-receiving (AB-BA) and turn usurping (AB-XY) events


## look at how these events change over time:

sliceCounts<- lapply(seq(from = 0,to=45,by = 5),function(onset){
  pShiftCount(network.extract(cls33_10_16_96,onset,length = 5))
})
# convert to a matrix
sliceCounts<-do.call(rbind,sliceCounts)
sliceCounts


## searches for motifs are driven by the type of data we have
## writing algorithms to search for specific motifs isn't too tricky

# typically we would investigate if a particular motif occurs more frequently (or with shorter latencies) than observed in randomized graphs.




## some other quick ideas for visualizing temporal data
## this may or may not! work well on your machine.

library(ndtv)
data(short.stergm.sim)
short.stergm.sim
render.animation(short.stergm.sim)
render.d3movie(short.stergm.sim)
filmstrip(short.stergm.sim,displaylabels=FALSE)
timeline(short.stergm.sim)
head( as.data.frame(short.stergm.sim) )

timeline(short.stergm.sim,slice.par=list(start=0,end=25,interval=1,
          aggregate.dur=0,rule='latest'),plot.vertex.spells=FALSE)


# much more information on animation here:
# http://statnet.csde.washington.edu/workshops/SUNBELT/current/ndtv/ndtv_workshop.html