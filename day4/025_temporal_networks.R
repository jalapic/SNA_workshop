#### Temporal/Dynamic Network Analysis

# Need to account for 'time' in network analysis

# Two basic structures of temporal social networks:
    # 1. time-aggregated  - discrete, sliding windows, lastN etc. 
    #                       [series of static networks]
    #                       (sometimes called panels, waves, or longitudinal data) 
    # 2. time-ordered     - on (on/off) data of all interactions in sequence

# Methods are still evolving of how best to deal with time in networks.
# descriptive methods exist, but hypothesis testing is still being developed



#### Descriptives ----

### Time-Aggregated.

# clearly, one can aggregate static networks over discrete time periods
# also, see timeordered tutorial for examples of how to aggregate networks

timedf <- readr::read_csv("data/timedf.csv")
head(timedf)



## Examples of methods of aggregating event data

library(igraph)

g <- graph_from_edgelist(as.matrix(timedf[,1:2]))
g <- simplify(g)
plot(g, edge.arrow.size=.3, edge.color='gray33',vertex.color='tomato')
layoutg <- layout_nicely(g)


## Discrete  e.g. by day --

days <- split(timedf, timedf$day)
days

days.g <- lapply(days, 
                 function(x) simplify(graph_from_edgelist(as.matrix(x[,1:2]),directed = F)))
days.g

# just plot first 10 days
par(mfrow=c(2,5))
for(i in 1:10){plot(days.g[[i]], edge.arrow.size=.3, edge.color='gray33',
                    vertex.color='tomato',main=i,layout=layoutg)}
par(mfrow=c(1,1))




## Lag --

lag.g<-NULL
for(i in 1:max(timedf$day)){ 
  tmp<-timedf[timedf$day<=i,]
  lag.g[[i]] <- simplify(graph_from_edgelist(as.matrix(tmp[,1:2])))
    }

lag.g

unlist(lapply(lag.g, transitivity))

plot(unlist(lapply(lag.g, transitivity)),type="l",ylim=c(0,1),ylab = "transitivity",xlab="day")  #lag
lines(1:30,unlist(lapply(days.g, transitivity)),col="green")    #days



## Sliding-Window --

WS <- 20 #window size
ws.g<-NULL
for (i in 1:(nrow(timedf)-WS)){
  tmp<-timedf[timedf$event>=i,]
  tmp<-tmp[tmp$event<=(i+WS),]
  ws.g[[i]] <- simplify(graph_from_edgelist(as.matrix(tmp[,1:2])))
}
ws.g

  #function so we can look at transitivity over different window sizes
window.graphs <- function(WS){
  ws.g<-NULL
  for (i in 1:(nrow(timedf)-WS)){
    tmp<-timedf[timedf$event>=i,]
    tmp<-tmp[tmp$event<=(i+WS),]
    ws.g[[i]] <- simplify(graph_from_edgelist(as.matrix(tmp[,1:2])))
  }
  return(ws.g)
}

window.graphs(20)


# example - transitivity over sliding windows.

unlist(lapply(window.graphs(20),transitivity))

NR<-nrow(timedf)

plot(1:(NR-40),unlist(lapply(window.graphs(40),transitivity)),type="l",ylim=c(0,.5),ylab = "transitivity",xlab="event")  
lines(1:(NR-50),unlist(lapply(window.graphs(50),transitivity)),col="red")
lines(1:(NR-60),unlist(lapply(window.graphs(60),transitivity)),col="green")
lines(1:(NR-70),unlist(lapply(window.graphs(70),transitivity)),col="blue")



## last-N--
## see dominance tutorial for example.





## Other methods can be employed if have 'duration' of edges
# these data typically have an onset/offset time for each interaction
# see timeordered and tsna tutorials for more information on these.






#### Some Statistical Methods ----

# 1.Can generate metrics from each network and use as replicated observations in a e.g.linear mixed model.
# 2.Can compare metrics between periods based on appropriate randomizations.
# 3.Time-series analyses of time-aggregated network statistics can be used for 
#   hypothesis testing or pattern identification.
# e.g. changepoint detection - Social network change detection (SNCD) can be used to identity when network metrics (e.g., betweenness, transitivity) exhibit statistically significant structural change between time periods (McCulloh 2009).




## Time aggregated example of randomizations

# assume this is a directed network.

timedf$period <- ifelse(timedf$day<=10, 1, ifelse(timedf$day>=20,3,2))
head(timedf)
tail(timedf)

period.g <- lapply(split(timedf, timedf$period), function(x) simplify(graph_from_edgelist(as.matrix(x[,1:2]))))
period.g 

lapply(period.g, reciprocity)

# $`1`
# [1] 0.5
# 
# $`2`
# [1] 0.3116
# 
# $`3`
# [1] 0.4592

# is the lower reciprocity observed in week 2 unexpected by chance?

# we could randomize the times to investigate.

recips <- lapply(period.g, reciprocity)
obs12 <- recips[[1]]-recips[[2]] #.1884
obs32 <- recips[[3]]-recips[[2]] #.1476

# randomize interaction times (here randomize periods)
# e.g. one randomization:

set.seed(17)
lapply(
  lapply(split(timedf, sample(timedf$period)), # this line randomizes times units
       function(x) simplify(graph_from_edgelist(as.matrix(x[,1:2])))),  # make graphs on randomizations
      reciprocity) # get reciprocity over 3 randomized new graphs

# $`1`
# [1] 0.3957
# 
# $`2`
# [1] 0.4088
# 
# $`3`
# [1] 0.4167

# run randomization many times:

results <-
  replicate(1000,
  lapply(
  lapply(split(timedf, sample(timedf$period)), # this line randomizes times units
         function(x) simplify(graph_from_edgelist(as.matrix(x[,1:2])))),  # make graphs on randomizations
  reciprocity) # get reciprocity over 3 randomized new graphs
  )

results[,1]
results[,1000]
resdf <- as.data.frame(matrix(unlist(results),ncol=3,byrow=T))
colnames(resdf)<-c("period1","period2","period3")

difs12 <- apply(resdf, 1, function(x) x[1]-x[2])
difs32 <- apply(resdf, 1, function(x) x[3]-x[2])

hist(difs12, breaks=30, xlim=c(-.2,.2),main="period1 - period2 reciprocity")
abline(lty=2,col='red',v=obs12)

hist(difs32, breaks=30, xlim=c(-.25,.25),main="period3 - period2 reciprocity")
abline(lty=2,col='red',v=obs32)

sum(difs12>=obs12)/1000  #pvalue = 0.001
sum(difs32>=obs32)/1000  #pvalue = 0.026



# plot over time:
dg <- simplify(graph_from_edgelist(as.matrix(timedf[,1:2]),directed = T))
periods.dg <- lapply(split(timedf,timedf$period), 
                  function(x) simplify(graph_from_edgelist(as.matrix(x[,1:2]),directed = T)))
periods.dg


par(mfrow=c(1,3))
layoutdg <- layout_nicely(dg)
plot(simplify(periods.dg[[1]]),edge.arrow.size=.2,vertex.color='tomato',layout=layoutdg,main="P1")
plot(simplify(periods.dg[[2]]),edge.arrow.size=.2,vertex.color='tomato',layout=layoutdg,main="P2")
plot(simplify(periods.dg[[3]]),edge.arrow.size=.2,vertex.color='tomato',layout=layoutdg,main="P3")
par(mfrow=c(1,1))



#### Some Visual Methods ----

# see timeordered and tsna tutorials for more
# visualize aggregated networks (as above)
# MusicNotation Method (event data)

library(devtools)
install_github('jalapic/musicnotationR', username = "jalapic")

library(musicnotationR)
musicnot(flies, gridcolor=T, gridlinesize = 0.5, labels="name")


p <- musicnot(flies, gridcolor=T, gridlinesize = 0.5, labels="name", colors=c("black", "orange1", "limegreen", "dodgerblue1"))

p + 
  ggtitle("Social Interactions of Flies") +
  xlab("Time in seconds") +
  theme(panel.background = element_rect(fill = "mistyrose1"),
        panel.grid.minor = element_blank() 
  )

## see shiny app:  https://jalapic.shinyapps.io/music/



# There is a lot of room for improvement in the visual depiction of time-ordered/time-aggregated networks!








#### Some additional specific questions that can be answered using temporal methods ----

## Temporal Path Analysis (see timeordered and tsna tutorials)
## Diffusion sets (see timeordered tutorial)
## Resource Flows / Diffusion (not covered)
## Network Formation  (e.g. hierformR) - state space evolution.

## Network Motif Analysis
# (also briefly see tsna tutorial)

## Community Membership over Time
# several algorithms currently being developed (none in R)



