
### How robust are my graph / node metrics ?

## three options during exploratory analysis:
# we will cover jackknifing and bootstrapping more elswewhere
# here - one exploratory method is to remove a % of data and
# examine the consistency of findings.



# how much data can be removed for measures to be robust ? 

library(igraph)

g<-readRDS("data/anim_g.rds")

g
plot(g, vertex.size=6, vertex.label=NA)
vcount(g)

# e.g. consistency across removals - e.g. transitivity.

igraph::transitivity(g) #0.3286171 - observed value

Nrep <- 500 # 50 removals or 1,2,3,4.....99,100 nodes
#Nrep <- 5000 # 5000 removals or 1,2,3,4.....99,100 nodes
resmat <- matrix(NA, Nrep, 100) #Nrep rows, 100 cols to store results in

# remove 1 to 100 nodes from graph....
# and do that 5000 times each...
# this will take some time !!!

for(i in 1:100){
resmat[,i] <- replicate(Nrep,igraph::transitivity(delete_vertices(g, sample(1:vcount(g),i,F))))
}

resmat
resmat[,100] # when 100 nodes removed, results of transitivity


#plot
library(ggplot2)
library(tidyverse)
library(dplyr)
rbind(
  data.frame(Var2=0, mean=igraph::transitivity(g), sd=NA,se=NA),
reshape2::melt(resmat) %>%
  group_by(Var2) %>%
  summarize(mean = mean(value), sd = sd(value), se = sd(value)/sqrt(Nrep))
  ) %>%
ggplot(aes(x=Var2, y=mean))+
geom_point(size=.5)+
geom_line()+
geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.3, color=NA) 

#

rbind(
  data.frame(Var2=0, mean=igraph::transitivity(g), sd=NA,se=NA),
  reshape2::melt(resmat) %>%
    group_by(Var2) %>%
    summarize(mean = mean(value), sd = sd(value), se = sd(value)/sqrt(Nrep))
) %>%
  ggplot(aes(x=Var2, y=mean))+
  geom_point(size=.5)+
  geom_line()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.3, color=NA) +
  ylim(0,1)




