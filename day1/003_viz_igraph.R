
## igraph visualizations



## A very good reference for igraph plotting:
# http://kateto.net/network-visualization
?igraph.plotting # help guide



## igraph visualization

## example dataset:
library(igraph)

friends1_nodes <- read.csv("data/friends1_nodes.csv", stringsAsFactors = F)
friends1_edges <- read.csv("data/friends1_edges.csv", stringsAsFactors = F)
g <- graph_from_data_frame(d = friends1_edges, vertices = friends1_nodes, directed = FALSE)
g
V(g)$name
V(g)$gender
E(g)$hours


# Default Plot
plot(g)



### Vertices ----


# Plot network and color vertices by gender
V(g)$color <- ifelse(V(g)$gender == "F", "orange", "#aabedd")
plot(g, 
     vertex.label.color = "black") # also change vertex label color


# Change border color of vertices
plot(g, 
     vertex.label.color = "black",
     vertex.frame.color="red") 


# no border
plot(g, 
     vertex.label.color = "black",
     vertex.frame.color=NA) 


# frame color by group
V(g)$frame.color <- ifelse(V(g)$gender == "F", "red", "navy")
plot(g, 
     vertex.label.color = "black") 



# change shape of node
shapes()
# "circle", "square", "csquare", "rectangle", "crectangle", "vrectangle", "pie", "sphere", "none"
shapes <- rep("circle", gorder(g))
shapes

plot(g, 
     vertex.label.color = "black",
     vertex.shape=shapes) 

shapes <- rep("csquare", gorder(g))
shapes

plot(g, 
     vertex.label.color = "black",
     vertex.shape=shapes) 

V(g)$shape <- ifelse(V(g)$gender == "F", "csquare", "circle")
plot(g, 
     vertex.label.color = "black")




# change size of shapes


plot(g, 
     vertex.label.color = "black",
     vertex.size=25)


plot(g, 
     vertex.label.color = "black",
     vertex.size=5)

plot(g, 
     vertex.label.color = "black",
     vertex.size=15)



# You would think changing the thickness of the border/frame of a vertex would be easy....

# .... it's not

# a function to define precisely a new shape
mycircle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  vertex.frame.color <- params("vertex", "frame.color")
  if (length(vertex.frame.color) != 1 && !is.null(v)) {
    vertex.frame.color <- vertex.frame.color[v]
  }
  vertex.frame.width <- params("vertex", "frame.width")
  if (length(vertex.frame.width) != 1 && !is.null(v)) {
    vertex.frame.width <- vertex.frame.width[v]
  }
  
  mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
         vertex.size, vertex.frame.width,
         FUN=function(x, y, bg, fg, size, lwd) {
           symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
                   circles=size, add=TRUE, inches=FALSE)
         })
}

add.vertex.shape("fcircle", clip=igraph.shape.noclip,
                 plot=mycircle, parameters=list(vertex.frame.color=1,
                                                vertex.frame.width=1))

## end function



plot(g, 
     vertex.label.color = "black",
     vertex.shape="fcircle", 
     vertex.frame.width=1)

plot(g, 
     vertex.label.color = "black",
     vertex.shape="fcircle", 
     vertex.frame.width=3)



#### Labels... ----


# Add custom label and alter size of label
plot(g, 
     vertex.label.color = "black",
     vertex.label=LETTERS[1:gorder(g)], 
     vertex.label.cex=.6) 


# remove labels
plot(g, 
     vertex.label=NA)
     

# change font of labels
plot(g, 
     vertex.label.color = "black",
     vertex.label=LETTERS[1:gorder(g)], 
     vertex.label.cex=1.2,
     label.font=5) # try change this 1:5



# change location of labels
plot(g, 
     vertex.label.color = "black",
     vertex.label.dist = 0 ) 

plot(g, 
     vertex.label.color = "black",
     vertex.label.dist = 2) 





#### Edges ----
dev.off()

# edge weights

plot(g, 
     vertex.label.color = "black",
     edge.width = E(g)$hours)


# can set linetypes too

plot(g, 
     vertex.label.color = "black",
     edge.lty = 1 )


plot(g, 
     vertex.label.color = "black",
     edge.lty = 3)   # try 0:5


# set edge types for particular edges...
edgetypes <- as.data.frame(get.edgelist(g))
edgetypes$from <- V(g)$gender[match(edgetypes[,1],V(g)$name)]
edgetypes$to <- V(g)$gender[match(edgetypes[,2],V(g)$name)]
edgetypes$grp <- paste0(edgetypes$from,edgetypes$to)
edgetypes$lty <- ifelse(edgetypes$grp=="MF" | edgetypes$grp=="FM" , 3,1)
edgetypes

plot(g, 
     vertex.label.color = "black",
     edge.lty = edgetypes$lty)   




## directed edges examples ...
  
library(igraph)
measles <- read.csv("data/measles.csv")
g <- graph_from_data_frame(measles, directed=T)
g

# Is the graph directed?
is.directed(g)


# Make a basic plot
plot(g, 
     vertex.label.color = "black")

# adjust arrow size and edge color
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray77',
     edge.arrow.size = 0.1)

# decrease node size and label size
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray22',
     vertex.size = 0,
     edge.arrow.size = 0.1,
     vertex.label.cex=.8)



# Calculate betweenness of each vertex
g.b <- betweenness(g, directed = TRUE)


# remove labels and increase node size based on betweenness
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.1)

# change curvature of arrows
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.05,
     edge.curved=.2)


# alternatively, state if desire curved or not
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.05,
     edge.curved=F)

plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.05,
     edge.curved=T)



# alter width of arrows
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.05,
     edge.arrow.width = 1.25,
     edge.curved=T)




  
#### layouts ----

gx <- sample_gnp(40,.05)
gx


## there are  many, many custom layouts...

plot(gx, vertex.label.color = "black", layout = layout.circle(gx)) # circle layout
plot(gx, vertex.label.color = "black", layout = layout_with_fr(gx)) # Fruchterman-Reingold layout 
plot(gx, vertex.label.color = "black", layout = layout.grid(gx)) # grid layout
plot(gx, vertex.label.color = "black", layout = layout_as_tree(gx)) # tree layout
plot(gx, vertex.label.color = "black", layout = layout_nicely(gx)) # igraph decides

# there are several others in igraph, but layout_nicely() usually comes up with a good algorithm

# rerunning some algorithms several times leads to slightly different layouts


# You may also wish to specify exact x,y coordinates. 
# e.g. if you have spatial coordinates that animals spend most time in
# you could overlay the observed network over these points.





### One very useful feature is that we can set layouts based on one graph to use with others
# if they possess  the same nodes....


# example

gf1 <- sample_forestfire(40,.1)
gf2 <- sample_forestfire(40,.1)
gf3 <- sample_forestfire(40,.1)

layoutg <- layout_nicely(gf1)

par(mfrow=c(2,3))
plot(gf1, layout = layoutg, edge.arrow.size=.02, edge.color='gray21')
plot(gf2, layout = layoutg, edge.arrow.size=.02, edge.color='tomato')
plot(gf3, layout = layoutg, edge.arrow.size=.02, edge.color='dodgerblue')
plot(gf1, edge.arrow.size=.02, edge.color='gray21')
plot(gf2, edge.arrow.size=.02, edge.color='tomato')
plot(gf3, edge.arrow.size=.02, edge.color='dodgerblue')
par(mfrow=c(1,1))





## note about large networks...
# in very large networks you may wish to reduce edges via
# thresholding,
# transparency of ties, changing color of ties
# or reducing image to hub vertices only
# we cover this later



#### EXERCISE:

# Import Data Set and Visualize.







###########   EXPORTING  ##################





### Exporting / Saving as high-quality svg image

library(igraph)
measles <- read.csv("data/measles.csv")
g <- graph_from_data_frame(measles, directed=T)
g


# say you really like the figure below and want to save it as  a high res image.


# use these packages
library(svglite)
library(rsvg)
library(png)

# save plot
svglite("plot.svg", width = 10, height = 7)

g.b <- betweenness(g)
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.25,
     edge.curved=.2)

dev.off()


bitmap <- rsvg("plot.svg") # render it into a bitmap array
png::writePNG(bitmap, "bitmap.png") # save as png



