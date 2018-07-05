
### threejs tutorial

library(igraph)
library(randomNames)
library(threejs)


### Random Example

## Function for making random name edgelist dataframe
random_name_df <- function(nlinks = 100,iter=1000,prox=10,seed=444){
  set.seed(seed)
  df <- data.frame(source = randomNames(iter,which.names='both', name.order = 'first.last', name.sep=' '), target = '')
  df <- df[rep(seq_len(nrow(df)), sample(1:prox,nrow(df), replace=T)),]
  df <- df[sample(nrow(df),nlinks),] 
  df$target = sample(df$source,nrow(df), replace = T)
  df = df[df[,1]!=df[,2], ] 
  return(df)
}

mydf <- random_name_df()
g <- graph.data.frame(mydf, directed=F) # raw graph
i <- edge.betweenness.community(g)$membership


#color names or hex(without opacity) are ok
g <- set_vertex_attr(g, "color", value=c("#e70351", "#e8fd02", "#eb03fe", "#fb9104", "#fd99ee", "#e8d97d", "#ea958a", "#fd01af")[i])
g  <- set_edge_attr(g,"color", value = "black")
g  <- set_edge_attr(g, "weight", value=3)

graphjs(g, repulsion=0.15,bg="white")




###

library(igraph)
library(threejs)

atts1 <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/Attributes/PPRaces.csv", stringsAsFactors=FALSE)
atts <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/Attributes/PPCondRaceGender.csv", stringsAsFactors=FALSE)
mat <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/FriendMatrixT1.csv", stringsAsFactors=FALSE)
rownames(mat)<-paste0("X", mat[,1])
mat<-mat[,-1]
mat <- as.matrix(mat)

#get gaph object
g <- graph.adjacency(mat)

#get giant component
cl<-clusters(g)
g <- induced.subgraph(g, which(cl$membership == which.max(cl$csize)))

#set node and edge attributes
bt <- betweenness(g, normalized=T,directed=F)
V(g)$size <- 20*(bt/max(bt))
postbac<-as.numeric(addNA(atts$Postbac[match(V(g)$name, paste0("X",atts[,1]))]))
V(g)$postbac <- postbac
V(g)$color <- c("#66ccff", "#ffc299", "#d6d6c2")[postbac]
E(g)$color <- "white"
genders<-atts$Gender[match(V(g)$name, paste0("X",atts[,1]))]
V(g)$gender <- ifelse(genders==1, "Female", ifelse(genders==0, "Male", "NA"))
V(g)$race <-  atts1$RaceCode[match(V(g)$name, paste0("X",atts1[,1]))]


graphjs(g, repulsion = 0.3, curvature = 0.4, bg="black", fg="white")



### Zachary example

g<-make_graph("Zachary")

# Set a vertex attribute called 'color' to 'dodgerblue' 
g <- set_vertex_attr(g, "color", value = "dodgerblue")

# Redraw the graph and make the vertex size 1
graphjs(g, vertex.size = 1)

# Create numerical vector of vertex eigenvector centralities 
ec <- as.numeric(eigen_centrality(g)$vector)

# Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 5
v <- 5*sqrt(ec)

# Plot threejs plot of graph setting vertex size to v
graphjs(g, vertex.size = v)



### Another Zachary example (communities)

g<-make_graph("Zachary")
kc = fastgreedy.community(g)


# Create an object 'i' containin the memberships of the fast-greedy community detection
i <-  membership(kc)

# Check the number of different communities
sizes(kc)

# Add a color attribute to each vertex, setting the color based on community membership
g <- set_vertex_attr(g, "color", value = c("yellow", "blue", "red")[i])

# Plot the graph using threejs
graphjs(g)





### can also do animations!

# A graph animation that shows several layouts
data("LeMis")
graphjs(LeMis,
        layout=list(
          layout_randomly(LeMis, dim=3),
          layout_on_sphere(LeMis),
          layout_with_drl(LeMis, dim=3),  # note! somewhat slow...
          layout_with_fr(LeMis, dim=3, niter=30)),
        main=list("random layout", "sphere layout", "drl layout", "fr layout"),
        fpl=300)

