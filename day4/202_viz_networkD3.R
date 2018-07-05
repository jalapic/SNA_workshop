### networkD3

library(networkD3)

src <- c("A","A","B","B","C","E")
target<-c("B","C","C","D","B","C")
net_edge<-data.frame(src,target)
simpleNetwork(net_edge)

net_D3 <- simpleNetwork(net_edge)
saveNetwork(net_D3, file="nd3.html", selfcontained = T)  #nb can get issues if try to overwrite existing file

#forceNetwork - 2dfs,
#links = edgelist
#nodes = nodes and properties of nodes, categorical vars only - if numbers must start at 0


library(networkD3)
data(MisLinks, MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4)



# Load package
library(networkD3)

# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData)



#Load data
data(MisLinks)
data(MisNodes)

# Plot
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)



### save file
library(magrittr)

simpleNetwork(networkData) %>%
  saveNetwork(file = 'Net1.html')






##Change Link Distance

library(networkD3)
data(MisLinks)
data(MisNodes)
forceNetwork(Links = MisLinks, 
             Nodes = MisNodes, 
             Source = "source", 
             Target = "target", 
             Value = "value", 
             NodeID = "name", 
             Group = "group", 
             linkDistance = 100
             )

# can also do this using javascript functions
value <- 2.1

forceNetwork(Links = MisLinks, 
             Nodes = MisNodes, 
             Source = "source", 
             Target = "target", 
             Value = "value", 
             NodeID = "name", 
             Group = "group", 
             linkDistance=JS('function(d) {', paste('return d.value *', value,';'), '}')
             )



### Control color of nodes
# Load package
library(networkD3)
library(dplyr) # to make the joins easier

# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target, stringsAsFactors = FALSE)

nodes <- data.frame(name = unique(c(src, target)), stringsAsFactors = FALSE)
nodes$id <- 0:(nrow(nodes) - 1)


# create a data frame of the edges that uses id 0:9 instead of their names
edges <- networkData %>%
  left_join(nodes, by = c("src" = "name")) %>%
  select(-src) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("target" = "name")) %>%
  select(-target) %>%
  rename(target = id)

edges$width <- 1

# make a grouping variable that will match to colours
nodes$group <- ifelse(nodes$name %in% src, "lions", "tigers")

# simple with default colours
forceNetwork(Links = edges, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE)

# control colours with a JS ordinal scale
ColourScale <- 'd3.scale.ordinal()
.domain(["lions", "tigers"])
.range(["#FF6900", "#694489"]);'

forceNetwork(Links = edges, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             colourScale = JS(ColourScale))


####


### Examples
library(networkD3)

# simpleNetwork
src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)
simpleNetwork(networkData)

# with sans-serif 
simpleNetwork(networkData, fontFamily = "sans-serif")

# with another font 
simpleNetwork(networkData, fontFamily = "fantasy")

# forceNetwork 
data(MisLinks)
data(MisNodes)

# Create graph
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T)

# with a simple click action - make the circles bigger when clicked
MyClickScript <- 
  'd3.select(this)
     .select("circle")
     .transition()
     .duration(750)
     .attr("r", 30)'

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             clickAction = MyClickScript)

# showing how you can re-use the name of the clicked-on node (which is 'd')
# You are unlikely to want to do this pop-up alert, but you might want 
# instead to use Shiny.onInputChange() to allocate d.XXX to an element
# input$XXX for user in a Shiny app.

MyClickScript <- 'alert("You clicked " + d.name + " which is in row " + (d.index + 1) +
" of your original R data frame");'

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             clickAction = MyClickScript)

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             clickAction = "alert('Ouch!')")

# With a different font, and node text faintly visible when not hovered over
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             fontFamily = "cursive", opacityNoHover = 0.3)

# Create graph with legend and varying radius
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Nodesize = 'size', radiusCalculation = "d.nodesize",
             Group = "group", opacity = 1, legend = T, bounded = F) 

# Create graph with legend and varying radius and a bounded box
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Nodesize = 'size', radiusCalculation = " Math.sqrt(d.nodesize)+6",
             Group = "group", opacity = 1, legend = T, bounded = T) 



