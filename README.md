# SNA_workshop

Glasgow, UK - 2nd July 2018 - 5th July 2018

Prof James P. Curley, University of Texas at Austin

**Course Overview:**
This workshop will provide students with the opportunity to learn how to use social network analysis to analyze social relational datasets such as human friendship networks or animal social networks. Attendees will learn how to use R and several R packages including igraph, sna, network, asnipe, timeordered, tsna to create network graphs, calculate descriptive network metrics, use randomization and random models to evaluate the significance of these metrics, determine graph structural properties including community structures, use QAP and MRQAP methods to assess how network characteristics relate to other individual and relational attributes, and measure change over time in dynamic networks. Attendees will also learn how to produce high quality network visualizations using R. Examples from the human and animal literatures will be covered and time will be allocated for attendees to apply these methods to their own data.


**Day 1**

*- Introduction to Social Networks Theory.* 
This will cover central themes of social network analysis: the major data formats, structures and collection methods, the different types of graphs and networks; how to generate and visualize social networks and generate basic descriptive statistics, and what hypotheses and questions can be addressed using social network analysis. We will also discuss data types and questions of interest to attendees.

*- R refresher and R packages.* 
This module will provide a quick overview of the major packages used for social network analysis in R including ‘igraph’, ‘sna’, ‘network’. We shall learn how to convert raw data formats to network objects in R; how to interface with R network objects and how to create simple network visualizations.

*- Intro to Visualizing Networks.* 
We shall cover how to generate and beautify networks using the ‘igraph’ R package covering issues such as layout decisions, coloring and sizing of nodes and edges by network attributes, metrics or community. 

*- Network Graph Characteristics.* 
We shall cover concepts such as dyad and triad censuses, transitivity, reciprocity, clustering and density. We shall discuss their significance and importance for answering relevant questions to the patterns of social associations and behavior in networks. We will introduce the method of correspondence analysis for comparing structural features across networks. We will also introduce the jackknifing and bootstrapping of data/graphs to generate confidence intervals and certainty around observed metrics.


*- Identifying important nodes and edges.* 
Learn how to evaluate key indicators of each node’s significance to the network including, degree centrality, eigenvector centrality, power centrality, and betweenness. Describe how to calculate for directed vs. undirected and weighted vs unweighted networks. Learn how to assess simple relationships between nodes including geodesic distances, identifying neighbors, determining shortest and longest paths.


**Day 2**

*- Identifying roles in social networks.*
 We shall identifying similarity and dissimilarity in node characteristics and  how we can determine social roles of actors. We will specifically explore the concepts of brokerage, cutpoints, blockmodeling, structural and regular equivalence. 

*- Identifying subgroups in social networks*
We will evaluate methods for identifying how indiviudals preferentially associate with or avoid others. In this section we shall cover topics such as cliques, clusters, spatial autocorrelation, and assortativity/homophily. 

*- Introduction to Network Randomization and Random Models.* 
It is critical in network analysis to evaluate how likely it is to observe a given network metric for our network of interest. This module will introduce how to use null models (randomizations/permutations or random graphs) to test whether networks have characteristics that are especially surprising after accounting for non-independence. We will examine conditional uniform graphs, standard random graph models as well as determining if these methods are appropriate for our data formats.

**Day 3**

*- Community Detection.* 
Overview of what communities (modules) mean for animal and human social networks – that a high proportion of nodes or edge weights cluster within a sub-group of nodes/edges rather than between sub-groups. We shall review the numerous community detection methods and describe the implementation of major ones in R. How to generate robustness in evaluation of community detection. How to to determine the degree of community structure in a network using the index of modularity (Q) and bootstrapping techniques such as community assortativity (rcom). Hierarchical clustering for analysis of hierarchically organized social societies.


*- Quadratic Assignment Procedure (QAP) Regression.* 
Using QAP regression to control for non-independence of data when comparing network position or strength between networks or comparing individual/dyadic network metrics with other individual/dyadic attributes. Extending analyses to using multiple regression quadratic assignment procedure (MRQAP). How to perform in base-R and using the ‘asnipe’ R package. How to generate effect sizes when using QAP and MRQAP.


*- Randomizations and Random models II.*
This module will further explore how to determine the appropriate choice of null models for behavioral data. This is not always a trivial exercise for behavioral datasets. We will use the ‘asnipe’ R package for network permutation and ‘igraph’ R package for null model generation. We shall further explore testing specific node, dyadic and graph level hypotheses.


**Day 4**

*- Dominance hierarchy analysis.* 
We shall cover traditional and network based methods for identifying social hierarchies and powerful/influential actors in social networks. We shall also discuss  how to apply temporal methods to track changes in hierarchical social networks over time.

*- Dynamic Networks.* 
Key questions that are often neglected are how do network structures remain stable or change over time and can we infer how meaningful this stability and instability is? This module will discuss various methods for analysis of change for time-ordered and time-aggregated networks. We will use R packages for analysis of dynamic networks including ‘timeordered’, ‘networkDynamic’ and ‘tsna’.﻿
 
*- Visualizing Networks II.* 
We shall extend visualization methods to cover how to create dynamic three-dimentional network plots using the R package ‘threejs’. We shall also discuss how to use the ‘ggplot’ based ‘ggraph’ and 'tidygraph' R packages which has many customizable features for plotting networks. This module will also tackle advanced options for network plotting, including how to export ‘igraph’ R objects to Gephi for generating even more beautiful customized plots, how to create interactive web based network visualizations using R packages such as ‘threejs’, ‘visNetwork’ and ‘networkD3’, and how to plot or animate dynamic social networks.
 
