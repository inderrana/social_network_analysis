#clean the environment 
rm(list = ls())
gc()
cat("\014")

##set working directort
setwd("C:/Users/irana/Downloads/soc-karate")

#load libraries
library(igraphdata)
library(igraph)
library(threejs)


######################load data######################
data(karate)
#?karate

#######################plot graph data######################
plot(karate)
dev.off()

######################cluster liden meathod######################
#?cluster_leiden
#plot using cluster liden meathod
kc <- cluster_leiden(karate)

#check the length of the network
length(kc)

#check community sizes
sizes(kc)

#check community memberships
membership(kc)

#plot
plot(kc,karate)
dev.off()


###################### backup the network data for later use######################
kc.cpm1 <- kc  

#For comparison, lets compute the density and weighted density of the karate club
#to compare with the  cluster_leiden meathod

n <- length(V(karate)) #number vertices
graph.density(karate)

length(E(karate))/(n*(n-1)/2)

sum(E(karate)$weight)/(n*(n-1)/2)

kc <- cluster_leiden(karate, 
                     resolution_parameter = sum(E(karate)$weight)/(n*(n-1)/2))
length(kc)

sizes(kc)

membership(kc)

plot(kc,karate)

kc.cpmdens <- kc

#lets look at the contingancy table
table(membership(kc.cpm1),membership(kc.cpmdens))

###Before checking for modularity lets see what happens with the default cluster_leiden() 
#options on the unweighted version of the karate club network:
karate.unweighted <- delete_edge_attr(karate,"weight")
plot(karate.unweighted)


c <- cluster_leiden(karate.unweighted)
length(kc)

sizes(kc)

membership(kc)

plot(kc,karate)

#Check the impact of gamma value
gamma <- seq(0.25,2,0.025)
nc <- vector("numeric",length(gamma))
for (i in 1:length(gamma)){
  gc <- cluster_leiden(karate, objective_function = "modularity",
                       n_iterations = 3, resolution_parameter = gamma[i])
  nc[i] <- length(gc)
}

#plot the results
plot(gamma,nc,xlab="gamma",ylab="# Communities",main="Zachary Karate Club - Gamma Exp")

#since the plot is not much helpful, lets try playing with the resolution parametre

#try with resolution_parameter 0.5
kc2 <- cluster_leiden(karate, objective_function = "modularity",
                      n_iterations = 3, resolution_parameter = 0.5)
plot(kc2,karate)


table(V(karate)$Faction,membership(kc2))

#try with resolution_parameter 0.75
kc3 <- cluster_leiden(karate, objective_function = "modularity",
                      n_iterations = 3, resolution_parameter = 0.75)
plot(kc3,karate)

table(membership(kc2),membership(kc3))

#try with resolution_parameter 0.1
kc4 <- cluster_leiden(karate, objective_function = "modularity",
                      n_iterations = 3, resolution_parameter = 1.0)
plot(kc4,karate)

table(membership(kc3),membership(kc4))

#try with resolution_parameter 1.5
kc5 <- cluster_leiden(karate, objective_function = "modularity",
                      n_iterations = 3, resolution_parameter = 1.5)
plot(kc5,karate)


table(membership(kc4),membership(kc5))

######################walk trap#####################
a <- walktrap.community(karate, weights = E(karate)$weight, steps = 4, merges =
                     TRUE, modularity = FALSE)
plot(a, karate)


######################fast-greedy######################
# Perform fast-greedy community detection on network graph
kcg = fastgreedy.community(karate)

# Determine sizes of each community
sizes(kcg)

# Determine which individuals belong to which community
membership(kcg)

# Plot the community structure of the network
plot(kcg, karate)
dev.off()
######################edge-betweenness######################
# Perform edge-betweenness community detection on network graph
gc = edge.betweenness.community(karate)

# Determine sizes of each community
sizes(gc)

# Plot community networks determined by fast-greedy and edge-betweenness methods side-by-side
par(mfrow = c(1, 2)) 
plot(kcg, karate)
plot(gc, karate)
dev.off()

######################interactive plot######################
# Set a vertex attribute called 'color' to 'dodgerblue' 
g <- set_vertex_attr(karate, "color", value = "dodgerblue")

# Redraw the graph and make the vertex size 1
graphjs(g, vertex.size = 1)

