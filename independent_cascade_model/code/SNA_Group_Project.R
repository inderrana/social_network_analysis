
#install.packages('igraphdata')
#clean the environment
rm(list = ls())
gc()
cat("\014")
library(igraph)
library(igraphdata)

#----------------------------------------------
# Description of Data
# Social network between members of a university karate club, 
# led by president John A. and karate instructor Mr. Hi (pseudonyms).
# The edge weights are the number of common activities the 
# club members took part of. These activities were:
# 1. Association in and between academic classes at the university.
# 2. Membership in Mr. Hi's private karate studio on the east side of the city where Mr. 
# Hi taught nights as a part-time instructor.
# 3. Membership in Mr. Hi's private karate studio on the east side of the city, #
# where many of his supporters worked out on weekends.
# 4. Student teaching at the east-side karate studio referred to in (2). 
# This is different from (2) in that student teachers interacted with each other, 
# but were prohibited from interacting with their students.
# 5. Interaction at the university rathskeller, located in the same basement as the karate club's workout area.
# 6. Interaction at a student-oriented bar located across the street from the university campus.
# 7. Attendance at open karate tournaments held through the area at private karate studios.
# 8. Attendance at intercollegiate karate tournaments held at local universities. 
# Since both open and intercollegiate tournaments were held on Saturdays, attendance at both was impossible.
#-------------------------------------------------

set.seed(42)

data(karate)
# the number of nodes in the Data set
n = 34
#ploting the data to have the first view
plot(karate)

A <- as.matrix(as_adjacency_matrix(karate))
A

D <- colSums(A)
D
T <- A/D
T
(p0 =rbinom(n,1,0.3))
(p1 =T%*%p0)
(p2 =T%*%p1)
(p3 =T%*%p2)


# Number of ittration
Nsim = 50

p = p0
P = p
#first for loop with node zero to find the inluenced nodes
for(s in 2:Nsim){
  
  p = T%*%p
  P = cbind(P,p)
  
  plot(1:s, P[1,], type = 'l')
  
  for(i in 2:n){
    lines(1:s, P[i,], type = 'l')
  }
  
}

#-----------------------------------------------------------
#
# INDEPENDENT CASCADE MODEL
#
#-----------------------------------------------------------

#doing manually the first iteration to run the wighle loop
l <- layout_with_fr(karate)
l
Prob <- 1/igraph::degree(karate)

Vertex.col <- rep(0,n)

Initial.member <- c(2,3,4)
Vertex.col[Initial.member] <- 4
V(karate)$color <- Vertex.col
plot(karate, layout= l, vertex.size=20, vertex.label = NA)
Active <- Initial.member
Explore <- TRUE
i <- 1
X <- A
g <- karate

# running the while loop of independent cascade model
while(i <= n && Explore){
  
  if (i <= length(Active) && length(Active) < n){
    
    Candidate.nodes <- which(X[Active[i],]>0)
    
    New.Active <- NULL
    for(j in Candidate.nodes){
      Add.node <- rbinom(1,1,Prob[j])
      if(Add.node > 0){
        New.Active <- c(New.Active, j)
      }
    }
    
    print(c(Candidate.nodes, New.Active))
    
    Active <- unique(c(Active, New.Active))
    Vertex.col[Active] <- 4
    V(g)$color <- Vertex.col
    plot(g, layout= l, vertex.size=20, vertex.label = NA)
  }else{
    Explore <- FALSE
  }
  i <- i+1
  Sys.sleep(1)
  
}
#-----------------------------------------------------------
#
# simulate the process function
#
#-----------------------------------------------------------

#putting all above steps in the function

simulate <- function(Seeds, g, Prob, display){
  
  # Plot the initial state
  
  if(display){
    Vertex.col <- rep(0,n)
    Vertex.col[Seeds] <- 4
    V(g)$color <- Vertex.col
    plot(g, layout= l, vertex.size=20, vertex.label = NA)
  }
  
  # Build the adjacency matrix
  
  X <- as.matrix(get.adjacency(g))
  
  Active <- Seeds
  Explore <- TRUE
  i <- 1
  while(i <= n && Explore){
    
    if (i <= length(Active) && length(Active) < n){
      
      Candidate.nodes <- which(X[Active[i],]>0)
      
      New.Active <- NULL
      for(j in Candidate.nodes){
        Add.node <- rbinom(1,1,Prob[j])
        if(Add.node > 0){
          New.Active <- c(New.Active, j)
        }
      }
      
      #print(c(Candidate.nodes, New.Active))
      
      Active <- unique(c(Active, New.Active))
      
      if(display){
        Vertex.col[Active] <- 4
        V(g)$color <- Vertex.col
        plot(g, layout= l, vertex.size=20, vertex.label = NA)
      }
      
    }else{
      Explore <- FALSE
    }
    i <- i+1
    Sys.sleep(0)
    
  }
  
  return(length(Active))
  
}



#----------------------------------------------
# Simulate the system multiple times starting 
# from the same initial seed
#----------------------------------------------

simulate(Initial.member, karate, Prob, TRUE )

Nsim = 500
MyResults = rep(0,Nsim)

for(s in 1:Nsim){
  MyResults[s] = simulate(Initial.member, g, Prob, FALSE )
}

hist(MyResults, breaks = 100)

#----------------------------------------------
# Find the best initial node to maximize the 
# number of influenced nodes
#----------------------------------------------
 
#running the simulation function for 1000 iterations
Nsim = 1000

ICM_MeanCentrality = rep(0,n)
ICM_VarCentrality = rep(0,n)

for(MyNode in 1:n){
  
  Initial.member <- MyNode
  
  MyResults = rep(0,Nsim)
  
  for(s in 1:Nsim){
    MyResults[s] = simulate(Initial.member, karate, Prob, FALSE )
  }
  
  hist(MyResults, breaks = 100, main = paste("Node", MyNode))
  
  ICM_MeanCentrality[MyNode] = mean(MyResults)
  ICM_VarCentrality[MyNode] = sd(MyResults)
  
}

ord = order(ICM_VarCentrality)

plot(ICM_VarCentrality[ord], ICM_MeanCentrality[ord], type = "o", lwd = 3)


#-----------------------------
#simulation of different nodes

# calculate that how much influence have the node number 34

Initial.member <- 34
n.infected <- 0
for(i in 1:Initial.member){
  n.infected <- n.infected + simulate(Initial.member,karate, Prob, FALSE)
}
n.infected/n


# calculate that how much influence have the node number 27

Initial.member <- 33
n.infected <- 0
for(i in 1:Initial.member){
  n.infected <- n.infected + simulate(Initial.member,karate, Prob, FALSE)
}
n.infected/n

# calculate that how much influence have the node number 27

Initial.member <-32
n.infected <- 0
for(i in 1:Initial.member){
  n.infected <- n.infected + simulate(Initial.member,karate, Prob, FALSE)
}
n.infected/n

