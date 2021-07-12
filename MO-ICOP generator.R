library(ecr)

pathToMOICOP <- "./MO-ICOP"
dir.create(paste0(pathToMOICOP,"/P1"))
dir.create(paste0(pathToMOICOP,"/P2"))
## Define dimension
d <- 2
## Define number of non-dominated seeds (here a random number between 5 and 20)
nb_nd_seeds <- sample(5:20,1)
## Define number of dominated seeds (here a random number between 5 and 20)
nb_d_seeds <- sample(1:80,1)

## Assign fitness in both objectives to each non-dominated seeds
#  ensuring no seed in this set dominates an other one

nd_seeds <-  matrix(runif(2), nrow=1, ncol = 2) # assign random fitness to the first seed
for(i in 2:nb_nd_seeds){
  bad <- TRUE
  while(bad){
    newseed <- runif(2) # generate a random location in objective space
    bad1 <- sum(apply(nd_seeds, 1, function(x){return(newseed[1]<x[1] & newseed[2]<x[2])})) >0    # check if it does not dominate another seed
    bad2 <- sum(apply(nd_seeds, 1, function(x){return(newseed[1]>=x[1] & newseed[2]>=x[2])})) >0  # check if it is not dominated by another seed
    bad <- bad1 | bad2
  }
  nd_seeds <- rbind(nd_seeds, newseed)
}

## Assign fitness in both objectives to each dominated seed
d_seeds <- matrix(0,ncol = 2, nrow = nb_d_seeds)
for(i in 1:nb_d_seeds){
  bad <- TRUE
  while(bad){
    newseed <- runif(2)
    bad <- sum(apply(nd_seeds, 1, function(x){return(newseed[1]<x[1] & newseed[2]<x[2])})) >0 # check if not dominatating seeds from the ND set
  }
  d_seeds[i,] <- newseed
}
Ys <- rbind(nd_seeds, d_seeds)

## Assign random positions to seeds in decision space
Xs <- matrix(runif((nb_nd_seeds + nb_d_seeds)*d, min = -5, max = 5), ncol = d, nrow = (nb_nd_seeds + nb_d_seeds))

## Save seeds positions and fitnesses in file
write.table(Xs, paste0(pathToMOICOP,"/P1/Xs.csv"), row.names = FALSE, col.names = FALSE, sep = ",")
write.table(Xs, paste0(pathToMOICOP,"/P2/Xs.csv"), row.names = FALSE, col.names = FALSE, sep = ",")
write.table(Ys[,1], paste0(pathToMOICOP,"/P1/Ys.csv"), row.names = FALSE, col.names = FALSE, sep = ",")
write.table(Ys[,2], paste0(pathToMOICOP,"/P2/Ys.csv"), row.names = FALSE, col.names = FALSE, sep = ",")

#Visualise seeds in objective space
plot(Ys, col = 1+nondominated(t(Ys)), ylab = "F2", xlab = "F1")


#