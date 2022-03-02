library(tidyverse)

Seed <- c(1:16)
firstPick <- c(.114,.113,.112,.111,.099,.089,.079,.069,.059,.049,.039,.029,.019,.009, .006, .004)

conditionalProbs <- numeric()
secondPick <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
conditionalProbs2 <- numeric()
thirdPick <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
conditionalProbs3 <- numeric()
fourthPick <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
conditionalProbs4 <- numeric()
fifthPick <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
remainingPicks <- matrix(nrow=11, ncol=16)
remainingPicks[is.na(remainingPicks)] <- 0


findFixedTeam <- function(probs,remainingPicks,restProb) {
  for (i in 1:11) {
  
    nonzeros <- cbind(Seed,probs)
    row_sub = apply(nonzeros, 1, function(row) all(row !=0 ))
    nonzeros <- nonzeros[row_sub,]
    if (is.null(dim(nonzeros))) {
      nonzeros <- t(nonzeros)
    }
    team <- min(nonzeros[,1])
    remainingPicks[i,team] <- remainingPicks[i,team] + restProb
    probs[team] <- 0
    
  }
  remainingPicks
}


for (i in 1:length(firstPick)) {
  firstRemoved <- firstPick
  firstRemoved[i] <- 0 
  totalProb <- sum(firstRemoved)
  for (x in 1:length(firstRemoved)) {
    conditionalProbs[x] <- firstPick[i]*firstRemoved[x]/totalProb
    secondPick[x] <- secondPick[x] + conditionalProbs[x]
  }
  
  for (j in 1:length(firstPick)) {
    if (i != j) {
      
      secondRemoved <- firstRemoved
      secondRemoved[j] <- 0
      totalProb <- sum(secondRemoved)
      
      for (y in 1:length(secondRemoved)) {
        conditionalProbs2[y] <- conditionalProbs[j]*secondRemoved[y]/totalProb
        thirdPick[y] <- thirdPick[y] + conditionalProbs2[y]
      }
      
      for (k in 1:length(firstPick)) {
        if (j != k && i != k) {
          thirdRemoved <- secondRemoved
          thirdRemoved[k] <- 0
          totalProb <- sum(thirdRemoved)
          
          for (z in 1:length(thirdRemoved)) {
            conditionalProbs3[z] <- conditionalProbs2[k]*thirdRemoved[z]/totalProb
            fourthPick[z] <- fourthPick[z] + conditionalProbs3[z]
          }
          
          for (l in 1:length(firstPick)) {
            if (k != l && j != l && i != l) {
              fourthRemoved <- thirdRemoved
              fourthRemoved[l] <- 0
              totalProb <- sum(fourthRemoved)
              
              for (zz in 1:length(fourthRemoved)) {
                conditionalProbs4[zz] <- conditionalProbs3[l]*fourthRemoved[zz]/totalProb
                fifthPick[zz] <- fifthPick[zz] + conditionalProbs4[zz]
              }
          
              for (m in 1:length(firstPick)) {
                if (k != m && j != m && i != m && l != m) {
                  restProb <- conditionalProbs4[m]
                  fifthRemoved <- fourthRemoved
                  fifthRemoved[m] <- 0
                  remainingPicks <- findFixedTeam(fifthRemoved, remainingPicks, restProb)
                }
              }
            }
          }
        }
      }
    } 
  }  
}

remainingPicks <- t(remainingPicks)
colnames(remainingPicks) <- c("6th", "7th", "8th", "9th", "10th", "11th", "12th", "13th", "14th", "15th", "16th" )

completeTable <- cbind(Seed, firstPick) 
completeTable <- cbind(completeTable, secondPick) 
completeTable <- cbind(completeTable, thirdPick)
completeTable <- cbind(completeTable, fourthPick)
completeTable <- cbind(completeTable, fifthPick)
completeTable <- cbind(completeTable, remainingPicks)

completeTable[completeTable == 0] <- NA
write.csv(completeTable, file="NBA Lottery Draft Probability.csv")

