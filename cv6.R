rm(list=ls())
setwd("V:/MPA/MPAPRG/exercise_06")

# install.packages("combinat")
library(combinat)


## TASK 1
DoubleDigestProblem <- function(a_frag, b_frag, ab_frag){
  seq_length <- sum(a_frag)
  a_perm <- permn(a_frag)
  b_perm <- permn(b_frag)
  
  for (a in a_perm){
    for (b in b_perm){
      a_zero <- c(0, a)
      b_zero <- c(0, b)
      a_pos <- c(0,cumsum(a_zero))
      b_pos <- c(0,cumsum(b_zero))
        
      ab <- c(sort(unique(c(a_pos, b_pos))))
      rs <- sort(diff(ab))
      if (sum(rs == ab_frag) == length(ab_frag)){
        
        return(list(a,b))
      }
    }
  }
}

a <- c(3,5,2,10)
b <- c(7,3,10)
ab <- c(1,2,2,5,5,5)

# print(DoubleDigestProblem(a, b, ab))


## TASK 2
Remove <- function(deltaX, to_remove){
  for (val in to_remove){
    deltaX <- deltaX[-which(deltaX == val)[1]]
  }
  return(deltaX)
}

Place <- function(deltaX, X, width){
  if (length(deltaX) == 0){
    print(X)
    return()
    }
   
  y <- max(deltaX)
  if (all(abs(y - X) %in% deltaX)){
    deltaX <- Remove(deltaX, abs(y - X))
    X <- c(X, y)
    Place(deltaX, X, width)
    X <- X[X != y]
    deltaX <- c(deltaX, abs(y - X))
  }
  
  if (all(abs(width - y - X) %in% deltaX)){
    deltaX <- Remove(deltaX, abs(width - y -X))
    X <- c(X, width - y)
    Place(deltaX, X, width)
    X <- X[X != y] 
    deltaX <- c(deltaX, abs(width - y - X))
  }
  
  return()
}

deltaX <- c(2, 2, 3, 3, 4, 5, 6, 7, 8, 10)
X <- c(0)
width <- 10

a <- Place(deltaX, X, width)
print(a)


