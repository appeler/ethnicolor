library(stringdist)
library(phonics)

fl_voters <- data.table::fread("fl_reg_name_race.csv") # way faster than read.csv at this size

set.seed(2017)
idx <- sample(nrow(fl_voters), 500, replace = FALSE)
X <- fl_voters[idx]
W <- cbind(X$name_last, X$name_middle, X$name_first)

total.string.distance <- function(a, b, weight = 0.5){
  # assumes multiple names, sums diagonal (first vs. first) and lower triangle (e.g., first vs. middle)
  # weight is for comparisons like first vs. middle name (less than 1 indicates less important)
  D <- stringdistmatrix(a, b, method = "soundex")
  return(sum(D[cbind(1:nrow(D), 1:nrow(D))]) + weight*sum(D[lower.tri(D)]))
}

predict.kNN <- function(Xtrain, Xtest, y, k, weight = TRUE){
  
  nearest <- apply(Xtest, 1, function(xtest, Xtrain){
    which(rank(apply(Xtrain, 1, total.string.distance, xtest)) < k)
  }, Xtrain)
  
  cat(sum(unlist(lapply(nearest, function(x) length(x) == 0))), "degenerate cases.")
  
  labs <- sort(unique(y))
  weights <- if(weight) table(y)/length(y) else NULL
  y.hat <- matrix(nrow = length(nearest))
  
  for(i in 1:length(nearest)){

    if(length(y[nearest[[i]]]) == 0){ # "loners"
      y.hat[i] <- sample(labs, 1, prob = weights) 
      # which(rank(rep(1, 99)) < 10) yields integer(0) since the ranks all tie and are placed at midpoint
      # in event no 'meaningul' neighbors, just takes a random draw
    }else{
      
      tab <- table(y[nearest[[i]]])
      tmp <- which(tab == max(tab))
      if(length(tmp) == 1){
        y.hat[i] <- labs[tmp]
      } else{
        w <- weights[tmp]/sum(weights[tmp])
        y.hat[i] <- sample(labs[tmp], 1, prob = w)
      }
    }
    
  }
  return(y.hat)
}


set.seed(2017)
#N <- 2000
#pTraining <- 0.9
#train <- sample(nrow(fl_voters), size = pTraining*N, replace = FALSE)
#test <- sample(1:nrow(fl_voters)[-train], size = (N - length(train)), replace = FALSE)

#X <- cbind(fl_voters$name_last, fl_voters$name_middle, fl_voters$name_first)
#Xtrain <- X[train,]
#Xtest <- X[test,]

#y.pred <- predict.kNN(Xtrain, Xtest, y = fl_voters$race[train], k = 25)
#table(y.obs = fl_voters$race[test], y.pred)
#mean(fl_voters$race[test] == y.pred) # 0.64
