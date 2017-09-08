library(stringdist)
library(phonics)

fl_voters <- data.table::fread("fl_reg_name_race.csv") # way faster than read.csv at this size

set.seed(2017)
idx <- sample(nrow(fl_voters), 100, replace = FALSE)
X <- fl_voters[idx]
W <- cbind(X$name_last, X$name_middle, X$name_first)

total.string.distance <- function(a, b){
  # assumes multiple names, sums diagonal (first vs. first) and lower triangle (e.g., first vs. middle)
  D <- stringdistmatrix(a, b, method = "soundex")
  return(sum(D[cbind(1:nrow(D), 1:nrow(D))]) + sum(D[lower.tri(D)]))
}

