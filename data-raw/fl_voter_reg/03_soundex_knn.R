# KNN model based on Soundex coding of names.
# data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UBIG3F
# data cleaning: https://github.com/soodoku/ethnicolor/blob/master/data-raw/fl_voter_reg/01_load_clean.R

# Set seed
set.seed(2017)

# for KNN
standardize <- function(x, NAremove = TRUE){
  z <- (x - mean(x, na.rm = TRUE)/(sd(x, na.rm = TRUE)))
  if(NAremove) z[is.na(x)] = 0
  return(z)
}


fwrite(fl_voters, file="fl_reg_name_race.csv", row.names=F) # 441 megabytes

N <- 5000
pTraining <- 0.9
train <- sample(nrow(fl_voters), size = pTraining*N, replace = FALSE)
test <- sample(1:nrow(fl_voters)[-train], size = (N - length(train)), replace = FALSE)


Xtrain = cbind(as.factor(fl_voters$name_last[train]), as.factor(fl_voters$name_first[train]), 
               fl_voters$last_length[train], fl_voters$first_length[train], fl_voters$middle_length[train])
Xtest = cbind(as.factor(fl_voters$name_last[test]), as.factor(fl_voters$name_first[test]), 
              fl_voters$last_length[test], fl_voters$first_length[test], fl_voters$middle_length[test])

race_pred <- knn(Xtrain, Xtest, 
                 as.factor(fl_voters$race[train]), k = 20)

table(fl_voters$race[test], race_pred)
mean(race_pred == fl_voters$race[test])

