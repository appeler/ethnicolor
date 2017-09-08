# Florida Voter Registration Data as of 02-07-2017
# data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UBIG3F
# data cleaning: https://github.com/soodoku/ethnicolor/blob/master/data-raw/fl_voter_reg/fl_voter_reg.R

# set directory
setwd(basedir)
setwd("data-raw/fl_voter_reg/")
setwd("20170207_VoterDetail/")

library(data.table) # very fast for reading from disk

# Iterate over directory

loadClean <- function(x, phonics = TRUE, storeLength = TRUE){
  # phonics more compact than storing names in memory
  # if phonics = FALSE, run for loop below on 'data'
  
  function(x){
    d <- fread(x, select = c(3:6, 21))
    colnames(d) <- c("name_last", "name_suffix", "name_first", "name_middle", "race")
    if(storeLength){
      d[["last_length"]] <- nchar(d[["name_last"]])
      d[["middle_length"]] <- nchar(d[["name_middle"]])
      d[["first_length"]] <- nchar(d[["name_first"]])
    }
    if(phonics){ 
      for(i in colnames(d)[grep("name_", colnames(d))]){
        d[[i]] <- soundex(d[[i]]) 
      }
    }
    return(d)
  }
}

data  <- lapply(dir(pattern = ".txt"), loadClean()) # may want dir(pattern = "_20170207.txt")
# rbind the list
fl_voters  <- do.call("rbind", data)
dim(fl_voters) # 13,710,358 x 5
remove(data)

# for KNN
standardize <- function(x, NAremove = TRUE){
  z <- (x - mean(x, na.rm = TRUE)/(sd(x, na.rm = TRUE)))
  if(NAremove) z[is.na(x)] = 0
  return(z)
}

fl_voters$last_length <- standardize(fl_voters$last_length)
fl_voters$middle_length <- standardize(fl_voters$middle_length)
fl_voters$first_length <- standardize(fl_voters$first_length)

# meaningful col. names (now in lapply function...)
# names(fl_voters) <- c("name_last", "name_suffix", "name_first", "name_middle", "race")

# recode race 
fl_voters$race <- car::recode(fl_voters$race, "1 ='native_indian'; 2 = 'asian'; 3 = 'nh_black'; 4 ='hispanic'; 5 = 'nh_white' ; 6 = 'other'; 7 = 'multi_racial'; 9 = 'unknown'")
# remove infrequent categories 
fl_voters <- subset(fl_voters, !(race %in% c("other", "unknown", "native_indian", "multi_racial", "asian")))
# write out the file

fwrite(fl_voters, file="fl_reg_name_race.csv", row.names=F) # 441 megabytes

library(class)

table(fl_voters$race)
set.seed(2017)
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

