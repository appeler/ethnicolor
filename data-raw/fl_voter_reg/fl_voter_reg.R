# Florida Voter Registration Data as of 02-07-2017
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UBIG3F

# set directory
setwd(basedir)
setwd("data-raw/fl_voter_reg/")
setwd("20170207_VoterDetail/")

# Load library
library(data.table)

# Iterate over directory

loadClean <- function(x, phonics = TRUE){
  # phonics more compact than storing names in memory
  # if phonics = FALSE, run for loop below on 'data'
  
  function(x){
    d <- fread(x, select = c(3:6, 21))
    colnames(d) <- c("name_last", "name_suffix", "name_first", "name_middle", "race")
    if(phonics){ 
      for(i in colnames(d)[grep("name_", colnames(d))]){
        d[[i]] <- soundex(d[[i]]) 
      }
    }
    return(d)
  }
}

data  <- lapply(dir(pattern = ".txt"), clean) # may want dir(pattern = "_20170207.txt")

# rbind the list
fl_voters  <- do.call("rbind", data)
dim(fl_voters) # 13,710,358 x 5
remove(data)

# meaningful col. names (now in lapply function...)
# names(fl_voters) <- c("name_last", "name_suffix", "name_first", "name_middle", "race")

# recode race 
fl_voters$race <- car::recode(fl_voters$race, "1 ='native_indian'; 2 = 'asian'; 3 = 'nh_black'; 4 ='hispanic'; 5 = 'nh_white' ; 6 = 'other'; 7 = 'multi_racial'; 9 = 'unknown'")
# hose the rows where race is unknown or other 
fl_voters <- subset(fl_voters, !(race %in% c("other", "unknown")))
# write out the file
write.csv(fl_voters, file="fl_reg_name_race.csv", row.names=F) # 441 megabytes

library(class)

table(fl_voters$race)
set.seed(2017)
N <- 5000
pTraining <- 0.9
train <- sample(nrow(fl_voters), size = pTraining*N, replace = FALSE)
test <- sample(1:nrow(fl_voters)[-train], size = (N - length(train)), replace = FALSE)

race_pred <- knn(cbind(as.factor(fl_voters$name_last[train]), as.factor(fl_voters$name_first[train])), 
                 cbind(as.factor(fl_voters$name_last[test]), as.factor(fl_voters$name_first[test])), 
                 as.factor(fl_voters$race[train]), k = 20)

table(fl_voters$race[test], race_pred)
mean(race_pred == fl_voters$race[test])

