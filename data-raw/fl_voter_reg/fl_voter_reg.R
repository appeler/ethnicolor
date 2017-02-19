# Florida Voter Registration Data as of 02-07-2017

# set directory
setwd(basedir)
setwd("data/fl_voter_reg/20170207_VoterDetail/")

# Load library
library(data.table)

# Iterate over directory
data             <- lapply(dir(), function(x) fread(x, select = c(3:6, 21)))
# rbind the list
fl_voters        <- do.call("rbind", data)
# meaningful col. names
names(fl_voters) <- c("name_last", "name_suffix", "name_first", "name_middle", "race")
# recode race 
fl_voters$race <- car::recode(fl_voters$race, "1 ='native_indian'; 2 = 'asian'; 3 = 'nh_black'; 4 ='hispanic'; 5 = 'nh_white' ; 6 = 'other'; 7 = 'multi_racial'; 9 = 'unknown'")
# hose the rows where race is unknown or other 
fl_voters <- subset(fl_voters, !(race %in% c("other", "unknown")))
# write out the file
write.csv(fl_voters, file="fl_reg_name_race.csv", row.names=F)
