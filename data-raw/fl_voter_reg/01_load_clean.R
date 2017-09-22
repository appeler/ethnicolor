# Florida Voter Registration Data as of 02-07-2017
# data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UBIG3F

# set directory
setwd(basedir)
setwd("data/fl_voter_reg/")
setwd("20170207_VoterDetail/")

# Load library
library(data.table) # very fast for reading from disk
library(phonics)

# Iterate over directory, rbind the list
fl_reg   <- do.call("rbind", lapply(dir(), function(x) fread(x, select = c(3:6, 21)))) # 5 * 13710358

# meaningful col. names
names(fl_reg) <- c("name_last", "name_suffix", "name_first", "name_middle", "race")

# recode race 
fl_reg$race <- car::recode(fl_reg$race, "1 ='native_indian'; 2 = 'asian'; 3 = 'nh_black'; 4 ='hispanic'; 5 = 'nh_white' ; 6 = 'other'; 7 = 'multi_racial'; 9 = 'unknown'")

# Subset to only NH White, NH Black, and Hispanic
# Removing other, unknown, multi_racial, and native_indian for now as other analysis suggests 
# that we can't predict well based on name alone
fl_reg <- subset(fl_reg, race %in% c("nh_white", "nh_black", "hispanic", "asian"))

# Length of first, last, and middle names
fl_reg$last_length   <- nchar(fl_reg$last_length)
fl_reg$middle_length <- nchar(fl_reg$middle_length)
fl_reg$first_length  <- nchar(fl_reg$first_length)

# Let's concatenate the name. We use semi-colon to collapse to let diff. parts of the name be handled diff.
fl_reg$name <- do.call(paste, c(fl_reg[, c("name_first", "name_middle", "name_last", "name_suffix")], sep=";"))

# Soundex coding of the name (semi-colon breaks soundex. so soundex and then concatenate. just first and last for now.)
fl_reg$name_first_p <- soundex(fl_reg$name_first, maxCodeLen = 25L)
fl_reg$name_last_p <- soundex(fl_reg$name_last, maxCodeLen = 25L)

# Concatenate Soundex coding
fl_reg$name_p <- do.call(paste0, c(fl_reg[, c("name_first_p", "name_last_p")]))
