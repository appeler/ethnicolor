# Florida Voter Registration Data as of 02-07-2017
# data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UBIG3F

# data.dir assuming working directory == project directory
data.dir <- "data-raw/fl_voter_reg/20170207_VoterDetail"
files <- dir(data.dir, pattern = ".txt", full.names = TRUE) # 67 data files

# Load library
library(data.table) # very fast for reading from disk
library(phonics)

# Iterate over directory, rbind the list
fl_reg   <- do.call("rbind", lapply(files, function(f) fread(f, select = c(3:6, 21)))) 
# 13,710,358 x 5

# meaningful col. names
names(fl_reg) <- c("name_last", "name_suffix", "name_first", "name_middle", "race")

# recode race
fl_reg$race <- car::recode(fl_reg$race, "1 ='native_indian'; 2 = 'asian'; 3 = 'nh_black'; 4 ='hispanic'; 5 = 'nh_white' ; 6 = 'other'; 7 = 'multi_racial'; 9 = 'unknown'")

# Subset to only NH White, NH Black, and Hispanic
# Removing other, unknown, multi_racial, and native_indian for now as other analysis suggests 
# that we can't predict well based on name alone
fl_reg <- subset(fl_reg, race %in% c("nh_white", "nh_black", "hispanic", "asian"))

# Length of first, last, and middle names
fl_reg$last_length   <- nchar(fl_reg$name_last)
fl_reg$middle_length <- nchar(fl_reg$name_middle)
fl_reg$first_length  <- nchar(fl_reg$name_first)

# Dummy for suffix and middle name 
fl_reg$suffix_dum <- ifelse(fl_reg$name_suffix=="", 0, 1)
fl_reg$middle_dum <- ifelse(fl_reg$name_middle=="", 0, 1)
fl_reg$middle_dum <- ifelse(is.na(fl_reg$middle_dum), 0, fl_reg$middle_dum) # not needed for suffix

# Let's concatenate the name. We use semi-colon to collapse to let diff. parts of the name be handled diff.
fl_reg$name <- do.call(paste, c(fl_reg[, c("name_first", "name_middle", "name_last", "name_suffix")], sep=";"))

# Soundex coding of the name (semi-colon breaks soundex. so soundex and then concatenate. just first and last for now.)
fl_reg$name_first_p <- soundex(fl_reg$name_first, maxCodeLen = 25L)
fl_reg$name_last_p <- soundex(fl_reg$name_last, maxCodeLen = 25L)

# Concatenate Soundex coding
fl_reg$name_p <- do.call(paste0, c(fl_reg[, c("name_first_p", "name_last_p")]))

fl_reg$name_last_p[fl_reg$name_last_p == ""] <- "space"
fl_reg$name_first_p[fl_reg$name_first_p == ""] <- "space"

# ranking soundex. pooling first and last names ...

# ranks <- rank(c(fl_reg$name_first_p, fl_reg$name_last_p)) # very slow
counts <- table(c(fl_reg$name_first_p, fl_reg$name_last_p)) # instantaneous. length 48915
ranks <- rank(counts)    # slow but tolerable
ranks <- max(ranks) + 1 - ranks 
# make most popular 1, third most popular 3, etc. 
# easier to select top 1,000 if population criteria or data cleaning methods change slightly

fl_reg$last_rank <- match(fl_reg$name_last_p, names(ranks))
fl_reg$first_rank <- match(fl_reg$name_first_p, names(ranks))

# fwrite(fl_reg, "data-raw/fl_voter_reg/fl_reg.csv") # 1.2 gigabytes
# system("zip data-raw/fl_voter_reg/fl_reg.zip data-raw/fl_voter_reg/fl_reg.csv") # 411.5 MB
