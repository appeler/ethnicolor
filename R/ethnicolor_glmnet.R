library(pacman)
p_load(data.table, ggplot2, glmnet)

fl_reg <- fread("data-raw/fl_voter_reg/fl_reg.csv")

zeroOne <- function(x, ...) x/max(x, ...)

set.seed(2017)
N <- nrow(fl_reg) # 13,044,056

X <- dplyr::select(fl_reg, last_rank, first_rank, suffix_dum, middle_dum)
for(v in names(X))
  X[[v]] <- ifelse(is.na(X[[v]]), max(X[[v]], na.rm = TRUE), X[[v]])

X$last_rank <- zeroOne(X$last_rank)  # least popular = 1, most popular = 0 (near 0)
X$first_rank <- zeroOne(X$first_rank)
X$name_last_p <- fl_reg$name_last_p
X$name_first_p <- fl_reg$name_first_p # _p denotes phonic sound
X$name_p <- fl_reg$name_p
race <- fl_reg$race
# remove(fl_reg) # fl_reg can be removed about here

# for debugging...
# convergence, speed issues starting at prob = c(0.005, 0.995)
# out of sample accuracy increases gradually from 70% to 75% as more data is added...
# note increasing N means increasing P because using soundex measures with high precision
# still overpredicts nh_white...
partition <- sample(c("include", "omit"), size = N, replace = TRUE, prob = c(0.01, 0.99))
split <- sample(c("train", "test"), size = sum(partition == "include"), replace = TRUE, prob = c(0.8, 0.2))

y <- race[partition == "include"]  
y_train <- y[split == "train"]
y_test <- y[split == "test"]



# take care to ensure ncol(x_train) == ncol(x_test)
x_tmp <- sparse.model.matrix(~ last_rank * first_rank * suffix_dum * middle_dum + 
                                 name_last_p + name_first_p + name_p,
                               data = dplyr::filter(X, partition == "include"))

x_train <- x_tmp[split == "train", ]
x_test <- x_tmp[split == "test", ]

  

elastic.est <- glmnet(x_train, y_train, family = "multinomial") 

print(elastic.est) 
# plot(elastic.est)


fit <- predict(elastic.est, x_test)
dim(fit)
s <- dim(fit)[3]
predictions <- attributes(fit)$dimnames[[2]][apply(fit[,,s], 1, which.max)]
cat("Out of sample, the model gets ", 
    100*round(mean(y_test == predictions),3), "% right (at s = ", s, ").", sep= "")
confusion <- 100*round(table(y_test, predictions)/length(y_test), 4)
confusion 

