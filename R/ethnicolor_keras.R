library(pacman)
p_load(data.table, keras, abind, raster, tensorflow, ggplot2, Matrix)

fl_reg <- fread("data-raw/fl_voter_reg/fl_reg.csv")

set.seed(2017)
p <- 0.0075 # pData to work with
K <- 4 # length(unique(fl_reg$race))
RHS <- formula(~ last_rank * first_rank * suffix_dum * middle_dum + name_last_p + name_first_p)
# using name_p, with or without name_last_p and name_first_p, yields 
# transfer from fl_reg to X # Error in asMethod(object) : 
# Cholmod error 'problem too large' at file ../Core/cholmod_dense.c, line 105
batch_size <- 64
Nepochs <- 25

zeroOne <- function(x, ...) x/max(x, ...)

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


partition <- sample(c("include", "omit"), size = N, replace = TRUE, prob = c(p, 1- p))
split <- sample(c("train", "test"), size = sum(partition == "include"), replace = TRUE, prob = c(0.8, 0.2))

y <- race[partition == "include"] 

y <- to_categorical(-1 + match(y, unique(y))) 
# -1 for python indexing arrays starting at 0
y_train <- y[split == "train",]
y_test <- y[split == "test",]

# take care to ensure ncol(x_train) == ncol(x_test)
x_tmp <- sparse.model.matrix(RHS, data = dplyr::filter(X, partition == "include"))

x_train <- x_tmp[split == "train", ]
x_test <- x_tmp[split == "test", ]
P <- ncol(x_train) 

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(P)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = K, activation = "softmax")

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = Nepochs, batch_size = batch_size, 
  validation_split = 0.2
)

plot(history) + theme_minimal()

model %>% evaluate(x_test, y_test)

predictions <- model %>% predict_classes(x_test)
table(race[partition == "include"][split == "test"], 4 - predictions)


