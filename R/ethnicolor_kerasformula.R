library(pacman)
p_load(data.table, kerasformula, ggplot2, Matrix)

fl_reg <- fread("data-raw/fl_voter_reg/fl_reg.csv")

zeroOne <- function(x, ...) x/max(x, ...)

for(v in c("last_rank", "first_rank", "last_length", "first_length"))
  fl_reg[[v]] <- zeroOne(ifelse(is.na(fl_reg[[v]]), max(fl_reg[[v]], na.rm = TRUE), fl_reg[[v]]))

set.seed(2018)
include <- sample(nrow(fl_reg), size = 0.001*nrow(fl_reg))
# proportion of data to work with (training + validate + holdout)

dense_fit <- kms("race ~ last_length * first_length * last_rank * first_rank + suffix_dum + middle_dum + name_last_p + name_first_p", 
                 fl_reg[include,])

dense_fit$evaluations$acc
dense_fit$confusion
dense_fit$P # 4495

k <- keras_model_sequential()
k %>%
  layer_embedding(input_dim = dense_fit$P, output_dim = dense_fit$P) %>% 
  layer_lstm(units = 512, dropout = 0.4, recurrent_dropout = 0.2) %>% 
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 4, # number of levels observed on y (outcome)  
              activation = 'sigmoid')

k %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'rmsprop',
  metrics = c('accuracy')
)

lstm_fit <- kms("race ~ last_length * first_length * last_rank * first_rank + suffix_dum + middle_dum + name_last_p + name_first_p", 
                fl_reg[include,], k)

