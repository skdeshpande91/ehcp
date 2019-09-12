library(BART)
library(coda)
load("data/all_training.RData")

drop_cols <- c("gameId", "playId", "Y", "outcome", "time_snap", "time_pass", "time_play", 
               "receiver_id", "def_pass_id", "def_play_id", "tie", "score_diff_cat", "score_diff")

y <- all_training[,"Y"]
x <- all_training[,!colnames(all_training) %in% drop_cols]

n <- nrow(x)

X_train <- list()
Y_train <- list()

X_test <- list()
Y_test <- list()
# Create 10 training/testing splits
for(r in 1:10){
  set.seed(129*r + 1)
  train_index <- sample(1:n, floor(0.75*n), replace = FALSE)
  test_index <- which(!1:n %in% train_index)
  
  X_train[[r]] <- x[train_index,]
  X_test[[r]] <- x[test_index,]
  Y_train[[r]] <- y[train_index]
  Y_test[[r]] <- y[test_index]
  
}
save(X_train, X_test, Y_train, Y_test, file = "data/test_train_splits.RData")

source("scripts/logistic_stan_code.R")
stan_logit_model <- stan_model(model_code = logit_code1)
save(stan_logit_model, file = "stan_logit_model.RData")


print(paste("Starting bart_fit1 at", Sys.time()))
bart_fit1 <- lbart(x.train = x, y.train = y, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)
save(bart_fit1, file = "data/bart_fit1.RData")
print(paste("Finished bart_fit1 at", Sys.time()))
print(paste("Starting bart_fit2 at", Sys.time()))
bart_fit2 <- lbart(x.train = x, y.train = y, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)
save(bart_fit2, file = "data/bart_fit2.RData")
print(paste("Finished bart_fit2 at", Sys.time()))