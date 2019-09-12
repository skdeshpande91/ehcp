# For the stan models
library(rstan)
library(BART)

load("data/test_train_splits.RData")
load("stan_logit_model.RData")

# Need to prepare the X matrix

binary_var <- c("1_score_lead", "1_score_trail", "2_score_lead", "2_score_trail")


for(r in 1:10){
  print(paste("Starting", r, "at", Sys.time()))
  x_train <- X_train[[r]]
  y_train <- Y_train[[r]]
  
  x_test <- X_test[[r]]
  y_test <- Y_test[[r]]
  
  std_x_train <- x_train
  std_x_test <- x_test
  
  for(v in colnames(x_train)){
    if(!v %in% binary_var){
      mu <- mean(x_train[,v], na.rm = TRUE)
      sigma <- sd(x_train[,v], na.rm = TRUE)
      std_x_train[,v] <- (x_train[,v] - mu)/(2 * sigma)
      std_x_test[,v] <- (x_test[,v]  - mu)/(2 * sigma)
    } else{
      phat <- mean(x_train[,v], na.rm = TRUE)
      std_x_train[,v] <- x_train[,v] - phat
      std_x_test[,v] <- x_test[,v] - phat
    }
  }
  
  data <- list(N_train = nrow(std_x_train), N_test = nrow(std_x_test), p = ncol(std_x_train), x_train = std_x_train, x_test = std_x_test, y = y_train)
  stan_fit <- sampling(object = stan_logit_model, data = data, pars = c("phat_train", "phat_test"), include = TRUE, chains = 2, iter = 2000, thin = 2)
  
  phat_train_stan <- colMeans(extract(stan_fit, pars = c("phat_train"))[["phat_train"]])
  phat_test_stan <- colMeans(extract(stan_fit, pars = c("phat_test"))[["phat_test"]])
  rm(stan_fit)
  
  
  bart_fit1 <- lbart(x.train = x_train, y.train = y_train, x.test = x_test, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)
  
  phat_train_bart1 <- colMeans(1/(1 + exp(-1 * bart_fit1$yhat.train)))
  phat_test_bart1 <- colMeans(1/(1 + exp(-1 * bart_fit1$yhat.test)))
  rm(bart_fit1)
  
  bart_fit2 <- lbart(x.train = x_train, y.train = y_train, x.test = x_test, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)
  
  phat_train_bart2 <- colMeans(1/(1 + exp(-1 * bart_fit2$yhat.train)))
  phat_test_bart2 <- colMeans(1/(1 + exp(-1 * bart_fit2$yhat.test)))
  
  rm(bart_fit2)

  phat_train_bart <- 0.5 * phat_train_bart1 + 0.5 * phat_train_bart2
  phat_test_bart <- 0.5 * phat_test_bart1 + 0.5 * phat_test_bart2
  
  # train_log_loss
  train_log_loss <- c()
  train_log_loss["stan"] <- -1 * mean( (y_train * log(phat_train_stan) + (1 - y_train) * log(1-phat_train_stan)))
  train_log_loss["bart"] <- -1 * mean( (y_train * log(phat_train_bart) + (1 - y_train) * log(1 - phat_train_bart)))
  
  test_log_loss <- c()
  test_log_loss["stan"] <- -1 * mean( (y_test * log(phat_test_stan) + (1 - y_test)* log(1 - phat_test_stan)))
  test_log_loss["bart"] <- -1 * mean( (y_test * log(phat_test_bart) + (1 - y_test)*log(1 - phat_test_bart)))
  
  train_mse <- c()
  train_mse["stan"] <- mean( (y_train - phat_train_stan)^2)
  train_mse["bart"] <- mean( (y_test - phat_test_stan)^2)
  
  test_mse <- c()
  test_mse["stan"] <- mean( (y_test - phat_test_stan)^2)
  test_mse["bart"] <- mean( (y_test - phat_test_bart)^2)
  
  train_miss <- c()
  train_miss["stan"] <- mean( y_train != 1*(phat_train_stan >= 0.5))
  train_miss["bart"] <- mean( y_train != 1*(phat_train_bart >= 0.5))
  
  test_miss <- c()
  test_miss["stan"] <- mean( y_test != 1*(phat_test_stan >= 0.5))
  test_miss["bart"] <- mean( y_test != 1*(phat_test_bart >= 0.5))
  
  assign(paste0("train_phat_", r), cbind("stan" = phat_train_stan, "bart" = phat_train_bart))
  assign(paste0("test_phat_", r), cbind("stan" = phat_test_stan, "bart" = phat_test_bart))
  
  assign(paste0("train_loss_", r), cbind("mse" = train_mse, "miss" = train_miss, "log_loss" = train_log_loss))
  assign(paste0("test_loss_", r), cbind("mse" = test_mse, "miss" = test_miss, "log_loss" = test_log_loss))
  
  save(list = paste0(c("train_phat_", "test_phat_", "train_loss_", "test_loss_"), r), file = paste0("validation/split_", r, ".RData"))
}
train_phat <- array(dim = c(nrow(x_train), 2, 10), dimnames = list(c(), c("stan", "bart"), c()))
test_phat <- array(dim = c(nrow(x_test), 2, 10), dimnames = list(c(), c("stan", "bart"), c()))
train_loss <- array(dim = c(2, 3, 10), dimnames = list(c("stan", "bart"), c("mse", "miss", "log_loss"), c()))
test_loss <- array(dim = c(2, 3, 10), dimnames = list(c("stan", "bart"), c("mse", "miss", "log_loss"), c()))
for(r in 1:10){
  train_phat[,,r] <- get(paste0("train_phat_", r))
  test_phat[,,r] <- get(paste0("test_phat_", r))
  train_loss[,,r] <- get(paste0("train_loss_", r))
  test_loss[,,r] <- get(paste0("test_loss_", r))
}

train_loss_avg <- apply(train_loss, MARGIN = c(1,2), FUN = mean)
train_loss_sd <- apply(train_loss, MARGIN = c(1,2), FUN = sd)

test_loss_avg <- apply(test_loss, MARGIN = c(1,2), FUN = mean)
test_loss_sd <- apply(test_loss, MARGIN = c(1,2), FUN = sd)

save(train_phat, test_phat, train_loss, test_loss, file = "validation/results.RData")

### Check calibrartion
p_seq <- seq(0, 1, by = 0.05)
stan_cal <- matrix(nrow = length(p_seq) - 1, ncol = 10)
bart_cal <- matrix(nrow = length(p_seq) - 1, ncol = 10)
for(r in 1:10){
  for(ix in 1:(length(p_seq) - 1)){
    stan_index <- which(p_seq[ix] <= test_phat[,"stan", r] & test_phat[,"stan",r] < p_seq[ix+1])
    bart_index <- which(p_seq[ix] <= test_phat[,"bart",r] & test_phat[,"bart",r] < p_seq[ix+1])
    stan_cal[ix,r] <- mean(Y_test[[r]][stan_index])
    bart_cal[ix,r] <- mean(Y_test[[r]][bart_index])
  }
}

par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0), cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.9)
plot(p_seq[-1] - (p_seq[2] - p_seq[1])/2, bart_cal[,1], pch = 16, cex = 0.5, 
     xlab = expression(hat(p)), ylab = expression(hat(y)), main = "Calibrartion on Test Set")
points(p_seq[-1] - (p_seq[2] - p_seq[1])/2, stan_cal[,1], pch = 3, cex = 0.5, col = 'red')
abline(a = 0, b = 1, col = 'grey', lty = 2)
legend("bottomright", legend = c("bart","linear"), cex = 0.6, col = c("black", "red"), pch = c(16,3), bty = "n")

for(ix in 1:(length(p_seq) - 1)){
  stan_index <- which(train_phat)
  stan_cal[ix] <- mean(y_test[)
}





