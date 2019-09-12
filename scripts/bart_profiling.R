# See how adjusting a few variables in the kupp TD changes the catch probability
library(BART)
load("data/bart_fit1.RData")
load("data/bart_fit2.RData")
load("kupp_td_covariates.RData")
load("data/all_training.RData")


profiling_results <- list()

profiling_vars <- c("receiver_s_play", "receiver_dist_after", "separation_play", 
                    "receiver_s_pass", "receiver_dist_before", "separation_pass")


#profiling_vars <- c("separation_pass", "receiver_s_pass", "ball_receiver_dist_pass", "receiver_s_play", "separation_play", "receiver_dist_total")

profiling_sd <- apply(all_training[,profiling_vars], FUN = sd, MARGIN = 2)

drop_cols <- c("gameId", "playId", "Y", "outcome", "time_snap", "time_pass", "time_play", 
               "receiver_id", "def_pass_id", "def_play_id", "tie", "score_diff_cat", "score_diff")

X_orig <- kupp_td[!names(kupp_td) %in% drop_cols]

for(x in profiling_vars){
  new_range <- seq(max(0,X_orig[1,x] - 3 * profiling_sd[x]), X_orig[1,x] + 3 * profiling_sd[x], length = 100)
  newX <- data.frame(X_orig)
  for(r in 1:100){
    newX[r+1,] <- X_orig
  }
  newX[,x] <- c(X_orig[1,x],new_range)
  preds <- rbind(predict.lbart(bart_fit1, newdata = newX)$prob.test, predict.lbart(bart_fit2, newdata = newX)$prob.test)
  tmp_preds <- cbind("L95" = apply(preds, FUN = quantile, MARGIN = 2,probs = 0.025),
                     "Mean" = apply(preds, FUN = mean, MARGIN = 2),
                     "U95" = apply(preds, FUN = quantile, MARGIN = 2, probs = 0.975))
  profiling_results[[x]] <- cbind("X" = c(X_orig[,x], new_range), tmp_preds)
  #print(paste(x, tmp_preds[1, "Mean"], tmp_preds[2, "Mean"], tmp_preds[101, "Mean"]))
  #assign(paste0(x, "_profile"), cbind("X" = c(X_orig[,x], new_range), tmp_preds))
  
}

for(x in profiling_vars){
  print(paste(x,round(100 * profiling_results[[x]][1, "Mean"], digits = 2), 
              round(100 * profiling_results[[x]][2, "Mean"], digits = 2), 
              round(100 * profiling_results[[x]][101, "Mean"], digits = 2)))
}



plot_mains <- c("Speed at Catch", "Dist. Travelled until Catch", "Separation at Catch",
                "Speed at Pass", "Dist. Travelled until Pass", "Separation at Pass")
plot_xlab <- c("Yards/sec", "Yards", "Yards", "Yards/sec", "Yards", "Yards")

png(file ="JQAS_revision/kupp_completion_prob_profiling.png", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0), mfrow = c(2,3), cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
for(ix in 1:6){
  x <- profiling_vars[ix]
  plot(1, type = "n", xlim = range(profiling_results[[x]][,"X"]), ylim = c(0,1), 
       xlab = plot_xlab[ix], ylab = "Completion Probability", main = plot_mains[ix])
  lines(profiling_results[[x]][-1,"X"], profiling_results[[x]][-1, "Mean"])
  lines(profiling_results[[x]][-1, "X"], profiling_results[[x]][-1, "L95"], lty = 2)
  lines(profiling_results[[x]][-1, "X"], profiling_results[[x]][-1, "U95"], lty = 2)
  points(profiling_results[[x]][1,"X"], profiling_results[[x]][1, "Mean"], pch = 16, col = 'red')
  #lines(c(profiling_results[[x]][1,"X"], profiling_results[[x]][1,"X"]), c(par("usr")[3], profiling_results[[x]][1, "Mean"]), col = 'red')
}

dev.off()



round(profiling_results[["separation_pass"]][c(1, 2, 101),], digits = 4)




round(receiver_s_pass_profile[c(1, 2, 101),], digits = 4)
round()

# The AE suggested separation
separation_seq <- seq(0, 10, length = 100)
newX <- data.frame(X_orig)
for(r in 1:100){
  newX[r+1,] <- X_orig
}
newX[,"separation_pass"] <- c(X_orig["separation_pass",1], separation_seq)
preds_1 <- predict.lbart(bart_fit1, newdata = newX)
preds_2 <- predict.lbart(bart_fit2, newdata = newX)
preds <- rbind(predict.lbart(bart_fit1, newdata = newX)$prob.test, predict.lbart(bart_fit2, newdata = newX)$prob.test)

plot(1, xlim = range(separation_seq), ylim = c(0,1), xlab = "Separation (Pass)", ylab = "Completion Probability", type = "n")
lines(separation_seq, colMeans(preds)[-1])
lines(separation_seq, apply(preds, MARGIN = 2, FUN = quantile, probs = 0.975)[-1], col = 'red')
lines(separation_seq, apply(preds, MARGIN = 2, FUN = quantile, probs = 0.025)[-1], col = 'red')
# Most important variable is speed
speed_seq <- X_orig[1,"receiver_s_pass"] + seq(-3, 3, length = 100)
newX <- data.frame(X_orig)
for(r in 1:100){
  newX[r+1,] <- X_orig
}

# referee also suggested ball_receiver_dist_pass
ball_receiver_dist_pass <- seq(0, 10, length = 100)
newX <- data.frame(X_orig)
for(r in 1:100){
  newX[r+1,] <- X_orig
}
newX[,"ball_receiver_dist_pass"] <- c(X_orig["ball_receiver_dist_pass",1], ball_receiver_dist_pass)
preds <- rbind(predict.lbart(bart_fit1, newdata = newX)$prob.test, predict.lbart(bart_fit2, newdata = newX)$prob.test)

colMeans(preds)[2]
colMeans(preds)[101]



plot(1, xlim = range(separation_seq), ylim = c(0,1), xlab = "Separation (Pass)", ylab = "Completion Probability", type = "n")
lines(separation_seq, colMeans(preds)[-1])
lines(separation_seq, apply(preds, MARGIN = 2, FUN = quantile, probs = 0.975)[-1], col = 'red')
lines(separation_seq, apply(preds, MARGIN = 2, FUN = quantile, probs = 0.025)[-1], col = 'red')



newX[,"receiver_s_pass"] <- c(X_orig["receiver_s_pass",1], speed_seq)
preds_1 <- predict.lbart(bart_fit1, newdata = newX)
preds_2 <- predict.lbart(bart_fit2, newdata = newX)
preds <- rbind(predict.lbart(bart_fit1, newdata = newX)$prob.test, predict.lbart(bart_fit2, newdata = newX)$prob.test)

#plot(1, xlim = range(speed_seq), ylim = c(0,1))
#lines(speed_seq, colMeans(preds[,-1]))






test_pred <- predict.lbart(bart_fit1, newdata = as.data.frame(X_orig))

cov_names <- c("time_snap_to_pass")


newX <- data.frame("time_snap_to_pass" = rep(kupp_td["time_snap_to_pass"], times = 100),
                   "time_pass_to_play" = rep(kupp_td["time_pass_to_play"], times = 100),
                   "time_total" = rep(kull_td))



counter_df <- data.frame("time_snap_to_pass" = rep(time_snap_to_pass, times = n_samples))
counter_df[,"time_pass_to_play"] <- as.numeric(as.POSIXct(time_play) - as.POSIXct(time_pass))
counter_df[,"time_total"] <- total_time

def_pass_id <- get_closest_def(play_df, receiver_id, time_pass)
def_play_id <- get_closest_def(play_df, receiver_id, time_play)

# Get receiver speed
counter_df[,"receiver_s_pass"] <- get_cov(play_df, time_pass, receiver_id, "s")
counter_df[,"receiver_s_play"] <- get_cov(play_df, time_play, receiver_id, "s")
counter_df[,"receiver_s_change"] <- counter_df[,"receiver_s_play"] - counter_df[,"receiver_s_pass"]

# Receiver directions
counter_df[,"receiver_dir_pass"] <- get_cov(play_df, time_pass, receiver_id, "dir")
counter_df[,"receiver_dir_play"] <- get_cov(play_df, time_play, receiver_id, "dir")
counter_df[,"receiver_dir_change"] <- get_cov(play_df, time_play, receiver_id, "dir") - get_cov(play_df, time_pass, receiver_id, "dir")

# Vector from ball to receiver at time of pass
counter_df[,"ball_receiver_x_pass"] <- get_cov(play_df, time_pass, receiver_id, "x") - get_cov(play_df, time_pass, 0, "x")
counter_df[,"ball_receiver_y_pass"] <- get_cov(play_df, time_pass, receiver_id, "y")- get_cov(play_df, time_pass, 0, "x")
counter_df[,"ball_receiver_dist_pass"] <- get_def_dist(play_df, time_pass, receiver_id, 0) # distance from ball to receiver at time of pass
