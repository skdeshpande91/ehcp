# Load in the data to make manuscript figures
load("manuscript_data.RData")

# play 1: kupp TD
# play 2: tolzien interception returned for TD

# histogram of the completion prob posteriors for the two plays

png("latex/completition_posterior_hist.png", width = 4, height = 4, units = "in", res = 300)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0), cex.main = 0.7, cex.lab = 0.7, cex.axis = 0.7)
hist(postSamples_1, breaks = seq(0,1, length = 50), freq = FALSE, ylim = c(0, 5), xlab = "Completition Probability", col = rgb(0,0,1,1/3), main = "Completion Probability Posterior")
hist(postSamples_2, breaks = seq(0, 1, length = 50), freq = FALSE, add = TRUE, col = rgb(1,0,0,1/3))
legend(x = 0.6, y = 4.5, col = c(rgb(0,0,1,1/3), rgb(1,0,0,1/3)), pch = 15, legend = c("Kupp TD", "Tolzien INT"), cex = 0.8, bty = "n")

dev.off()


png("latex/ehcp_posterior_hist.png", width = 4, height = 4, units = "in", res = 300)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0), cex.main = 0.7, cex.lab = 0.7, cex.axis = 0.7)
hist(play_1[[time_play_orig_1]][,paste0("id.", receiver_id_orig_1)], breaks = seq(0, 1, length = 50), freq = FALSE, col = rgb(0,0,1,1/3), main = "", xlab = "EHCP Posterior")
hist(play_2[[time_play_orig_2]][,paste0("id.", receiver_id_orig_2)], breaks = seq(0, 1, length = 50), freq = FALSE, col = rgb(1,0,0,1/3), add = TRUE)
legend(x = 0.2, y = 7, col = c(rgb(0,0,1,1/3), rgb(1,0,0,1/3)), pch = 15, legend = c("Kupp TD", "Tolzien INT"), cex = 0.7, bty = "n")
dev.off()

play_1_means[time_play_orig_1, paste0("id.", receiver_id_orig_1)]
play_2_means[time_play_orig_2, paste0("id.", receiver_id_orig_2)]

# Look at Kupp's route specifically
kupp_id <- 2557898
kupp_time_1 <- "2017-09-10 21:11:30.50"
kupp_time_2 <- "2017-09-10 21:11:28.40"
kupp_time_3 <- "2017-09-10 21:11:27.20"

play_1_means[c(kupp_time_1, kupp_time_2, kupp_time_3), paste0("id.", kupp_id)]

quantile(play_1[[kupp_time_1]][,paste0("id.", kupp_id)], probs = c(0.025, 0.975))
quantile(play_1[[kupp_time_2]][,paste0("id.", kupp_id)], probs = c(0.025, 0.975))
quantile(play_1[[kupp_time_3]][,paste0("id.", kupp_id)], probs = c(0.025, 0.975))

# Who had highest posterior mean EHCP
colnames(play_1_means)[which(play_1_means == max(play_1_means), arr.ind = TRUE)[2]] # id.2543457
# This is Sammy Watkins
watkins_id <- 2543457
watkins_time_1 <- "2017-09-10 21:11:27.20"
play_1_means[watkins_time_1, paste0("id.", watkins_id)]
quantile(play_1[[watkins_time_1]][,paste0("id.", watkins_id)], probs = c(0.025, 0.975))

# time_snap was 21:11:25.7.
# some kupp_time_1 is 4.8 secs, kupp_time_2 is 2.6 seconds
# watkins_time_1 is 1.5 seconds
# How often did receiver_id_1 have the highest EHCP at time_play_orig1


############
# Annotations for play2

# original receiver was TY Hilton
hilton_id <- 2532865

hilton_time_1 <- "2017-09-10 20:14:46.70"
hilton_time_2 <- "2017-09-10 20:14:44.80"

colnames(play_2_means)[which(play_2_means == max(play_2_means), arr.ind = TRUE)[2]] # 2543614 Donte Moncrief
moncrief_id <- 2543614
moncrief_time_1 <- "2017-09-10 20:14:44.80"

play_2_means[c(hilton_time_1, hilton_time_2), paste0("id.", hilton_id)]

quantile(play_2[[hilton_time_1]][,paste0("id.", hilton_id)], probs = c(0.025, 0.975))
quantile(play_2[[hilton_time_2]][,paste0("id.", hilton_id)], probs = c(0.025, 0.975))

play_2_means[moncrief_time_1, paste0("id.", moncrief_id)]
quantile(play_2[[moncrief_time_1]][, paste0("id.", moncrief_id)], probs = c(0.025, 0.975))

# play 2: snap was 20:14:42.40. So hilton_time1 = 4.3, hilton_time_2 = 2.4, moncrief_time_1 = 2.4

row_max_1 <- apply(play_1[[time_play_orig_1]], MAR = 1, FUN = max)
mean(play_1[[time_play_orig_1]][,paste0("id.", receiver_id_orig_1)] == row_max_1)

row_max_2 <- apply(play_2[[time_play_orig_2]], MAR = 1, FUN = max)
mean(play_2[[time_play_orig_2]][,paste0("id.", receiver_id_orig_2)] == row_max_2)

