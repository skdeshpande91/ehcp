library(BART)
library(coda)
load("data/all_training.RData")
#all_training[,"score_diff_sgn"] <- 0
#all_training[all_training[,"score_diff"] < 0, "score_diff_sgn"] <- -1
#all_training[all_training[,"score_diff"] > 0, "score_diff_sgn"] <- 1
#all_training[,"score_diff_cat"] <- as.factor(all_training[,"score_diff_cat"])
#all_training[,"separation_change"] <- all_training[,"separation-change"]

#drop_cols <- c("gameId", "playId", "Y", "outcome", "time_snap", "time_pass", "time_play", "score_diff", "separation-change")

drop_cols <- c("gameId", "playId", "Y", "outcome", "time_snap", "time_pass", "time_play", 
               "receiver_id", "def_pass_id", "def_play_id", "tie", "score_diff_cat", "score_diff")

y <- all_training[,"Y"]
x <- all_training[,!colnames(all_training) %in% drop_cols]


print(paste("Starting bart_fit1 at", Sys.time()))
bart_fit1 <- lbart(x.train = x, y.train = y, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)
save(bart_fit1, file = "data/bart_fit1.RData")
print(paste("Finished bart_fit1 at", Sys.time()))
print(paste("Starting bart_fit2 at", Sys.time()))
bart_fit2 <- lbart(x.train = x, y.train = y, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)
save(bart_fit2, file = "data/bart_fit2.RData")
print(paste("Finished bart_fit2 at", Sys.time()))

rhat <- gelman.diag(mcmc.list(mcmc(bart_fit1$prob.train), mcmc(bart_fit2$prob.train)), multivariate = FALSE)
ess <- effectiveSize(mcmc.list(mcmc(bart_fit1$prob.train), mcmc(bart_fit2$prob.train)))
# assess the convergence using the coda package

save(rhat, ess, file = "data/bart_fit_diags.RData")
