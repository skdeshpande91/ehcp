library(BART)
load("data/bart_fit1.RData")
load("data/bart_fit2.RData")

varcount <- rbind(bart_fit1$varcount, bart_fit2$varcount)
varprob <- rbind(bart_fit1$varprob, bart_fit2$varprob)

varcount_mean <- colMeans(varcount)
varcount_sd <- apply(varcount, FUN = sd, MARGIN = 2)



sort(colMeans(varcount), decreasing = TRUE)[1:10]
sort(colMeans(varprob), decreasing = TRUE)[1:10]


