
library(gganimate)
library(cowplot)
library(tidyverse)


# load bartfit1, bartfit2 and all_training

prob_train1 <- bart_fit1$prob.train
prob_train2 <- bart_fit2$prob.train
prob_train <- rbind(prob_train1, prob_train2)

prob_means <- apply(prob_train, mean, MAR=2)

trained_plus_phat <- cbind(all_training, prob_means)
head(trained_plus_phat)

low_p_catch <- trained_plus_phat %>% filter(prob_means < .05 ) %>%
                filter(outcome == "pass_outcome_caught")
dim(low_p_catch)

high_p_inc <- trained_plus_phat %>% filter(prob_means > .95 ) %>%
              filter(outcome == "pass_outcome_incomplete")
dim(high_p_inc)

sample_catch <- low_p_catch[2,]
sample_inc <- high_p_inc[4,]

sample_catch_playID <- sample_catch$playId[1]
sample_inc_playID <- sample_inc$playId[1]

sample_catch_gameID <- sample_catch$gameId[1]
sample_inc_gameID <- sample_inc$gameId[1]
sample_catch_gameID
sample_inc_gameID

load("/Users/katherineevans/Dropbox/NFL_big_data_bowl/data/data_2017091009.RData")
play_info <- data.frame(tmp$plays)

sample_catch_play <- play_info %>% filter(playId == sample_catch_playID)
sample_inc_play <- play_info %>% filter(playId == sample_inc_playID)

sample_catch
sample_catch_play

sample_inc
sample_inc_play
## Would need play_means to make the graphs. 