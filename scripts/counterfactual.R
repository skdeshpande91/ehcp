# Things needed for counterfactual
library(tidyverse)
library(BART)

load("data/all_training.RData")
load("data/game_id_list.RData")
load("data/bart_fit1.RData")
load("data/bart_fit2.RData")

source("scripts/build_training_df.R")
source("scripts/get_cf_probs.R")
cols_to_sample <- c("ball_receiver_x_play", "ball_receiver_y_play", "ball_receiver_dist_play", 
                    "ball_def_play_x_play", "ball_def_play_y_play", "ball_def_play_dist_play",
                    "separation_play", "def_play_dist_total", "def_play_dist_cum")


traits <- all_training[,cols_to_sample]


for(game_id in unique(all_training[,"gameId"])){
  # Lodad in the data
  load(paste0("data/data_", game_id, ".RData"))
  tracking <- tmp[["tracking"]]
  plays <-tmp[["plays"]]
  gameInfo <- tmp[["gameInfo"]]
  
  
  
  pass_plays <- 
    plays %>%
    filter(!is.na(PassLength) & !is.na(PassResult))
  pass_tracking <-
    tracking %>%
    filter(playId %in% pull(pass_plays, playId))
  
  
  for(play_id in unique(pull(pass_tracking, playId))){
    at_index <- which(all_training[,"gameId"] == game_id & all_training[,"playId"] == play_id)
    play_df <- 
      pass_tracking %>%
      filter(playId == play_id)
    receiver_id_orig <- all_training[at_index, "receiver_id"]
    time_snap_orig <- all_training[at_index, "time_snap"]
    time_pass_orig <- all_training[at_index, "time_pass"]
    time_play_orig <- all_training[at_index, "time_play"]
    
    # Counter-factual probabilities for whole trajectory
    test <- get_cf_probs(play_df, time_snap_orig, time_pass_orig, time_play_orig)
    
    # at what points was prob of completion highest for each receiver?
    
    # can get the means and variances for each. This will help us visualize
    
    # also get the estimated probability from bart for this particular value
    # use at.index
    
  }
  
}

# Just to highlight what we can do

row_max <- apply(counter_factual_probs[[time_play_orig]],MAR = 1, FUN = max)
# posterior prob that original receiver had highest catch prob. at the original time of the pass
mean(counter_factual_probs[[time_play_orig]][,paste0("id.", receiver_id_orig)] == row_max)

hist(counter_factual_probs[[time_play_orig]][,paste0("id.", receiver_id_orig)], breaks = seq(0, 1, length = 50), freq = FALSE, col = rgb(0,0,1,1/3), ylim = c(0, 10))
hist(counter_factual_probs[[time_play_orig]][,1], breaks = seq(0, 1, length = 50), add = TRUE, col = rgb(1,0,0,1/3), freq = FALSE)
