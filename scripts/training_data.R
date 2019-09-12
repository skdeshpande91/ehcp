library(tidyverse)
load("data/game_id_list.RData")
source("scripts/build_training_df.R")
skip_list <- c()
for(game_ix in 1:length(game_id_list)){
  print(paste("Starting game", game_ix, "of", length(game_id_list), "at", Sys.time()))
  game_id <- game_id_list[game_ix]
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
  
  tmp_training <- tryCatch({build_training_df(pass_tracking, pass_plays, gameInfo)},
                     error = function(e){NULL})
  if(is.null(tmp_training)){
    game_skip_list <- c(skip_list, game_id)
  } else{
    #tmp_training <- cbind("gameId" = rep(game_id,nrow(tmp_df)), tmp_df)
    #tmp_training[,"time_snap_to_pass"] <- as.numeric(as.POSIXct(tmp_training[,"time_pass"]) - as.POSIXct(tmp_training[,"time_snap"]))
    #tmp_training[,"time_pass_to_play"] <- as.numeric(as.POSIXct(tmp_training[,"time_play"]) - as.POSIXct(tmp_training[,"time_pass"]))
    #tmp_training[,"Y"] <- ifelse(tmp_training[,"outcome"] %in% c("pass_outcome_caught", "pass_outcome_touchdown"), 1, 0)
  
    assign(paste0("training_", game_id), tmp_training)
    if(exists("all_training")){
      all_training <- rbind(all_training, tmp_training)
    } else{
      all_training <- tmp_training
    }
    save(list = paste0("training_", game_id), file = paste0("data/training_", game_id, ".RData"))
  }
}
all_training_tbl <- as_tibble(all_training)
time_pairs <- all_training[,c("time_snap_to_pass", "time_pass_to_play", "time_total")]
ball_rec_pass <- all_training[,c("ball_receiver_dist_pass", "ball_receiver_x_play", "ball_receiver_y_play", "ball_receiver_dist_play")]
#ball_rec_pass <- all_training[,c("ball_receiver_x_pass", "ball_receiver_y_pass", "ball_receiver_dist_pass")]

save(all_training, all_training_tbl, time_pairs, ball_rec_pass, file = "data/all_training.RData")
