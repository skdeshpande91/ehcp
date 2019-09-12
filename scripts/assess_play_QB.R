assess_plays_QB <- function(game_id){
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
  
  tmp_results <- as.data.frame(
    pass_plays %>%
      select(playId, possessionTeam) %>%
      distinct()
  )
  n_plays <- nrow(tmp_results)
  tmp_results[,"n_receivers"] <- rep(NA, times = n_plays)
  tmp_results[,"receiver_id"] <- rep(NA, times = n_plays)
  tmp_results[,"QB_name"] <- rep(NA, times = n_plays)
  tmp_results[,"Max_mean"] <- rep(NA, times = n_plays) # maximum EHCP
  tmp_results[,"Min_mean"] <- rep(NA, times = n_plays) # minimum posterior mean EHCP
  tmp_results[,"Targeted_mean"] <- rep(NA, times = n_plays) # mean EHCP for targetted receiver
  tmp_results[,"Targeted_best"] <- rep(NA, times = n_plays) # indicator that targetted had highest mean EHCP
  tmp_results[,"Targeted_worst"] <- rep(NA, times = n_plays) # indicator that targetted had lowest mean EHCP
  tmp_results[,"Targeted_best_prob"] <- rep(NA, times = n_plays) # posterior prob that receiver_orig had highest EHCP
  tmp_results[,"Targeted_worst_prob"] <- rep(NA, times = n_plays) # posterior prob that receiver_orig had lowest EHCP
  tmp_results[,"Targeted_avg_rank"] <- rep(NA, times = n_plays) # average rank
  
  
  for(play_ix in 1:n_plays){
    play_id <- tmp_results[play_ix, "playId"]
    
    if(play_ix %% 20 == 0) print(paste("Starting play", play_ix, "of", n_plays, "at", Sys.time()))
    at_index <- which(all_training[,"gameId"] == game_id & all_training[,"playId"] == play_id)
    if(length(at_index) > 0){
      play_df <- 
        pass_tracking %>%
        filter(playId == play_id)
      #print("got play_df")
      receiver_id_orig <- all_training[at_index, "receiver_id"]
      QB_name <- ifelse("punt_fake" %in% unique(play_df$event), "No QB",
                        play_df %>% filter(Position == 'QB') %>% slice(1) %>% unique() %>% .$displayName)
      time_snap_orig <- all_training[at_index, "time_snap"]
      time_pass_orig <- all_training[at_index, "time_pass"]
      time_play_orig <- all_training[at_index, "time_play"]
      #print("starting to compute ehcp")
      
      # [SKD]: We could spend time de-bugging all of the games to see what's going on.
      # But it may be simpler to just do
      # tryCatch({tmp_cf <- ehcp_pass(play_id, play_df, time_snap_orig, time_pass_orig, time_play_orig, receiver_id_orig)},
      #            error = function(e){NULL}, warning = function(e){NULL}))
      # if(!is.null(tmp_cf)){
      #  ## write all of the stuff to tmp_results here
      #}
      tmp_cf <- ehcp_pass(play_id,play_df, time_snap_orig, time_pass_orig, time_play_orig, receiver_id_orig)
      n_receivers <- ncol(tmp_cf)
      post_means <- colMeans(tmp_cf)
      post_ranks <- t(n_receivers - apply(tmp_cf, MAR = 1, FUN = rank) + 1)
      tmp_results[play_ix, "n_receivers"] <- n_receivers
      tmp_results[play_ix, "receiver_id"] <- receiver_id_orig
      tmp_results[play_ix, "QB_name"] <- QB_name
      tmp_results[play_ix,"Max_mean"] <- max(post_means)
      tmp_results[play_ix,"Min_mean"] <- min(post_means)
      tmp_results[play_ix,"Targeted_mean"] <- post_means[paste0("id.", receiver_id_orig)]
      tmp_results[play_ix, "Targeted_best"] <- 1*(max(post_means) == post_means[paste0("id.", receiver_id_orig)])
      tmp_results[play_ix, "Targeted_worst"] <- 1*(min(post_means) == post_means[paste0("id.", receiver_id_orig)])
      tmp_results[play_ix,"Targeted_best_prob"] <-  mean(post_ranks[,paste0("id.", receiver_id_orig)] == 1)
      tmp_results[play_ix, "Targeted_worst_prob"] <- mean(post_ranks[,paste0("id.", receiver_id_orig)] == n_receivers)
      tmp_results[play_ix, "Targeted_avg_rank"] <- mean(post_ranks[,paste0("id.", receiver_id_orig)])
    }

  }
  na.index <- which(is.na(tmp_results[,"n_receivers"]))
  if(length(na.index) == n_plays){
    return(NULL)
  } else{
    return(as_tibble(tmp_results[-na.index,]))
  }
}
