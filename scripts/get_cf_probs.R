cols_to_sample <- c("ball_receiver_x_play", "ball_receiver_y_play", "ball_receiver_dist_play", 
                     "ball_def_play_x_play", "ball_def_play_y_play", "ball_def_play_dist_play",
                     "separation_play", "def_play_dist_total", "def_play_dist_cum")


traits <- all_training[,cols_to_sample]


get_cf_probs <- function(play_df, time_snap_orig, time_pass_orig, time_play_orig, receiver_id_orig, n_samples = 100, n_bart_samples = 1000){
	
  receiver_list <- 
    play_df %>%
    filter(possession == 1 & time == time_snap_orig & Position %in% c("WR", "TE", "RB", "FB")) %>%
    pull(nflId)
  unik_times <- unique(pull(play_df, time))
  unik_times <- unik_times[(unik_times <= time_play_orig) & (unik_times >= time_snap_orig)]
  receiver_list <- 
    play_df %>%
	filter(possession == 1 & time == time_snap_orig & Position %in% c("WR", "TE", "RB", "FB")) %>%
	pull(nflId)
  if(length(unik_times) > 4){
  	cf_probs <- list()
  	new_unik_times <- unik_times[-(1:4)]
  	
  	time_snap <- time_snap_orig
  	for(time_play in new_unik_times){
  	  print(paste("Starting", time_play))
  	  total_time <- as.numeric(as.POSIXct(time_play) - as.POSIXct(time_snap))
  	  time_index <- which(abs(time_pairs[,"time_total"] - total_time) < 0.15)
  	  if(length(time_index) > 0){
  	  	time_snap_to_pass <- round(mean(time_pairs[time_index, "time_snap_to_pass"]), digits = 1)
        time_pass <-format(as.POSIXct(time_snap) + time_snap_to_pass + 0.001, format = "%Y-%m-%d %H:%M:%OS2")
        tmp_probs <- matrix(nrow = n_bart_samples, ncol = length(receiver_list), dimnames = list(c(), paste0("id.", receiver_list)))
        for(receiver_id in receiver_list){
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
          
          # Need to sample location of vector from ball to receiver in order to figure out where the ball is at time_play in hypothetical pass
          
          # restrict so we have valid defender distance travelled metrics
          index <- which(traits[,"def_play_dist_total"] > get_cov(play_df, time_pass, def_play_id, "cum_dis_game") - get_cov(play_df, time_snap, def_play_id, "cum_dis_game"))
          
          if(length(index) > 0){
            sample_index <- sample(x = index, size = n_samples, replace = TRUE)
            
            # Vector from ball to receiver at time of play
            counter_df[,"ball_receiver_x_play"] <- traits[sample_index, "ball_receiver_x_play"]
            counter_df[,"ball_receiver_y_play"] <- traits[sample_index, "ball_receiver_y_play"]
            counter_df[,"ball_receiver_dist_play"] <- traits[sample_index, "ball_receiver_dist_play"]
            
            # Vector from ball to def_pass at time of pass
            counter_df[,"ball_def_pass_x_pass"] <- get_cov(play_df, time_pass, def_pass_id, "x") - get_cov(play_df, time_pass, 0, "x")
            counter_df[,"ball_def_pass_y_pass"] <- get_cov(play_df, time_pass, def_pass_id, "y") - get_cov(play_df, time_pass, 0, "y")
            counter_df[,"ball_def_pass_dist_pass"] <- get_def_dist(play_df, time_pass, def_pass_id, 0)
            
            # Vector from ball to def_play at time of play
            counter_df[,"ball_def_play_x_play"] <- traits[sample_index, "ball_def_play_x_play"]
            counter_df[,"ball_def_play_y_play"] <- traits[sample_index, "ball_def_play_y_play"]
            counter_df[,"ball_def_play_dist_play"] <- traits[sample_index, "ball_def_play_dist_play"]
            
            # Separation
            counter_df[,"separation_pass"] <- get_def_dist(play_df, time_pass, receiver_id, def_pass_id)
            counter_df[,"separation_play"] <- traits[sample_index, "separation_play"]
            counter_df[,"separation_change"] <- counter_df[,"separation_play"] - counter_df[,"separation_pass"]
            
            # Distance travelled
            counter_df[,"receiver_dist_before"] <- get_cov(play_df, time_pass, receiver_id, "cum_dis_game") - get_cov(play_df, time_snap, receiver_id, "cum_dis_game")
            counter_df[,"receiver_dist_after"] <- get_cov(play_df, time_play, receiver_id, "cum_dis_game") - get_cov(play_df, time_pass, receiver_id, "cum_dis_game")
            counter_df[,"receiver_dist_total"] <- get_cov(play_df, time_play, receiver_id, "cum_dis_game") - get_cov(play_df, time_snap, receiver_id, "cum_dis_game")
            counter_df[,"receiver_dist_cum"] <- get_cov(play_df, time_play, receiver_id, "cum_dis_game")
            
            # We are imputing the distance travelled by the defender
            counter_df[,"def_play_dist_total"] <- traits[sample_index, "def_play_dist_total"]
            counter_df[,"def_play_dist_cum"] <- get_cov(play_df, time_snap, def_play_id, "cum_dis_game")  + traits[sample_index, "def_play_dist_total"]
            
            # Game info stuff
            counter_df[,"time_remaining_half"] <- 
              all_training_tbl %>%
              filter(gameId == game_id & playId == play_id) %>%
              pull(time_remaining_half)
            counter_df[,"down"] <-
              all_training_tbl %>%
              filter(gameId == game_id & playId == play_id) %>%
              pull(down)
            counter_df[,"yardsToGo"] <- 
              all_training_tbl %>%
              filter(gameId == game_id & playId == play_id) %>%
              pull(yardsToGo)
            counter_df[,"1_score_lead"] <- 
              all_training_tbl %>%
              filter(gameId == game_id & playId == play_id) %>%
              pull("1_score_lead")
            counter_df[,"1_score_trail"] <-
              all_training_tbl %>%
              filter(gameId == game_id & playId == play_id) %>%
              pull("1_score_trail")
            counter_df[,"2_score_lead"] <-
              all_training_tbl %>%
              filter(gameId == game_id & playId == play_id) %>%
              pull("2_score_lead")
            counter_df[,"2_score_trail"] <-
              all_training_tbl %>%
              filter(gameId == game_id & playId == play_id) %>%
              pull("2_score_trail")
            counter_df[,"score_diff_sgn"] <-
              all_training_tbl %>%
              filter(gameId == game_id & playId == play_id) %>%
              pull(score_diff_sgn)
              
            phat_1 <- rowMeans(predict.lbart(bart_fit1, newdata = counter_df)$prob.test)
            phat_2 <- rowMeans(predict.lbart(bart_fit2, newdata = counter_df)$prob.test)
            phat <- c(phat_1, phat_2)
          } else{
          	phat <- rep(NA, times = n_bart_samples)
          } # closes if/else checking that there are actual plays where def_play ran as far as the distance hypothesized def_play ran before hypothesized pass
          
          tmp_probs[,paste0("id.", receiver_id)] <- phat 
        } # closes loop over receiver ids
        cf_probs[[time_play]] <- tmp_probs
  	  } else{
  	  	cf_probs[[time_play]] <- NA
  	  }# closes if/else checking that there are plays that are approximately as long as the hypothesized play
  	} # closes loop over hypothesized time_play values
  } else{
  	cf_probs <- NULL
  } # closes if/else checking that there are at least 4 unique time stamps in the play (i.e. it lasted over 0.4 seconds)

	return(cf_probs)
}