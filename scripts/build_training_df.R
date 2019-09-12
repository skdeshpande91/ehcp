#library(tidyverse)

# Input: data frame of a single play, ID of a receiver (could be any player), timestamp
# Output: ID of opponent closest to receiver at that time of play
get_closest_def <- function(play_df, receiver_id, time_stamp){
  rec_x <- 
    play_df %>%
    filter(time == time_stamp & nflId == receiver_id) %>%
    pull(x)
  rec_y <-
    play_df %>%
    filter(time == time_stamp & nflId == receiver_id) %>%
    pull(y)
  opp_dist <-
    play_df %>%
    filter(time == time_stamp & possession == 0) %>%
    mutate(dist_rec = sqrt( (x - rec_x)^2 + (y - rec_y)^2)) %>%
    pull(dist_rec)
  names(opp_dist) <- 
    play_df %>%
    filter(time == time_stamp & possession == 0) %>%
    pull(nflId)
  return(as.numeric(names(opp_dist)[which.min(opp_dist)]))
}


# Input: df of a single play, time stamp, player ID and a covariate of interst
# Output: that covariate or the first appearance of that covariate
get_cov <- function(play_df, time_stamp, player_id, var){
  results <-
    play_df %>%
    filter(nflId == player_id & time == time_stamp) %>%
    pull(var)
  if(length(results) > 1) results <- results[1]     # Do we expect the length to ever be >1 given player_id and time_stamp should uniquely identify a single row?
  return(results)
}

# Get distance from player1 to player2 at time_stamp
# Input: df of a single play, time stamp, two player IDs
# Output: The distance between those two players.
get_def_dist <- function(play_df, time_stamp, player1_id, player2_id){
  x_1 <- 
    play_df %>%
    filter(time == time_stamp & nflId == player1_id) %>%
    pull(x)
  if(length(x_1) > 1) x_1 <- x_1[1]  # should this be if(length(x_1) > 1) x_1 <- x_1[1] ? 
  x_2 <- 
    play_df %>%
    filter(time == time_stamp & nflId == player2_id) %>%
    pull(x)
  if(length(x_2) > 1) x_2 <- x_2[1]
  y_1 <- 
    play_df %>%
    filter(time == time_stamp & nflId == player1_id) %>%
    pull(y)
  if(length(y_1) > 1) y_1 <- y_1[1]   # should this be if(length(y_1) > 1) y_1 <- y_1[1] ? 
  y_2 <- 
    play_df %>%
    filter(time == time_stamp & nflId == player2_id) %>%
    pull(y)
  if(length(y_2) > 1) y_2 <- y_2[1]

  return(sqrt( (x_1 - x_2)^2 + (y_1 - y_2)^2))
}

# Input: play_id, play level info
# Output: time remaining in the half for the play given
get_time_half <- function(play_id, play_info){
  
  game_clock <- as.character(
    play_info %>%
      filter(playId == play_id) %>%
      pull(GameClock))
  quarter <- 
    play_info %>%
    filter(playId == play_id) %>%
    pull(quarter)
  minutes <- as.numeric(substr(game_clock, 1, 2))
  seconds <- as.numeric(substr(game_clock, 4, 5))
  if(quarter %in%  c(1, 3)){
    time_remaining <- 15*60 + 60 * minutes + seconds
  }
  if(quarter %in% c(2, 4)){
    time_remaining <- 60 * minutes + seconds
  }
  return(time_remaining)
}

# Input: play id, info on that play, info on that game
# Output: Score difference (offensive team's score - defensive teams score) and score diff category (9,1-8,0)
get_score <- function(play_id, play_info, game_info){
  home_team <-pull(game_info, homeTeamAbbr)
  visitor_team <- pull(game_info, visitorTeamAbbr)
  offense <-
    play_info %>%
    filter(playId == play_id) %>%
    pull(possessionTeam)
  home_score <- 
    play_info %>%
    filter(playId == play_id) %>%
    pull(HomeScoreBeforePlay)
  away_score <-
    play_info %>%
    filter(playId == play_id) %>%
    pull(VisitorScoreBeforePlay)
  
  score_diff <- ifelse(home_team == offense, home_score - away_score, away_score - home_score)
  
  if(score_diff == 0) score_diff_cat <- "tie"
  if(score_diff < -8) score_diff_cat <- "2_score_trail"
  if(score_diff >= -8 & score_diff <= -1) score_diff_cat <- "1_score_trail"
  if(score_diff >= 1 & score_diff <= 8) score_diff_cat <- "1_score_lead"
  if(score_diff > 8) score_diff_cat <- "2_score_lead"
  return(list(score_diff = score_diff, score_diff_cat = score_diff_cat))
}

# Input: data on all passing plays, all play info, all game info
# Output: the training data frame
build_training_df <- function(pass_tracking, play_info, game_info){
  
  play_id_list <- unique(pull(pass_tracking, playId))
  game_id <- pull(game_info, gameId)
  training_df <- data.frame("gameId" = rep(game_id, times = length(play_id_list)),"playId" = play_id_list)
  training_df[,c("Y", "outcome", "time_snap", "time_pass", "time_play", "time_snap_to_pass", "time_pass_to_play", "time_total")] <- NA
  training_df[,c("receiver_id", "def_play_id", "def_pass_id")] <- NA # def_pass_id is defender nearest receiver at the time of the pass
  
  training_df[,c("receiver_s_pass", "receiver_s_play", "receiver_s_change")] <- NA # recevier speed at time of pass, play, and change in speed
  training_df[,c("receiver_dir_pass", "receiver_dir_play", "receiver_dir_change")] <- NA # receiver direction at time of pass, play, and change
  training_df[,c("ball_receiver_x_pass", "ball_receiver_y_pass", "ball_receiver_dist_pass")] <- NA # vector from ball to receiver at time of pass
  training_df[,c("ball_receiver_x_play", "ball_receiver_y_play", "ball_receiver_dist_play")] <- NA # vector from ball to receiver at time of play
  
  training_df[,c("ball_def_pass_x_pass", "ball_def_pass_y_pass", "ball_def_pass_dist_pass")] <- NA
  training_df[,c("ball_def_play_x_play", "ball_def_play_y_play", "ball_def_play_dist_play")] <- NA
  
  training_df[,c("separation_pass", "separation_play", "separation_change")] <- NA # distance from receiver to nearest defender at time of pass, play, and change
  
  # Distance travelled by receiver and nearest defender (at time of play)
  training_df[,c("receiver_dist_before", "receiver_dist_after", "receiver_dist_total", "receiver_dist_cum")] <- NA # distance travelled before pass, between pass & play, cumulative during game of receiver
  # Distance travelled by nearest defender at time of play
  training_df[,c("def_play_dist_total", "def_play_dist_cum")] <- NA
  training_df[,c("time_remaining_half", "down", "yardsToGo", "score_diff")] <- NA
  training_df[,c("1_score_lead", "1_score_trail", "2_score_lead", "2_score_trail", "tie", "score_diff_cat")] <- 0
  
  
  
  for(play_ix in 1:length(play_id_list)){
    play_id <- play_id_list[play_ix]
    play_df <- 
      pass_tracking %>%
      filter(playId == play_id)
    # Get the actual outcome
    outcome <- 
      play_df %>%
      filter(playId == play_id & grepl("pass_outcome_", event) & nflId == 0) %>%
      pull(event)
    if(length(outcome) > 1) outcome <- outcome[1]
    if(length(outcome) == 1){
      # Get the time of the snap, pass, and play
      time_play <- 
        play_df %>%
        filter(nflId == 0 & event == outcome) %>%
        pull(time)
      if(length(time_play) > 1) time_play <- time_play[1]
      time_pass <-
        play_df %>% 
        filter(nflId == 0 & event == "pass_forward") %>%
        pull(time)
      if(length(time_pass) > 1) time_pass <- time_pass[1]
      time_snap <- 
        play_df %>% 
        filter(nflId == 0 & event == "ball_snap") %>%
        pull(time)
      if(length(time_snap) > 1) time_snap <- time_snap[1]
      
      if(length(time_snap) == 1 & length(time_pass) == 1 & length(time_play) == 1){
        # Get the receiver's id ... for testing we don't need to re-compute the receiver's id
        receiver_id <- 
          play_df %>%
          filter(possession == 1 & time == time_play & closest_to_ball == 1) %>%
          pull(nflId)
        if(length(receiver_id) > 1) receiver_id <- receiver_id[1]
        
        # Get the defender closest to receiver at the time of the play
        # Write a function that takes in receiver_id and a time_stamp
        def_play_id <- get_closest_def(play_df, receiver_id, time_play) # id of defender nearest to receiver at time of the play
        #def_ball_id <- get_closest_def(play_df, 0, time_play) # id of the defender nearest to the ball at the time of the play
        def_pass_id <- get_closest_def(play_df, receiver_id, time_pass) # id of the defender nearest to the receiver at the time of the pass
        
        training_df[play_ix,"outcome"] <- outcome
        training_df[play_ix, "Y"] <- ifelse(outcome %in% c("pass_outcome_caught", "pass_outcome_touchdown"), 1, 0)
        training_df[play_ix, "time_snap"] <- time_snap
        training_df[play_ix, "time_pass"] <- time_pass
        training_df[play_ix, "time_play"] <- time_play
        training_df[play_ix, "time_snap_to_pass"] <- as.numeric(as.POSIXct(time_pass) - as.POSIXct(time_snap))
        training_df[play_ix, "time_pass_to_play"] <- as.numeric(as.POSIXct(time_play) - as.POSIXct(time_pass))
        training_df[play_ix, "time_total"] <- as.numeric(as.POSIXct(time_play) - as.POSIXct(time_snap))
        
        training_df[play_ix, "receiver_id"] <- receiver_id
        training_df[play_ix, "def_play_id"] <- def_play_id
        training_df[play_ix, "def_pass_id"] <- def_pass_id
        
        # Receiver speed
        training_df[play_ix, "receiver_s_pass"] <- get_cov(play_df, time_pass, receiver_id, "s")
        training_df[play_ix, "receiver_s_play"] <- get_cov(play_df, time_play, receiver_id, "s")
        training_df[play_ix, "receiver_s_change"] <- get_cov(play_df, time_play, receiver_id, "s") - get_cov(play_df, time_pass, receiver_id, "s")
        
        # Receiver direction
        training_df[play_ix, "receiver_dir_pass"] <- get_cov(play_df, time_pass, receiver_id, "dir")
        training_df[play_ix, "receiver_dir_play"] <- get_cov(play_df, time_play, receiver_id, "dir")
        training_df[play_ix, "receiver_dir_change"] <- get_cov(play_df, time_play, receiver_id, "dir") - get_cov(play_df, time_pass, receiver_id, "dir")
        
        # Vector from ball to receiver at time of pass
        training_df[play_ix, "ball_receiver_x_pass"] <- get_cov(play_df, time_pass, receiver_id, "x") - get_cov(play_df, time_pass, 0, "x")
        training_df[play_ix, "ball_receiver_y_pass"] <- get_cov(play_df, time_pass, receiver_id, "y") - get_cov(play_df, time_pass, 0, "y")
        training_df[play_ix, "ball_receiver_dist_pass"] <- get_def_dist(play_df, time_pass, receiver_id, 0)
        
        # Vector from ball to def_pass to ball at time of pass
        training_df[play_ix, "ball_def_pass_x_pass"] <- get_cov(play_df, time_pass, def_pass_id, "x") - get_cov(play_df, time_pass, 0, "x")
        training_df[play_ix, "ball_def_pass_y_pass"] <- get_cov(play_df, time_pass, def_pass_id, "y") - get_cov(play_df, time_pass, 0,"y")
        training_df[play_ix, "ball_def_pass_dist_pass"] <- get_def_dist(play_df, time_pass, receiver_id, def_pass_id)
        
        # Vector from ball to receiver at time of play
        training_df[play_ix, "ball_receiver_x_play"] <- get_cov(play_df, time_play, receiver_id, "x") - get_cov(play_df, time_play, 0, "x")
        training_df[play_ix, "ball_receiver_y_play"] <- get_cov(play_df, time_play, receiver_id, "y") - get_cov(play_df, time_play, 0, "y")
        training_df[play_ix, "ball_receiver_dist_play"] <- get_def_dist(play_df, time_play, receiver_id, 0)
        
        # Vector from ball to def_play at time of play
        training_df[play_ix, "ball_def_play_x_play"] <- get_cov(play_df, time_play, def_play_id, "x") - get_cov(play_df, time_play, 0, "x")
        training_df[play_ix, "ball_def_play_y_play"] <- get_cov(play_df, time_play, def_play_id, "y") - get_cov(play_df, time_play, 0, "y")
        training_df[play_ix, "ball_def_play_dist_play"] <- get_def_dist(play_df, time_play, def_play_id, 0)
        
        
        # Separation
        training_df[play_ix, "separation_pass"] <- get_def_dist(play_df, time_pass, receiver_id, def_pass_id)
        training_df[play_ix, "separation_play"] <- get_def_dist(play_df, time_play, receiver_id, def_play_id)
        training_df[play_ix, "separation_change"] <- get_def_dist(play_df, time_play, receiver_id, def_play_id) - get_def_dist(play_df, time_pass, receiver_id, def_pass_id)
        
        
        
        # How far receiver travelled (this is a proxy for fatigue)
        training_df[play_ix, "receiver_dist_before"] <- get_cov(play_df, time_pass, receiver_id, "cum_dis_game") - get_cov(play_df, time_snap, receiver_id, "cum_dis_game")
        training_df[play_ix, "receiver_dist_after"] <- get_cov(play_df, time_play, receiver_id, "cum_dis_game") - get_cov(play_df, time_pass, receiver_id, "cum_dis_game")
        training_df[play_ix, "receiver_dist_total"] <- get_cov(play_df, time_play, receiver_id, "cum_dis_game") - get_cov(play_df, time_snap, receiver_id, "cum_dis_game")
        training_df[play_ix, "receiver_dist_cum"] <- get_cov(play_df, time_play, receiver_id, "cum_dis_game")
        
        training_df[play_ix, "def_play_dist_total"] <- get_cov(play_df, time_play, def_play_id, "cum_dis_game") - get_cov(play_df, time_snap, def_play_id, "cum_dis_game")
        training_df[play_ix, "def_play_dist_cum"] <- get_cov(play_df, time_play, def_play_id, "cum_dis_game")

        training_df[play_ix, "time_remaining_half"] <- get_time_half(play_id, plays)
        training_df[play_ix, "down"] <-
          play_info %>%
          filter(playId == play_id) %>%
          pull(down)
        
        training_df[play_ix, "yardsToGo"] <- 
          play_info %>%
          filter(playId == play_id) %>%
          pull(yardsToGo)
        
        tmp_score <- get_score(play_id, plays, gameInfo)
        score_diff_sgn <- 0
        if(tmp_score$score_diff > 0) score_diff_sgn <- 1
        if(tmp_score$score_diff < 0) score_diff_sgn <- -1
        training_df[play_ix, "score_diff_sgn"] <- score_diff_sgn
        training_df[play_ix, tmp_score$score_diff_cat] <- 1
        training_df[play_ix, "score_diff_cat"] <- tmp_score$score_diff_cat

      } # closes if(length(time_snap) == 1 & length(time_pass) == 1 & length(time_play) == 1)
    } # closes if(length(outcome) == 1)
  } # closes loop over the plays
  drop_index <- which(is.na(training_df$outcome))
  training_df <- training_df[-drop_index,]
  return(training_df)
}
