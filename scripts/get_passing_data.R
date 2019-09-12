# Function to pull out covariates along the entire trajectory
# eventually we will have info about the nearest opponent so we can just pull that information 
get_features <- function(play_id, player_id, feature_name, pass_tracking){
  tmp_x <- 
    pass_tracking %>%
    filter(playId == play_id & nflId == player_id) %>%
    pull(feature_name)
  names(tmp_x) <-
    pass_tracking %>%
    filter(playId == play_id & nflId == player_id) %>%
    pull(event)
  # Do we want to truncate to 
  snap_index <- which(names(tmp_x) == "ball_snap")
  tmp_x <- tmp_x[-(1:(snap_index-1))] # drop everything before the ball was snapped
  return(tmp_x)
}

get_closest_opp <- function(df, playID, time_id, playerID){
  # Subset by play
  play_df <- df %>% filter(playId == playID)  
  
  # Subset by time
  time_df <- play_df %>% filter(time == time_id )
  
  # Get team for player of interest
  rel_team <- time_df %>% filter(nflId == playerID) %>% select(team) %>% pull()
  
  #Find closest opponent player 
  closest_opp <- time_df %>% filter(team != rel_team, team != 'ball' ) %>%
    mutate(rel_dist = sqrt(
      ((time_df %>% filter(nflId == playerID) %>% select(x) %>% pull()) - x)^2  + 
        ((time_df %>% filter(nflId == playerID) %>% select(y) %>% pull()) - y)^2)
    ) %>% slice(which.min(rel_dist)) %>%
    mutate(closest_opp_nflId = nflId,
           closest_opp_x = x,
           closest_opp_y = y,
           closest_opp_s = s,
           closest_opp_dis = dis,
           closest_opp_cum_dis = cum_dis,
           closest_opp_dist_to_ball = dist_to_ball,
           closest_opp_distance = rel_dist) %>%
    select(closest_opp_nflId, closest_opp_x, closest_opp_y, closest_opp_s, closest_opp_dis, closest_opp_cum_dis, closest_opp_dist_to_ball, closest_opp_distance)
  # Some plays are weird, fill in with NA
  if(dim(closest_opp)[1] == 0){
    return(rep(NA, 8))
  }
  else{
    return(closest_opp)
  }
}


# Load in the passing plays data


pass_plays <-
  plays %>%
  filter(!is.na(PassLength) & !is.na(PassResult))
pass_tracking <- 
  tracking %>%
  filter(playId %in% pull(pass_plays,playId)) %>%
  group_by(playId, nflId) %>%
  mutate(cum_dis = cumsum(dis)) %>% # cumulative distance travelled by all players on a given play
  ungroup()

# To fit model of catch probability we only need to isolate a handful of time-points: 
# ball-snap, pass_forward, and pass_outcome_caught/pass_outcome_incomplete
play_id <- pull(pass_plays, playId)[1]

pass_features <- function(play_id, pass_tracking){
  
  # Identify target and closest defender
  
  # tbl containing data at time stamps when a recorded event happens
  event_tracking <- 
    pass_tracking %>%
    filter(playId == play_id & event %in% c("ball_snap", "pass_forward", "pass_outcome_caught", "pass_outcome_incomplete",
                                            "pass_outcome_interception", "pass_outcome_touchdown"))
  outcome <- 
    event_tracking %>%
    filter(playId == play_id & grepl("pass_outcome_", event) & nflId == 0) %>%
    pull(event)
    
  target_id <- 
    event_tracking %>% 
    filter(grepl("pass_outcome_", event) & closest_to_ball == 1 & possession == 1) %>%
    pull(nflId)
  # Eventually we only need to look at the right row in the tracking data
  
  # Start pulling features along the entire trajectory
  target_rel_dis <- get_features(play_id,target_id, "dist_to_ball", pass_tracking) # relative distance of target to ball
  defender_rel_dis <- get_features(play_id, target_id, "", pass_tracking) # relative distance of target'snearest defender to ball
  target_speed <- get_features(play_id, target_id, "s", pass_tracking)
  defender_speed <- get_features(play_id, target_id, "", pass_tracking) # speed of target's nearest defender
  ball_speed <- get_features(play_id, 0, "s", pass_tracking)
  
  target_x <- get_features(play_id, target_id, "x", pass_tracking) # x position of target
  target_y <- get_features(play_id, target_id, "y", pass_tracking) # y position of target
  defender_x <- get_features(play_id, target_id, "", pass_tracking) # x position of target's nearest defender
  defender_y <- get_features(play_id, target_id, "", pass_trackin) # y position of target's nearest defender
  
  ball_x <- get_features(play_id, 0, "x", pass_tracking) # x position of ball
  ball_y <- get_features(play_id, 0, "y", pass_trackin) # y position of ball
  
  # cumulative distance travelled
  target_cum_dis <- get_features(play_id, target_id, "cum_dis", pass_tracking)
  defender_cum_dis <- get_features(play_id, target_id, "", pass_tracking)
  ball_cum_dis <- get_features(play_id, 0, "cum_dis", pass_tracking)
  # speed of nearest defender to target
  
  
  
  training_features <- c()
  training_features["ball_s"] <- ball_speed[outcome]
  training_features["target_s"] <- target_speed[outcome]
  training_features["defender_s"] <- defender_speed[outcome]
  training_features["target_dir"] <- target_dir[outcome]
  training_features["defender_s"] <- defender_dir[outcome]
  
  
  
  defender_id <-
    event_tracking %>% 
    filter(grepl("pass_outcome_", event) & closest_to_ball == 1 & possession == 0) %>%
    pull(nflId)
  
  # Pull out the features for the whole trajectory
  # For training we only need what happened at outcome
  
  # Relative distance to the ball
  target_rel_dis <- get_features(play_id,target_id, "dist_to_ball", pass_tracking)
  defender_rel_dis <- get_features(play_id, defender_id, "dist_to_ball", pass_tracking)
  # speed
  target_speed <- get_features(play_id, target_id, "s", pass_tracking)
  defender_speed <- get_features(play_id, defender_id, "s", pass_tracking)
  ball_speed <- get_features(play_id, 0, "s", pass_tracking)
  # cumulative distance travelled
  target_dist <- get_features(play_id, target_id, "cum_dis", pass_tracking)
  defender_dist <- get_features(play_id, defender_id, "cum_dis", pass_tracking)
  ball_dist <- get_features(play_id, 0, "cum_dis", pass_tracking)
  
  # direction
  target_dir <- get_features(play_id, target_id, "dir", pass_tracking)
  defender_dir <- get_features(play_id, defender_id, "dir", pass_tracking)
  ball_dir <- get_features(play_id, 0, "dir", pass_tracking)
  
  training_features <- c()
  training_features["ball_s"] <- ball_speed[outcome]
  training_features["target_s"] <- target_speed[outcome]
  training_features["defender_s"] <- defender_speed[outcome]
  training_features["target_dir"] <- target_dir[outcome]
  training_features["defender_s"] <- defender_dir[outcome]
  # Relative distance to ball
  training_features["target_rel_dis"] <- target_rel_dis[outcome]
  training_features["defender_rel_dis"] <- defender_rel_dis[outcome]
  
  training_features["ball_dist"] <- ball_dist[outcome] - ball_dist["pass_forward"]
  training_features["target_dist_before"] <- target_dist["pass_forward"] - target_dist["ball_snap"]
  training_features["target_dist_total"] <- target_dist[outcome] - target_dist["ball_snap"]
  training_features["target_dist_after"] <- target_dist[outcome] - target_dist["pass_forward"]
  
  
  
  # We can discard everything before the snap
  n_traj <- length(target_rel_dis)
  
  
  # 
  
  
  
  # Assemble the data frame
  new_df <- data.frame(time =)
  
  
  
  train <- c(
    "target_rel_dis" = target_rel_dis[outcome]
  )
  
  
  
  
  # Get the vector of distance travelled by target, defender, and ball
  target_cum_dis <- get_cum_distances(play_id, target_id, pass_tracking)
  defender_cum_dis <- get_cum_distances(play_id, defender_id, pass_tracking)
  ball_cum_dis <- get_cum_distances(play_id, 0, pass_tracking)
  # Get the vector of speeds
  target_speed <- 
    
}



tmp_tracking <-
  pass_tracking %>%
  filter(playId == play_id & event %in% c("ball_snap", "pass_forward", "pass_outcome_caught", "pass_outcome_incomplete", 
                      "pass_outcome_interception", "pass_outcome_touchdown"))

# Find the intended target
target_id <- 
  tmp_tracking %>%
  filter(grepl("pass_outcome_", event) & closest_to_ball == 1 & possession == 1) %>%
  pull(nflId)
defender_id <-
  tmp_tracking %>% 
  filter(grepl("pass_outcome_", event) & closest_to_ball == 1 & possession == 0) %>%
  pull(nflId)

# Get the cumulative distances travelled
# annotate with snap, pass_forward, and pass_outcome_*
get_features <- function(play_id, player_id, feature_name, pass_tracking){
  tmp_dis <- 
    pass_tracking %>%
    filter(playId == play_id & nflId == player_id) %>%
    pull(feature_name)
  names(tmp_dis) <-
    pass_tracking %>%
    filter(playId == play_id & nflId == player_id) %>%
    pull(event)
  return(tmp_dis)
}


# How far has receiver run from snap of ball
target_dis_0 <- 
  tmp_tracking %>%
  filter(nflId == target_id & event == "ball_snap") %>%
  pull(cum_dis)
defender_dis_0 <- 
  tmp_tracking %>%
  filter(nflId == defender_id & event == "ball_snap") %>%
  pull(cum_dis)

target_dis <-
  tmp_tracking %>%
  



tmp_tracking %>% filter(grepl("pass_outcome_", event) & closest_to_ball == 1)
  




# we want to get things like 
  
  
  tracking %>%
  filter()

# filter only to the nearest defender


# need to take cumulative sum of dis column, which tracks distance travelled from previous time spot
# just pull dis for the relevant players
