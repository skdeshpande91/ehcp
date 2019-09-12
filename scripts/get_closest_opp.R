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
           closest_opp_dir = dir,
           closest_opp_dis = dis,
           closest_opp_cum_dis = cum_dis,
           closest_opp_dist_to_ball = dist_to_ball,
           closest_opp_distance = rel_dist) %>%
    select(closest_opp_nflId, closest_opp_x, closest_opp_y, closest_opp_s, closest_opp_dir,closest_opp_dis, closest_opp_cum_dis, closest_opp_dist_to_ball, closest_opp_distance)
  # Some plays are weird, fill in with NA
  if(dim(closest_opp)[1] == 0){
    return(rep(NA, 8))
  }
  else{
    return(closest_opp)
  }
}
