# Function to convert time
convert_time <- function(x, counter){
  chr_x <- as.character(x) # converts the time to character
  new_x <- paste0(chr_x, ".", counter-1) # appends a decimal countined deciseconds (10ths of seconds)
  new_time <- as.POSIXct(new_x, format = "%Y-%m-%d %H:%M:%OS") # converts back to a date object
  return(format(new_time + 0.005, format = "%Y-%m-%d %H:%M:%OS2")) # adding to get the rounding correct and rounding to 2 digits for no real reason.
}


# Function to pre-process the raw tracking data
process_data <- function(game_id, verbose = TRUE){
  tracking_file_prefix <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_"
  tracking_file <- paste0(tracking_file_prefix, game_id, ".csv")
  gameInfo <- raw_games %>% filter(gameId == game_id)
  if(verbose == TRUE) print(paste("Starting to process game_id =", game_id))
  # Subset raw_plays
  # who is the home team and who is the away team
  homeTeam <- pull(gameInfo, homeTeamAbbr)
  awayTeam <- pull(gameInfo, visitorTeamAbbr)
  
  raw_tracking <- 
    read_csv(tracking_file, col_types = cols(), progress = FALSE) %>%
    replace_na(list(nflId = 0)) %>% # Let the ball have an id number = 0
    filter(!is.na(time) & !is.na(x) & !is.na(y)) %>% # drop any row that is missing time or location
    group_by(time, nflId) %>% 
    mutate(time_counter = 1:n()) %>% # this keeps a running count of number of times we encounter a player-timestamp combination
    ungroup() %>% 
    filter(time_counter <= 10) %>% # this is probably superfluous
    mutate(new_time = convert_time(time, time_counter)) # now we have fractional time! 
  if(verbose == TRUE) print("    Finished reading in raw_tracking")
  raw_tracking <- left_join(raw_tracking, select(raw_players, "nflId", "PositionAbbr"), by = c("nflId" = "nflId"))
  raw_tracking <- 
    raw_tracking %>%
    rename(Position = PositionAbbr) %>%
    mutate(team = case_when(
      team == "home" ~ homeTeam,
      team == "away" ~ awayTeam,
      team == "ball" ~ "ball"
    ))
    
    
  plays <- 
    raw_plays %>%
    filter(gameId == game_id)
  
  raw_tracking <- 
    raw_tracking %>%
    left_join(select(plays,playId, possessionTeam), by = c("playId", "playId")) %>%
    mutate(possession = case_when( (possessionTeam == team & team != "ball") ~ 1,
                                  (possessionTeam != team & team != "ball") ~ 0,
                                  (team == "ball") ~ NA_real_))
  if(verbose == TRUE) print("    Added Possession")
  
  # Need to annotate the ball's trajectories
  time_event <- 
    raw_tracking %>%
    filter(nflId != 0) %>%
    select(new_time, event) %>%
    distinct() %>% 
    rename(new_event = event)
  raw_tracking <- left_join(raw_tracking, time_event, by = c("new_time", "new_time"))
  if(verbose == TRUE) print("    Updated event field")
  # Add the ball's position to each row
  ball_position <-
    raw_tracking %>%
    filter(nflId == 0) %>%
    select(new_time, x, y, s, dis, dir) %>%
    rename(ball_x = x,
           ball_y = y,
           ball_s = s,
           ball_dis = dis,
           ball_dir = dir)
  
  raw_tracking <- left_join(raw_tracking, ball_position, by = c("new_time", "new_time"))
  raw_tracking <-
    raw_tracking %>%
    mutate(rel_x = x - ball_x,
           rel_y = y - ball_y,
           #dist_to_ball = ifelse(nflId == 0, 1e6,sqrt(rel_x^2 +rel_y^2))) %>%
           dist_to_ball = sqrt(rel_x^2 + rel_y^2)) %>%
    group_by(new_time, team) %>%
    mutate(closest_to_ball = 1*(dist_to_ball == min(dist_to_ball))) %>%
    ungroup()
  if(verbose == TRUE) print("    Computed distance to ball")
  
  
  
  
  
  tracking <- 
    raw_tracking %>%
    select(gameId, playId, new_time, new_event, nflId, displayName, Position, team, possession,
           x, y, s, dis, dir, ball_x, ball_y, rel_x, rel_y, dist_to_ball, closest_to_ball) %>%
    rename(time = new_time, event = new_event) %>%
    arrange(time)
  return(list(tracking = tracking, plays = plays, gameInfo = gameInfo))
}
