# Read in all of the data and process it to get the tracking information
library(tidyverse)
source("scripts/process_data_function.R")

game_file <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
raw_games <- read_csv(game_file, col_types = cols())
save(raw_games, file = "data/raw_games.RData")


# Get the players file
players_files <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv"
raw_players <- read_csv(players_files, col_types = cols())
raw_players <- add_row(raw_players, nflId = 0, PositionAbbr = "ball") # include the ball as a player
save(raw_players, file = "data/raw_players.RData")

# Get the plays file
plays_file <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
raw_plays <- read_csv(plays_file, col_types = cols())
save(raw_plays, file = "data/raw_plays.RData")

game_id_list <- pull(raw_games, gameId)

for(game_ix in 1:length(game_id_list)){
  game_id <- game_id_list[game_ix]
  print(paste("Processing game", game_ix, "of", length(game_id_list), "at", Sys.time()))
  tmp <- process_data(game_id_list[game_ix], verbose = FALSE)
  save(tmp, file = paste0("data/data_", game_id, ".RData"))
}
save(game_id_list, file = "data/game_id_list.RData")
