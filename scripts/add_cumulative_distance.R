# Add cumulative distances
library(tidyverse)
load("data/game_id_list.RData")
for(game_id in game_id_list){
  load(paste0("data/data_", game_id, ".RData"))
  tracking <- 
    tmp[["tracking"]] %>%
    group_by(playId, nflId) %>%
    mutate(cum_dis = cumsum(dis)) %>% # cumulative distance travelled by all players on a given play
    ungroup()
  tmp[["tracking"]] <- tracking
  save(tmp, file = paste0("data/data_", game_id, ".RData"))
}

# Add cumulative distances over the course of the game
for(game_id in game_id_list){
  load(paste0("data/data_", game_id, ".RData"))
  tracking <- 
    tmp[["tracking"]] %>%
    arrange(time) %>%
    rename(cum_dis_play = cum_dis) %>%
    group_by(nflId) %>%
    mutate(cum_dis_game = cumsum(dis)) %>%
    ungroup()
  tmp[["tracking"]] <- tracking
  save(tmp, file = paste0("data/data_", game_id, ".RData"))
}