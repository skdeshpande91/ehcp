### Creates a DF of all passing plays and their posterior means of catch probability. 
# columns of prob_train correspond to rows of all_training.Rdata

library(gganimate)
library(cowplot)
library(tidyverse)


# load bartfit1, bartfit2 and all_training

prob_train1 <- bart_fit1$prob.train
prob_train2 <- bart_fit2$prob.train
prob_train <- rbind(prob_train1, prob_train2)

prob_means <- apply(prob_train, mean, MAR=2)

trained_plus_phat <- cbind(all_training, prob_means)
head(trained_plus_phat)

high_chance <- trained_plus_phat %>% filter(prob_means > .90)
levels(as.factor(high_chance$outcome))
high_chance_inc <- high_chance %>% filter(outcome == "pass_outcome_incomplete")
high_chance_int <- high_chance %>% filter(outcome == "pass_outcome_interception")
high_chance_td <-high_chance %>% filter(outcome == "pass_outcome_touchdown")

high_chance_int$prob_means

high_chance_td_match <- high_chance_td %>% filter(prob_means > .932 & prob_means < .938)
dim(high_chance_td_match)

sample_td <- high_chance_td_match[1,]
sample_int <- high_chance_int[1,]


low_chance_td <- trained_plus_phat %>% filter(prob_means > .40 & prob_means < .50) %>%
  filter(outcome == "pass_outcome_touchdown")
dim(low_chance_td)

low_chance_int <- trained_plus_phat %>% filter(prob_means > .40 & prob_means < .50) %>% 
  filter(outcome == "pass_outcome_interception")
dim(low_chance_int)

sample_td <- low_chance_td[1,]
sample_int <- low_chance_int[1,]

sample_td_playID <- sample_td$playId[1]
sample_int_playID <- sample_int$playId[1]

sample_td_gameID <- sample_td$gameId[1]
sample_int_gameID <- sample_int$gameId[1]

load("/Users/katherineevans/Dropbox/NFL_big_data_bowl/data/data_2017091009.RData")
play_info <- data.frame(tmp$plays)
sample_td_play <- play_info %>% filter(playId == sample_td_playID)

sample_int_play <- play_info %>% filter(playId == sample_int_playID)

sample_td_play
sample_td

sample_int_play
sample_int



###### Visualize me

# load visualize_this.Rdata

file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091009.csv"
tracking.example <- read_csv(file.tracking)
tracking.players <- tracking.example %>% select(nflId,jerseyNumber)

#### PLAY 1, TD TO KUPP #####
relevant_recs_1 <- as.numeric(unlist(strsplit(colnames(play_1_means), split='.', fixed=TRUE))[c(2,4,6,8,10)])

play_1_recs <- play_df_1 %>% filter(nflId %in% relevant_recs_1) %>% inner_join(tracking.players)
play_1_recs <- unique(play_1_recs) %>% arrange(nflId, time)

## Now need the catch probabilities

play_1_hats <- rownames_to_column(data.frame(play_1_means), "time")
colnames(play_1_hats) <- c('time', relevant_recs_1)
play_1_hats <- play_1_hats %>% gather(nflId, p_hat, 2:6)
play_1_hats$nflId <- as.numeric(play_1_hats$nflId)
play_1_recs$nflId <- as.numeric(play_1_recs$nflId)

play_1_recs <- data.frame(play_1_recs) %>% inner_join(play_1_hats)
play_1_recs_reduced <- play_1_recs[seq(1, nrow(play_1_recs), 4), ] 

## Get line of scrimmage 
LOS_line_1 <- 120 - sample_td_play$yardlineNumber[1] -10

# To annotate, need the last reading for each player
play_1_recs_middle <- data.frame(play_1_recs %>%
                    group_by(nflId) %>%
                    arrange(time) %>%
                    slice(24))

# Get a middle reading as well
play_1_recs_last <- data.frame(play_1_recs %>%
                                 group_by(nflId) %>%
                                 arrange(time) %>%
                                 slice(n()))

plot_df_1 <- rbind(play_1_recs_reduced, play_1_recs_middle, play_1_recs_last) %>%
              arrange(time, nflId)
 
save(plot_df_1, file = "data/play_1_plot_df.RData")

## Play_1 Plot

ggplot() +
  geom_path(data = plot_df_1, aes(x = (xmax-y), y = x, group = nflId, color=x)) +
  geom_point(data = plot_df_1, aes(x = (xmax-y), y = x, 
                                      colour = p_hat, group = nflId, pch = team, size = team)) +
  xlim(7, 57) +
  # Put name at end of route
  geom_text(data = play_1_recs_last,
            aes(x = (xmax-y) * 1.05, y = x,
                label = displayName),
            check_overlap = TRUE,
            nudge_y=1.5, size=3) +
  # Put EHCP at end of route
  # geom_text(data = play_1_recs_last,
  #           aes(x = (xmax-y) , y = x* .99,
  #               label = paste('EHCP =', round(p_hat,3))),
  #           check_overlap = TRUE,
  #           nudge_y=1.5, size=2.5) +
  # # Put EHCP in middle-ish of route
  # geom_text(data = play_1_recs_middle,
  #           aes(x = (xmax-y) , y = x* .99,
  #               label = paste('EHCP =', round(p_hat,3))),
  #           check_overlap = TRUE,
  #           nudge_y=1.5, size=2.5) +
  geom_hline(aes(yintercept=LOS_line_1)) +
  annotate("text", 55, LOS_line_1, vjust = -1, label = "LOS") +
  scale_size_manual(values = c(3, 2, 3), guide = FALSE) + 
  scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
  scale_colour_gradientn(
    colours=c('blue','yellow','red'), 
    limits=c(0,1)) +
  theme_bw() +
  labs(color='EHCP') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())

#### END OF PLAY 1 STUFF


#### PLAY 2, Colts INT #####
relevant_recs_2 <- as.numeric(unlist(strsplit(colnames(play_2_means), split='.', fixed=TRUE))[c(2,4,6,8,10)])

play_2_recs <- play_df_2 %>% filter(nflId %in% relevant_recs_2) %>% inner_join(tracking.players)
play_2_recs <- unique(play_2_recs) %>% arrange(nflId, time)

## Now need the catch probabilities

play_2_hats <- rownames_to_column(data.frame(play_2_means), "time")
colnames(play_2_hats) <- c('time', relevant_recs_2)
play_2_hats <- play_2_hats %>% gather(nflId, p_hat, 2:6)
play_2_hats$nflId <- as.numeric(play_2_hats$nflId)
play_2_recs$nflId <- as.numeric(play_2_recs$nflId)

play_2_recs <- data.frame(play_2_recs) %>% inner_join(play_2_hats)
play_2_recs_reduced <- play_2_recs[seq(1, nrow(play_2_recs), 4), ] 

## Get line of scrimmage 
LOS_line_2 <- sample_int_play$yardlineNumber[1] +10

# To annotate, need the last reading for each player
play_2_recs_middle <- data.frame(play_2_recs %>%
                                   group_by(nflId) %>%
                                   arrange(time) %>%
                                   slice(24))

# Get a middle reading as well
play_2_recs_last <- data.frame(play_2_recs %>%
                                 group_by(nflId) %>%
                                 arrange(time) %>%
                                 slice(n()))

plot_df_2 <- rbind(play_2_recs_reduced, play_2_recs_middle, play_2_recs_last) %>%
  arrange(time, nflId)

save(plot_df_2, file = "data/play_2_plot_df.RData")

## Play_2 Plot

ggplot() +
  geom_path(data = plot_df_2, aes(x = (xmax-y), y = x, group = nflId, color=x)) +
  geom_point(data = plot_df_2, aes(x = (xmax-y), y = x, 
                                   colour = p_hat, group = nflId, pch = team, size = team)) +
  xlim(5, 40) +
  # Put name at end of route
  geom_text(data = play_2_recs_last,
            aes(x = (xmax-y) * 1.05, y = x,
                label = displayName),
            check_overlap = TRUE,
            nudge_y=1.5, size=3) +
  # Put EHCP at end of route
  # geom_text(data = play_2_recs_last,
  #           aes(x = (xmax-y) , y = x* .99,
  #               label = paste('EHCP =', round(p_hat,3))),
  #           check_overlap = TRUE,
  #           nudge_y=1.5, size=2.5) +
  # # Put EHCP in middle-ish of route
  # geom_text(data = play_2_recs_middle,
  #           aes(x = (xmax-y) , y = x* .99,
  #               label = paste('EHCP =', round(p_hat,3))),
  #           check_overlap = TRUE,
#           nudge_y=1.5, size=2.5) +
geom_hline(aes(yintercept=LOS_line_2)) +
  annotate("text", 40, LOS_line_2, vjust = -1, label = "LOS") +
  scale_size_manual(values = c(3, 2, 3), guide = FALSE) + 
  scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
  scale_colour_gradientn(
    colours=c('blue','yellow','red'), 
    limits=c(0,1)) +
  theme_bw() +
  labs(color='EHCP') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())

#### END OF PLAY 2 STUFF



