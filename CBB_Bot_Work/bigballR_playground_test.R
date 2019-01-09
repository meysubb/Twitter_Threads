library(bigballR)

sec_today_sched <- get_date_games(date = as.character(format(Sys.Date(), "%m/%d/%Y")), conference = "SEC")

sec_today_pbp <- sec_today_sched %>% filter(!is.na(GameID)) %>% 
  mutate(
    pbp = purrr::map(GameID,get_play_by_play),
    lineups = purrr::map(pbp,get_lineups,keep.dirty=T,garbage.filter=F),
    player_stats = purrr::map(pbp,get_player_stats,keep.dirty=T,garbage.filter=F))



### Create a function, to make the plots easier. 
p4 <- sec_today_pbp$player_stats[[2]]

p4[,7:ncol(p4)] <- sapply(p4[,7:ncol(p4)],as.numeric)


### Order Minutes
p4 <- p4 %>% 
  ungroup() %>%   # As a precaution / handle in a separate .grouped_df method
  arrange(Team,MINS) %>%   # arrange by facet variables and continuous values
  mutate(.r = row_number())

ggplot(p4,aes(x=.r,y=MINS,fill=Team)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~Team,ncol=1,scales='free') +  
    scale_x_continuous(
      breaks = p4$.r,
      labels = p4$Player
    ) + 
  labs(title = 'Minutes Distribution',
       subtitle = 'Texas A&M @ Kentucky',
       caption = '@msubbaiah1\nData courtsey of bigballR(@jakef1873)',
       x = '',
       y = 'Minutes') + 
theme_bw(base_size=16)  +
  scale_fill_manual(values = c('#0033A0','#500000'))
                     #labels = c(away_team, home_team))
