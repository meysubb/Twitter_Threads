library(bigballR)

sec_today_sched <- get_date_games(date = as.character(format(Sys.Date()-1, "%m/%d/%Y")), conference = "SEC")

sec_today_pbp <- sec_today_sched %>% filter(!is.na(GameID)) %>% 
  mutate(
    pbp = purrr::map(GameID,get_play_by_play),
    lineups = purrr::map(pbp,get_lineups,keep.dirty=T,garbage.filter=F),
    player_stats = purrr::map(pbp,get_player_stats,keep.dirty=T,garbage.filter=F),
    mins_dist_plot = purrr::map(player_stats,gg_minute_dist_plot))
 


gg_minute_dist_plot <- function(p_df){
  ### probably a better way to do this. 
  p_df[,7:ncol(p_df)] <- sapply(p_df[,7:ncol(p_df)],as.numeric)
  p_df <- p_df %>% 
    ungroup() %>%   
    arrange(Team,MINS) %>%   
    mutate(.r = row_number())
  
  home_team <- unique(p_df$Home)
  away_team <- unique(p_df$Away)
  home_color <-  ncaa_colors[ncaa_colors$ncaa_name == home_team,'primary_color']
  away_color <-  ncaa_colors[ncaa_colors$ncaa_name == away_team,'primary_color']
    
  ggplot(p_df,aes(x=.r,y=MINS,fill=Team)) + 
    geom_col() + 
    coord_flip() + 
    facet_wrap(~Team,ncol=1,scales='free') +  
    scale_x_continuous(
      breaks = p_df$.r,
      labels = p_df$Player
    ) + 
    labs(title = 'Minutes Distribution',
         subtitle = paste0(away_team,' @ ',home_team),
         caption = '@msubbaiah1\nData courtsey of bigballR(@jakef1873)',
         x = '',
         y = 'Minutes') + 
    theme_bw(base_size=16)  +
    scale_fill_manual(values = c(home_color,away_color)) 
}



### Loop through dataframe and plot 

for(i in 1:nrow(sec_today_pbp)){
  home_team <- sec_today_pbp$Home[i]
  home_hashtag <- sec_teams[sec_teams$sec_teams == home_team,'hashtags']
  away_team <- sec_today_pbp$Away[i]
  away_hashtag <- sec_teams[sec_teams$sec_teams == away_team,'hashtags']
  
  ggsave('mins_dist.png',sec_today_pbp$mins_dist_plot[[i]],height=8,width=12)
  
  tweet_text <- paste0("Mins Distribution for ",away_team," @ ",home_team,". ",home_hashtag," ",away_hashtag)
  post_tweet(status = tweet_text,media = 'mins_dist.png') 
}
