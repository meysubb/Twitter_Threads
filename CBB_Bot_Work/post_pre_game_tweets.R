library(ncaahoopR)
library(lubridate)
library(rtweet)
source("twitter_auth.R")

create_token(
  app = t_app,
  consumer_key = t_consumer_key,
  consumer_secret = t_consumer_secret,
  access_token = t_access_token,
  access_secret = t_access_secret)

## Load SEC teams

sec_teams <- readRDS("sec_teams_list.RDS")

today <- Sys.Date()

daily_sched <- get_master_schedule(year(today),month(today),day(today))

daily_sched_sec <- daily_sched %>% filter(home %in% sec_teams$sec_teams | away %in% sec_teams$sec_teams)

get_game_info <- function(game_id){
  base_url <- "http://www.espn.com/mens-college-basketball/game?gameId="
  full_url <- paste0(base_url,game_id)
  first_nodes <- ".status-detail , .game-network , .icon-location-solid-before , .caption-wrapper"
  second_nodes <- ".line , .status-detail"
  info <- read_html(full_url) %>% html_nodes(first_nodes) %>% html_text()
  info2 <- read_html(full_url) %>% html_nodes(second_nodes)  %>% html_text()
  all_info <- c(info,info2)
  ### go to n-1 (to preserve the line of the game)
  clean_info <- gsub("[^[:alnum:]]", " ", all_info[1:length(all_info)-1])
  clean_info[length(all_info)] <- all_info[length(all_info)]
  trim_info <- trimws(clean_info)
  return(trim_info)
}

for(i in 1:nrow(daily_sched_sec)){
  val <- daily_sched_sec$game_id[i]
  home_team <- daily_sched_sec$home[i]
  home_hashtag <- sec_teams[sec_teams$sec_teams == home_team,'hashtags']
  away_team <- daily_sched_sec$away[i]
  away_hashtag <- sec_teams[sec_teams$sec_teams == away_team,'hashtags']
  
  s <- get_game_info(val)
  s <- s[s != ""]
  
  arena <- s[1]
  cov <- s[2]
  line <- s[4]
  
  
  line1 <- paste0("Today's Game ", i ,": ",away_team, " @ ",home_team," (",arena,"),",cov)
  line2 <- paste0("Line: ",line,".")
  line3 <- paste0(home_hashtag," ",away_hashtag)
  
  final_line <- paste(line1,line2,line3)
  
  post_tweet(status = final_line) 
}

