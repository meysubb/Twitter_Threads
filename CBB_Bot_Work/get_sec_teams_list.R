library(rvest)
### First identify SEC teams
url <- 'http://www.espn.com/mens-college-basketball/standings'
sec_teams_raw <- read_html(url) %>% html_nodes("div:nth-child(26) .hide-mobile a") %>% html_text()
sec_teams <- gsub("\\s*\\w*$", "", sec_teams_raw)
sec_teams[3] <- "Alabama"
sec_teams[6] <- "Miss St"
match(sec_teams,teams$team)
saveRDS(sec_teams,"sec_teams_list.RDS")