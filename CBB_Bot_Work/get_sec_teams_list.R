library(rvest)
library(ncaahoopR)
### First identify SEC teams
url <- 'http://www.espn.com/mens-college-basketball/standings'
sec_teams_raw <- read_html(url) %>% html_nodes("div:nth-child(26) .hide-mobile a") %>% html_text()
sec_teams <- gsub("\\s*\\w*$", "", sec_teams_raw)
sec_teams[3] <- "Alabama"
sec_teams[6] <- "Miss St"
teams <- ids
match(sec_teams,teams$team)
sec_teams[15] <- "Texas A&M TA&M" 
sec_teams[16] <- "Mississippi State" 


sec_df <- data.frame(sec_teams)

sec_hashtags <- c("#Vols","#HottyToddy #OleMiss","#Bama",
                  "#ARKY #WPS","#HereSC #USc","#HailState #CLANGA",
                  "#WarEagle","#GeuxTigers #LSU","#Mizzou","#BBN",
                  "#AnchorDown #Vandy","#Gators #UF","#UGA #DAWGS",
                  "#TAMU #Aggies","#TAMU #Aggies","#HailState #CLANGA")

sec_df$hashtags <- sec_hashtags

saveRDS(sec_df,"sec_teams_list.RDS")
