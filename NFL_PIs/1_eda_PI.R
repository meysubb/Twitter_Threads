library(tidyverse)
library(lubridate)
library(nflscrapR)

pbp_15 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2015.csv")
pbp_16 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2016.csv")
pbp_17 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv")
pbp_18 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")


pbp_func <- function(x){
  x %>% filter(penalty==1 & penalty_type=='Defensive Pass Interference') %>% 
    select("home_team","away_team","qtr","down","goal_to_go","time","ydstogo",
           "penalty_team","penalty_yards","posteam","defteam","game_date",
           "posteam_score","defteam_score","epa") %>% mutate(year = year(game_date),
                                                       difference = abs(posteam_score-defteam_score)) %>% select(-game_date)
} 

all_pis <- list( pbp_15,pbp_16,pbp_17,pbp_18 ) %>%
  lapply( pbp_func ) %>% bind_rows()

## combine LAC + SD and STL + LA
all_pis[all_pis$posteam == "SD","posteam"] = "LAC"
all_pis[all_pis$posteam == "STL","posteam"] = "LA"

all_pis[all_pis$defteam == "SD","defteam"] = "LAC"
all_pis[all_pis$defteam == "STL","defteam"] = "LA"

# General PI Distribution 
dat <- data.frame(yds = c(10,15,20,30),yds_text = paste0(c(10,15,20,30)," Yards"))

mean_yards_team <- all_pis %>% group_by(posteam) %>% summarize(mean_yards = mean(penalty_yards,na.rm=T))
mean_yards_against <- all_pis %>% group_by(defteam) %>% summarize(mean_yards_def = mean(penalty_yards,na.rm=T))

all_pis2 <- all_pis %>% inner_join(mean_yards_team)
all_pis2 <- all_pis2 %>% inner_join(mean_yards_against)

library(viridis)   
 

### PI Distribution
yearly_pi_dist<-ggplot(all_pis2) + 
  geom_histogram(aes(penalty_yards,fill=factor(year)),alpha=0.8) + 
  facet_grid(year~.) + theme_bw(base_size = 16) + 
  scale_fill_brewer(palette="Dark2") + 
  geom_vline(data=dat,aes(xintercept = yds), colour = "firebrick3",linetype='dotted')+
  geom_text(data=dat,aes(x=yds-1,y=30,label=yds_text,angle=90,size=2)) + 
  guides(fill=FALSE,size=FALSE) + 
  labs(x = "Penalty Yards",y="Count",
       caption = '@msubbaiah1',
       subtitle = "Distrbution of Defense PIs by Year")

### PI Distribution by team 
library(ggridges)
library(viridis)

pi_drawn<-ggplot(all_pis2, aes(x = penalty_yards,y = forcats::fct_rev(factor(posteam)),fill=mean_yards)) + 
  geom_density_ridges(scale = 3, rel_min_height = 0.01) + 
  theme_bw(base_size = 16) + 
  scale_fill_viridis(option="inferno",direction=-1,name="Penalty \n Yds") +
  labs(x="Penalty Yards",y="Team",
       title = "Defense PIs drawn (Offense)",
       subtitle = "2015 - 2018",
       caption = "@msubbaiah1")

pi_committed<-ggplot(all_pis2, aes(x = penalty_yards,y = forcats::fct_rev(factor(defteam)),fill=mean_yards_def)) + 
  geom_density_ridges(scale = 3, rel_min_height = 0.01) + 
  theme_bw(base_size = 16) + 
  scale_fill_viridis(option="inferno",direction=-1,name="Penalty \n Yds") +
  labs(x="Penalty Yards",y="Team",
       title = "Defense PIs committed (Defense)",
       subtitle = "2015 - 2018",
       caption = "@msubbaiah1")



### Cluch situations 
# 1 - less than 2 minutes in second quarter 
# 2 - 10 point game in the fourth quarter 
clutch_df <- 
  all_pis2 %>% 
  mutate(clutch = ifelse(qtr==2 & time <= hms(120),"clutch",
                         ifelse(qtr>4,"clutch",
                         ifelse(qtr==4 & difference <= 10,"clutch","not")))) %>% filter(clutch == "clutch")

int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 
clutch_df_means <- clutch_df %>% group_by(year) %>% summarize(m_yards = mean(penalty_yards)) 

clutch_df <- clutch_df %>% inner_join(clutch_df_means)

year_clutch_pi <- ggplot(clutch_df,aes(x=penalty_yards,y=forcats::fct_rev(factor(year)),alpha=0.8,fill='firebrick3'))+ 
  geom_density_ridges(scale=3,rel_min_height=0.01) + theme_bw(base_size=16) + guides(alpha=F,fill=F) + 
  scale_x_continuous(breaks=seq(0,60,by=10)) + 
  labs(x="Penalty Yards",y="Year",
       title="Defensive PIs in Clutch situations",
       subtitle="Clutch Situations Def: \n2 mins to go in 2nd, OT, and \nor 10pt game in 4th",
       caption='@msubbaiah1')

  

all_pbp <- bind_rows(list( pbp_15,pbp_16,pbp_17,pbp_18) %>% lapply(function(x) x %>% select(epa)))
sd_value <- sd(all_pbp$epa,na.rm=T)
mean_value <- mean(all_pbp$epa,na.rm=T)

epa_sd_values <- data.frame(values = c(mean_value+sd_value,mean_value+(2*sd_value),mean_value + (3*sd_value)),
                            labels = c("mu+sigma","mu+2*sigma","mu+3*sigma"))


clutch_df <- clutch_df %>% filter(epa>-2)

epa_clutch_year <- ggplot(clutch_df,aes(epa),alpha=0.8) + 
  geom_histogram() + 
  facet_grid(year~.) + theme_bw(base_size=16) + 
  scale_y_continuous(breaks=int_breaks) + 
  geom_vline(data=epa_sd_values,aes(xintercept = values),colour = "firebrick3",linetype='dotted') + 
  geom_text(data=epa_sd_values,aes(x=values-0.02,y=6,label=labels,angle=90,size=7),parse=T) + 
  guides(size=F) + 
  labs(x="EPA",y="Count",
       title="Effect of PIs in clutch situations?",
       subtitle = "Note: Avg/STD calculated from all plays",
       caption = "@msubbaiah1")


### comments on teams drawing PIs and committed
### Draw - Pretty much expected to see GB, NE and even the Rams (STL+LA) at the top to some extent. Throughly surprised that 
### Buffalo is up there in drawing PIs. Looking through the data further, in the 3 year time span, Buffalo drew 22 PIs. A lot of yards
### gained on those 22 PIs. Note team with the most PIs over this time frame Chargers (49), then Cardinals (44), and Steelers (44). 
### Jacksonville had an all time low of 9 PI calls in that same period. 
### Committed - The plot on the left makes me curious about blitz rate and how often corners/safeties are left in man coverage and how that affects PI calls. 
### Houston is the team that immediately sticks out. 
cowplot::plot_grid(pi_drawn,pi_committed)

epa_clutch_year
