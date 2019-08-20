library(collegeballR)
library(tidyverse)
library(lubridate)

conf_types = collegeballR::cfb_conf_types_df 

conf_types = conf_types %>% mutate(
  conf_df = purrr::map(name,cfb_conf_team)
)

conf_teams = c("ACC","Big 12","Big Ten","SEC","Pac-12")

total_conf = bind_rows(conf_types$conf_df) %>% select(school,conference) %>% 
  mutate(
    p5 = ifelse(conference %in% conf_teams,"P5","G6")
  )

snooz_df = read_csv("snoozleQuery (1).csv") %>% janitor::clean_names() 

sn2 = snooz_df %>% left_join(total_conf,by=c("vis_team"="school")) %>% 
  left_join(total_conf,by=c("home_team"="school"),suffix=c("_vis","_home")) %>% 
  filter(over_under>0)


## Let's Look when P5 vs P5 
p5 = sn2 %>% filter(p5_vis=="P5",p5_home=="P5")
p5$year = year(p5$date)

int_breaks_rounded <- function(x, n = 5)  pretty(x, n)[round(pretty(x, n),1) %% 1 == 0]


tt = p5 %>% group_by(year) %>% summarize(
  avg_ou = mean(over_under),
  cnt = n()
) %>% filter(year<2019) 
  
ggplot(tt,aes(x=year,y=avg_ou,label=round(avg_ou,1))) + 
  geom_line() + 
  geom_label_repel() +
  labs(x="Year",y="Average O/U",
       title = "Average Over/Under",
       subtitle = "P5 vs P5",
       caption = "@msubbaiah1") +
  theme_bw(base_size = 16) + 
  scale_x_continuous(breaks = int_breaks_rounded)+
  scale_y_continuous(limits=c(30,60))

## Let's Look when G6 vs G6
g6 = sn2 %>% filter(p5_vis=="G6",p5_home=="G6")
g6$year = year(g6$date)

g6 %>% group_by(year) %>% summarize(
  avg_ou = mean(over_under),
  cnt = n()
) %>% filter(year<2019) %>% ggplot(aes(x=year,y=avg_ou,label=round(avg_ou,1))) + 
  geom_line() + 
  geom_label_repel() +
  labs(x="Year",y="Average O/U",
       title = "Average Over/Under",
       subtitle = "G6 vs G6",
       caption = "@msubbaiah1") +
  theme_bw(base_size = 16) + 
  scale_x_continuous(breaks = int_breaks_rounded)+
  scale_y_continuous(limits=c(30,60))

## Let's Look when P5 vs G6