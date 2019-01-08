library(ncaahoopR)
library(lubridate)
sec_teams <- readRDS("sec_teams_list.RDS")

today <- Sys.Date()

todays_sched <- get_master_schedule(year(today),month(today),day(today))
get_master_schedule(2019,1,8)

