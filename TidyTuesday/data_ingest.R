library(tidyverse)
library(janitor)
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

nuclear_enr_facilities <- readxl::read_excel("Documents/Twitter_Threads/TidyTuesday/furhmann_nl_dataset_v.1.2.xlsx")

nuclear_enr_facilities = nuclear_enr_facilities %>% mutate(
  enr_type_desc = case_when( 
    enr_type == 1 ~ "reprocessing",
    enr_type == 2 ~ "gas_diffusion",
    enr_type == 3 ~ "centrifuge",
    enr_type == 4 ~ "EMIS",
    enr_type == 5 ~ "chemical_ion",
    enr_type == 6 ~ "isotope_sep",
    enr_type == 7 ~ "laser",
    enr_type == 8 ~ "thermal_diffusion"
    ),
  operation_end = ifelse(operation_end==7777,2016,operation_end)
) %>% filter(operation_start != -99 & operation_start != 9999 & operation_end != -99)


long_enr_fac = nuclear_enr_facilities %>% group_by(country_name,facility_name,enr_type_desc) %>% 
  tidyr::nest(operation_start, operation_end) %>%
  mutate(year = purrr::map(data, ~seq(unique(.x$operation_start), unique(.x$operation_end), by=1))) %>%
  tidyr::unnest(year) 


country_year_enr_cnt = long_enr_fac %>% group_by(country_name,enr_type_desc,year) %>% 
  summarize(
    total_enr_facs =n()
  ) 



ggplot(nuclear_count_by_year,aes(x=year,y=cnt,color=country)) +
  geom_point () + 
  ## end of cold_war
  ## IAEA formation - 1957
  ## NPT  - 68 signed, effective 70
  ## South Africa accedes to NPT - 1990
  ## CTBT  - 1996
  geom_vline(xintercept = 1957,color='blue',linetype="dashed") + 
  geom_vline(xintercept = 1970, color = 'blue',linetype='dashed') + 
  geom_vline(xintercept = 1990, color='green',linetype='dotted') + 
  geom_vline(xintercept = 1996, color='blue',linetype='dashed') + 
  labs(x='Year',y='# of Tests') + 
  theme_bw(base_size=16)

## Get World Map

world <- map_data("world")



ggplot(world)+
  geom_polygon(mapping = aes(x=long,y=lat,group=group),fill="grey", color="grey28") + 
  theme_bw(base_size = 16) +
  geom_point(data = nuclear_explosions, 
             aes(x=longitude,y=latitude, 
                 color = country, size = yield_upper),alpha=1/2)
