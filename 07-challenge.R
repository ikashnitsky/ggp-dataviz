#===============================================================================
# 2019-10-23 GGP dataviz
# dataviz challenge
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-07

df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv") %>% 
    janitor::clean_names() %>% 
    mutate(indicator = str_remove(indicator, "Pupil-teacher ratio in"),
           indicator = str_remove(indicator, "(headcount basis)"),
           indicator = str_remove(indicator, "\\(\\)"),
           indicator = str_trim(indicator),
           indicator = stringr::str_to_title(indicator)) %>% 
    select(-time_2) %>% 
    rename("country_code" = location,
           "student_ratio" = value,
           "year" = time)
