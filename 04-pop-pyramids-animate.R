#===============================================================================
# 2019-10-22 GGP dataviz
# Population pyramid -- animate
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# animation ---------------------------------------------------------------

# the power of moving charts
# https://twitter.com/jburnmurdoch/status/1107552367795412992?lang=en

# https://www.ft.com/video/83703ffe-cd5c-4591-9b4f-a3c087aa6d19

# the social effect of a revolutionary idea 
# https://socialblade.com/twitter/user/jburnmurdoch

# plotly book
# https://plotly-r.com/introduction.html

library(tidyverse)
library(gganimate)


load(here::here("data/df_dk.rda"))


gg <- df_dk %>% 
    filter(
        !sex == "T"
    ) %>% 
    spread(sex, values) %>% 
    ggplot(aes(age))+
    geom_step(aes(y = `F`), color = "purple")+
    geom_step(aes(y = -M), color = "orange")+
    coord_flip()+
    scale_y_continuous(
        breaks = seq(-40000, 40000, 10000),
        labels = seq(-40000, 40000, 10000) %>% abs %>% paste %>% 
            str_replace("0000", "0K")
    )


gg + transition_time(time) +
    labs(title = "Year: {frame_time}")



# line / path
library(janitor)
df_aq <- airquality %>%
    clean_names() %>%
    mutate(
        date = paste(day, month, "1973", sep = "-") %>%  lubridate::dmy(),
        month = month %>% factor
    )



p <- ggplot(df_aq, aes(x = date, y = temp)) +
    geom_line()

ani <- p + 
    geom_point()+
    transition_reveal(date)+
    ease_aes("cubic-in-out")


animate(
    ani, nframes = nrow(df_aq) * 2, width = 500, height = 400, res = 96, start_pause = 3, end_pause = 10
)

anim_save('test-anim.gif')

