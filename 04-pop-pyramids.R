#===============================================================================
# 2019-10-22 GGP dataviz
# Population pyramid example
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


# load the package
library(tidyverse)
library(magrittr)


# download eurostat data
library(eurostat)

eu_pop <- get_eurostat("demo_pjan")


# clean the dataset
df_dk <- eu_pop %>% 
    filter(
        !age %in% c("TOTAL", "UNK", "Y_OPEN"),
        geo == "DK"
    ) %>% 
    mutate(
        year = time %>% lubridate::year(),
        age = age %>% 
            paste %>% 
            str_replace("Y_LT1", "Y_0") %>% 
            str_remove("_") %>%  
            str_remove("Y") %>% 
            as.numeric()
    ) %>% 
    arrange(time, sex, age)


save(df_df, file = here::here("data/df_dk.rda"))


df_dk %>% 
    filter(
        year == 2018, sex == "T"
    ) %>% 
    ggplot(aes(age, values))+
    geom_col()
    
    
# both sex, coord_flip    

df_dk %>% 
    filter(
        year == 2018, !sex == "T"
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

# two years and annotations
df_dk %>% 
    filter(
        year %in% c(1960, 2018), !sex == "T"
    ) %>% 
    spread(sex, values) %>% 
    ggplot(aes(age))+
    geom_hline(yintercept = 0, size = .5, color = "gray20")+
    geom_path(aes(y = `F`, linetype = year %>% factor), color = "purple")+
    geom_path(aes(y = -M, linetype = year %>% factor), color = "orange")+
    coord_flip()+
    scale_y_continuous(
        breaks = seq(-40000, 40000, 10000),
        labels = seq(-40000, 40000, 10000) %>% 
            abs %>% divide_by(1000) %>% as.character()  %>% paste0(., "K")
        
    )+
    annotate(geom = "text", x = 100, y = -4e4, label = "MALES", hjust = 0, vjust = 1)+
    annotate(geom = "text", x = 100, y = 4e4, label = "FEMALES", hjust = 1, vjust = 1)



# compare two countries ---------------------------------------------------

df_two <- eu_pop %>% 
    filter(
        !age %in% c("TOTAL", "UNK", "Y_OPEN"),
        geo %in% c("IT", "BG")
    ) %>% 
    mutate(
        year = time %>% lubridate::year(),
        age = age %>% 
            paste %>% 
            str_replace("Y_LT1", "Y_0") %>% 
            str_remove("_") %>%  
            str_remove("Y") %>% 
            as.numeric()
    ) %>% 
    arrange(time, sex, age) %>% 
    group_by(sex, geo, time) %>% 
    mutate(values = values / sum(values))


df_two %>% 
    filter(
        year == 2018, sex == "T"
    ) %>% 
    ggplot(aes(age, values, color = geo))+
    geom_step()+
    coord_cartesian(expand = F)+
    scale_y_continuous(labels = scales::percent)+
    theme_minimal()+
    theme(legend.position = c(.9,.9))


compare_pop <- function(cntr = c("IT", "BG")) {
    
    df_two <- eu_pop %>% 
        filter(
            !age %in% c("TOTAL", "UNK", "Y_OPEN"),
            geo %in% cntr 
        ) %>% 
        mutate(
            year = time %>% lubridate::year(),
            age = age %>% 
                paste %>% 
                str_replace("Y_LT1", "Y_0") %>% 
                str_remove("_") %>%  
                str_remove("Y") %>% 
                as.numeric()
        ) %>% 
        arrange(time, sex, age) %>% 
        group_by(sex, geo, time) %>% 
        mutate(values = values / sum(values))
    
    
    df_two %>% 
        filter(
            year == 2018, sex == "T"
        ) %>% 
        ggplot(aes(age, values, color = geo))+
        geom_step()+
        coord_cartesian(expand = F)+
        scale_y_continuous(labels = scales::percent)+
        theme_minimal()+
        theme(legend.position = c(.9,.9))
}


c("UK", "ES", "IT", "DE", "FR") %>% compare_pop()


# a glance at interactive plotly magic
library(plotly)
gg <- ggplot2::last_plot()
ggplotly(gg) 

# plotly book
# https://plotly-r.com/introduction.html



# a more intelligent (?) approach ---------------------------------------------

# https://community.rstudio.com/t/ggplot2-alter-scale-on-pyramid-plot/14934/2

library(tidyverse)
remotes::install_github("erocoar/ggpol")
library(ggpol)

df <- tibble(
    Population = c(5, 8.7, 16.7, 24.8, 38, -4.6, -6.4, -16.1, -39.6, -55.3),
    Gender = c("Male", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female"),
    AgeBand = c("65-69", "70-74", "75-79", "80-84", "85+", "65-69", "70-74", "75-79", "80-84", "85+")
)

ggplot(df, aes(x = AgeBand, y = Population, fill = Gender)) +
    geom_bar(stat = "identity") +
    facet_share(~Gender, dir = "h", scales = "free", reverse_num = TRUE) +   # note: scales = "free"
    coord_flip() +
    theme_minimal() +
    labs(y = "Count", x = "Age Band", title = " ") +
    scale_fill_manual(values = c("pink", "blue"))


df_dk %>% 
    filter(
        year == 2018, !sex == "T"
    ) %>% 
    mutate(mult = case_when(sex=="M" ~ -1, sex=="F" ~ 1),
           sex = sex %>% fct_rev) %>% 
    # spread(sex, values) %>% 
    ggplot(aes(age, values*mult))+
    geom_step(aes(color = sex))+
    coord_flip()+
    facet_share(~sex, dir = "h", scales = "free", reverse_num = TRUE) +
    scale_y_continuous(
        # breaks = seq(-40000, 40000, 10000),
        labels = scales::unit_format(scale = 1e-3, suffix = "K")
    )

