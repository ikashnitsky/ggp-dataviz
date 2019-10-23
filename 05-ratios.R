#===============================================================================
# 2019-10-23 GGP dataviz
# Plotting ratios
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================



# The super common mistake
# https://twitter.com/ikashnitsky/status/1176246650517561347


library(tidyverse)
library(magrittr)

EuStockMarkets %>% 
    as_tibble() %>% 
    View


sto <- EuStockMarkets %>% 
    as_tibble() %>% 
    mutate(day = seq_along(DAX)) %>% 
    pivot_longer(DAX:FTSE)


sto %>% 
    ggplot(aes(day, value, color = name))+
    geom_path()

# too noisy, let's smooth the data
sto_sm <- sto %>% 
    group_by(name) %>% 
    mutate(value = value %>% 
               smooth.spline(spar = .5) %>% 
               extract2("y"))

sto_sm %>% 
    ggplot(aes(day, value, color = name))+
    geom_path()


# relative to beginnig
sto_sm %>% 
    group_by(name) %>% 
    mutate(value = value %>% divide_by(value[1])) %>% 
    ggplot(aes(day, value, color = name))+
    geom_hline(yintercept = 1, color = 1)+
    geom_path()


# relative to day 1500
sto_sm %>% 
    group_by(name) %>% 
    mutate(value = value %>% divide_by(value[1500])) %>% 
    ggplot(aes(day, value, color = name))+
    geom_hline(yintercept = 1, color = 1)+
    geom_path()


# log scale fixes the issue
sto_sm %>% 
    group_by(name) %>% 
    mutate(value = value %>% divide_by(value[1])) %>% 
    ggplot(aes(day, value, color = name))+
    geom_hline(yintercept = 1, color = 1)+
    geom_path()+
    scale_y_continuous(trans = "log", breaks = c(1/4, 1/2, 4/5, 1, 5/4, 2, 4))

sto_sm %>% 
    group_by(name) %>% 
    mutate(value = value %>% divide_by(value[1500])) %>% 
    ggplot(aes(day, value, color = name))+
    geom_hline(yintercept = 1, color = 1)+
    geom_path()+
    scale_y_continuous(trans = "log", breaks = c(1/4, 1/2, 4/5, 1, 5/4, 2, 4))



# odds ratios -------------------------------------------------------------

