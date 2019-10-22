

library(eurostat)

eu_gind <- get_eurostat("demo_gind")

eu_gind$indic_de %>% unique()

# list countries ordered by pop share within EU-28
countries <- eu_gind %>% 
        mutate(cntr = geo %>% paste) %>% 
        filter(nchar(cntr) == 2, 
               time %>% lubridate::year() == 2017,
               indic_de == "POPSHARE") %>% 
        arrange(desc(values)) %>% 
        pull(cntr)
        

set.seed(911)

hereweare <- c("Arianna", "Augusto", "Cédric", "Coomlan Cyrus Darius", "Elena", "Gianluca", "Jarl", "Jolien", "Katrin", "Leen", "Mariana", "Md Mahfuzur", "Montserrat", "Nobutaka", "Pablo Sebastián", "Pavlos", "Ronald", "Vanessa", "Zsuzsanna", "Zuzanna")


nstud <- length(hereweare)

ncntr <- countries[1:nstud]


df_assign <- tibble(
        email = hereweare,
        country = sample(ncntr)
) 

df_assign %>% View
        
        