#===============================================================================
# 2019-10-22 GGP dataviz
# plotly example
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# load the packages
library(tidyverse)
library(plotly)

p <- ggplot(airquality) +
    geom_jitter(aes(
        x = factor(Month),
        y = Temp,
        color = factor(Month)
    ),
    width = .2)

ggplotly(p) %>% 
    add_text(text = ~Month)


diamonds %>%
    plot_ly(x = ~cut) %>% 
    add_histogram() %>%
    group_by(cut) %>%
    summarise(n = n()) %>%
    add_text(
        text = ~scales::comma(n), y = ~n, 
        textposition = "top middle", 
        cliponaxis = FALSE
    )


diamonds %>%
    plot_ly(x = ~cut) %>% 
    add_histogram() %>%
    group_by(cut) %>%
    summarise(n = n()) %>% 
    plotly_data()


p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.5)
subplot(
    add_markers(p, color = ~cyl, showlegend = FALSE) %>% 
        colorbar(title = "Viridis"),
    add_markers(p, color = ~factor(cyl))
)


col1 <- c("#132B43", "#56B1F7")
col2 <- viridisLite::inferno(10)
col3 <- colorRamp(c("red", "white", "blue"))
subplot(
    add_markers(p, color = ~cyl, colors = col1) %>%
        colorbar(title = "ggplot2 default"),
    add_markers(p, color = ~cyl, colors = col2) %>% 
        colorbar(title = "Inferno"),
    add_markers(p, color = ~cyl, colors = col3) %>% 
        colorbar(title = "colorRamp")
) %>% hide_legend()
