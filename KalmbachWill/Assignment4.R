#assignment4

library(gapminder)
library(here)
library(tidyverse)
library(ggrepel)
library(socviz)
library(dplyr)


COUNTIES <- read_csv(here("covid-19-data" ,"us-counties.csv"))
#PA <- filter(COUNTIES, state == "Pennsylvania")
PA <- COUNTIES %>% filter(state=="Pehhsylvania")
p <- ggplot(PA, mapping = aes(x = cases, y = deaths, label = county))

p_title <- "COVID-19 Deaths vs Cases for PA as of 2021-4-12"
x_label <- "Cases"
y_label <- "Deaths"

p + geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  
  geom_text_repel() + 
  labs(x = x_label, y = y_label, 
       title = p_title) 
