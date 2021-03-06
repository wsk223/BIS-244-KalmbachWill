library(tidyverse)
Temp <- getwd()
setwd("./covid-19-data/")
states <- read.csv("us-states.csv")
Pennsylvania <- filter(states, state=="Pennsylvania")
n <- length(Pennsylvania$date)
for (i in 2:n) {
  Pennsylvania$adj_deaths[i] <- (Pennsylvania$deaths[i]) 
}
Pennsylvania <- Pennsylvania %>% mutate(adj_deaths = ifelse(
  (format(date) == "2020-04-21"), deaths - 282, ifelse(format(date) == "2020-04-22", 
  deaths - 297, 
  deaths)
  ))
sum(Pennsylvania$adj_deaths)
