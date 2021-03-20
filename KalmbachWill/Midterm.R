#WillKalmbach
rm(list=ls(all=TRUE))
cat("\014")

install.packages("here")
library(here)
library(tidyverse)
RESULTS <- read_csv("AAPL.csv")
n <- length(RESULTS$Date)
for (i in 2:n) {
  RESULTS$`Adj Close`[i] <- (RESULTS$Open[i]-RESULTS$Close[i-1]) 
}
mean(RESULTS$`Adj Close`)
mean(RESULTS$'Adj Close', na.rm=TRUE)
meancases <- mean(RESULTS$'Adj Close', na.rm=TRUE)

RESULTS$above_close <- 0
RESULTS$below_close <- 0

for (i in 1:n) {
  if(RESULTS$'Adj Close'[i]>=meancases) {
    RESULTS$above_close[i] <- RESULTS$'Adj Close'[i]
  } else {
    RESULTS$below_close[i] <- RESULTS$'Adj Close'[i]
  }
}

RESULTS$'Adj Close' <- 1
for (i in 2:n) {
  RESULTS$'Adj Close'[i] <- (RESULTS$Close[i]-RESULTS$Close[i-1]) 
}
for (i in 1:n) {
  if(RESULTS$'Adj Close'[i]>=0) {
    RESULTS$above_close[i] <- RESULTS$'Adj Close'[i]
  } else {
    RESULTS$below_close[i] <- RESULTS$'Adj Close'[i]
  }
}

p = ggplot() + 
  geom_point(data = RESULTS, aes(x = Date, y = above_close), color = "green") +
  geom_point(data = RESULTS, aes(x = Date, y = below_close), color = "red") +
  labs(x = "03/19/2020 Through 03/18/2021", y = "Changes in adjusting closing Price",
       title = "Changes in AAPL Daily Prices Over Past Five years",
       subtitle = "Will Kalmbach - Green = Increase Red = Decrease")
p




