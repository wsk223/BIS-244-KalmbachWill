# Solution file for BIS 244 Assignment 03, Spring 2021

# Clear out Console and Environment
rm(list=ls(all=TRUE))
cat("\014")

#if (!require("here")) install.packages("here")
#library("here")

# We'll use package readr, which is part of the tidyverse
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Reading the PRESIDENT_precinct_primary.csv file in as a data frame
RESULTS <- read_csv("PRESIDENT_precinct_primary.csv")

# Or, if you'd prefer to keep the data file zipped to keep it smaller....
# RESULTS <- read_csv(unz("PRESIDENT_precinct_primary.csv.zip", "PRESIDENT_precinct_primary.csv")) 

# Making candidate and state factor variables
RESULTS$candidate <- as.factor(RESULTS$candidate)
RESULTS$state <- as.factor(RESULTS$state)
n_candidates <- length(levels(RESULTS$candidate))
n_states <- length(levels(RESULTS$state))

# dplyr approach
RESULTS <- group_by(RESULTS, state, candidate)
COUNTS1 <- summarise(RESULTS, votes = sum(votes))

# Determining unique values for states and candidates factors
STATES <- levels(RESULTS$state)
CANDIDATES <- levels(COUNTS1$candidate)

# Replacing multiple variables/instances 
COUNTS1 <- mutate(COUNTS1,cand_cons=case_when(candidate =="JOSEPH R BIDEN" ~ "BIDEN",
                                              candidate == "DONALD J TRUMP" ~ "TRUMP",
                                              candidate == "JOSEPH R BIDEN/KAMALA HARRIS" ~ "BIDEN",
                                              candidate == "JOSEPH R BIDEN JR" ~ "BIDEN",
                                              candidate == "BIDEN / HARRIS" ~ "BIDEN",
                                              candidate == "BIDEN AND HARRIS" ~ "BIDEN",
                                              candidate == "JOE BIDEN" ~ "BIDEN",
                                              candidate == "JOSEPH ROBINETTE BIDEN" ~ "BIDEN",
                                              candidate == "DONALD J TRUMP/MICHAEL R PENCE" ~ "TRUMP",
                                              candidate == "TRUMP / PENCE" ~ "TRUMP",
                                              candidate == "TRUMP AND PENCE" ~ "TRUMP",
                                              TRUE ~ "OTHER"))
CAND_CONS <- levels(as.factor(COUNTS1$cand_cons))
CAND_CONS

RESULTS <- group_by(COUNTS1, state, cand_cons)
COUNTS2 <- summarise(RESULTS, votes = sum(votes))

n_COUNTS2 <- length(COUNTS2$state)
COUNTS3 <- data.frame(STATES)
COUNTS3$votes <- 0
COUNTS3$winner <- NA
for (i in 1:n_states) {
  BIDEN <- 0
  TRUMP <- 0
  VOTES <- 0
  for (j in 1:n_COUNTS2){
    if (COUNTS2$state[j]==COUNTS3$STATES[i]){
      VOTES <- VOTES + COUNTS2$votes[j]
      if (COUNTS2$cand_cons[j]=="BIDEN") {
        BIDEN <- BIDEN + COUNTS2$votes[j]
      }
      else if (COUNTS2$cand_cons[j]=="TRUMP") {
        TRUMP <- TRUMP + COUNTS2$votes[j]
      }
      else {}
    }
  }
  COUNTS3$votes[i] <- VOTES
  if (BIDEN > TRUMP) {
    COUNTS3$winner[i] <- "BIDEN"
  }
  else COUNTS3$winner[i] <- "TRUMP"
}

view(COUNTS2)

library(ggplot2)

R <- ggplot(data = COUNTS2,
            mapping = aes(x = cand_cons,
                          y = votes, color = cand_cons))

R <- R + geom_point(mapping = aes(group = state)) + 
  facet_wrap(~ state)      

R <- R + labs(x = "Candidates", y = "Votes", title = "Presidential Candidacy Results",
          subtitle = "Sorted by Presidential Candidate", caption = "Source: jcuriel-unc (github)")
R


