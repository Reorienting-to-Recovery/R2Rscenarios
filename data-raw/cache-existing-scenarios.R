library(purrr)
library(dplyr)
library(readxl)
library(R2Rscenario)
scenarios <- list()

# kitchen sink scenario 
kitchen_sink <- data.frame(
  watershed = c(rep("All", 6), "Sacramento River"),
  action = c(2, 5, 6, 14, 16, 19, 23),
  years =  rep("All", 7))

# ... more


usethis::use_data(scenarios)
