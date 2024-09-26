library(purrr)
library(dplyr)
library(readxl)
library(R2Rscenario)
# Blended scenarios ------------------------------------------------------------
# kitchen sink scenario 
kitchen_sink <- data.frame(
  watershed = rep("All", 7),
  action = c(2, 5, 6, 14, 16, 19, 23),
  years =  rep("All", 7))

# Habitat and Hatcheries 
habitat_and_hatcheries <- data.frame(
  watershed = c(rep("All", 6)),
  action = c(2, 5, 6, 15, 19, 22),
  years =  rep("All", 6))

# Dry year
# Dry year, 
# Food subsidies (TODO, right now being applied in all years) to sacramento (lower and lower mid), butte, yuba, and feather, sutter, and yolo

dry_years <- c(2, 6, 8:13, 15)

dry_year <- data.frame(
  watershed = c("All", "All", "All", "All", "All", "All", "All", "All"),
  action = c(1, 4, 5, 16, 17, 19, 23, 25),
  years =  I(list("All", c(dry_years), "All", "All", "All", "All", dry_years, "All")))


# Balanced scenarios -----------------------------------------------------------
# Elephant (HRL)
elephant <- data.frame(
  watershed = rep("All", 8),
  action = c(3, 7, 8, 11, 14, 20, 24, 26),
  years = rep("All", 8)
)

# Platypus (Kitchen Sink)
platypus <- data.frame(
  watershed = c("All", "All", "All", "All", "All", "All", "All", "All"),
  action = c(2, 23, 5, 6, 25, 15, 11, 19),
  years = rep("All", 8)
)

# Tortoise (Dry Year)
dry_years <- c(2, 6, 8:13, 15)

tortoise <- data.frame(
  watershed = rep("All", 11),
  action = c(1, 23, 5, 7, 25, 16, 17, 11, 20, 26, 29),
  years = I(list("All", c(dry_years), c(dry_years), c(dry_years), "All", "All", "All", "All", "All", "All",
                 "All"))
  # years = I(list("All", "All", "All", "All", "All", "All", "All", "All", "All", "All",
  #                "All"))
)

# combine -----------------------------------------------------------------

blended_scenarios <- list("kitchen_sink" = kitchen_sink, 
                          "habitat_and_hatcheries" = habitat_and_hatcheries,
                          "dry_year" = dry_year)

balanced_scenarios <- list("tortoise" = tortoise,
                           "platypus" = platypus,
                           "elephant" = elephant)

scenarios <- list("blended_scenarios" = blended_scenarios, 
                  "balanced_scenarios" = balanced_scenarios)
usethis::use_data(scenarios, overwrite = TRUE)

