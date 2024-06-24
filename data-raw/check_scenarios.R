library(tidyverse)
library(R2Rscenario)

# check that scenarios are matching up with what we have in our original params, 
# once they are all looking good we can update fallRunDSM to remove params lists besides baseline

# Kitchen sink 
scenario_df <- data.frame(
  watershed = c(rep("All", 6), "Sacramento River"),
  action = c(2, 5, 6, 14, 16, 19, 23),
  years =  rep("All", 7))

scenario <- load_scenario(scenario = scenario_df,
                          species = DSMscenario::species$FALL_RUN,
                          params = fallRunDSM::r_to_r_baseline_params)

check_scenarios <- function(names) {
  print(names)
  correct_res <- scenario[[names]] != fallRunDSM::r_to_r_kitchen_sink_params[[names]]
  return(sum(correct_res))
}

res <- map(names(fallRunDSM::r_to_r_baseline_params), check_scenarios) 
which(res > 0)

# ones that are different  
#  88 spawning habitat - includes decay
#  89 inchannel habitat fry - includes decay
#  90 inchannel habitat juvenile - includes decay
#  92 weeks flooded - all good here, issue with original weeks flooded in kitchen sink params
#  99 prop_high_predation - scaling diff in kitchen sink vs here, this is better
# 100 Contact Points - scaling diff in kitchen sink vs here, this is better
# 101 Delta contact points - scaling diff in kitchen sink vs here, this is better
# 102 Delta high predation - scaling diff in kitchen sink vs here, this is better
# 121 Hatchery release - in params, when updating format we just did default hatchery release, this should actually be all 0s so this is good
