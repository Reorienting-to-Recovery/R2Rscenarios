# library(microbenchmark)
# library(DSMscenario)
# library(fallRunDSM)
#
# model_inputs <- fallRunDSM::load_baseline_data()
#
# habitat_inputs = list(spawning_habitat = model_inputs$spawning_habitat,
#                       inchannel_habitat_fry = model_inputs$inchannel_habitat_fry,
#                       inchannel_habitat_juvenile = model_inputs$inchannel_habitat_juvenile,
#                       floodplain_habitat = model_inputs$floodplain_habitat)
#
# scenario <- data.frame(watershed = "Upper Sacramento River",
#                        action = c(2, 3, 4, 5),
#                        start_year = 1980,
#                        end_year = 1999,
#                        units_of_effort = 1)
#
# results <-
#   microbenchmark(
#     a = load_scenario(scenario, habitat_inputs),
#     b = load_baseline_data(), times = 50)
#
#

library(DSMscenario)


model_inputs <- fallRunDSM::params

habitat_inputs = list(spawning_habitat = model_inputs$spawning_habitat,
                      inchannel_habitat_fry = model_inputs$inchannel_habitat_fry,
                      inchannel_habitat_juvenile = model_inputs$inchannel_habitat_juvenile,
                      floodplain_habitat = model_inputs$floodplain_habitat)
scenario <- tibble::tibble(
  watershed = DSMscenario::watershed_labels,
  action = 1,
  start_year = 1980,
  end_year = 1981,
  units_of_effort = 1)
set.seed(30) # stochastic floodplain fixed for testing
updated_inputs <- load_scenario(scenario, habitat_inputs)
# tribs_with_no_decay <-
#   !DSMscenario::regulated_watersheds |
#   (DSMscenario::watershed_groups > 7)
# all(updated_inputs$spawning_habitat[!tribs_with_no_decay,,]<model_inputs$spawning_habitat[!tribs_with_no_decay,,], na.rm=T)
# mean(updated_inputs$spawning_habitat[tribs_with_no_decay,,]-model_inputs$spawning_habitat[tribs_with_no_decay,,], na.rm=T)
# updated_inputs$spawning_habitat[tribs_with_no_decay,10,12] == model_inputs$spawning_habitat[tribs_with_no_decay,10,12]
# DSMscenario::watershed_labels
# # bear creek 4 and butte creek 6
#
# load_scenario(scenarios$ONE, 1)
