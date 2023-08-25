  decay <- decay_amount_matrices()
  upper_sac_index <- 1
  antelope_index <- 2
  san_joaq_index <- 31
  year_index_1 <- 1
  year_index_2 <- 20

test_that("spawn decay works", {
  expect_equal(decay$spawn[antelope_index, year_index_1], 1)
  expect_equal(decay$spawn[antelope_index, year_index_2], 1)

  expect_equal(decay$spawn[san_joaq_index, year_index_1], 1)
  expect_equal(decay$spawn[san_joaq_index, year_index_2], 1)
  # decay rate is runif(min = watershed spawn decay rate, max = 1)
  expect_gte(decay$spawn[upper_sac_index, year_index_1],
             DSMscenario::spawn_decay_rate[["Upper Sacramento River"]])
  expect_lte(decay$spawn[upper_sac_index, year_index_1], 1)

  expect_gte(decay$spawn[upper_sac_index, year_index_2],
             DSMscenario::spawn_decay_rate[["Upper Sacramento River"]])
  expect_lte(decay$spawn[upper_sac_index, year_index_2], 1)
})

test_that("rear decay works", {
  expect_equal(decay$rear[antelope_index, year_index_1], 1)
  expect_equal(decay$rear[antelope_index, year_index_2], 1)

  expect_equal(decay$rear[san_joaq_index, year_index_1], 1)
  expect_equal(decay$rear[san_joaq_index, year_index_2], 1)

  # decay rate is runif(min = watershed rear decay rate, max = 1)
  expect_gte(decay$rear[upper_sac_index, year_index_1],
             DSMscenario::rear_decay_rate[["Upper Sacramento River"]])
  expect_lte(decay$rear[upper_sac_index, year_index_1], 1)

  expect_gte(decay$rear[upper_sac_index, year_index_2],
             DSMscenario::rear_decay_rate[["Upper Sacramento River"]])
  expect_lte(decay$rear[upper_sac_index, year_index_2], 1)
})

# habitats <- list(
#   spawning_habitat = fallRunDSM::params$spawning_habitat,
#   inchannel_habitat_fry = fallRunDSM::params$inchannel_habitat_fry,
#   inchannel_habitat_juvenile = fallRunDSM::params$inchannel_habitat_juvenile,
#   floodplain_habitat = fallRunDSM::params$floodplain_habitat,
#   weeks_flooded = fallRunDSM::params$weeks_flooded
# )
#
# scenario <- load_scenario(scenario = DSMscenario::scenarios$SIX,
#                           species = DSMscenario::species$FALL_RUN,
#                           habitat_inputs = habitats, stochastic = F)
#
# scenario_b <- load_scenario(scenario = DSMscenario::scenarios$NO_ACTION,
#                             species = DSMscenario::species$FALL_RUN,
#                             habitat_inputs = habitats, stochastic = F)
#
# scenario_c <- load_scenario(scenario = DSMscenario::scenarios$NO_ACTION,
#                             species = DSMscenario::species$FALL_RUN,
#                             habitat_inputs = habitats, stochastic = F)
#
# ws="Clear Creek"
# ws="Upper Sacramento River"
# ws="Butte Creek"
# all(scenario$spawning_habitat[ws,,] != scenario_b$spawning_habitat[ws,,])
#
# all(scenario_c$spawning_habitat[ws,,] == scenario_b$spawning_habitat[ws,,])
#
# no_decay_tribs = scenarios$SIX$no_decay
# dont_decay = !DSMscenario::regulated_watersheds$SPRING | (DSMscenario::watershed_groups > 7)
# watershed_labels[dont_decay]
# watershed_labels[!dont_decay]
