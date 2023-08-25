model_inputs <- fallRunDSM::load_baseline_data()

habitat_inputs = list(spawning_habitat = model_inputs$spawning_habitat,
                      inchannel_habitat_fry = model_inputs$inchannel_habitat_fry,
                      inchannel_habitat_juvenile = model_inputs$inchannel_habitat_juvenile,
                      floodplain_habitat = model_inputs$floodplain_habitat)

habitat_inputs_fixed = list(
  spawning_habitat = array(model_inputs$spawning_habitat[,1,1], c(31, 12, 22)),
  inchannel_habitat_fry = array(model_inputs$inchannel_habitat_fry[,1,1], c(31, 12, 21)),
  inchannel_habitat_juvenile = array(model_inputs$inchannel_habitat_juvenile[,1,1], c(31, 12, 21)),
  floodplain_habitat = array(model_inputs$floodplain_habitat[,1,1], c(31, 12, 21))
)

scenario <- dplyr::bind_rows(
  tibble::tibble(
    watershed = "Upper Sacramento River",
    action = c(2, 3, 4, 5),
    start_year = 1980,
    end_year = 1999,
    units_of_effort = 1),
  tibble::tibble(watershed = "Feather River",
         action = c(2, 3),
         start_year = 1980,
         end_year = 1989,
         units_of_effort = 1))

set.seed(30) # stochastic floodplain fixed for testing
updated_inputs <- load_scenario(scenario, habitat_inputs)

test_that("spawn degrade and add works", {

  expect_equal(updated_inputs$spawning_habitat["Antelope Creek", "Oct", "1982"],
            habitat_inputs$spawning_habitat["Antelope Creek", "Oct", "1982"])

  expect_lt(updated_inputs$spawning_habitat["Stony Creek", "Oct", "1982"],
            habitat_inputs$spawning_habitat["Stony Creek", "Oct", "1982"])

  expect_gt(updated_inputs$spawning_habitat["Upper Sacramento River", "Oct", "1982"],
            habitat_inputs$spawning_habitat["Upper Sacramento River", "Oct", "1982"])

  degrade_ratio <- updated_inputs$spawning_habitat["Stony Creek", "Oct", "1979"]/
    habitat_inputs$spawning_habitat["Stony Creek", "Oct", "1979"]

  # decay rate is runif(min = watershed spawn decay rate, max = 1)
  expect_gte(degrade_ratio, DSMscenario::spawn_decay_rate[["Stony Creek"]])
  expect_lte(degrade_ratio, 1)

  after <- updated_inputs$spawning_habitat["Stony Creek", 10 , 22]
  before <- habitat_inputs$spawning_habitat["Stony Creek", 10 , 22]
  decay_rate <- (after/before)^(1/22)
  expect_gt(decay_rate, DSMscenario::spawn_decay_rate[["Stony Creek"]])

  after <- updated_inputs$spawning_habitat["Feather River", 10 , 22]
  before <- habitat_inputs$spawning_habitat["Feather River", 10 , 22]
  added_habitat <- DSMhabitat::acres_to_square_meters(1)*10
  decay_rate <- ((after-added_habitat)/before)^(1/12)
  expect_gt(decay_rate, DSMscenario::spawn_decay_rate[["Feather River"]])

})

test_that("inchannel degrade and add works", {

  expect_equal(updated_inputs$inchannel_habitat_fry["Antelope Creek", "Jan", "1982"],
            habitat_inputs$inchannel_habitat_fry["Antelope Creek", "Jan", "1982"])

  expect_lt(updated_inputs$inchannel_habitat_fry["Stony Creek", "Jan", "1982"],
            habitat_inputs$inchannel_habitat_fry["Stony Creek", "Jan", "1982"])

  expect_gt(updated_inputs$inchannel_habitat_fry["Upper Sacramento River", "Jan", "1982"],
            habitat_inputs$inchannel_habitat_fry["Upper Sacramento River", "Jan", "1982"])

  expect_lt(updated_inputs$inchannel_habitat_juvenile["Stony Creek", "Jan", "1982"],
            habitat_inputs$inchannel_habitat_juvenile["Stony Creek", "Jan", "1982"])

  expect_gt(updated_inputs$inchannel_habitat_juvenile["Upper Sacramento River", "Jan", "1982"],
            habitat_inputs$inchannel_habitat_juvenile["Upper Sacramento River", "Jan", "1982"])

  degrade_ratio <- updated_inputs$inchannel_habitat_fry["Stony Creek", "Jan", "1980"]/
    habitat_inputs$inchannel_habitat_fry["Stony Creek", "Jan", "1980"]

  # decay rate is runif(min = watershed spawn decay rate, max = 1)
  expect_gte(degrade_ratio, DSMscenario::rear_decay_rate[["Stony Creek"]])
  expect_lte(degrade_ratio, 1)

  degrade_ratio <- updated_inputs$inchannel_habitat_juvenile["Stony Creek", "Jan", "1980"]/
    habitat_inputs$inchannel_habitat_juvenile["Stony Creek", "Jan", "1980"]

  expect_gte(degrade_ratio, DSMscenario::rear_decay_rate[["Stony Creek"]])
  expect_lte(degrade_ratio, 1)

  after <- updated_inputs$inchannel_habitat_fry["Feather River", 10 , 21]
  before <- habitat_inputs$inchannel_habitat_fry["Feather River", 10 , 21]
  added_habitat <- DSMhabitat::acres_to_square_meters(2)*10
  decay_rate <- ((after-added_habitat)/before)^(1/11)
  expect_gt(decay_rate, DSMscenario::spawn_decay_rate[["Feather River"]])
})

test_that("floodplain doesn't degrade and add works", {

  expect_equal(updated_inputs$floodplain_habitat["Antelope Creek", "Jan", "1982"],
               habitat_inputs$floodplain_habitat["Antelope Creek", "Jan", "1982"])

  expect_gt(updated_inputs$floodplain_habitat["Upper Sacramento River", "Jan", "1982"],
            habitat_inputs$floodplain_habitat["Upper Sacramento River", "Jan", "1982"])
})

