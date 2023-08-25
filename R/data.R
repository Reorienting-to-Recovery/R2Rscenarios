#' Watershed Labels
#' @description An ordered vector of watershed names included in the CVPIA SIT's
#'     Decision Support Models
#' @format NULL
"watershed_labels"

#' Decay Rates
#' @description An ordered vector of minimum decay rate for each watershed
#' @name decay_rate


#' @rdname decay_rate
#' @format NULL
"spawn_decay_rate"

#' @rdname decay_rate
#' @format NULL
"rear_decay_rate"

#' SIT Watershed Groupings
#' @description SIT defined watershed diversity groups developed from the Central Valley Chinook Salmon and Steelhead Recovery Plan \href{https://archive.fisheries.noaa.gov/wcr/publications/recovery_planning/salmon_steelhead/domains/california_central_valley/cv_chin_stlhd_r_plan_fs_071614.pdf}{NOAA 2014}
"watershed_groups"

#' Regulated Watersheds
#' @description An ordered vector of boolean values, 1 represents a major dam
#'   on the watershed.
"regulated_watersheds"

#' Species
#' @description A helper list object for setting the species value when running \code{load_scenario}
#' @examples
#' habitats <- list(
#'   spawning_habitat = fallRunDSM::params$spawning_habitat,
#'   inchannel_habitat_fry = fallRunDSM::params$inchannel_habitat_fry,
#'   inchannel_habitat_juvenile = fallRunDSM::params$inchannel_habitat_juvenile,
#'   floodplain_habitat = fallRunDSM::params$floodplain_habitat,
#'   weeks_flooded = fallRunDSM::params$weeks_flooded
#' )
#'
#' scenario_df <- data.frame(watershed = c("Upper Sacramento River",
#'                                         "Upper Sacramento River",
#'                                         "American River", "Feather River",
#'                                         "Lower-mid Sacramento River",
#'                                         "Battle Creek", "Butte Creek",
#'                                         "Deer Creek", "Stanislaus River"),
#'                           action = c(3, 3, 3, 3, 3, 3, 3, 3, 3),
#'                           start_year = c(1980, 1990, 1980, 1980, 1980, 1990,
#'                                          1990, 1990, 1990),
#'                           end_year = c(1989, 1999, 1989, 1989, 1989, 1999,
#'                                          1999, 1999, 1999),
#'                           units_of_effort = c(2, 1, 1, 1, 1, 1, 1, 1, 1))
#'
#' scenario <- load_scenario(scenario = R2Rscenario::scenarios$ONE,
#'                           habitat_inputs = habitats,
#'                           species = R2Rscenario::species$FALL,
#'                           spawn_decay_rate = R2Rscenario::spawn_decay_rate,
#'                           rear_decay_rate = R2Rscenario::rear_decay_rate,
#'                           stochastic = TRUE)
"species"

#' Theoretical Maximum Habitat
#' @description A list for each species containing an ordered vector of the
#' maximum possible suitable area for each watershed
#' @details Suitable habitat modeling captures degraded existing habitat conditions.
#' For the purposes of scenario development, we assumed that existing maximum habitat
#' (which occurs at the most suitable flow conditions in the watershed) can be increased
#' by a maximum of 100% over the twenty year simulation period. This reflects both
#' feasibility of habitat restoration over twenty years as well as physical limitations
#' (e.g. homes, levees, bridges, etc.) on habitat conditions.
#' @examples
#' # Subset by run to find max spawn and rear habitat area
#' R2Rscenario::max_spawn_area$FALL
#' R2Rscenario::max_rear_area$FALL
#' @name max_habitat
NULL

#' @rdname max_habitat
#' @format NULL
"max_spawn_area"

#' @rdname max_habitat
#' @format NULL
"max_rear_area"
