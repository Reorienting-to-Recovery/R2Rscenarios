#' Load Scenario
#' @description Modify baseline model inputs based on a set of actions
#' @param scenario a list containing scenario information, see details below
#' @param habitat_inputs a list with spawning habitat, inchannel fry and juvenile rearing habitat,
#'    floodplain rearing habitat, and weeks flooded matrices from the lifecycle model
#'    "params" data object. See the example for more details.
#' @param species provide \code{"fr"}, \code{"lfr"}, \code{"wr"}, \code{"sr"}, or \code{"st"} for fall run, late-fall run,
#'    winter run, spring run, or steelhead respectively to designate which \code{params} data object
#'    to be modified. For example, supply \code{"fr"} if running the \code{fallRunDSM::fall_run_model}.
#' @param spawn_decay_rate length 31 vector of 1 - spawning decay rate estimates
#' @param rear_decay_rate length 31 vector of 1 - rearing decay rate estimates
#' @param stochastic boolean, TRUE for creating scenarios with stochasticity
#' @details
#' A scenario is a list of 31 by 20 matrices with each value representing the units
#' of effort to apply in a tributarty in a given year for a action type.
#'
#' The scenario list should be formatted like this:
#'
#' \code{list(spawn = matrix(), inchannel = matrix(), floodplain = matrix(), survival = matrix())}
#'
#' A scenario can also include a boolean vector named \code{no_decay} if the user
#' desires to exclude a watershed from spawning and rearing habitat decay.
#' @examples
#' scenario_df <- dplyr::tibble(watershed = "Lower Sacramento River",
#'                       action = 3,
#'                       start_year = 1980,
#'                       end_year = 1999,
#'                       units_of_effort = 1)
#' no_decay_alt <- watershed_labels %in% c("Clear Creek", "Butte Creek", "Upper Sacramento River")
#' names(no_decay_alt) <- watershed_labels
#' custom_scenario <- get_action_matrices(scenario_df)
#' custom_scenario$no_decay <- no_decay_alt
#'
#' habitats <- list(
#'   spawning_habitat = fallRunDSM::params$spawning_habitat,
#'   inchannel_habitat_fry = fallRunDSM::params$inchannel_habitat_fry,
#'   inchannel_habitat_juvenile = fallRunDSM::params$inchannel_habitat_juvenile,
#'   floodplain_habitat = fallRunDSM::params$floodplain_habitat,
#'   weeks_flooded = fallRunDSM::params$weeks_flooded
#' )
#'
#' scenario_custom <- load_scenario(scenario = custom_scenario,
#'                                  species = R2Rscenario::species$FALL_RUN,
#'                                  habitat_inputs = habitats)
#'
#' scenario_one <- load_scenario(scenario = R2Rscenario::scenarios$ONE,
#'                               species = R2Rscenario::species$FALL_RUN,
#'                               habitat_inputs = habitats)
#' @export
load_scenario <- function(scenario, habitat_inputs,
                          species = c("fr", "wr", "sr", "st", "lfr"),
                          spawn_decay_rate = ..params$spawn_decay_rate,
                          rear_decay_rate = ..params$rear_decay_rate,
                          spawn_decay_multiplier = ..params$spawn_decay_multiplier,
                          stochastic = TRUE) {

  species <- match.arg(species)

  spawn_theoretical_habitat_max <- switch(species,
                                          "fr" = R2Rscenario::max_spawn_area$FALL,
                                          "wr" = R2Rscenario::max_spawn_area$WINTER,
                                          "sr" = R2Rscenario::max_spawn_area$SPRING,
                                          "st" = R2Rscenario::max_spawn_area$STEELHEAD,
                                          "lfr" = R2Rscenario::max_spawn_area$LATE_FALL)

  rear_theoretical_habitat_max <- switch(species,
                                         "fr" = R2Rscenario::max_rear_area$FALL,
                                         "wr" = R2Rscenario::max_rear_area$WINTER,
                                         "sr" = R2Rscenario::max_rear_area$SPRING,
                                         "st" = R2Rscenario::max_rear_area$STEELHEAD,
                                         "lfr" = R2Rscenario::max_rear_area$LATE_FALL)
  one_acre <- 4046.86
  two_acres <- 8093.72
  three_acres <- 12140.59

  decay <- decay_amount_matrices(spawn_years = dim(habitat_inputs$spawning_habitat)[3],
                                 rear_years = dim(habitat_inputs$inchannel_habitat_fry)[3],
                                 spawn_decay_rate = spawn_decay_rate,
                                 rear_decay_rate = rear_decay_rate,
                                 species = species,
                                 no_decay_tribs = scenario$no_decay,
                                 stochastic = stochastic)
  
  if (species == "fr" && !is.null(spawn_decay_multiplier)) {
    spawning_habitat <- modify_spawning_habitat(habitat = habitat_inputs$spawning_habitat,
                                                action_units = scenario$spawn,
                                                amount = one_acre,
                                                stochastic = stochastic,
                                                theoretical_max = spawn_theoretical_habitat_max,
                                                decay_multipliers = spawn_decay_multiplier)
  } else {
    spawning_habitat <- modify_habitat(habitat = habitat_inputs$spawning_habitat,
                                       action_units = scenario$spawn,
                                       amount = one_acre,
                                       decay = decay$spawn,
                                       theoretical_max = spawn_theoretical_habitat_max,
                                       stochastic = stochastic)
  }

  spawning_habitat <- modify_habitat(habitat = habitat_inputs$spawning_habitat,
                                     action_units = scenario$spawn,
                                     amount = one_acre,
                                     decay = decay$spawn,
                                     theoretical_max = spawn_theoretical_habitat_max,
                                     stochastic = stochastic)

  inchannel_habitat_fry <- modify_habitat(habitat = habitat_inputs$inchannel_habitat_fry,
                                          action_units = scenario$inchannel,
                                          amount = two_acres,
                                          decay = decay$rear,
                                          theoretical_max = rear_theoretical_habitat_max,
                                          stochastic = stochastic)

  inchannel_habitat_juvenile <- modify_habitat(habitat = habitat_inputs$inchannel_habitat_juvenile,
                                               action_units = scenario$inchannel,
                                               amount = two_acres,
                                               decay = decay$rear,
                                               theoretical_max = rear_theoretical_habitat_max,
                                               stochastic = stochastic)

  floodplain_habitat <- modify_floodplain_habitat(habitat = habitat_inputs$floodplain_habitat,
                                                  weeks_flooded = habitat_inputs$weeks_flooded,
                                                  action_units = scenario$floodplain,
                                                  amount = three_acres,
                                                  stochastic = stochastic)

  survival_adjustment <- modify_survival(scenario$survival)

  return(list(spawning_habitat = spawning_habitat,
              inchannel_habitat_fry = inchannel_habitat_fry,
              inchannel_habitat_juvenile = inchannel_habitat_juvenile,
              floodplain_habitat = floodplain_habitat$habitat,
              weeks_flooded = floodplain_habitat$weeks_flooded,
              survival_adjustment = survival_adjustment))
}

#' Modify Survival
#' @description Adds 5\% to the survival rate per each action unit applied
#' @param actions_units matrix of actions units
#' @noRd
modify_survival <- function(action_units) {
  (action_units * .01) + 1
}

#' Modify Spawning Habitat
#' TODO can decay be passed in as argument? for sensitivity analysis
#' @export
modify_spawning_habitat <- function(habitat, action_units, amount, theoretical_max, stochastic,
                                    decay_multipliers) {
  
  years <- dim(habitat)[3]
  
  if (stochastic) {
    amounts <- add_parital_controllability(amount, 31*years)
  } else {
    amounts <- rep(amount, 31*years)
  }
  
  amount_matrix <- matrix(amounts, nrow = 31)
  
  cumulative_amount_matrix <- t(apply(amount_matrix*action_units, MARGIN = 1, cumsum))
  
  # for each month within a year, add or degrade same volume of habitat
  for (i in 1:12) {
    # add habitat by number of units
    habitat[ , i, ] <- habitat[ , i, ] + cumulative_amount_matrix
  }
  
  for (i in 1:31) {
    habitat[i,,] <- habitat[i,,] * decay_multipliers[i,,]
  }
  
  # Do not let habitat amount exceed theoretical habitat maximum for spawn and inchannel rearing
  habitat <- pmin(habitat, theoretical_max)
  
  
  return(habitat)
}

#' Modify Inchannel Habitat
#' @description Change amount of spawning or inchannel rearing habitat
#' @details
#' This function adds a given amount of habitat per action unit applied within a watershed.
#' The amount of habitat is variable to reflect partial controllability of restoration projects.
#' The habitat is added to the base amount of existing habitat for that year and each year after.
#' If no action is taken on a regulated watershed, then decay is applied for that year
#' and each following year.
#' @param habitat a 3-dimensional array [watersheds, months, years] containing spawning or
#'     inchannel rearing habitat amounts in square meters
#' @param action_units a matrix [watersheds, years] containing the count of actions taken in a watershed per year
#' @param amount amount of habitat to add in square meters
#' @param decay a matrix [watersheds, years] containing a decay rate scalar
#' @noRd
modify_habitat <- function(habitat, action_units, amount, decay = NULL, theoretical_max = NULL, stochastic) {

  years <- dim(habitat)[3]

  if (stochastic) {
    amounts <- add_parital_controllability(amount, 31 * years)
  } else {
    amounts <- rep(amount, 31 * years)
  }

  amount_matrix <- matrix(amounts, nrow = 31)

  cumulative_amount_matrix <- t(apply(amount_matrix * action_units, MARGIN = 1, cumsum))
  annual_decay <- as.numeric(!action_units) * decay

  for (i in 1:nrow(annual_decay)) {
    annual_decay[i, annual_decay[i,] != 0] <- cumprod(annual_decay[i, annual_decay[i,] != 0])
  }
  cumulative_decay_matrix <- replace(annual_decay, annual_decay == 0, 1)

  # for each month within a year, add or degrade same volume of habitat
  for (i in 1:12) {
    # add habitat by number of units
    habitat[ , i, ] <- habitat[ , i, ] + cumulative_amount_matrix
    # degrade habitat if none was added
    habitat[ , i, ] <- habitat[ , i, ] * cumulative_decay_matrix
  }

  # Do not let habitat amount exceed theoretical habitat maximum for spawn and inchannel rearing
  habitat <- pmin(habitat, theoretical_max)


  return(habitat)
}

#' Modify Floodplain Habitat
#' @description Change amount of floodplain habitat and weeks flooded
#' @param habitat a 3-dimensional array [watersheds, months, years] containing floodplain rearing
#'    habitat amounts in square meters
#' @param weeks_flooded a 3-dimensional array [watersheds, months, years] containing number of weeks flooded
#' @param action_units a matrix [watersheds, years] containing the count of actions taken in a watershed per year
#' @param amount amount of habitat to add in square meters
#' @noRd
modify_floodplain_habitat <- function(habitat, weeks_flooded, action_units, amount,
                                      stochastic) {
  # partial controllability of building 3 acres
  years <- dim(habitat)[3]

  if (stochastic) {
    amounts <- add_parital_controllability(amount, 31*years)
  } else {
    amounts <- rep(amount, 31*years)
  }

  amount_matrix <- matrix(amounts, nrow = 31)

  # acres built times unit of effort with acres built accumulating
  cumulative_amount_matrix <- t(apply(amount_matrix*action_units, MARGIN = 1, cumsum))

  for (year in 1:years) {
    if (stochastic) {
      # floodplain built activated 2 out of 3 years
      if (rbinom(1, 1, 0.67)) {
        # floodplain active 2 months between 1-4
        first_active_month <- sample(1:3, 1)
        active_months <- c(first_active_month, first_active_month + 1)
        habitat[ , active_months, year] <- habitat[ , active_months, year] + cumulative_amount_matrix[ , year]
        weeks_flooded[ , active_months, year] <- pmax(weeks_flooded[ , active_months, year], 2)
      }
    } else {
      if (year %in% which(1:21 %% 3 != 0)) {
        active_months <- c(1, 3)
        habitat[ , active_months, year] <- habitat[ , active_months, year] + cumulative_amount_matrix[ , year]
        weeks_flooded[ , active_months, years] <- pmax(weeks_flooded[ , active_months, years], 2)
      }
    }
  }

  return(list(habitat = habitat, weeks_flooded = weeks_flooded))

}

#' Add Partial Controllability to Habitat Amount
#' Randomly increase or decrease amount of habitat created
#' @param sqm square meter of base habitat amount
#' @param n the number of amounts to generate
#' @noRd
add_parital_controllability <- function(sqm, n = 1) {
  sqm * pmin(pmax(rgamma(n, 44.44444, scale = 0.02250), 0.5), 1.5)
}

#' Decay Matrices
#' @description Generates matrix [31 watersheds, years] of decay rates for spawning
#'    and inchannel rearing
#' @noRd
decay_amount_matrices <- function(spawn_years, rear_years, spawn_decay_rate,
                                  rear_decay_rate, species, no_decay_tribs,
                                  stochastic) {


    spawn_decay_amount <- t(sapply(1:31, function(index) {
      if (stochastic) {
        runif(spawn_years, min = spawn_decay_rate[index], max = 1)
      } else {
        rep(mean(c(spawn_decay_rate[index], 1)), spawn_years)
      }
    }))

    rear_decay_amount <- t(sapply(1:31, function(index) {
      if (stochastic) {
        runif(rear_years, min = rear_decay_rate[index], max = 1)
      } else {
        rep(mean(c(rear_decay_rate[index], 1)), rear_years)
      }
    }))


  # remove decay from non-regulated tribs and Bypasses and San Joaquin River
  tribs_with_no_decay <-if (!is.null(no_decay_tribs)) {
      !R2Rscenario::regulated_watersheds | (R2Rscenario::watershed_groups > 7) | no_decay_tribs
  } else {
    !R2Rscenario::regulated_watersheds | (R2Rscenario::watershed_groups > 7)
  }

  spawn_decay_amount[tribs_with_no_decay, ] <- 1
  rear_decay_amount[tribs_with_no_decay, ] <- 1

  return(list(spawn = spawn_decay_amount, rear = rear_decay_amount))

}

#' Get Action Matrices
#' @description Converts scenario dataframe into a list of matrices [31 watersheds, years]
#'    for each action type. The matrix values are the number of units of effort applied
#'    for that action type within a watershed for that year.
#' @param scenario_df scenario dataframe
#' @details
#' The \code{scenario_df} is a dataframe with each row representing a scenario action.
#' The dataframe must contain the following columns:
#' \itemize{
#'   \item \strong{watershed} - The name or index for a watershed, ex. "Upper Sacramento River" or 1
#'   \item \strong{action} - The action taken represented by the code 1 - 5, ex: 3 to represent adding inchannel rearing
#'   \item \strong{start_year} - The simulation year the action begins
#'   \item \strong{end_year} - The simulation year the action ends
#'   \item \strong{units_of_effort} - number of action units taken, ex: .5 or 1
#' }
#' @section Actions and Units of Effort:
#' \itemize{
#'   \item \strong{1} - Do nothing
#'   \item \strong{2} - Add 1 acre of spawning habitat
#'   \item \strong{3} - Add 1 acres of inchannel rearing habitat
#'   \item \strong{4} - Add 1 acres of floodplain rearing habitat
#'   \item \strong{5} - Increase rearing survival by 1\%
#'   \item \strong{6} - Increase migratory survival by 1\%
#'   \item \strong{7} - Increase prespawn survival by 1\%
#'   \item \strong{8} - Increase growth by moving up 1 category in bioenergetic transition (ex: med to high)
#'
#' }
#' @export
get_action_matrices <- function(scenario_df) {

  spawn_actions <- get_action_units(scenario_df, action_type = 2)
  ic_actions <- get_action_units(scenario_df, action_type = 3)
  flood_actions <- get_action_units(scenario_df, action_type = 4)
  rearing_survival_actions <- get_action_units(scenario_df, action_type = 5)
  migratory_survival_actions <- get_action_units(scenario_df, action_type = 6)
  
  return(list(spawn = spawn_actions, inchannel = ic_actions,
              floodplain = flood_actions, survival = survival_actions))
}

#' Get Action Units
#' @description Produces a matrix for an action type where the values are the number
#'    of units of effort applied for a watershed that year.
#' @param scenario_df scenario dataframe
#' @param action_type action_type 1-5
#' @noRd
get_action_units <- function(scenario_df, action_type) {

  actions <- subset(scenario_df, action == action_type)
  watersheds <- purrr::map_df(seq_len(nrow(actions)), function(index) {
    row <- actions[index, ]
    data.frame(
      year = seq(row$start_year, row$end_year),
      watershed = row$watershed,
      units_of_effort = row$units_of_effort
    )
  })

  start_year = ifelse(action_type == 2, 1979, 1980)

  action_units <- dplyr::bind_rows(
    expand.grid(
      year = start_year:2000,
      watershed = R2Rscenario::watershed_labels,
      units_of_effort = 0
    ),
    watersheds) %>%
    dplyr::group_by(year, watershed) %>%
    dplyr::summarise(units_of_effort = sum(units_of_effort)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(year, units_of_effort) %>%
    dplyr::mutate(watershed = factor(watershed, levels = R2Rscenario::watershed_labels)) %>%
    dplyr::arrange(watershed) %>%
    dplyr::select(-watershed) %>%
    as.matrix()

  row.names(action_units) <- R2Rscenario::watershed_labels

  return(action_units)

}


