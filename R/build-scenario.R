#' Load Scenario
#' @description Modify baseline model inputs based on a set of actions
#' @param scenario a list containing scenario information, see details below
#' @param params Lifecycle model"params" data object. See the example for more details.
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
#' scenario_df <- data.frame(watershed = rep("All", 5), 
#'                           action = c(2, 14, 16, 18, 21), 
#'                           years = rep("All", 5))
#' 
#' custom_scenario <- get_action_matrices()
#'
#' params <- fallRunDSM::r_to_r_baseline_params
#'
#' scenario_custom <- load_scenario(scenario = custom_scenario,
#'                                  species = R2Rscenario::species$FALL_RUN,
#'                                  params = params)
#'
#' @export
load_scenario <- function(scenario, params = fallRunDSM::r_to_r_baseline_params,
                          species = c("fr", "wr", "sr", "st", "lfr"),
                          spawn_decay_rate = fallRunDSM::r_to_r_baseline_params$spawn_decay_rate,
                          rear_decay_rate = fallRunDSM::r_to_r_baseline_params$rear_decay_rate,
                          spawn_decay_multiplier = fallRunDSM::r_to_r_baseline_params$spawn_decay_multiplier) {
  
  ### SET UP -------------------------------------------------------------------
  species <- match.arg(species)
  action_numbers <- scenario$action
  max_habitat <- case_when(species == "fr" ~ fallRunDSM::r_to_r_tmh_params
                           # TODO add in SR and WR 
                           # species == "sr" ~ springRunDSM::r_to_r_tmh_params
                           # species == "wr" ~ winterRunDSM::r_to_r_tmh_params # TODO add this in 
                           )
  
  starting_hydrology <- case_when(22 %in% scenario$action ~ "biop_itp_2018_2019", 
                                  23 %in% scenario$action ~ "eff_sac", 
                                  24 %in% scenario$action ~ "LTO_12a",  
                                  31 %in% scenario$action ~ "LTO_12a_eff_dy")
  
  starting_habitat <- case_when(1 %in% action_numbers & 
                                  starting_hydrology == "biop_itp_2018_2019" ~ "r_to_r_baseline",
                                2 %in% action_numbers & 
                                  starting_hydrology == "biop_itp_2018_2019" ~ "r_to_r_tmh", 
                                1 %in% action_numbers & 
                                  starting_hydrology == "eff_sac" ~ "r_to_r_eff_baseline", 
                                2 %in% action_numbers & 
                                  starting_hydrology == "eff_sac" ~ "r_to_r_tmh_eff", 
                                3 %in% action_numbers & 
                                  starting_hydrology == "LTO_12a" ~ "r_to_r_hrl",
                                30 %in% action_numbers & 
                                  starting_hydrology == "LTO_12a_eff_dy" ~ "r_to_r_hrl_eff") 
  
  ### habitat modification -----------------------------------------------------
  updated_habitat <- apply_habitat_actions(scenario = scenario, 
                                           params = params, 
                                           starting_habitat = starting_habitat, 
                                           starting_hydrology = starting_hydrology,  
                                           species = species) 
  
  # decay_habitat 
  for (i in 1:31) {
    updated_habitat$spawning_habitat[i,,] <- updated_habitat$spawning_habitat[i,,] * spawn_decay_multiplier[i,,]
    updated_habitat$inchannel_habitat_fry[i,,] <- updated_habitat$inchannel_habitat_fry[i,,] * rear_decay_rate[i]
    updated_habitat$inchannel_habitat_juvenile[i,,] <- updated_habitat$inchannel_habitat_juvenile[i,,] * rear_decay_rate[i]
  }
  
  # Ensure that habitat additions are less than TMH 
  # updated_habitat$spawning_habitat <- pmin(updated_habitat$spawning_habitat, eval(parse(text = paste0("DSMhabitat::", species, "_spawn$r_to_r_tmh")))) 
  # updated_habitat$inchannel_habitat_fry <- pmin(updated_habitat$inchannel_habitat_fry, eval(parse(text = paste0("DSMhabitat::", species, "_fry$r_to_r_tmh")))) 
  # updated_habitat$inchannel_habitat_juvenile <- pmin(updated_habitat$inchannel_habitat_juvenile, eval(parse(text = paste0("DSMhabitat::", species, "_juv$r_to_r_tmh")))) 
  # updated_habitat$floodplain_habitat <- pmin(updated_habitat$floodplain_habitat, eval(parse(text = paste0("DSMhabitat::", species, "_fp$r_to_r_tmh"))))
  # updated_habitat$sutter_habitat <- pmin(updated_habitat$sutter_habitat, DSMhabitat::sutter_habitat$biop_itp_2018_2019)
  # updated_habitat$yolo_habitat <- pmin(updated_habitat$yolo_habitat, DSMhabitat::yolo_habitat$biop_itp_2018_2019)
  # updated_habitat$delta_habitat <- pmin(updated_habitat$delta_habitat, DSMhabitat::delta_habitat$r_to_r_tmh)
  
  ### harvest modifications ----------------------------------------------------
  updated_harvest <- apply_harvest_actions(scenario = scenario, 
                                           params = params, 
                                           species = species)
    
  
  
  ### hatchery modifications ---------------------------------------------------
  updated_hatchery <- apply_hatchery_actions(scenario = scenario, 
                                             params = params, 
                                             species = species)
    
  ### hydrology modifications --------------------------------------------------
  updated_hydrology <- apply_hydrology_actions(scenario = scenario,
                                               params = params, 
                                               starting_hydrology = starting_hydrology, 
                                               species = species) 
  
  ### apply updates only to specific watersheds and or years (ex just dry years)
  # create table that has year, param, and boolean use updated or not 
  all_param_updates <- pmap(scenario, expand_row) |> 
    reduce(bind_rows) |> 
    select(watershed, year, param) |> 
    distinct()
  
  
  # create final param object
  updated_params <- c(updated_habitat, updated_harvest, 
                      updated_hatchery, updated_hydrology)
  # initial final params as baseline params 
  final_params <- params
  
  # map through each row in the all_param_updates to pull in updates 
  for (i in 1:nrow(all_param_updates)) {
    selected_watershed <- all_param_updates[i, "watershed"] |> as.character()
    selected_year <- all_param_updates[i, "year"] |> unlist()
    selected_param <- all_param_updates[i, "param"] |> as.character()
    scenario_param <- update_param(final_params = final_params,
                                 updated_params = updated_params, 
                                 watershed = selected_watershed, 
                                 year = selected_year, 
                                 param_name = selected_param)
    final_params[[selected_param]] <- scenario_param[[selected_param]]
   }
  # could do helper functions for each watershed 
  # All non action specified years, leave as baseline inputs (these are in params)

  return(final_params)
}

#' Apply Habitat Actions to Update Habitat Conditions
#'
#' This function updates habitat conditions based on the provided scenario, parameters, starting habitat, starting hydrology, and species.
#'
#' @param scenario A list containing the actions to be applied to the habitat.
#' @param params A list of parameters including prey density, prop_high_predation, contact_points, delta_contact_points, delta_prop_high_predation.
#' @param starting_habitat A string specifying the starting habitat type.
#' @param starting_hydrology A string specifying the starting hydrology type.
#' @param species A string specifying the species for which habitat is being updated.
#'
#' @return A list containing the updated habitat conditions for spawning habitat, inchannel habitat for fry and juvenile stages, floodplain habitat, yolo habitat, sutter habitat, delta habitat, weeks flooded, average temperature, and degree days.
#' 
#' @details
#' The function performs several actions based on the provided scenario:
#' \itemize{
#'   \item Updates habitat conditions based on the species and starting habitat type actions 1 - 3.
#'   \item Applies action 4 to update yolo habitat based on rice lands salmon rearing practice.
#'   \item Applies action 5 to increase prey density.
#'   \item Applies action 6 to decrease predation by scaling contact points.
#' }
#'
#' @examples
#' \dontrun{
#' scenario <- data.frame('watershed = c(rep("All", 6), "Sacramento River"),
#'                        action = c(2, 5, 6, 14, 16, 19, 23),
#'                        years =  rep("All", 7))
#' params <- fallRunDSM::r_to_r_baseline_params
#' starting_habitat <- "r_to_r_tmh_eff"
#' starting_hydrology <- "sac_eff"
#' species <- "fr"
#' updated_habitat <- apply_habitat_actions(scenario, params, starting_habitat, starting_hydrology, species)
#' 
#' @export
apply_habitat_actions <- function(scenario, params, starting_habitat, starting_hydrology, species) {
  #TODO add check to ensure they have picked one base hydrology 
  
  # get updated habitat for species and hydrology/starting habitat type 
  updated_habitat <- list(spawning_habitat = eval(parse(text = paste0("DSMhabitat::", species, "_spawn$", starting_habitat))), 
                          inchannel_habitat_fry = eval(parse(text = paste0("DSMhabitat::", species, "_fry$", starting_habitat))), 
                          inchannel_habitat_juvenile = eval(parse(text = paste0("DSMhabitat::", species, "_juv$", starting_habitat))),
                          floodplain_habitat = eval(parse(text = paste0("DSMhabitat::", species, "_fp$", starting_habitat))),
                          # ONLY VARY BY CALSIM
                          yolo_habitat = DSMhabitat::yolo_habitat$biop_itp_2018_2019,  # no updates for VA
                          sutter_habitat = case_when(starting_habitat == "r_to_r_hrl" ~ DSMhabitat::sutter_habitat$r_to_r_hrl,
                                                     TRUE ~ DSMhabitat::sutter_habitat$biop_itp_2018_2019),
                          delta_habitat = case_when(starting_habitat %in% c("r_to_r_tmh_eff", "r_to_r_tmh") ~ DSMhabitat::delta_habitat$r_to_r_tmh,
                                                      starting_habitat %in% c("r_to_r_eff_baseline", "r_to_r_baseline") ~ DSMhabitat::delta_habitat$r_to_r_baseline,
                                                    starting_habitat == "r_to_r_hrl" ~ DSMhabitat::delta_habitat$r_to_r_hrl), 
                          weeks_flooded = case_when(starting_hydrology == "biop_itp_2018_2019" ~ DSMhabitat::weeks_flooded$biop_itp_2018_2019,
                                                      starting_hydrology == "eff_sac" ~ DSMhabitat::weeks_flooded$eff_sac, #TODO broken figure out fix
                                                      starting_hydrology == "LTO_12a" ~ DSMhabitat::weeks_flooded$lto_12a,
                                                    starting_hydrology == "LTO_12a_eff_dy" ~ DSMhabitat::weeks_flooded$lto_12a), 
                          # Note: these temp variables change with calsim updates
                          avg_temp = DSMtemperature::stream_temperature$biop_itp_2018_2019, #TODO update once we have VA
                          degree_days = DSMtemperature::degree_days$biop_itp_2018_2019, # TODO also add in degree days above dam logic stuff 
                          prey_density <- params$prey_density, # matrix("hi", nrow = 31, ncol = 20),
                          prey_density_delta <- params$prey_density_delta #matrix("hi", nrow = 2, ncol = 20)
                          )
  
  # additional layers 
  # 4 - Rice Lands Salmon Rearing Practice Standard 
  # Currently only in yolo and in months 1 and 2 based on what Skyler/Paul provided however we could try and add to sutter too, (esp with VA)
  if (4 %in% scenario$action) {
    updated_habitat$yolo_habitat[1:2,] <- updated_habitat$yolo_habitat[1:2,] * DSMhabitat::acres_to_square_meters(9000)
  } 
  
  # 5 - Increase prey density 
  if (5 %in% scenario$action) {
    updated_habitat$prey_density <- matrix("hi", nrow = 31, ncol = 20)
    updated_habitat$prey_density_delta <- matrix("hi", nrow = 2, ncol = 20)
  } else if(7 %in% scenario$action) {
    updated_habitat$prey_density <- params$prey_density
    updated_habitat$prey_density[17,] <- rep("hi", 20) # Sutter Bypass
    updated_habitat$prey_density[21,] <- rep("hi", 20) # Colusa Basin, lower-mid sac
    updated_habitat$prey_density[24,] <- rep("hi", 20) # Colusa Basin, lower sac
  } 
  # 6 - Decrease predation (scale contact points by 1/3)
  if (6 %in% scenario$action) {
    updated_habitat$prop_high_predation = params$prop_high_predation * 1/3
    updated_habitat$contact_points= params$contact_points * 1/3
    updated_habitat$delta_contact_points = params$delta_contact_points * 1/3
    updated_habitat$delta_prop_high_predation = params$delta_prop_high_predation * 1/3
  } 
  if(8 %in% scenario$action) {
    updated_habitat$contact_points <- params$contact_points
    updated_habitat$contact_points["Feather River"] = params$contact_points["Feather River"] - 1
    # check against e-mail/spreadsheet from Lisa Elliot
    updated_habitat$contact_points["Upper Sacramento River"] = params$contact_points["Upper Sacramento River"] - 9
    updated_habitat$contact_points["Antelope Creek"] = params$contact_points["Antelope Creek"] - 1
    updated_habitat$contact_points["Butte Creek"] = params$contact_points["Butte Creek"] - 1
  } else {
    updated_habitat$prop_high_predation = params$prop_high_predation 
    updated_habitat$contact_points = params$contact_points 
    updated_habitat$delta_contact_points = params$delta_contact_points 
    updated_habitat$delta_prop_high_predation = params$delta_prop_high_predation 
  }
  # TODO additional habitat modifications = adding acres and doing rice field practice
  
  if(27 %in% scenario$action) {
    updated_habitat$spawning_habitat = create_weir_effect_on_spawning_habitat(updated_habitat)
  }
  
  if(28 %in% scenario$action) {
    updated_habitat$inchannel_habitat_juvenile = create_spring_run_effect_on_fall_run_juvenile_habitat(updated_habitat)$inchannel_habitat_juv_sr_effect
    updated_habitat$floodplain_habitat = create_spring_run_effect_on_fall_run_juvenile_habitat(updated_habitat)$floodplain_habitat_sr_effect
  }
  
  if(29 %in% scenario$action) {
    # raise floodplain habitat in san joaquin and tributaries to half of theoretical max habitat
    updated_habitat$floodplain_habitat["San Joaquin River",,] <- DSMhabitat::fr_fp$r_to_r_tmh["San Joaquin River",,] * .5
    updated_habitat$floodplain_habitat["Merced River",,] <- DSMhabitat::fr_fp$r_to_r_tmh["Merced River",,] * .5
    updated_habitat$floodplain_habitat["Stanislaus River",,] <- DSMhabitat::fr_fp$r_to_r_tmh["Stanislaus River",,] * .5
    updated_habitat$floodplain_habitat["Tuolumne River",,] <- DSMhabitat::fr_fp$r_to_r_tmh["Tuolumne River",,] * .5
    updated_habitat$floodplain_habitat["Mokelumne River",,] <- DSMhabitat::fr_fp$r_to_r_tmh["Mokelumne River",,] * .5
    updated_habitat$floodplain_habitat["Calaveras River",,] <- DSMhabitat::fr_fp$r_to_r_tmh["Calaveras River",,] * .5
    updated_habitat$floodplain_habitat["Cosumnes River",,] <- DSMhabitat::fr_fp$r_to_r_tmh["Cosumnes River",,] * .5
  }
  
  if(32 %in% scenario$action) {
    scaling_factor <- 13
    updated_habitat$spawning_habitat = updated_habitat$spawning_habitat * scaling_factor 
    #updated_habitat$inchannel_habitat_fry = updated_habitat$inchannel_habitat_fry * scaling_factor 
    #updated_habitat$inchannel_habitat_juvenile = updated_habitat$inchannel_habitat_juvenile * scaling_factor
    updated_habitat$floodplain_habitat = updated_habitat$floodplain_habitat * scaling_factor
    #updated_habitat$yolo_habitat = updated_habitat$yolo_habitat * scaling_factor
    #updated_habitat$sutter_habitat = updated_habitat$sutter_habitat * scaling_factor
    #updated_habitat$delta_habitat = updated_habitat$delta_habitat * scaling_factor
  }
  
  return(updated_habitat)
}
#' Apply Harvest Actions to Update Harvest Parameters
#'
#' This function updates harvest parameters based on the provided scenario, parameters, and species.
#'
#' @param scenario A list containing the actions to be applied to the harvest parameters.
#' @param params A list of parameters including restrict_harvest_to_hatchery_ocean, restrict_harvest_to_hatchery_trib, ocean_harvest_percentage, tributary_harvest_percentage, crr_scaling, no_cohort_harvest_years, intelligent_crr_harvest, intelligent_hatchery_harvest, preserve_tribal_harvest.
#' @param species A string specifying the species for which harvest parameters are being updated.
#'
#' @return A list containing the updated harvest parameters.
#' 
#' @details
#' The function performs several actions based on the provided scenario:
#' \itemize{
#'   \item Updates harvest parameters based on the baseline values provided in params.
#'   \item Applies action 11 to preserve tribal harvest.
#'   \item Applies action 12 to set in-river harvest only.
#'   \item Applies action 13 to set ocean harvest only.
#'   \item Applies action 14 to enable intelligent CRR harvest.
#'   \item Applies action 15 to enable intelligent habitat harvest.
#'   \item Applies action 16 to restrict ocean harvest to hatchery fish only.
#'   \item Applies action 17 to restrict tributary harvest to hatchery fish only.
#' }
#'
#' @examples
#' \dontrun{
#' scenario <- data.frame('watershed = c(rep("All", 6), "Sacramento River"),
#'                        action = c(2, 5, 6, 14, 16, 19, 23),
#'                        years =  rep("All", 7))
#' params <- fallRunDSM::r_to_r_baseline_params
#' species <- "fr"
#' updated_harvest <- apply_harvest_actions(scenario, params, species)
#' }
#' 
#' @export
#' 
apply_harvest_actions <- function(scenario, params, species) {
  # * 10: Baseline Harvest (default run this first)
    updated_harvest <- list(restrict_harvest_to_hatchery_ocean = params$restrict_harvest_to_hatchery_ocean,
                            restrict_harvest_to_hatchery_trib = params$restrict_harvest_to_hatchery_trib,
                            ocean_harvest_percentage = params$ocean_harvest_percentage, 
                            tributary_harvest_percentage = params$tributary_harvest_percentage, 
                            crr_scaling = params$crr_scaling, 
                            no_cohort_harvest_years = params$no_cohort_harvest_years, 
                            intelligent_crr_harvest = params$intelligent_crr_harvest,
                            intelligent_habitat_harvest = params$intelligent_hatchery_harvest,
                            preserve_tribal_harvest = params$preserve_tribal_harvest)
  # * 11: Tribal harvest only 
  if (11 %in% scenario$action) { 
    updated_harvest$preserve_tribal_harvest = TRUE
  }
  # * 12: In river harvest only
  if (12 %in% scenario$action) { 
    updated_harvest$ocean_harvest_percentage = 0
  }
  # * 13: Ocean harvest only 
  if (13 %in% scenario$action) { 
    zero_harvest <- rep(0, 31)
    names(zero_harvest) <- fallRunDSM::watershed_labels
    updated_harvest$tributary_harvest_percentage = zero_harvest
  }
  # * 14: Intelligent CRR harvest
  if (14 %in% scenario$action) { 
    updated_harvest$intelligent_crr_harvest = TRUE
  }
  # * 15: Intelligent habitat harvest 
  if (15 %in% scenario$action) { 
    updated_harvest$intelligent_habitat_harvest = TRUE
  }
  # * 16: Harvest only hatchery fish
  if (16 %in% scenario$action) { 
    updated_harvest$restrict_harvest_to_hatchery_ocean = TRUE
  }
  # * 17: Harvest only hatchery fish
  if (17 %in% scenario$action) { 
    updated_harvest$restrict_harvest_to_hatchery_trib = TRUE
  }
  if (25 %in% scenario$action) { 
    # TODO might want to make this more generalizable to year...instead of just doing no harvest of dry year cohorts
    updated_harvest$no_cohort_harvest_years = c(2, 6, 8, 9, 10, 11, 12, 13, 15)
  }

  return(updated_harvest)
}

apply_hatchery_actions <- function(scenario, params, species) {
  # * 18: Baseline hatchery 
  updated_hatchery <- list(hatchery_release = params$hatchery_release, 
                           hatchery_release_proportion_bay = params$hatchery_release_proportion_bay,
                           terminal_hatchery_logic = params$terminal_hatchery_logic,
                           proportion_hatchery = params$proportion_hatchery) 
  # * 19: Only terminal hatchery / outplanting 
  if (19 %in% scenario$action) {
  updated_hatchery$terminal_hatchery_logic <-  TRUE
  params$hatchery_release[params$hatchery_release > 0] <- 0
  updated_hatchery$hatchery_release <- params$hatchery_release
  } 
  # * 20: Phased hatcheries 
  if (20 %in% scenario$action) {
    updated_hatchery$hatchery_release <- create_phased_hatchery_release(updated_hatchery, 
                                                                        strategy = "high_early_years") 
  } 
  # * 21: Release 50% in bay 
  # TODO confirm that works 
  if (21 %in% scenario$action) {
    updated_hatchery$hatchery_release_proportion_bay <- .5
  } 
  
  # * 26: install weir at hatchery, remove 20% hatchery? 
  # TODO confirm these methods
  if (26 %in% scenario$action) {
    updated_hatchery$proportion_hatchery = params$proportion_hatchery * .80
  }
  return(updated_hatchery)
}

apply_hydrology_actions <- function(scenario, params, starting_hydrology, species) {
  is_calsim_output_version <- ifelse(starting_hydrology %in% c("eff_sac", "LTO_12a_eff_dy"), FALSE, TRUE)
  if(is_calsim_output_version) {
    # no san_joaquin_flow object for calsims
    san_joaquin_flows <- matrix(0, nrow = 12, ncol = 21,
                                dimnames = list(month.abb, 1980:2000))
  } else {
    # use eff san_joaquin_flows
    san_joaquin_flows <- eval(parse(text = paste0("DSMflow::san_joaquin_flows$eff_sac")))
  }

  updated_hydrology <- list(upper_sacramento_flows = eval(parse(text = paste0("DSMflow::upper_sacramento_flows$", starting_hydrology))), 
                            san_joaquin_flows = san_joaquin_flows, # TODO confirm - call a SJ eff object (only san joaquin flows object rn)
                            freeport_flows = eval(parse(text = paste0("DSMflow::freeport_flow$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))), # defaults to 2019 biop if starting hydro is a manipulation on a calism run 
                            vernalis_flows = eval(parse(text = paste0("DSMflow::vernalis_flow$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            stockton_flows = eval(parse(text = paste0("DSMflow::stockton_flow$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            CVP_exports = eval(parse(text = paste0("DSMflow::cvp_exports$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))), 
                            SWP_exports = eval(parse(text = paste0("DSMflow::swp_exports$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            proportion_diverted = eval(parse(text = paste0("DSMflow::proportion_diverted$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            total_diverted = eval(parse(text = paste0("DSMflow::total_diverted$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            delta_proportion_diverted = eval(parse(text = paste0("DSMflow::delta_proportion_diverted$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            delta_total_diverted = eval(parse(text = paste0("DSMflow::delta_total_diverted$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            prop_pulse_flows = eval(parse(text = paste0("DSMflow::proportion_pulse_flows$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            delta_inflow = eval(parse(text = paste0("DSMflow::delta_inflow$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),
                            cc_gates_days_closed = eval(parse(text = paste0("DSMflow::delta_cross_channel_closed$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019"))))[1, ],
                            cc_gates_prop_days_closed = eval(parse(text = paste0("DSMflow::delta_cross_channel_closed$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019"))))[2, ],
                            proportion_flow_bypass = eval(parse(text = paste0("DSMflow::proportion_flow_bypasses$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))), 
                            gates_overtopped = eval(parse(text = paste0("DSMflow::gates_overtopped$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))), 
                            flows_oct_nov = eval(parse(text = paste0("DSMflow::hatchery_oct_nov_flows$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),  
                            flows_apr_may = eval(parse(text = paste0("DSMflow::hatchery_apr_may_flows$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))))
  return(updated_hydrology)
}

# helper functions 
expand_row <- function(watershed, years, action) {
  params_affected_by_action <- list("1" = list("spawning_habitat", "inchannel_habitat_fry",
                                               "inchannel_habitat_juvenile", "floodplain_habitat",
                                               "yolo_habitat", "sutter_habitat", "delta_habitat",
                                               "weeks_flooded", "avg_temp","degree_days"),
                                    "2" = list("spawning_habitat", "inchannel_habitat_fry",
                                               "inchannel_habitat_juvenile", "floodplain_habitat",
                                               "yolo_habitat", "sutter_habitat", "delta_habitat",
                                               "weeks_flooded", "avg_temp","degree_days"),
                                    "3" = list("spawning_habitat", "inchannel_habitat_fry",
                                               "inchannel_habitat_juvenile", "floodplain_habitat",
                                               "yolo_habitat", "sutter_habitat", "delta_habitat",
                                               "weeks_flooded", "avg_temp","degree_days"),
                                    "4" = list("yolo_habitat"),
                                    "5" = list("prey_density", "prey_density_delta"), 
                                    "6" = list("prop_high_predation", "contact_points", "delta_contact_points",
                                               "delta_prop_high_predation"),
                                    "7" = list("prey_density"),  
                                    "8" = list("contact_points"), 
                                    "9" = list(), # TODO 
                                    "10" = list(), # TODO this is just baseline so do not need to update anything? Confirm
                                    "11" = list("preserve_tribal_harvest"), 
                                    "12" = list("ocean_harvest_percentage"), 
                                    "13" = list("tributary_harvest_percentage"), 
                                    "14" = list("intelligent_crr_harvest"), 
                                    "15" = list("intelligent_habitat_harvest"), 
                                    "16" = list("restrict_harvest_to_hatchery_ocean"), 
                                    "17" = list("restrict_harvest_to_hatchery_trib"), 
                                    "18" = list(), # TODO this is just baseline so do not need to update anything? Confirm
                                    "19" = list("terminal_hatchery_logic", "hatchery_release"),
                                    "20" = list("hatchery_release"),
                                    "21" = list("hatchery_release_proportion_bay"),
                                    "22" = list("upper_sacramento_flows", "san_joaquin_flows", "freeport_flows", "vernalis_flows",
                                                "stockton_flows", "CVP_exports", "SWP_exports", "proportion_diverted",
                                                "total_diverted", "delta_proportion_diverted", "delta_total_diverted",
                                                "prop_pulse_flows", "delta_inflow", "cc_gates_days_closed", "cc_gates_prop_days_closed",
                                                "proportion_flow_bypass", "gates_overtopped", "flows_oct_nov", "flows_apr_may"),
                                    "23" = list("upper_sacramento_flows", "san_joaquin_flows", "freeport_flows", "vernalis_flows",
                                                "stockton_flows", "CVP_exports", "SWP_exports", "proportion_diverted",
                                                "total_diverted", "delta_proportion_diverted", "delta_total_diverted",
                                                "prop_pulse_flows", "delta_inflow", "cc_gates_days_closed", "cc_gates_prop_days_closed",
                                                "proportion_flow_bypass", "gates_overtopped", "flows_oct_nov", "flows_apr_may"),
                                    "24" = list("upper_sacramento_flows", "san_joaquin_flows", "freeport_flows", "vernalis_flows",
                                                "stockton_flows", "CVP_exports", "SWP_exports", "proportion_diverted",
                                                "total_diverted", "delta_proportion_diverted", "delta_total_diverted",
                                                "prop_pulse_flows", "delta_inflow", "cc_gates_days_closed", "cc_gates_prop_days_closed",
                                                "proportion_flow_bypass", "gates_overtopped", "flows_oct_nov", "flows_apr_may"),
                                    "25" = list("no_cohort_harvest_years"),
                                    "26" = list("proportion_hatchery"),
                                    "27" = list("spawning_habitat"),
                                    "28" = list("inchannel_habitat_juvenile", "floodplain_habitat"),
                                    "29" = list("floodplain_habitat"),
                                    "30" = list("spawning_habitat", "inchannel_habitat_fry",
                                                "inchannel_habitat_juvenile", "floodplain_habitat",
                                                "yolo_habitat", "sutter_habitat", "delta_habitat",
                                                "weeks_flooded", "avg_temp","degree_days"),
                                    "31" = list("upper_sacramento_flows", "san_joaquin_flows", "freeport_flows", "vernalis_flows",
                                                "stockton_flows", "CVP_exports", "SWP_exports", "proportion_diverted",
                                                "total_diverted", "delta_proportion_diverted", "delta_total_diverted",
                                                "prop_pulse_flows", "delta_inflow", "cc_gates_days_closed", "cc_gates_prop_days_closed",
                                                "proportion_flow_bypass", "gates_overtopped", "flows_oct_nov", "flows_apr_may"),
                                    "32" = list("spawning_habitat", "inchannel_habitat_fry",
                                                "inchannel_habitat_juvenile", "floodplain_habitat",
                                                "yolo_habitat", "sutter_habitat", "delta_habitat")
  )
  
  relevent_action_params <- params_affected_by_action[[as.character(action)]] |> unlist()
  
  params_to_update <- expand.grid(watershed = watershed, 
                                  param = relevent_action_params) |> 
    mutate(action = action,
           year = list(years))
  return(params_to_update)
}

update_param <- function(final_params = final_params, updated_params = updated_params, param_name, watershed, year) {
  selected_parameter <- param_name
  update_type <- param_type_lookup |> filter(param_name == selected_parameter) |> pull(update_type)
  if (update_type == "character(0)") warning(paste("There is no param named", selected_parameter, "in the param_type_lookup."))
  # TODO if has watershed == FALSE but action specified on watershed, through warning 
  # Includes
  has_watershed <- grepl(" wa",  update_type, fixed = TRUE)
  has_delta <- grepl(" delta", update_type)
  has_bypass <- grepl(" bypass", update_type)
  has_month <- grepl(" m", update_type)
  has_year <- grepl(" y", update_type)
  # Type 
  two_d_matrix <- grepl("2D", update_type)
  three_d_matrix <- grepl("3D", update_type)
  vector <- grepl("vector", update_type)
  single_value <- grepl("single value", update_type)
  list_of_matricies <- grepl("list of matrices", update_type)
  year <- unlist(year)
  
  if (any(watershed == "Sacramento River")) {
   watershed <- c("Upper Sacramento River", "Upper-mid Sacramento River", "Lower-mid Sacramento River", "Lower Sacramento River")
  } 
  
  # First deal with the ones that you will update the full params list 
  if (any(watershed == "All") & any(year == "All")) {
    final_params[[param_name]] <- updated_params[[param_name]]
  } 
  
  if (single_value) {
    final_params[[param_name]] <- updated_params[[param_name]]
  } else if (single_value & any(watershed != "All") & any(year != "All")) {
    # all these apply for all years and locations  
    message <- paste("The parameter:", param_name, "can not be adjusted by watershed and year. This param must remain consistant throughout the simulation.")
    warning(message)
  } 
  
  # Now update ones that have distinct years but are applied to all watersheds 
  if (any(watershed == "All") & any(year != "All") & has_year) {
    # year indexing depends on 
    if (three_d_matrix) {
      if (has_delta | has_bypass) {
        if (any(watershed %in% c("North Delta", "South Delta"))) {
          final_params[[param_name]][,year,watershed] <- updated_params[[param_name]][,year,watershed]
        } 
        if (any(watershed %in% c("Sutter Bypass", "Yolo Bypass"))){
          final_params[[param_name]][,year,watershed] <- updated_params[[param_name]][,year,watershed]
        }
      } else {
        final_params[[param_name]][,, year] <- updated_params[[param_name]][,, year]
      }
    }
    if (two_d_matrix){
      final_params[[param_name]][, year] <- updated_params[[param_name]][, year]
    }
    if (list_of_matricies) {
      final_params[[param_name]][year] <- updated_params[[param_name]][year]
    }
  } 
  
  # Now update ones with distinct watersheds but applied to all years 
  if (any(watershed != "All") & any(year == "All")) {
    # year indexing depends on 
    if (three_d_matrix) {
      if (has_watershed) {
        final_params[[param_name]][watershed,,] <- updated_params[[param_name]][watershed,,]
      }
      if (has_delta | has_bypass) {
        if (any(watershed %in% c("North Delta", "South Delta"))) {
        final_params[[param_name]][,,watershed] <- updated_params[[param_name]][,,watershed]
        } 
        if (any(watershed %in% c("Sutter Bypass", "Yolo Bypass"))){
          final_params[[param_name]][,,watershed] <- updated_params[[param_name]][,,watershed]
        }
      }
    }
    if (two_d_matrix & has_watershed & any(watershed %in% c("American River", "Battle Creek", "Feather River", "Merced River", "Mokelumne River"))){
      # only watershed 
      final_params[[param_name]][watershed, ] <- updated_params[[param_name]][watershed, ]
    } else if (two_d_matrix){
      # only watershed 
      final_params[[param_name]] <- updated_params[[param_name]]
    }
    # TODO check to see if we want a vector delta vs vector watershed
    if (vector){
      final_params[[param_name]][watershed] <- updated_params[[param_name]][watershed]
    }
    if (list_of_matricies) {
      # TODO we should udpate hatchery release to be in a different format
    }
  }
  
  # now update ones with distinct watersheds and years
  if (any(watershed != "All") & any(year != "All")) {
    # year indexing depends on 
    if (three_d_matrix) {
      if (has_watershed) {
        final_params[[param_name]][watershed, , year] <- updated_params[[param_name]][watershed,, year]
      }
      if (has_delta | has_bypass) {
        if (any(watershed %in% c("North Delta", "South Delta"))) {
          final_params[[param_name]][, year, watershed] <- updated_params[[param_name]][, year, watershed]
        } 
        if (any(watershed %in% c("Sutter Bypass", "Yolo Bypass"))){
          final_params[[param_name]][, year, watershed] <- updated_params[[param_name]][, year, watershed]
        }
        else {
          final_params[[param_name]][, year, ] <- updated_params[[param_name]][, year, ] 
          
        }
      }
    }
    if (two_d_matrix & has_year & has_watershed){
      # only watershed 
      final_params[[param_name]][watershed, year] <- updated_params[[param_name]][watershed, year]
    }
    if (vector){
      warning(paste("The selected param:", param_name, "does not allow you to specify year. Please update scenario" ))
    }
  }
  return(final_params)

}

# function for calculating effect of weir on spawning habitat
# uses baseline sr habitat

create_weir_effect_on_spawning_habitat <- function(habitat_object) {
  total_overlap_tribs <- c("Upper Sacramento River", "Antelope Creek", "Feather River",
                           "Mokelumne River", "Stanislaus River",
                           "Tuolumne River")
  # TODO calculate RM of overlap here - Emanuel?
  partial_overlap_tribs <- c("Big Chico Creek", "Clear Creek", "Mill Creek") # Clear Creek too, but SR exceeds FR habitat on Clear
  overlap <- array(dim = c(3, 12, 22))
  
  fr_spawning_habitat_with_weir <- habitat_object$spawning_habitat
  
  for(i in 1:22) {
    # update tribs with total overlap to reduce fall run by 1/3
    fr_spawning_habitat_with_weir[,,i][rownames(fr_spawning_habitat_with_weir[,,i]) %in% total_overlap_tribs, ] <- fr_spawning_habitat_with_weir[,,i][rownames(fr_spawning_habitat_with_weir[,,i]) %in% total_overlap_tribs, ] * .66
    
    # yuba habitat split at DPD
    # TODO update this with rates per mile and then apply
    fr_spawning_habitat_with_weir[,,i][rownames(fr_spawning_habitat_with_weir[,,i]) == "Yuba River", ] <- fr_spawning_habitat_with_weir[,,i][rownames(fr_spawning_habitat_with_weir[,,i]) == "Yuba River", ] / 2
    
    # subtract overlap from fall run
    overlap[,,i] <- abs(fr_spawning_habitat_with_weir[,,i][rownames(fr_spawning_habitat_with_weir[,,i]) %in% partial_overlap_tribs, ] -
                          DSMhabitat::sr_spawn$r_to_r_baseline[,,i][rownames(fr_spawning_habitat_with_weir[,,i]) %in% partial_overlap_tribs, ])
    
    # will need these values to be smaller, most likely
    fr_spawning_habitat_with_weir[,,i][rownames(fr_spawning_habitat_with_weir[,,i]) %in% partial_overlap_tribs, ] <- fr_spawning_habitat_with_weir[,,i][rownames(fr_spawning_habitat_with_weir[,,i]) %in% partial_overlap_tribs, ] -
      overlap[,,i]
    
  }
  return(fr_spawning_habitat_with_weir)
}

# function for calculating effect of spring run above dam habitat on fall run
create_spring_run_effect_on_fall_run_juvenile_habitat <- function(habitat_object) {
  
  # above dam habitat
  # for now, ding in-channel and floodplain habitat
  # TODO fry?
  # inchannel_habitat_fry_sr_effect <- DSMhabitat::fr_fry$r_to_r_baseline
  sr_tribs <- c("Upper Sacramento River", "Antelope Creek", "Feather River",
                "Mokelumne River", "Stanislaus River",
                "Tuolumne River", "Big Chico Creek", "Deer Creek", "Mill Creek",
                "Clear Creek", "Yuba River", "Battle Creek")
  sr_effect <- 0.9 # 10% effect
  
  inchannel_habitat_juv_sr_effect <- habitat_object$inchannel_habitat_juvenile
  floodplain_habitat_sr_effect <- habitat_object$floodplain_habitat
  
  for(i in 1:21) {
    inchannel_habitat_juv_sr_effect[,,i][rownames(inchannel_habitat_juv_sr_effect[,,i]) %in% sr_tribs, ] <- inchannel_habitat_juv_sr_effect[,,i][rownames(inchannel_habitat_juv_sr_effect[,,i]) %in% sr_tribs, ] * sr_effect
    floodplain_habitat_sr_effect[,,i][rownames(floodplain_habitat_sr_effect[,,i]) %in% sr_tribs, ] <- floodplain_habitat_sr_effect[,,i][rownames(floodplain_habitat_sr_effect[,,i]) %in% sr_tribs, ] * sr_effect
  }
  
  return(list("inchannel_habitat_juv_sr_effect" = inchannel_habitat_juv_sr_effect,
              "floodplain_habitat_sr_effect" = floodplain_habitat_sr_effect))
  
}

# create phased hatchery release
create_phased_hatchery_release <- function(hatchery_object, strategy = c("phased_closures",
                                                                         "high_early_years")) {
  phased_release <- hatchery_object$hatchery_release 
  
  if(strategy == "phased_closures") {
  
    # phase 1 - current release
    phased_release[1:5] <- list(hatchery_object$hatchery_release[[1]],
                                hatchery_object$hatchery_release[[1]],
                                hatchery_object$hatchery_release[[1]],
                                hatchery_object$hatchery_release[[1]],
                                hatchery_object$hatchery_release[[1]])
    
    # phase 2 - remove 2 or 5 FR hatcheries 
    # TODO update this logic for other runs
    remove_2_hatcheries <- hatchery_object$hatchery_release[[1]]
    
    # options we could remove 
    # coleman - battle creek, 
    # nibus - american river, 
    # feather - feather rivr, 
    # mokeulmne - mokelumne river, 
    # merced - merced river
    
    # Remove coleman ()
    remove_2_hatcheries["Battle Creek" , ] <- c(0, 0, 0 , 0)
    remove_2_hatcheries["Feather Creek" , ] <- c(0, 0, 0 , 0)
    
    phased_release[6:10] <- list(remove_2_hatcheries, 
                                 remove_2_hatcheries, 
                                 remove_2_hatcheries, 
                                 remove_2_hatcheries, 
                                 remove_2_hatcheries)
    
    # last 10 years release is at 0 
    no_hatchery <- matrix(0, ncol = 4, nrow = 31, dimnames = list(fallRunDSM::watershed_labels, c("s", "m", "l", "xl")))
    phased_release[11:20] <- rep(list(no_hatchery)[1], 10)
    
  } else if(strategy == "high_early_years") {
    # phased hatchery object
    for(i in 1:20) {
      if(i %in% 1:5) {
        # phased_release["Battle Creek",,i] <- 100000000 # increase from 12 million to 100 million
        # phased_release["Feather River",,i] <- 60000000 # increase from 6 million to 60 million
        # phased_release["Merced River",,i] <- 10000000 # increase from 1 million to 10 million
        # phased_release["American River",,i] <- 40000000 # increase from 4 million to 40 million
        # phased_release["Mokelumne River",,i] <- 50000000 # increase from 5 million to 50 million
        phased_release["Battle Creek",,i] <- phased_release["Battle Creek",,i] * 5
        phased_release["Feather River",,i] <- phased_release["Feather River",,i] * 5
        phased_release["Merced River",,i] <- phased_release["Merced River",,i] * 5
        phased_release["American River",,i] <- phased_release["American River",,i] * 5
        phased_release["Mokelumne River",,i] <- phased_release["Mokelumne River",,i] * 5
      } else if(i %in% 6:10) {
        # reduce each year by 50%
        phased_release["Battle Creek",,i] <- phased_release["Battle Creek",,i-1] * .4
        phased_release["Feather River",,i] <- phased_release["Feather River",,i-1] * .4
        phased_release["Merced River",,i] <- phased_release["Merced River",,i-1] * .4
        phased_release["American River",,i] <- phased_release["American River",,i-1] * .4
        phased_release["Mokelumne River",,i] <- phased_release["Mokelumne River",,i-1] * .4
      } else if(i %in% 11:20) {
        phased_release["Battle Creek",,i] <- 0 # all terminal
        phased_release["Feather River",,i] <- 0 # all terminal
        phased_release["Merced River",,i] <- 0 # all terminal
        phased_release["American River",,i] <- 0 # all terminal
        phased_release["Mokelumne River",,i] <- 0 # all terminal
      }
    }
  }
  
  return(phased_release)
  
}
