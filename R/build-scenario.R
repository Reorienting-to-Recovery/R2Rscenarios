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
  max_habitat <- case_when(species == "fr" ~ fallRunDSM::r_to_r_tmh_params
                           # TODO add in SR and WR 
                           # species == "sr" ~ springRunDSM::r_to_r_tmh_params
                           # species == "wr" ~ winterRunDSM::r_to_r_tmh_params # TODO add this in 
                           )
  
  starting_hydrology <- case_when(22 %in% scenario$action ~ "biop_itp_2018_2019", 
                                  23 %in% scenario$action ~ "eff_sac", 
                                  24 %in% scenario$action ~ "hrl" # todo when we get HRL 
                                  )
  
  starting_habitat <- case_when(1 %in% action_numbers & 
                                  starting_hydrology == "biop_itp_2018_2019" ~ "r_to_r_baseline",
                                2 %in% action_numbers & 
                                  starting_hydrology == "biop_itp_2018_2019" ~ "r_to_r_tmh", 
                                1 %in% action_numbers & 
                                  starting_hydrology == "feff_sac" ~ "r_to_r_eff_baseline", 
                                2 %in% action_numbers & 
                                  starting_hydrology == "eff_sac" ~ "r_to_r_tmh_eff", 
                                3 %in% action_numbers ~ "hrl"  #todo add this when we get HRL 
                                ) 
  
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
  updated_habitat$spawning_habitat <- pmin(updated_habitat$spawning_habitat, eval(parse(text = paste0("DSMhabitat::", species, "_spawn$r_to_r_tmh")))) 
  updated_habitat$inchannel_habitat_fry <- pmin(updated_habitat$inchannel_habitat_fry, eval(parse(text = paste0("DSMhabitat::", species, "_fry$r_to_r_tmh")))) 
  updated_habitat$inchannel_habitat_juvenile <- pmin(updated_habitat$inchannel_habitat_juvenile, eval(parse(text = paste0("DSMhabitat::", species, "_juv$r_to_r_tmh")))) 
  updated_habitat$floodplain_habitat <- pmin(updated_habitat$floodplain_habitat, eval(parse(text = paste0("DSMhabitat::", species, "_fp$r_to_r_tmh"))))
  updated_habitat$sutter_habitat <- pmin(updated_habitat$sutter_habitat, DSMhabitat::sutter_habitat$biop_itp_2018_2019)
  updated_habitat$yolo_habitat <- pmin(updated_habitat$yolo_habitat, DSMhabitat::yolo_habitat$biop_itp_2018_2019)
  updated_habitat$delta_habitat <- pmin(updated_habitat$delta_habitat, DSMhabitat::delta_habitat$r_to_r_tmh)
  
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
  
  # join all params update to param type lookup 
  
  
  # create final param object
  updated_params <- c(updated_habitat, updated_harvest, 
                      updated_hatchery, updated_hydrology)
  # initial final params as baseline params 
  final_params <- params
  
  # map through each row in the all_param_updates to pull in updates 
  update_param <- function(final_params, updated_params, param_name, watershed, year) {
    check_class <- class(final_params[[param_name]])
    check_dim <- ifelse(check_class == c("matrix", "array"), dim(final_params[[param_name]]), NA)
    # TODO if has watershed == FALSE but action specified on watershed, through warning 
    has_watershed <- ifelse(param_name %in% c("spawning_habitat", "inchannel_habitat_fry",
                                              "inchannel_habitat_juvenile", "floodplain_habitat",
                                              ))
    if (watershed == "All" & year == "All") {
      final_params[[param_name]] <- updated_params[[param_name]]
    } 
    else if (watershed == "All" & year != "All") {
      # year indexing depends on 
      
    }   
    else if (watershed != "All" & year == "All") {
      # year indexing depends on 
      
    }
  }
  pmap(update_param)
  # could do helper functions for each watershed 
  # All non action specified years, leave as baseline inputs (these are in params)

  return(final_params)
}




apply_habitat_actions <- function(scenario, params, starting_habitat, starting_hydrology, species) {
  #TODO add check to ensure they have picked one base hydrology 
  
  # get updated habitat for species and hydrology/starting habitat type 
  updated_habitat <- list(spawning_habitat = eval(parse(text = paste0("DSMhabitat::", species, "_spawn$", starting_habitat))), 
                            inchannel_habitat_fry = eval(parse(text = paste0("DSMhabitat::", species, "_fry$", starting_habitat))), 
                            inchannel_habitat_juvenile = eval(parse(text = paste0("DSMhabitat::", species, "_juv$", starting_habitat))),
                            floodplain_habitat = eval(parse(text = paste0("DSMhabitat::", species, "_fp$", starting_habitat))),
                            # ONLY VARY BY CALSIM
                            yolo_habitat = DSMhabitat::yolo_habitat$biop_itp_2018_2019,  #TODO update once we have VA
                            sutter_habitat = DSMhabitat::sutter_habitat$biop_itp_2018_2019,  #TODO update once we have VA
                            delta_habitat = case_when(starting_habitat %in% c("r_to_r_tmh_eff", "r_to_r_tmh") ~ DSMhabitat::delta_habitat$r_to_r_tmh,
                                                      starting_habitat %in% c("r_to_r_eff_baseline", "r_to_r_baseline") ~ DSMhabitat::delta_habitat$r_to_r_baseline), 
                            weeks_flooded = case_when(starting_hydrology == "biop_itp_2018_2019" ~ DSMhabitat::weeks_flooded$biop_itp_2018_2019,
                                                      starting_hydrology == "eff_sac" ~ DSMhabitat::weeks_flooded$eff_sac #TODO broken figure out fix
                                                      ), 
                            # Note: these temp variables change with calsim updates
                            avg_temp = DSMtemperature::stream_temperature$biop_itp_2018_2019, #TODO update once we have VA
                            degree_days = DSMtemperature::degree_days$biop_itp_2018_2019 # TODO also add in degree days above dam logic stuff 
                          )
  
  # additional layers 
  # 4 - Rice Lands Salmon Rearing Practice Standard 
  # Currently only in yolo and in months 1 and 2 based on what Skyler/Paul provided however we could try and add to sutter too, (esp with VA)
  if (4 %in% scenario$action) {
    updated_habitat$yolo_habitat[1:2,] <- updated_habitat$yolo_habitat[1:2,] * DSMhabitat::acres_to_square_meters(9000)
  } 
  
  # 5 - Increase prey density 
  if (5 %in% scenario$action) {
    updated_habitat$prey_density <- rep("max", 31)
    updated_habitat$prey_density_delta <- rep("max", 2)
  } else {
    updated_habitat$prey_density <- params$prey_density
    updated_habitat$prey_density_delta <- params$prey_density_delta
  }
  # 6 - Decrease predation (scale contact points by 1/3)
  if (6 %in% scenario$action) {
    updated_habitat$prop_high_predation = params$prop_high_predation * 2/3
    updated_habitat$contact_points= params$contact_points * 2/3
    updated_habitat$delta_contact_points = params$delta_contact_points * 2/3
    updated_habitat$delta_prop_high_predation = params$delta_prop_high_predation * 2/3
  } else {
    updated_habitat$prop_high_predation = params$prop_high_predation 
    updated_habitat$contact_points = params$contact_points 
    updated_habitat$delta_contact_points = params$delta_contact_points 
    updated_habitat$delta_prop_high_predation = params$delta_prop_high_predation 
  }
  # TODO additional habitat modifications = adding acres and doing rice field practice
  
  return(updated_habitat)
}

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

  return(updated_harvest)
}


apply_hatchery_actions <- function(scenario, params, species) {
  # * 18: Baseline hatchery 
  updated_hatchery <- list(hatchery_release = params$hatchery_release, 
                           hatchery_release_proportion_bay = params$hatchery_release_proportion_bay,
                           terminal_hatchery_logic = params$terminal_hatchery_logic) 
  # * 19: Only terminal hatchery / outplanting 
  if (19 %in% scenario$action) {
  updated_hatchery$terminal_hatchery_logic <-  TRUE
  } 
  # * 20: Phased hatcheries 
  if (20 %in% scenario$action) {
  # phase 1 - current release
  phased_release <- params$hatchery_release 
  phased_release[1:5] <- list(params$hatchery_release[[1]],
                              params$hatchery_release[[1]],
                              params$hatchery_release[[1]],
                              params$hatchery_release[[1]],
                              params$hatchery_release[[1]])
  
  # phase 2 - remove 2 or 5 FR hatcheries 
  # TODO update this logic for other runs
  remove_2_hatcheries <- params$hatchery_release[[1]]
  
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
  
  updated_hatchery$hatchery_release <-  phased_release 
  } 
  # * 21: Release 50% in bay 
  # TODO confirm that works 
  if (21 %in% scenario$action) {
    updated_hatchery$hatchery_release_proportion_bay <- .5
  } 
  return(updated_hatchery)
}

apply_hydrology_actions <- function(scenario, params, starting_hydrology, species) {
  is_calsim_output_version <- ifelse(starting_hydrology %in% c("eff_sac"), FALSE, TRUE)
  updated_hydrology <- list(upper_sacramento_flows = eval(parse(text = paste0("DSMflow::upper_sacramento_flows$", starting_hydrology))), 
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
                            proportion_flow_bypasses = eval(parse(text = paste0("DSMflow::proportion_flow_bypasses$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))), 
                            gates_overtopped = eval(parse(text = paste0("DSMflow::gates_overtopped$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))), 
                            flows_oct_nov = eval(parse(text = paste0("DSMflow::hatchery_oct_nov_flows$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))),  
                            flows_apr_may = eval(parse(text = paste0("DSMflow::hatchery_apr_may_flows$", ifelse(is_calsim_output_version, starting_hydrology, "biop_itp_2018_2019")))))
  return(updated_hydrology)
}


# helper function 
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
                                    "7" = list(), # TODO 
                                    "8" = list(), # TODO 
                                    "9" = list(), # TODO 
                                    "10" = list(), # TODO this is just baseline so do not need to update anything? Confirm
                                    "11" = list("preserve_tribal_harvest"), 
                                    "12" = list("ocean_harvest_percentage"), 
                                    "13" = list("tributary_harvest_percentage"), 
                                    "14" = list("intelligent_crr_harvest"), 
                                    "15" = list("intelligent_habitat_harvest"), 
                                    "16" = list("restrict_harvest_to_hatchery_ocean"), 
                                    "17" = list("intelligent_habitat_harvest"), 
                                    "18" = list(), # TODO this is just baseline so do not need to update anything? Confirm
                                    "19" = list("terminal_hatchery_logic"),
                                    "20" = list("hatchery_release"),
                                    "21" = list("hatchery_release_proportion_bay"),
                                    "22" = list("upper_sacramento_flows", "freeport_flows", "vernalis_flows",
                                                "stockton_flows", "CVP_exports", "SWP_exports", "proportion_diverted",
                                                "total_diverted", "delta_proportion_diverted", "delta_total_diverted",
                                                "prop_pulse_flows", "delta_inflow", "cc_gates_days_closed", "cc_gates_prop_days_closed",
                                                "proportion_flow_bypasses", "gates_overtopped", "flows_oct_nov", "flows_apr_may"),
                                    "23" = list("upper_sacramento_flows", "freeport_flows", "vernalis_flows",
                                                "stockton_flows", "CVP_exports", "SWP_exports", "proportion_diverted",
                                                "total_diverted", "delta_proportion_diverted", "delta_total_diverted",
                                                "prop_pulse_flows", "delta_inflow", "cc_gates_days_closed", "cc_gates_prop_days_closed",
                                                "proportion_flow_bypasses", "gates_overtopped", "flows_oct_nov", "flows_apr_may"),
                                    "24" = list("upper_sacramento_flows", "freeport_flows", "vernalis_flows",
                                                "stockton_flows", "CVP_exports", "SWP_exports", "proportion_diverted",
                                                "total_diverted", "delta_proportion_diverted", "delta_total_diverted",
                                                "prop_pulse_flows", "delta_inflow", "cc_gates_days_closed", "cc_gates_prop_days_closed",
                                                "proportion_flow_bypasses", "gates_overtopped", "flows_oct_nov", "flows_apr_may")) 
  
  relevent_action_params <- params_affected_by_action[[as.character(action)]] |> unlist()
  
  params_to_update <- expand.grid(watershed = watershed, 
                                  year = year, 
                                  param = relevent_action_params) |> 
    mutate(action = action)
  return(params_to_update)
}



