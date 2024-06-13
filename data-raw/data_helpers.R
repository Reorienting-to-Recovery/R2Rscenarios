library(tidyverse)

watershed_labels <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                      "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                      "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                      "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                      "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                      "Feather River", "Yuba River", "Lower-mid Sacramento River",
                      "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                      "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                      "Tuolumne River", "San Joaquin River")
usethis::use_data(watershed_labels)

max_decay_rates <- read_csv('data-raw/Grouping.csv') %>%
  mutate(
    spawn_decay_rate_max = case_when(
      grp == 1 ~ 0.973647181 - (1 - 0.973647181),
      grp == 2 ~ 0.994949237 - (1 - 0.994949237),
      grp == 3 ~ 0.994949237 - (1 - 0.994949237),
      grp == 4 ~ 0.979148362 - (1 - 0.979148362),
      grp == 5 ~ 0.962256359 - (1 - 0.962256359),
      TRUE ~ 0.989793782 - (1 - 0.989793782)),
    rear_decay_rate_max = case_when(
      grp == 1 ~ 0.987175243 - (1 - 0.987175243),
      grp == 2 ~ 0.997487405 - (1 - 0.997487405),
      grp == 3 ~ 0.997487405 - (1 - 0.997487405),
      grp == 4 ~ 0.989793782 - (1 - 0.989793782),
      grp == 5 ~ 0.981853233 - (1 - 0.981853233),
      TRUE ~ 0.994949237 - (1 - 0.994949237)),
  ) %>%
  select(watershed, spawn_decay_rate_max, rear_decay_rate_max)

spawn_decay_rate <- max_decay_rates$spawn_decay_rate_max
names(spawn_decay_rate) <- watershed_labels

rear_decay_rate <- max_decay_rates$rear_decay_rate_max
names(rear_decay_rate) <- watershed_labels
usethis::use_data(spawn_decay_rate, overwrite = TRUE)
usethis::use_data(rear_decay_rate, overwrite = TRUE)

species <- list(FALL_RUN = "fr", WINTER_RUN = "wr", SPRING_RUN = "sr", STEELHEAD = "st", LATE_FALL_RUN = "lfr")
usethis::use_data(species)


regulated_watersheds <- c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                          1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0)
names(regulated_watersheds) <- DSMscenario::watershed_labels
usethis::use_data(regulated_watersheds, overwrite = TRUE)

grp <- read_csv('data-raw/Grouping.csv')
watershed_groups <- grp$grp
names(watershed_groups) <- DSMscenario::watershed_labels
watershed_groups["San Joaquin River"] <- 7
usethis::use_data(watershed_groups, overwrite = TRUE)
# TODO update csv after conversation

# Max habitat -----
fr_rear_max <- set_names(pmax(purrr::map_dbl(1:31, ~max(DSMhabitat::fr_fry[.,,])),
                              purrr::map_dbl(1:31, ~max(DSMhabitat::fr_juv[.,,]))),
                         DSMscenario::watershed_labels) * 2

fr_spawn_max <- set_names(purrr::map_dbl(1:31, ~max(DSMhabitat::fr_spawn[.,,])),
                          DSMscenario::watershed_labels) * 2

wr_rear_max <- set_names(pmax(purrr::map_dbl(1:31, ~max(DSMhabitat::wr_fry[.,,])),
                              purrr::map_dbl(1:31, ~max(DSMhabitat::wr_juv[.,,]))),
                         DSMscenario::watershed_labels) * 2

wr_spawn_max <- set_names(purrr::map_dbl(1:31, ~max(DSMhabitat::wr_spawn[.,,])),
                          DSMscenario::watershed_labels) * 2

sr_rear_max <- set_names(pmax(purrr::map_dbl(1:31, ~max(DSMhabitat::sr_fry[.,,])),
                              purrr::map_dbl(1:31, ~max(DSMhabitat::sr_juv[.,,]))),
                         DSMscenario::watershed_labels) * 2
sr_spawn_max <- set_names(purrr::map_dbl(1:31, ~max(DSMhabitat::sr_spawn[.,,])),
                          DSMscenario::watershed_labels) * 2

st_rear_max <- set_names(pmax(purrr::map_dbl(1:31, ~max(DSMhabitat::st_fry[.,,])),
                              purrr::map_dbl(1:31, ~max(DSMhabitat::st_juv[.,,]))),
                         DSMscenario::watershed_labels) * 2
st_spawn_max <- set_names(purrr::map_dbl(1:31, ~max(DSMhabitat::st_spawn[.,,])),
                          DSMscenario::watershed_labels) * 2

lfr_rear_max <- set_names(pmax(purrr::map_dbl(1:31, ~max(DSMhabitat::lfr_fry[.,,])),
                               purrr::map_dbl(1:31, ~max(DSMhabitat::lfr_juv[.,,]))),
                          DSMscenario::watershed_labels) * 2

lfr_spawn_max <- set_names(purrr::map_dbl(1:31, ~max(DSMhabitat::lfr_spawn[.,,])),
                           DSMscenario::watershed_labels) * 2

max_rear_area <- list(
  "FALL" = fr_rear_max,
  "LATE_FALL" = lfr_rear_max,
  "WINTER" = wr_rear_max,
  "SPRING" = sr_rear_max,
  "STEELHEAD" = st_rear_max)

max_spawn_area <- list(
  "FALL" = fr_spawn_max,
  "LATE_FALL" = lfr_spawn_max,
  "WINTER" = wr_spawn_max,
  "SPRING" = sr_spawn_max,
  "STEELHEAD" = st_spawn_max)

usethis::use_data(max_spawn_area, overwrite = TRUE)
usethis::use_data(max_rear_area, overwrite = TRUE)

spawning_decay_multiplier <- DSMhabitat::spawning_decay_multiplier
usethis::use_data(spawning_decay_multiplier, overwrite = TRUE)

param_type_lookup <- tibble(param_name = c("spawning_habitat", "inchannel_habitat_fry", "inchannel_habitat_juvenile", 
                                           "floodplain_habitat", "yolo_habitat", "sutter_habitat", "delta_habitat", 
                                           "weeks_flooded", "avg_temp", "degree_days", "prey_density", "prey_density_delta", 
                                           "prop_high_predation", "contact_points", "delta_contact_points", 
                                           "delta_prop_high_predation", "restrict_harvest_to_hatchery_ocean", 
                                           "restrict_harvest_to_hatchery_trib", "ocean_harvest_percentage", 
                                           "tributary_harvest_percentage", "crr_scaling", "no_cohort_harvest_years", 
                                           "intelligent_crr_harvest", "intelligent_habitat_harvest", "preserve_tribal_harvest", 
                                           "hatchery_release", "hatchery_release_proportion_bay", "terminal_hatchery_logic", 
                                           "upper_sacramento_flows", "freeport_flows", "vernalis_flows", 
                                           "stockton_flows", "CVP_exports", "SWP_exports", "proportion_diverted", 
                                           "total_diverted", "delta_proportion_diverted", "delta_total_diverted", 
                                           "prop_pulse_flows", "delta_inflow", "cc_gates_days_closed", "cc_gates_prop_days_closed", 
                                           "proportion_flow_bypass", "gates_overtopped", "flows_oct_nov", 
                                           "flows_apr_may"),
                            update_type = c("3D matrix - wa, m, y", 
                                     "3D matrix - wa, m, y",
                                     "3D matrix - wa, m, y",
                                     "3D matrix - wa, m, y",
                                     "2D matrix - m, y",
                                     "2D matrix - m, y",
                                     "3D matrix - m, y, delta",
                                     "3D matrix - wa, m, y",
                                     "3D matrix - wa, m, y",
                                     "3D matrix - wa, m, y",
                                     "vector - wa",
                                     "vector - delta",
                                     "vector - wa",
                                     "vector - wa",
                                     "vector - delta",
                                     "vector - delta",
                                     "single value",
                                     "single value",
                                     "single value",
                                     "vector - wa",
                                     "single value",
                                     "single value",
                                     "single value",
                                     "single value",
                                     "single value",
                                     "list of matrices by y - wa, sc",
                                     "single value",
                                     "single value",
                                     "2D matrix - m, y",
                                     "2D matrix - m, y",
                                     "2D matrix - m, y",
                                     "2D matrix - m, y",
                                     "2D matrix - m, y",
                                     "2D matrix - m, y",
                                     "3D matrix - wa, m, y",
                                     "3D matrix - wa, m, y",
                                     "3D matrix - m, y, delta",
                                     "3D matrix - m, y, delta",
                                     "2D matrix - wa, m",
                                     "3D matrix - m, y, delta",
                                     "single value",
                                     "single value",
                                     "3D matrix - m, y, bypass",
                                     "3D matrix - m, y, bypass",
                                     "2D matrix - wa, y",
                                     "2D matrix - wa, y"
                                     ))
usethis::use_data(param_type_lookup)
