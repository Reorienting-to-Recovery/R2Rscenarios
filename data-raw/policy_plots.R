library(tidyverse)
library(stringr)
library(fallRunDSM)

groupings <- read_csv('data-raw/Grouping.csv')

glimpse(groupings)
groupings %>%
  filter(grp == 1)

groupings %>%
  group_by(grp) %>%
  summarise(text = paste(watershed, collapse = ', '))

groupings$grp %>% table
View(groupings)

pp <- read_csv('data-raw/FP_zero_hab_degrade_policies_Mark_FP_rule.csv')
glimpse(pp)
View(pp)
action_labels <- c("Do nothing", "Add spawning habitat", "Add rearing habitat", "Add floodplain habitat", "Increase survival by 0.5%")

# annual monthly mean smallest size class survival rate
list2env(fallRunDSM::load_baseline_data(), .GlobalEnv)
..params <- fallRunDSM::params

size_small_mean_surv <- purrr::map_dfc(1:20, function(year) {
  purrr::map2_dfc(year, 1:8, function(year, month) {
    get_rearing_survival_rates(year, month, scenario = NULL,
                               ..surv_juv_rear_int= ..params$..surv_juv_rear_int,
                               ..surv_juv_rear_contact_points= ..params$..surv_juv_rear_contact_points,
                               ..surv_juv_rear_prop_diversions= ..params$..surv_juv_rear_prop_diversions,
                               ..surv_juv_rear_total_diversions= ..params$..surv_juv_rear_total_diversions,
                               ..surv_juv_bypass_int = ..params$..surv_juv_bypass_int,
                               ..surv_juv_delta_int = ..params$..surv_juv_delta_int,
                               ..surv_juv_delta_contact_points = ..params$..surv_juv_delta_contact_points,
                               ..surv_juv_delta_total_diverted = ..params$..surv_juv_delta_total_diverted)$inchannel[,1]
  }) %>% as.matrix() %>% rowMeans()
}) %>%
  set_names(1980:1999) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, mean_survival, -watershed)

# female spawners - number of redds rolling mean last 5 years
# at each year redds


# spawn habitat - min annual amount within months 10-12
min_spawning_habitat <- map_df(1:31, function(watershed) {
  min_spawn_hab <- apply(t(spawning_habitat[watershed, 10:12, ]), 1, min)
  tibble(
    watershed = watershed_attributes$watershed[watershed],
    year = names(min_spawn_hab),
    spawn_hab = as.numeric(min_spawn_hab)
  )
}) %>%
  arrange(year)

# rearing habitat - annual mean habitat (fry months 1-3, juv months 4-8)
fry_mean_habitat <- map_df(1:20, function(year) {
  map_df(1:3, function(m) {
    hab <- get_habitat(year = year, month = m)$inchannel
    tibble(watershed = names(hab),
           habitat = as.numeric(hab))
  }) %>%
    group_by(watershed) %>%
    summarise(
      mean_habitat = mean(habitat)
    ) %>% ungroup() %>%
    mutate(year = year + 1978)
})
# if wantting to add order
# %>%
#   left_join(select(watershed_attributes, watershed, order)) %>%
#   arrange(order) %>%
#   select(-order)

juv_mean_habitat <- map_df(1:20, function(year) {
  map_df(4:8, function(m) {
    hab <- get_habitat(year = year, month = m)$inchannel
    tibble(watershed = names(hab),
           habitat = as.numeric(hab))
  }) %>%
    group_by(watershed) %>%
    summarise(
      mean_habitat = mean(habitat)
    ) %>% ungroup() %>%
    mutate(year = year + 1978)
})


