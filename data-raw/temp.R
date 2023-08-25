# srrl <- 40
# frrl <- 30
# frra <- 800
# srsl <- 30
#
# srra <- (srrl/frrl)*frra
# srsa <- (srsl/srrl)*srra # or (srsl/frrl)*frra
#
# tma <- read_csv('data-raw/theoretical_max_area.csv')
#
# tma %>%
#   filter(species == 'fr', lifestage == 'rearing') %>%
#   mutate(tt = max_rear_area$fall) %>%
#   filter(max_suit_sqm - tt > 100)
#
# max_existing_habitat_fr_rear <- pmax(purrr::map_dbl(1:31, ~max(DSMhabitat::fr_fry[.,,])),
#                                      purrr::map_dbl(1:31, ~max(DSMhabitat::fr_juv[.,,])))
#
#
# index <- DSMscenario::watershed_labels[max_existing_habitat_fr_rear > max_rear_area$fall]
#
# a <- (max_existing_habitat_fr_rear - max_rear_area$fall)/max_rear_area$fall
# a[index]
# max_existing_habitat_fr_spawn <- purrr::map_dbl(1:31, ~max(DSMhabitat::fr_spawn[.,,]))
#
#
# rear_ratio <- fr_rear_max / max_existing_habitat_fr_rear
#
# mean(rear_ratio[DSMscenario::watershed_labels[fr_rear_max > max_existing_habitat_fr_rear]], na.rm = TRUE)
# min(rear_ratio[DSMscenario::watershed_labels[fr_rear_max > max_existing_habitat_fr_rear]], na.rm = TRUE)
# max(rear_ratio[DSMscenario::watershed_labels[fr_rear_max > max_existing_habitat_fr_rear]], na.rm = TRUE)
#
# ee <- read_csv('data-raw/earth_engine_total_channel_area_estimates.csv')
# ee %>%
#   mutate(sqm = DSMhabitat::acres_to_square_meters(FR_channel_area_of_length_modeled_acres) * .5,
#          tt = max_rear_area$fall[watershed]) %>% View
#   filter(abs(sqm - tt) < 100000)
#
# # big chico creek
# 64542.54623557283 #cjay
# DSMhabitat::square_meters_to_acres(179772.03817598658)
#
# compare <- tma %>%
#   mutate(new = case_when(
#     species == "fr" & lifestage == "spawning" ~ max_spawn_area$fall[watershed],
#     species == "fr" & lifestage == "rearing" ~ max_rear_area$fall[watershed],
#     species == "sr" & lifestage == "spawning" ~ max_spawn_area$spring[watershed],
#     species == "sr" & lifestage == "rearing" ~ max_rear_area$spring[watershed],
#     species == "st" & lifestage == "spawning" ~ max_spawn_area$steelhead[watershed],
#     species == "st" & lifestage == "rearing" ~ max_rear_area$steelhead[watershed],
#     TRUE ~ 0
#   ))
# View(compare)
#
# tma %>%
#   filter(species == 'sr')
# sr_rear_max
