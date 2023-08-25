dd <- fallRunDSM::params
habitats <- list(
 spawning_habitat = dd$spawning_habitat,
 inchannel_habitat_fry = dd$inchannel_habitat_fry,
 inchannel_habitat_juvenile = dd$inchannel_habitat_juvenile,
 floodplain_habitat = dd$floodplain_habitat,
 weeks_flooded = dd$weeks_flooded
)

x <- load_scenario(scenario = DSMscenario::scenarios$SEVEN, species = 'fr',
                   habitat_inputs = habitats,
                   stochastic = FALSE)


y <- load_scenario(scenario = DSMscenario::scenarios$SEVEN, species = 'fr', habitat_inputs = habitats,
                   stochastic = FALSE)

which(x$floodplain_habitat != y$floodplain_habitat)
which(x$inchannel_habitat_fry != y$inchannel_habitat_fry)
