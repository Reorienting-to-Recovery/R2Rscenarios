# R2Rscenario

This package provides tools for creating valid scenario input data to run with the [`fallRunDSM`]("https://Reorienting-to-Recovery/fallRunDSM/"), 
[`winterRunDSM`]("https://Reorienting-to-Recovery/fallRunDSM/"), or
[`springRunDSM`]("https://Reorienting-to-Recovery/fallRunDSM/") life cycle models.

## Installation
This package can be installed using the following commands: 
```{r}
# install.packages("remotes")
remotes::install_github("Reorienting-to-Recovery/DSMscenario")
```

## Usage

### Full Scenario Example

Scenarios can be defined within a dataframe and built using the `load_scenario`
function. Each row within the dataframe represents one unique scenario action for 
a given watershed, for a period of years and number of units of effort.

Actions are defined below:

* 1: Do nothing
* 2: Add 1 acre of spawning habitat
* 3: Add 1 acres of inchannel rearing habitat
* 4: Add 1 acres of floodplain rearing habitat
* 5: Increase rearing survival by 1%
* 6: Increase migratory survival by 1% 
* 7: Increase prespawn survival by 1% 

For more information run `?load_scenario` in the console.

The following example builds a scenario of adding inchannel rearing to the 
following watersheds for the fall run life cycle model:

* 4 units of effort (2 acres) applied in Upper Sacramento River for the years 1980-1989
* 2 unit of effort (2 acres) applied in Upper Sacramento River for the years 1990-1999
* 2 unit of effort (2 acres) applied in American River for the years 1980-1989
* 2 unit of effort (2 acres) applied in Feather River for the years 1980-1989
* 2 unit of effort (2 acres) applied in Lower-mid Sacramento River for the years 1980-1989
* 2 unit of effort (2 acres) applied in Battle Creek for the years 1990-1999
* 2 unit of effort (2 acres) applied in Butte Creek for the years 1990-1999
* 2 unit of effort (2 acres) applied in Deer Creek for the years 1990-1999
* 2 unit of effort (2 acres) applied in Stanislaus River for the years 1990-1999

```r

habitats <- list(
  spawning_habitat = fallRunDSM::params$spawning_habitat,
  inchannel_habitat_fry = fallRunDSM::params$inchannel_habitat_fry,
  inchannel_habitat_juvenile = fallRunDSM::params$inchannel_habitat_juvenile,
  floodplain_habitat = fallRunDSM::params$floodplain_habitat,
  weeks_flooded = fallRunDSM::params$weeks_flooded
)

scenario_df <- data.frame(
  watershed = c("Upper Sacramento River", "Upper Sacramento River",
                "American River", "Feather River", "Lower-mid Sacramento River",
                "Battle Creek", "Butte Creek", "Deer Creek", "Stanislaus River"),
  action = c(3, 3, 3, 3, 3, 3, 3, 3, 3),
  start_year = c(1980, 1990, 1980, 1980, 1980, 1990, 1990, 1990, 1990),
  end_year = c(1989, 1999, 1989, 1989, 1989, 1999, 1999, 1999, 1999),
  units_of_effort = c(4, 2, 2, 2, 2, 2, 2, 2, 2))

scenario <- get_action_matrices(scenario_df)

scenario <- load_scenario(scenario = scenario,
                          species = DSMscenario::species$FALL_RUN,
                          habitat_inputs = habitats)

```

### Helper Datasets

For a full list of proper watershed labels see: `R2Rscenario::watershed_labels`

The list `R2Rscenario::species` can be used to provide appropriate values to the 
`species` argument to the `load_scenario` function. For example, `R2Rscenario::species$FALL_RUN`.

The minimum decay rates for each watershed are stored in `R2Rscenario::spawn_decay_rate` and `R2Rscenario::rear_decay_rate`

Regulated watersheds are stored in `DSMscenario::regulated_watersheds` and SIT watershed
groupings are stored in `R2Rscenario::watershed_groups`

### Dependencies
The `R2Rscenario` package uses data from several packages within the [Reorienting To Recovery Organization](https://github.com/Reorienting-to-Recovery). These relationships are visualized in the dependency graph below. 

<img src="man/figures/dependencyChain.svg" width="100%"/>

<div style="margin-top: 40px;">Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="man/figures/TransLogoTreb.png" width="150px"/></div>
