# R2Rscenario

This package provides tools for creating valid scenario input data to run with the [`fallRunDSM`]("https://Reorienting-to-Recovery/fallRunDSM/"), 
[`winterRunDSM`]("https://Reorienting-to-Recovery/fallRunDSM/"), or
[`springRunDSM`]("https://Reorienting-to-Recovery/fallRunDSM/") life cycle models.

## Installation

This package can be installed using the following commands: 

```
# install.packages("remotes")
remotes::install_github("Reorienting-to-Recovery/DSMscenario")
```

## Usage

### Building a scenario

Scenarios can be defined within a dataframe and built using the `load_scenario`
function. Each row within the dataframe represents one unique scenario action for 
a given watershed, for a period of years and number of units of effort.

Actions are defined below:

#### Habitat 

First define flow-to-habitat relationship:

* 1: Baseline Habitat 
* 2: Theoretical Max Habitat 
* 3: Healthy Rivers & Landscapes (HRL) habitat

Layer on additional actions 

* 4: Add Rice Lands Salmon Rearing Practice Standard 
* 5: Increase prey density 
* 6: Decrease predation (scale contact points by 1/3)
* 7: Add 20,000 acres of fish food production (HRL)
* 8: Remove predation contact points (HRL)
* 27: Add effects of spring run weir on fall run
* 28: Add effects of above-dam spring run habitat on fall run
* 29: Add San Joaquin floodplain habitat

#### Harvest 

* 10: Baseline Harvest 
* 11: Tribal harvest only 
* 12: In river harvest only 
* 13: Ocean harvest only 
* 14: Intelligent CRR harvest
* 15: Intelligent habitat harvest 
* 16: Harvest only hatchery fish (ocean)
* 17: Harvest only hatchery fish (tributary)
* 25: No harvest of dry year cohort 

#### Hatchery 

* 18: Baseline hatchery 
* 19: Only terminal hatchery / outplanting 
* 20: Phased hatcheries
* 26: Install weir at hatchery

#### Hydrology

* 22: Use 2019 BiOp hydrology 
* 23: Use Functional Flow (FF) hydrology 
* 24: Use HRL hydrology
* 31: Use HRL + FF (dry years) hydrology

### Customizing actions

Some actions allow the user to specify the watershed and/or year for which you want the action to be implemented. 
Please refer to the more in-depth action documentation to see where this is possible:

* [Habitat Actions](https://reorienting-to-recovery.github.io/R2Rscenarios/articles/habitat-actions.html)
* [Hydrology Actions](https://reorienting-to-recovery.github.io/R2Rscenarios/articles/hydrology-actions.html)
* [Harvest Actions](https://reorienting-to-recovery.github.io/R2Rscenarios/articles/harvest-actions.html)
* [Hatchery Actions](https://reorienting-to-recovery.github.io/R2Rscenarios/articles/hatchery-actions.html)

### Example: Kitchen Sink (Blended Scenario)

For more information run `?load_scenario` in the console.

The following example builds the kitchen sink scenario:

Habitat

* 2: Theoretical Max Habitat in all locations
* 5: Increase prey density in all locations 

Harvest

* 14: Intelligent CRR harvest
* 16: Harvest only hatchery fish (ocean)

Hatchery 

* 19: Only terminal hatchery / outplanting 

Hydrology

* 23: Use FF hydrology on the Sacramento River

```r
scenario_df <- data.frame(
  watershed = c(rep("All", 6), "Sacramento River"),
  action = c(2, 5, 6, 14, 16, 19, 23),
  years =  rep("All", 7))

scenario <- load_scenario(scenario = scenario_df,
                          species = R2Rscenario::species$FALL_RUN,
                          params = fallRunDSM::r_to_r_baseline_params)

```

### Helper Datasets

See reference, datasets, for a full list of helper datasets. 

For a list of proper watershed labels see: `R2Rscenario::watershed_labels`

The minimum decay rates for each watershed are stored in `R2Rscenario::spawn_decay_rate` and `R2Rscenario::rear_decay_rate`

Regulated watersheds are stored in `DSMscenario::regulated_watersheds` and SIT watershed
groupings are stored in `R2Rscenario::watershed_groups`

### Dependencies
The `R2Rscenario` package uses data from several packages within the [Reorienting To Recovery Organization](https://github.com/Reorienting-to-Recovery). These relationships are visualized in the dependency graph below. 

<img src="man/figures/dependencyChain.svg" width="100%"/>
