library(purrr)
library(dplyr)
library(readxl)
library(DSMscenario)

# scenarios 1 - 7 ----
raw_scenarios <- read_excel("data-raw/scenarios.xlsx", sheet = "scenarios")

scenarios <- purrr::map(1:7, ~get_action_matrices(filter(raw_scenarios, scenario == .))) %>%
  purrr::set_names(toupper(c("one", "two", "three", "four", "five", "six", "seven")))

no_decay_alt <- watershed_labels %in% c("Clear Creek", "Butte Creek", "Upper Sacramento River")
names(no_decay_alt) <- watershed_labels
scenarios$SIX$no_decay <- no_decay_alt

# baseline scenario ----
scenarios$NO_ACTION <- get_action_matrices(tibble::tibble(
  watershed = DSMscenario::watershed_labels,
  action = 1,
  start_year = 1980,
  end_year = 1981,
  units_of_effort = 1))

# make scenarios from 2019 matrices function----
make_scenario <- function(selectedOptimalDecisions, Effort = NULL) {

  spawn <- matrix(0, nrow = 31, ncol = 22, dimnames = list(DSMscenario::watershed_labels, 1979:2000))
  spawn_index <- which(selectedOptimalDecisions == 2)

  inchannel <- matrix(0, nrow = 31, ncol = 21, dimnames = list(DSMscenario::watershed_labels, 1980:2000))
  inchannel_index <- which(selectedOptimalDecisions == 3)

  floodplain <- matrix(0, nrow = 31, ncol = 21, dimnames = list(DSMscenario::watershed_labels, 1980:2000))
  floodplain_index <- which(selectedOptimalDecisions == 4)

  survival <- matrix(0, nrow = 31, ncol = 21, dimnames = list(DSMscenario::watershed_labels, 1980:2000))
  survival_index <- which(selectedOptimalDecisions == 5)

  if (!is.null(Effort)) {
    spawn[spawn_index] <- Effort[spawn_index]
    inchannel[inchannel_index] <- Effort[inchannel_index]
    floodplain[floodplain_index] <- Effort[floodplain_index]
    survival[survival_index] <- Effort[survival_index]
  } else {
    spawn[spawn_index] <- 1
    inchannel[inchannel_index] <- 1
    floodplain[floodplain_index] <- 1
    survival[survival_index] <- 1
  }

  return(list(spawn = spawn,
              inchannel = inchannel,
              floodplain = floodplain,
              survival = survival))
}

# scenario 8 ----
Effort<-matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)

selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,5,5,5,5,5,5,5,5,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,4,3,3,4,4,4,4,3,4,4,3,4,3,4,4,4,3,3,4,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   4,3,3,3,3,4,4,3,3,4,3,4,3,3,4,4,4,3,4,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,4,4,4,4,3,3,4,3,3,3,4,4,3,3,4,4,3,3,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)

eight <- make_scenario(selectedOptimalDecisions, Effort)

scenarios$EIGHT$spawn <- eight$spawn
scenarios$EIGHT$inchannel <- eight$inchannel
scenarios$EIGHT$floodplain <- eight$floodplain
scenarios$EIGHT$survival <- eight$survival
# scenario 9 ----
selectedOptimalDecisions<-matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,
                                   3,3,3,NA,3,3,3,NA,3,3,3,NA,3,3,3,NA,3,3,3,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,3,3,NA,5,5,5,NA,5,5,5,NA,5,5,5,NA,5,5,5,NA,
                                   3,2,3,NA,3,3,3,NA,3,3,3,NA,3,3,3,NA,3,3,3,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,3,NA,NA,NA,3,NA,NA,NA,3,NA,NA,NA,3,NA,NA,NA,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   4,4,3,NA,4,3,3,NA,4,3,4,NA,4,4,3,NA,4,3,3,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,4,NA,NA,NA,4,NA,NA,NA,4,NA,NA,NA,4,NA,NA,NA,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,3,4,NA,3,4,3,NA,4,4,4,NA,3,4,4,NA,3,3,4,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
nine <- make_scenario(selectedOptimalDecisions)

scenarios$NINE$spawn <- nine$spawn
scenarios$NINE$inchannel <- nine$inchannel
scenarios$NINE$floodplain <- nine$floodplain
scenarios$NINE$survival <- nine$survival

# scenario 10 ----
selectedOptimalDecisions<-matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,
                                   NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,
                                   3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
ten <- make_scenario(selectedOptimalDecisions)

scenarios$TEN$spawn <- ten$spawn
scenarios$TEN$inchannel <- ten$inchannel
scenarios$TEN$floodplain <- ten$floodplain
scenarios$TEN$floodplain["Upper-mid Sacramento River", ] <- scenarios$TEN$floodplain["Upper-mid Sacramento River", ] * 3
scenarios$TEN$survival <- ten$survival
# scenario 11 ----
selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                   NA,NA,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,NA,NA,5,5,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,5,5,5,5,5,5,5,5,5,NA,5,NA,5,NA,NA,5,5,
                                   NA,NA,3,NA,NA,NA,3,3,3,3,3,3,NA,3,NA,3,NA,3,3,NA,
                                   2,2,NA,2,2,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   4,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,5,NA,5,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   4,4,NA,NA,4,NA,NA,NA,NA,NA,NA,NA,4,NA,NA,4,4,4,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,3,3,NA,3,NA,3,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,4,4,NA,NA,4,4,4,4,4,4,NA,4,NA,NA,NA,NA,4,4,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,3,NA,3,NA,3,NA,NA,3,NA,3,NA,3,NA,3,3,3,NA,3,3,
                                   NA,NA,NA,NA,NA,4,NA,NA,NA,NA,NA,NA,NA,NA,4,NA,NA,NA,NA,NA,
                                   5,NA,NA,NA,NA,NA,NA,5,NA,5,NA,5,NA,5,NA,NA,5,5,5,NA,
                                   NA,NA,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,NA,
                                   NA,NA,NA,NA,2,NA,NA,NA,NA,NA,NA,NA,NA,2,NA,NA,NA,NA,NA,NA,
                                   NA,5,5,4,4,NA,NA,NA,NA,NA,NA,NA,5,NA,5,NA,NA,NA,NA,5,
                                   NA,NA,NA,NA,NA,3,3,NA,3,NA,3,NA,NA,NA,NA,3,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)

eleven <- make_scenario(selectedOptimalDecisions)

scenarios$ELEVEN$spawn <- eleven$spawn
scenarios$ELEVEN$inchannel <- eleven$inchannel
scenarios$ELEVEN$floodplain <- eleven$floodplain
scenarios$ELEVEN$survival <- eleven$survival

# scenario 12 ----
selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,3,3,3,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                   4,3,3,3,4,3,3,4,3,4,3,4,4,3,4,4,4,3,4,3,
                                   5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
twelve <- make_scenario(selectedOptimalDecisions)

scenarios$TWELVE$spawn <- twelve$spawn
scenarios$TWELVE$inchannel <- twelve$inchannel
scenarios$TWELVE$floodplain <- twelve$floodplain
scenarios$TWELVE$survival <- twelve$survival

# scenario 13 ----
selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,3,3,3,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                   3,3,3,3,4,4,4,4,4,3,3,4,4,4,3,4,4,3,4,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   5,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
thirteen <- make_scenario(selectedOptimalDecisions)

scenarios$THIRTEEN$spawn <- thirteen$spawn
scenarios$THIRTEEN$inchannel <- thirteen$inchannel
scenarios$THIRTEEN$floodplain <- thirteen$floodplain
scenarios$THIRTEEN$survival <- thirteen$survival

# write data object ----
usethis::use_data(scenarios, overwrite = TRUE)
