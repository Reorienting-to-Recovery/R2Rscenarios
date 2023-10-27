library(tidyverse)
library(lubridate)


wet_year <- DSMflow::flows_cfs$biop_itp_2018_2019 |> 
  select(date, `Upper-mid Sacramento River`) |> 
  filter(year(date) == 1993) |>
  mutate(water_year = as.Date(ifelse(month(date) %in% 10:12, 
                             date,
                             as.Date(paste0(year(date) + 1, "-", month(date), "-", day(date)))))) |> glimpse() 
wet_year |> 
  ggplot(aes(x = water_year, y = `Upper-mid Sacramento River`)) + 
  geom_line()


# Build Functional flow curve 

functional_flows <- tibble(month = factor(month.abb[1:12], levels = month.abb[c(10, 11, 12, 1:9)]),
                           # month_name = month.abb[month],
                           flow_cfs = c(62000, # Jan Wet season base flow + Peak Magnitude flows, based on wet year monthly Natural Flows 
                                        62000, # Feb Wet season base flow + Peak Magnitude flows, based on wet year monthly Natural Flows 
                                        35000, # March Wet season base flow, based on wet year monthly Natural Flows 
                                        35000, # April Wet season base flow, based on wet year monthly Natural Flows 
                                        10700, # May Spring recession flow (10,700 for mig surv ~50%)
                                        10700, # June Spring recession flow (10,700 for mig surv ~50%)
                                        7500, # July half way between dry season baseflow and mig surv 50% threshold
                                        4308, # Aug - dry season baseflow (keep migratory surv at ~20%), 
                                        4308, # Sept - dry season baseflow (keep migratory surv at ~20%),
                                        4308, # oct - dry season baseflow 
                                        11600, # Nov add fall pulse flow, based on wet year monthly Natural Flows
                                        35000 # Dec Begin wet season baseflow and medium flow, based on wet year monthly Natural Flows
                                         )) |> glimpse()

write_csv(functional_flows, "functional_flows_wet_year.csv")

functional_flows |> 
  ggplot(aes(x = month, y = flow_cfs, group = 1)) + 
  geom_line(color = "lightblue") + 
  theme_minimal() + 
  labs(x = "Month", 
       y = "Flow CFS", 
       title = "Wet Year - Model Functional Montly Flows")
