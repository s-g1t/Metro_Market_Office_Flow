# *************************************************************************************************************************
# Metro Market Office Flow Forecast
# *************************************************************************************************************************

# This script contains the model specification and the office forecast production process. The code is designed in a way to
# allow retro forecast runs by changing the vintage configuration.
#
# Stephen Gardner, smg263@georgetown.edu

# *************************************************************************************************************************
# CONFIGURATION
# *************************************************************************************************************************

# clear the environment prior to starting
rm(list = ls())

# config.yml sits in the project root and the working directory is set automatically when opened, so no setwd() is needed.
# check for the config file before moving on, if it's not there the code will stop
if (!file.exists("config.yml"))
  stop("config.yml not found in ", getwd(), " - open the .Rproj so the working directory is the project root.") 

config <- yaml::read_yaml("config.yml")

# set the formatting for decimals
options(scipen = 999)

# check every directory in the config file and create any that are missing...
# new directories added to the config are picked up automatically - no code change needed
for (d in config$paths)
  {
   if (!dir.exists(d))
    {dir.create(d, recursive = TRUE)} # end if
   } # end for

# load libraries
library(DBI)
library(RPostgres)
library(tidyverse) # for almost any data transformation
library(readxl) # excel files
library(slider)
library(lubridate) # adjusting and formatting dates
library(glue) # something like fstrings
library(GGally)
library(janitor)
library(fpp3) # combined forecasting package: fable, beast, etc.
library(tseries) # for ts data transformations
library(knitr)
library(urca)
library(stringr) # string manipulations
library(yaml) # config file parsing

# *************************************************************************************************************************
# DATA IMPORT
# *************************************************************************************************************************

offc.fundamentals <- read_csv(config$data$offc.fundamentals)
offc.metro_fcast_vars <- read_csv(config$data$offc.metro_fcast_vars)
offc.constr_pipeline <- read_csv(config$data$offc.constr_pipeline)

# *************************************************************************************************************************
# FUNDAMENTAL & VARIABLE SELECTION
# *************************************************************************************************************************

# configure the forecast vintage from config.yml ("retro" or "latest")
vintage_desc <- config$vintage$desc

# the vintage date is the forecast "production" quarter, forecasts will start one step beyond this data
retro_date <- ymd(config$vintage$retro_date)
vintage_date <- if (vintage_desc == "retro") {ymd(floor_date(retro_date, "quarter"))} else {max(offc.fundamentals$date)}
vintage_label <- format(yearquarter(vintage_date), "%YQ%q")

# pull the office factor names into a list from the headers so we can reference them later
offc.fund_fcast_vars <- offc.fundamentals %>% select(date, msa_desc, class_desc, submkt_desc) %>% distinct()

# use this line to control the fundamental forecast flow...the order here is very important
offc_fcast_fundamentals <- offc.fundamentals %>% select(total_inventory, total_vacancy, total_absorption, wgtavg_rent) %>% names()

# regressor lags come from config.yml (numeric quarterly lags per fundamental)
lag_list <- config$lags

# training window starts come from config.yml (end is always the vintage date)
train_window <- config$train_window

# *************************************************************************************************************************
# TRAINING/TEST DATA SPLIT
# *************************************************************************************************************************

# create a blank list to pass them forecast df's into
fit.offc_flagcast_dummy_vars <- list()
fcast.offc_flagcast_dummy_vars <- list()

fit.models_offc <- list()
fcast.models_offc <- list()
offc_fundcast <- list() # this list will contain the finalized forecast output for each office fundamental
offc_metro_fund_vars <- list()

# loop through the fundamentals list to create a forecast dataset for each forecast variable
for (i in offc_fcast_fundamentals)
  {# pull the office factor names into a list from the headers so we can reference them later
   offc_fcast_vars <- offc.metro_fcast_vars %>%
                      left_join(offc.fund_fcast_vars, by = c("date", "msa_desc")) %>%
                      select(-c(date, msa_desc, class_desc, submkt_desc)) %>%
                      names()
   
   n_lag <- lag_list[[i]]
   
   hist.offc_fcast_vars <- list()
   hist.offc_fcast_vars[[i]] <- offc.fundamentals %>%
                                select(date, msa_desc, submkt_desc, class_desc, all_of(i)) %>%
                                left_join(offc.metro_fcast_vars, by = c("date", "msa_desc")) %>%
                                left_join(offc.fund_fcast_vars, 
                                          by = c("date", "msa_desc", "class_desc", "submkt_desc")) %>%
                                filter(date <= vintage_date)
  
   keys <- offc.fundamentals %>% distinct(class_desc, submkt_desc) %>% as_tibble()
  
   cast.offc_fcast_vars <- list()
   cast.offc_fcast_vars[[i]] <- offc.metro_fcast_vars %>%
                                mutate(date = yearquarter(date),
                                       "{i}" := NA,) %>%
                                filter(date > yearquarter(vintage_date)) %>%
                                crossing(keys) %>%
                                left_join(offc.fund_fcast_vars, by = c("date", "msa_desc", "class_desc", "submkt_desc"))
   
   histcast.offc_flagcast_vars <- list()
   histcast.offc_flagcast_vars[[i]] <- bind_rows(hist.offc_fcast_vars[[i]],
                                                 cast.offc_fcast_vars[[i]])
   
   for (x in names(n_lag))
     {histcast.offc_flagcast_vars[[i]] <- histcast.offc_flagcast_vars[[i]] %>%
                                          group_by(msa_desc, class_desc, submkt_desc) %>%
                                          arrange(date, .by_group = TRUE) %>%
                                          mutate("{x}_lag{n_lag[[x]]}" := lag(.data[[x]], n_lag[[x]]),
                                                 date = yearquarter(date),
                                                 cast_desc = ifelse(date <= yearquarter(vintage_date), "actual", "forecast")) %>%
                                          ungroup()
      } # end for (x in n_lag)
   
   histcast.offc_flagcast_dummy_vars <- list()
   histcast.offc_flagcast_vars[[i]] <- histcast.offc_flagcast_vars[[i]]
#...and dummy month variables
   for (n in 1:config$modeling$quarter_dummies) # adds 1 if the quarter index = quarter(date) is TRUE, otherwise 0
     {histcast.offc_flagcast_dummy_vars[[i]] <- histcast.offc_flagcast_vars[[i]] %>%
                                                mutate("{glue('quarter_{n}')}" := as.integer(quarter(date) == n),
                                                date = yearquarter(date)) %>%
                                                tsibble(index = date, key = c(msa_desc, class_desc, submkt_desc))
      } #end for (n in 1:3)
   
  cast_split <- split(histcast.offc_flagcast_dummy_vars[[i]],
                      histcast.offc_flagcast_dummy_vars[[i]]$cast_desc)
  
  fit.offc_flagcast_dummy_vars[[i]] <- cast_split$actual
  fcast.offc_flagcast_dummy_vars[[i]] <- cast_split$forecast
  
# *************************************************************************************************************************
# MODEL FITS & FORECASTS
# *************************************************************************************************************************
  
# create a new list combining offc_cast_vars with lag_list
  offc_flagcast_vars <- c()
  for (x in names(n_lag))
    {offc_flagcast_vars <- c(offc_flagcast_vars, glue("{x}_lag{n_lag[[x]]}"))}
  
# pull the dummy months into a list from the headers so we can reference them later
  qtr_vars <- fit.offc_flagcast_dummy_vars[[i]] %>% as.data.frame() %>% select(starts_with("quarter_")) %>% names()
  
# use this section to specify which models should be included in the forecast and how they should be structured,
# AR distributed lag variables come from the lag list
# ******************** #
# MODEL SPECIFICATION. #
# ******************** #
  # pure ar1 forecast
  ar1_pure <- glue("{i} ~ pdq(1,1,0) + PDQ(0,0,0) + {glue_collapse(qtr_vars, '+')}")
  # pure ar2 forecast
  ar2_pure <- glue("{i} ~ pdq(2,1,0) + PDQ(0,0,0) + {glue_collapse(qtr_vars, '+')}")
  # ar1 with all lagged variables
  ar1_lag_vars <- glue("{i} ~ pdq(1,1,0) + PDQ(0,0,0) + {glue_collapse(c(offc_flagcast_vars, qtr_vars), '+')}")
  # ar2 with all lagged variables
  ar2_lag_vars <- glue("{i} ~ pdq(2,1,0) + PDQ(0,0,0) + {glue_collapse(c(offc_flagcast_vars, qtr_vars), '+')}")
  # ar1 with all lagged variables
  ar1_lag_mavg <- glue("{i} ~ pdq(1,1,3) + PDQ(0,0,0) + {glue_collapse(c(offc_flagcast_vars), '+')}")
  # ar2 with all lagged variables
  ar2_lag_mavg <- glue("{i} ~ pdq(2,1,3) + PDQ(0,0,0) + {glue_collapse(c(offc_flagcast_vars), '+')}")
  # ar1 with all lagged variables
  ar1_lag_log <- glue("log({i}) ~ pdq(1,1,0) + PDQ(0,0,0) + {glue_collapse(c(offc_flagcast_vars, qtr_vars), '+')}")
  # ar2 with all lagged variables
  ar2_lag_log <- glue("log({i}) ~ pdq(2,1,0) + PDQ(0,0,0) + {glue_collapse(c(offc_flagcast_vars, qtr_vars), '+')}")
  
  fit.models_offc[[i]] <- fit.offc_flagcast_dummy_vars[[i]] %>%
                          filter_index(train_window[[i]] ~ .) %>%
                          model(ar1_pure = ARIMA(as.formula(ar1_pure)), 
                                ar2_pure = ARIMA(as.formula(ar2_pure)), 
                                ar1_lag = ARIMA(as.formula(ar1_lag_vars)), 
                                ar2_lag = ARIMA(as.formula(ar2_lag_vars)) 
                                )
  
  fcast.models_offc[[i]] <- fit.models_offc[[i]] %>% forecast(fcast.offc_flagcast_dummy_vars[[i]])
  
  offc_fundcast[[i]] <- list(stats.offc_fund = data.frame(glance(fit.models_offc[[i]])) %>%
                                               left_join(data.frame(accuracy(fit.models_offc[[i]])),
                                                         by = c(".model", "msa_desc", "class_desc", "submkt_desc")) %>%
                                               select(-c(ar_roots, ma_roots, .type)) %>%
                                               pivot_longer(-c(.model, msa_desc, class_desc, submkt_desc),
                                                            names_to = "Statistic", values_to = "stat_value") %>%
                                               pivot_wider(names_from = .model, values_from = stat_value) %>%
                                               mutate(vintage = vintage_label,
                                                      offc_fundamental_desc = i),
                             
                             resid.offc_fund = augment(fit.models_offc[[i]]) %>%
                                               features(.resid, ljung_box, lag = config$modeling$ljung_box_lag) %>%
                                               as.data.frame() %>%
                                               left_join(augment(fit.models_offc[[i]]) %>%
                                                         features(.resid, unitroot_kpss) %>%
                                                         as.data.frame(),
                                                         by = c("msa_desc", "class_desc", "submkt_desc", ".model")) %>%
                                               mutate(offc_fundamental_desc = i) %>%
                                               rename(lbox_stat = lb_stat, lbox_pvalue = lb_pvalue),
                             
                             coefs.offc_fund = data.frame(coef(fit.models_offc[[i]])) %>%
                                               mutate(offc_fundamental_desc = i,
                                                      vintage = vintage_label),
                                              
                             forecast.offc_fund = fcast.models_offc[[i]] %>%
                                                  hilo(as.numeric(config$modeling$interval_level)) %>%
                                                  as.data.frame() %>%
                                                  unpack_hilo(all_of(glue("{config$modeling$interval_level}%"))) %>%
                                                  rename(lower = all_of(glue("{config$modeling$interval_level}%_lower")),
                                                         upper = all_of(glue("{config$modeling$interval_level}%_upper"))) %>%
                                                  mutate(date = yq(date),
                                                         vintage = vintage_label,
                                                         offc_fundamental_desc = i,
                                                         std_dev = (upper - lower) / (2 * 1.96),
                                                         steps_ahead = lubridate::interval(vintage_date, date) / months(1)) %>%
                                                  select(vintage,
                                                         msa_desc, class_desc, submkt_desc,
                                                         cast_desc, date,
                                                         offc_fundamental_desc, offc_fundamental_val = .mean, lower, upper, .model)
                             ) # end list
  
# use the below section to specify what the final model selection for each fundamental should be
# *************************************************************************************************************************
# FINAL MODEL SELECTION
# *************************************************************************************************************************

# final model picks come from config.yml (one row per fundamental x class x submarket)
  final.models_offc <- bind_rows(config$final_models) %>% rename(.model = model)
  
# filter the forecasts to the final selection
  offc_fundcast[[i]]$finalcast.offc_fund <- offc_fundcast[[i]]$forecast.offc_fund %>%
                                            semi_join(final.models_offc,
                                                      by = c("offc_fundamental_desc", "class_desc", "submkt_desc", ".model")) # keeps only rows from final model spec
  
# *************************************************************************************************************************
# FORECAST & DATA OVERWRITES
# *************************************************************************************************************************

# take the final forecasts and bind them with the historical activity - this way the fundamental historical and forecast can be used as metro forecast variables
  offc_metro_fund_vars[[i]] <- offc_fundcast[[i]]$finalcast.offc_fund %>%
                               filter(date > vintage_date) %>%
                               select(date, msa_desc, class_desc, submkt_desc, offc_fundamental_desc, offc_fundamental_val) %>%
                               pivot_wider(names_from = offc_fundamental_desc, values_from = offc_fundamental_val) %>%
                               bind_rows(offc.fundamentals %>%
                               filter(date <= vintage_date) %>%
                               select(date, msa_desc, class_desc, submkt_desc, all_of(i)))
  
  if (i == "total_inventory")
# add total completions from construction pipeline data to total inventory, and calculate net deliveries - forecasts using inventory will now contain completions too
     {offc_metro_fund_vars[[i]] <- offc_metro_fund_vars[[i]] %>%
                                   left_join(offc.constr_pipeline %>% select(-total_prelease),
                                             by = c("date", "msa_desc", "class_desc", "submkt_desc")) %>%
                                   group_by(msa_desc, class_desc, submkt_desc) %>%
                                   arrange(date) %>%
                                   mutate("metro_{i}" := .data[[i]] + coalesce(total_completions, 0),
                                          inventory_shock = ifelse(yearquarter(date) < yearquarter(config$modeling$covid_cutoff), 0, .data[[i]]),
                                          inventory_delta = .data[[i]] - lag(.data[[i]], 1),
                                          inventory_delta_shock = ifelse(yearquarter(date) < yearquarter(config$modeling$covid_cutoff), 0, inventory_delta),
                                          rolling_inventory = slide_dbl(.data[[i]], mean, .before = 3, .complete = TRUE))

# overwrite the final inventory forecast with the one that includes completions
      offc_fundcast[[i]]$finalcast.offc_fund <- offc_fundcast[[i]]$finalcast.offc_fund %>%
                                                select(-offc_fundamental_val) %>%
                                                left_join(offc_metro_fund_vars[[i]] %>%
                                                          select(date, msa_desc, class_desc, submkt_desc, all_of(i)) %>%
                                                          pivot_longer(all_of(i),
                                                                       names_to = "offc_fundamental_desc", values_to = "offc_fundamental_val"),
                                                          by = c("date", "msa_desc", "class_desc", "submkt_desc", "offc_fundamental_desc"))
      
      } else if(i == "total_vacancy")
# create an identity "forecast" for vacancy
               {offc_metro_fund_vars[[i]] <- offc_metro_fund_vars[[i]] %>%
                                             left_join(offc_metro_fund_vars[["total_inventory"]],
                                                       by = c("date", "msa_desc", "class_desc", "submkt_desc")) %>%
                                             group_by(msa_desc, class_desc, submkt_desc) %>%
                                             arrange(date) %>%
                                             mutate(vacancy_shock = ifelse(yearquarter(date) < yearquarter(config$modeling$covid_cutoff), 0, .data[[i]]),
                                                    vacancy_delta = .data[[i]] - lag(.data[[i]], 1),
                                                    vacancy_delta_shock = ifelse(yearquarter(date) < yearquarter(config$modeling$covid_cutoff), 0, inventory_delta),
                                                    rolling_vacancy = slide_dbl(.data[[i]], mean, .before = 3, .complete = TRUE),
                                                    "{i}_rt" := .data[[i]]/ total_inventory) %>%
                                                    rename("metro_{i}" := i) %>%
                                             select(-contains("inventory"))
            
# overwrite vacancy rate forecasts, the offc_fundcast list now contains a new sub-list called total_vacancy_rt with the forecast.offc_fund df in it
                offc_fundcast[[glue("{i}_rt")]]$forecast.offc_fund <- offc_metro_fund_vars[[i]] %>%
                                                                      filter(date > vintage_date) %>%
                                                                      select(date, msa_desc, class_desc, submkt_desc, contains("_rt")) %>%
                                                                      mutate(cast_desc = "forecast",
                                                                             vintage = vintage_label,
                                                                             .model = "identity",
                                                                             lower = NA,
                                                                             upper = NA) %>%
                                                                      pivot_longer(contains("_rt"),
                                                                                   names_to = "offc_fundamental_desc", values_to = "offc_fundamental_val")

# create a final forecast df from the identity "forecast", and add it to the vacancy rate sub-list
                offc_fundcast[[glue("{i}_rt")]]$finalcast.offc_fund <- offc_fundcast[[glue("{i}_rt")]]$forecast.offc_fund
                
                } else if (i == "total_absorption")
# add total preleases from the construction pipeline to net aborption
                          {offc_metro_fund_vars[[i]] <- offc_metro_fund_vars[[i]] %>%
                                                        left_join(offc.constr_pipeline %>% select(-total_completions),
                                                                  by = c("date", "msa_desc", "class_desc", "submkt_desc")) %>%
                                                        group_by(msa_desc, class_desc, submkt_desc) %>%
                                                        arrange(date) %>%
                                                        mutate("metro_{i}" := .data[[i]] + coalesce(total_prelease, 0))

# overwrite the final absorption forecast with the one that includes preleases
                           offc_fundcast[[i]]$finalcast.offc_fund <- offc_fundcast[[i]]$finalcast.offc_fund %>%
                                                                     select(-offc_fundamental_val) %>%
                                                                     left_join(offc_metro_fund_vars[[i]] %>%
                                                                               select(date, msa_desc, class_desc, submkt_desc, all_of(i)) %>%
                                                                               pivot_longer(all_of(i),
                                                                                            names_to = "offc_fundamental_desc", values_to = "offc_fundamental_val"),
                                                                               by = c("date", "msa_desc", "class_desc", "submkt_desc", "offc_fundamental_desc"))

# create an identity "forecast" for net delivery, the offc_fundcast list now contains a new sub-list called net_delivery with the forecast.offc_fund df in it
     offc_fundcast[["total_delivery"]]$forecast.offc_fund <- offc_metro_fund_vars[[i]] %>%
                                                             left_join(offc_metro_fund_vars[["total_vacancy"]],
                                                                       by = c("date", "msa_desc", "class_desc", "submkt_desc")) %>%
                                                             filter(date > vintage_date) %>%
                                                             mutate(total_delivery = vacancy_delta + .data[[i]],
                                                                    cast_desc = "forecast",
                                                                    vintage = vintage_label,
                                                                    .model = "identity",
                                                                    lower = NA,
                                                                    upper = NA) %>%
                                                             select(-contains(c("vacancy", "absorption", "prelease", "completions"))) %>%
                                                             pivot_longer(total_delivery,
                                                                          names_to = "offc_fundamental_desc", values_to = "offc_fundamental_val")
  
# create a final forecast df from the identity "forecast", and add it to the net delivery sub-list
     offc_fundcast[["total_delivery"]]$finalcast.offc_fund <- offc_fundcast[["total_delivery"]]$forecast.offc_fund
  }# end if/else
  
  offc.fund_fcast_vars <- offc_metro_fund_vars[[i]] %>%
                          left_join(offc.fund_fcast_vars,
                                    by = c("date", "msa_desc", "class_desc", "submkt_desc"))
} # end for (i in offc_fcast_fundamentals)

# *************************************************************************************************************************
# FORECAST & DATA EXPORT
# *************************************************************************************************************************

# take each forecast list element and combine them by data type, this will aggregate all of the forecast files of one type together
offc_fundcast_split <- transpose(offc_fundcast)

# combine the dataframes, similar to pmap_df but for the sub-lists of each list
offc_fund_fcast <- map(offc_fundcast_split, \(sub_list) {bind_rows(sub_list)})

for (fcast in names(offc_fund_fcast))
  {write_csv(offc_fund_fcast[[fcast]], glue("{config$paths$forecast_dir}/{fcast}_{vintage_label}.csv"))}

# aggregate the metro fundamentals and join them with the metro forecast variables (for cap rate forecasting)
offc.metro_fcast_vars_ext <- offc_fund_fcast$finalcast.offc_fund %>%
                             filter(date > max(offc.fundamentals$date)) %>%
                             select(date, msa_desc, class_desc, submkt_desc, offc_fundamental_desc, offc_fundamental_val) %>%
                             pivot_wider(names_from = offc_fundamental_desc, values_from = offc_fundamental_val) %>%
                             bind_rows(offc.fundamentals) %>%
                             filter(class_desc == "Total") %>%
                             mutate(total_rent = wgtavg_rent * total_inventory) %>%
                             summarise(metro_total_inventory = sum(total_inventory),
                                       metro_total_absorption = sum(total_absorption),
                                       metro_total_vacancy = sum(total_vacancy),
                                       metro_wgtavg_rent = sum(total_rent) / sum(total_vacancy),
                                       .by = c((date), msa_desc)) %>%
                             group_by(msa_desc) %>%
                             arrange(date) %>%
                             mutate(metro_vacancy_rt = metro_total_vacancy / metro_total_inventory,
                                    metro_rent_1yr_growth = (metro_wgtavg_rent / lag(metro_wgtavg_rent, 1)) - 1,
                                    covid_shock = ifelse(yearquarter(date) < yearquarter(config$modeling$covid_cutoff), 0, 1),
                                    across(where(is.numeric) & !covid_shock, .fns = list(shock = ~ .x * covid_shock),.names = "{.col}_{.fn}")) %>%
                             ungroup() %>%
                             left_join(offc.metro_fcast_vars, by = c("date", "msa_desc"))

write_csv(offc.metro_fcast_vars_ext, glue("{config$paths$output_dir}/offc_metro_forecast_vars_{vintage_label}.csv"))

# *************************************************************************************************************************
# FORECAST & DATA EXPORT
# *************************************************************************************************************************
offc_fund_forecast <- offc_fund_fcast$finalcast.offc_fund %>%
                      bind_rows(offc.fundamentals %>%
                      filter(date <= vintage_date) %>%
                      pivot_longer(-c(date, msa_desc, class_desc, submkt_desc),
                                   names_to = "offc_fundamental_desc", values_to = "offc_fundamental_val") %>%
                      mutate(vintage = "Historical",
                             cast_desc = "actual"))

write_csv(offc_fund_forecast, glue("{config$paths$report_dir}/offc_fund_forecast_{vintage_label}.csv"))

if (vintage_desc == "retro")
  {
   backcast.offc_fund <- offc_fund_fcast$forecast.offc_fund %>%
                         bind_rows(offc.fundamentals %>%
                         pivot_longer(-c(date, msa_desc, class_desc, submkt_desc),
                                      names_to = "offc_fundamental_desc", values_to = "offc_fundamental_val") %>%
                         mutate(vintage = "historical",
                                cast_desc = "actual"))
  
   write_csv(backcast.offc_fund, glue("{config$paths$report_dir}/backcast.offc_fund_{vintage_label}.csv"))
  }

# *************************************************************************************************************************
# REPORTING
# *************************************************************************************************************************

# uncomment the below code to import downloaded files
finalcast_files <- list.files(config$paths$forecast_dir, pattern = "finalcast.") %>% str_remove("\\.csv$")

offc_finalcast <- list()

for (i in finalcast_files)
  {offc_finalcast[[i]] <- read_csv(glue("{config$paths$forecast_dir}/{i}.csv"), name_repair = "minimal")}

hist.offc_finalcast <- pmap_df(list(offc_finalcast), bind_rows) %>%
                       bind_rows(offc.fundamentals %>%
                               # filter(date <= vintage_date) %>%
                                 pivot_longer(-c(date, msa_desc, class_desc, submkt_desc),
                                              names_to = "offc_fundamental_desc", values_to = "offc_fundamental_val") %>%
                                 mutate(vintage = "Historical",
                                        cast_desc = "actual"))

forecast_files <- list.files(config$paths$forecast_dir, pattern = "forecast.") %>% str_remove("\\.csv$")

offc_forecast <- list()

for (i in forecast_files)
{offc_forecast[[i]] <- read_csv(glue("{config$paths$forecast_dir}/{i}.csv"), name_repair = "minimal")}

hist.offc_backcast <- pmap_df(list(offc_forecast), bind_rows) %>%
                      bind_rows(offc.fundamentals %>%
                              # filter(date <= vintage_date) %>%
                                pivot_longer(-c(date, msa_desc, class_desc, submkt_desc),
                                             names_to = "offc_fundamental_desc", values_to = "offc_fundamental_val") %>%
                                mutate(vintage = "Historical",
                                       cast_desc = "actual"))

print(glue("Forecast production for {yearquarter(vintage_date)} completed successfully! Files are accessible in the {config$paths$forecast_dir} directory"))
