# PULLMAN WEATHER DATA INTERPOLATION
# Fit random forest models to SCAN versus all other time series. Use 10-fold CV. 
# Predict out the results.

library(data.table)
library(purrr)
library(furrr)
library(caret)
library(Rutilitybelt)

plan(multicore(workers = 5))

load('project/weather_data/4ts_intermediate.RData') # loads time series.

# Rename PPFD to SR_in_Wm2 for Ameriflux
setnames(amfrc1_ts, old = 'PPFD', new = 'SR_in_Wm2')
setnames(amfrc2_ts, old = 'PPFD', new = 'SR_in_Wm2')

# Join all data together
id_vars <- c('Date','Month','Day','Time')
scan_long <- melt(scan_ts, id.vars = id_vars, value.name = 'SCAN')
awn_long <- melt(awn_ts, id.vars = id_vars, value.name = 'AWN')
amfrc1_long <- melt(amfrc1_ts, id.vars = id_vars, value.name = 'AMFRC1')
amfrc2_long <- melt(amfrc2_ts, id.vars = id_vars, value.name = 'AMFRC2')

all_long <- Reduce(\(...) merge(..., all = TRUE), list(scan_long, awn_long, amfrc1_long, amfrc2_long))

rf_fits <- group_nest_dt(all_long, variable)

# Set prediction bounds and number of decimal places to round in post-processing for all the variables
prediction_bounds <- data.table(
  variable = c('T_degC', 'WS_mph', 'RH_pct', 'PRCP_in', 'SR_in_Wm2'),
  lower_bound = c(NA, 0, 0, 0, 0),
  upper_bound = c(NA, NA, 100, NA, NA),
  n_decimal = c(1, 1, 0, 2, 2)
)

rf_fits <- rf_fits[prediction_bounds, on = 'variable']

# function to fit random forest model for a particular dataset (variable) and predictors
fit_rf <- function(variable, data, lower_bound, upper_bound, predictors, ...) {
  data_fit <- data[, mget(c('SCAN', predictors))]
  data_fit <- data_fit[complete.cases(data_fit)]
  
  rf_control <- trainControl(method = 'cv', number = 10, 
                             allowParallel = TRUE, verboseIter = TRUE, 
                             prediction_bounds = c(lower_bound, upper_bound))
  
  train(
    form = as.formula(paste('SCAN ~', paste(predictors, collapse = '+'))),
    data = data_fit,
    method = 'rf',
    trControl = rf_control
  )
}

# Apply the functions!!!
rf_fits[, fit_AWN := future_pmap(rf_fits, fit_rf, predictors = 'AWN')]
rf_fits[, fit_AMFRC1 := future_pmap(rf_fits, fit_rf, predictors = 'AMFRC1')]
rf_fits[, fit_AMFRC2 := future_pmap(rf_fits, fit_rf, predictors = 'AMFRC2')]
rf_fits[, fit_all := future_pmap(rf_fits, fit_rf, predictors = c('AWN', 'AMFRC1', 'AMFRC2'))]

rf_fits[, predict_AWN_insample := map(fit_AWN, predict)]
rf_fits[, predict_AMFRC1_insample := map(fit_AMFRC1, predict)]
rf_fits[, predict_AMFRC2_insample := map(fit_AMFRC2, predict)]
rf_fits[, predict_all_insample := map(fit_all, predict)]

rf_fits[, predict_AWN_outsample := map2(fit_AWN, data, ~ predict(.x, newdata = .y))]
rf_fits[, predict_AMFRC1_outsample := map2(fit_AMFRC1, data, ~ predict(.x, newdata = .y))]
rf_fits[, predict_AMFRC2_outsample := map2(fit_AMFRC2, data, ~ predict(.x, newdata = .y))]
rf_fits[, predict_all_outsample := map2(fit_all, data, ~ predict(.x, newdata = .y))]

# Additional post-processing can be done locally later.
saveRDS(rf_fits, file = 'project/weather_data/all_rf_fits.rds')
