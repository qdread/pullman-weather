# PULLMAN WEATHER DATA INTERPOLATION
# Post-processing of random forest model output to reduce file size so it can be downloaded from SciNet.
# QDR 2022-07-28

library(data.table)
library(caret)
library(purrr)
library(glue)

pred_names <- c('AWN', 'AMFRC1', 'AMFRC2', 'all')
pred_list <- list('AWN', 'AMFRC1', 'AMFRC2', c('AWN', 'AMFRC1', 'AMFRC2'))

fits <- map(pred_names, ~ readRDS(glue('project/weather_data/rf_fits{.}.rds'))) # Load objects

# Get summary information on final fit quality
fit_summaries <- cbind(
  expand.grid(variable = fits[[1]][['variable']], predictors = pred_names),
  map_dfr(fits, \(fitdt) map_dfr(fitdt[['fit']], \(fit) fit[['results']][fit[['results']][['mtry']] == fit[['bestTune']][1,1], ]))
)

# Create data frame of complete cases that were used for out of sample prediction in each case
walk(1:4, \(i) {
  vars <- pred_list[[i]]
  data_subset <- map(fits[[i]][['data']], \(dat) dat[complete.cases(dat[, ..vars])])
  fits[[i]][, data_subset := data_subset]
})

# Combine all predicted values together for each fit type, rounding appropriately
make_pred_data <- function(variable, n_decimal, data_subset, predict_outsample, ...) {
  y_pred <- round(predict_outsample, n_decimal)
  cbind(data.table(variable = variable, y_pred = predict_outsample), data_subset)
}

all_pred_list <- map(fits, \(fit) pmap_dfr(fit, make_pred_data))

# Put everything into single data.table and write to CSV
all_pred_dt <- map2_dfr(pred_names, all_pred_list, ~ data.table(predictors = .x, .y))
all_pred_dt[, c('SCAN', 'AWN', 'AMFRC1', 'AMFRC2') := NULL]

fwrite(all_pred_dt, 'project/weather_data/rf_all_pred.csv')
fwrite(fit_summaries, 'project/weather_data/rf_fit_summaries.csv')