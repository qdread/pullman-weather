# WEATHER DATA IMPUTATION

source('initial_processing.R')

# Function with five hour moving window rolling mean 
temp_roll <- frollmean(needed_raw_data$`TOBS.I-1 (degC)`, n = 5, align = 'center', fill = NA, algo = 'exact', hasNA = TRUE, na.rm = TRUE)

fivehour_roll <- function(x) frollmean(x, n = 5, align = 'center', fill = NA, algo = 'exact', hasNA = TRUE, na.rm = TRUE)

mean_vars <- obs_vars[1:4]

data_fivehour_rollmean <- copy(needed_raw_data)
data_fivehour_rollmean[, (mean_vars) := lapply(.SD, fivehour_roll), .SDcols = mean_vars]

# Fill in the missing value only if needed
data_fivehour_filled <- copy(needed_raw_data)
for (v in mean_vars) {
  data_fivehour_filled[[v]] <- ifelse(is.na(needed_raw_data[[v]]), data_fivehour_rollmean[[v]], needed_raw_data[[v]])
}

# Now look again at the RLE of missing values
rle_na_fill5 <- map_dfr(obs_vars, ~ do.call(cbind, c(variable = .x, rle(is.na(data_fivehour_filled[[.x]])))) |> as.data.table())
rle_na_fill5 <- rle_na_fill5[values == TRUE]
rle_na_fill5[, lengths := as.integer(lengths)]
table(rle_na_fill5$lengths)

# Next, we will take the long-run average of days and times. This will be done also with a 5 length moving average to get two days on either side of the target day.
# So to get the average for time t at day d, we will use the average for time t on days d-2, d-1, d, d+1, and d+2

get_fiveday_mean <- function(Date, Time, ...) {
  days_use <- Date + -2:2
  m_d_t_use <- data.table(Month = month(days_use), Day = day(days_use), Time = Time)
  data_use <- data_fivehour_filled[m_d_t_use, on = .(Month, Day, Time)]
  data_use[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mean_vars]
}

fiveday_means <- pmap(data_fivehour_filled, get_fiveday_mean) |> rbindlist()

# Added 22 June 2022: add random noise to the missing values so that the standard deviation of the imputed values is about the same  
# as the observed values.
# If you sum two normals, their variances are summed (but not the SDs as sd = sqrt(var))
# However we cannot just add normal noise to wind speed, RH, and SR because we can't have values <0 for these
# Do not add anything to the shortwave radiation, as the variance of imputed data appears to be similar to the observed data.

# For temperature, we can just add normal noise.
# For WS, we have a lower bound of 0 but no upper bound. So we can add a tiny number, take the log transform, add normal noise, and back-transform.
# For RH, we have a lower bound of 0 and an upper bound of 100. So we can divide by 100, take the logit transform, add normal noise, and back-transform. (If it's exactly 100, don't add any noise)

fiveday_means_withnoise <- copy(fiveday_means)

fiveday_means_withnoise[, `WSPDX.H-1 (mph)` := log(`WSPDX.H-1 (mph)`)]
fiveday_means_withnoise[, `RHUM.I-1 (pct)` := qlogis(`RHUM.I-1 (pct)`/100)]

dat_observed <- data_fivehour_filled[, ..mean_vars]

dat_observed[, `WSPDX.H-1 (mph)` := log(`WSPDX.H-1 (mph)`)]
dat_observed[, `RHUM.I-1 (pct)` := qlogis(`RHUM.I-1 (pct)`/100)]

var_imputed <- sapply(fiveday_means_withnoise, var, na.rm = TRUE)
var_observed <- sapply(dat_observed, \(x) var(x[is.finite(x)], na.rm = TRUE))
sd_noise <- sqrt(var_observed - var_imputed)

# Apply a ballpark standard deviation to the wind speed variable, calculated by hand
sd_noise[2] <- 0.4

set.seed(410)
noise <- sapply(sd_noise, \(sd) rnorm(n = nrow(fiveday_means), mean = 0, sd = sd))
# No noise added to shortwave radiation: set to zero
noise[, 'SRADV.H-1 (watt)'] <- 0

# Add noise and back-transform, setting any remaining negative values to zero
fiveday_means_withnoise <- fiveday_means_withnoise + noise
fiveday_means_withnoise[, `WSPDX.H-1 (mph)` := exp(`WSPDX.H-1 (mph)`)]
fiveday_means_withnoise[, `RHUM.I-1 (pct)` := plogis(`RHUM.I-1 (pct)`) * 100]

# Round the imputed data to the same number of decimal places as the original data
fiveday_means_withnoise[, `TOBS.I-1 (degC)` := round(`TOBS.I-1 (degC)`, 1)]
fiveday_means_withnoise[, `WSPDX.H-1 (mph)` := round(`WSPDX.H-1 (mph)`, 1)]
fiveday_means_withnoise[, `RHUM.I-1 (pct)` := round(`RHUM.I-1 (pct)`, 0)]
fiveday_means_withnoise[, `SRADV.H-1 (watt)` := round(`SRADV.H-1 (watt)`, 2)]

# Check the standard deviations
sapply(fiveday_means_withnoise, sd, na.rm = TRUE)
sapply(data_fivehour_filled[, ..mean_vars], sd, na.rm = TRUE)

# For precipitation, regardless of the length of the missing run, we will randomly sample an hourly value from +/- 2 hours and +/- 2 days in the current year and all other years.

random_precip_value <- function(Date, Time, ...) {
  days_use <- Date + -2:2
  hr <- as.numeric(substr(Time, 1, 2))
  times_use <- (hr + -2:2) %% 24
  m_d_t_use <- data.table(Month = month(days_use), Day = day(days_use))
  m_d_t_use <- m_d_t_use[, .(Time = times_use), by = .(Month, Day)]
  data_use <- data_fivehour_filled[m_d_t_use, on = .(Month, Day, Time)]
  sample(na.omit(data_use[["PRCP.H-1 (in)"]]), size = 1)
}

set.seed(919)
fiveday_precip <- pmap_dbl(data_fivehour_filled, random_precip_value)

# Fill in all the missing values with the filled in values, and create a column for each data variable to flag whether they are imputed

# Fill in the missing value only if needed
data_all_filled <- copy(data_fivehour_filled)
fiveday_means_combined <- cbind(fiveday_means_withnoise, `PRCP.H-1 (in)` = fiveday_precip)
for (v in obs_vars) {
  data_all_filled[[v]] <- ifelse(is.na(data_all_filled[[v]]), fiveday_means_combined[[v]], data_all_filled[[v]])
}

# Calculate the longwave radiation column using the Stefan-Boltzmann law
# j = emissivity * sigma * (temp_C + 273.15)^4
# where emissivity is set at 0.9 (unitless), and sigma is the Stefan-Boltzmann constant 5.670374e-8 W m-2 K-4 
sigma <- 5.670374e-8
emissivity <- 0.9
data_all_filled[, `LWR (W m-2)` := round(emissivity * sigma * (`TOBS.I-1 (degC)` + 273.15)^4, 2)]

# Make flags saying which values were imputed and which were measured. LWR is imputed if temperature is imputed.
imputation_flags <- needed_raw_data[, lapply(.SD, is.na), .SDcols = obs_vars]
setnames(imputation_flags, paste0('imputed_', names(imputation_flags)))
imputation_flags[, `imputed_LWR (W m-2)` := `imputed_TOBS.I-1 (degC)`]

# Combine flags with imputed data and write to a file.
data_all_filled <- cbind(data_all_filled, imputation_flags)
data_all_filled[, c('Month', 'Day') := NULL]
fwrite(data_all_filled, 'project/weather_data/data_all_imputed.csv')

# Also write file in the DHSVM format. Order columns as required by the format, and remove headers.
# Swap location of precipitation and longwave columns.
# Comvert windspeed units from mi/h to m/s, and precip units from in/h to m/h
datetime_formatted <- with(data_all_filled, paste(format(Date, '%m/%d/%Y'), substr(Time, 1, 2), sep = '-'))
cols_use <- c(obs_vars[1:4], 'LWR (W m-2)', obs_vars[5])
dhsvm_data <- data.table(DateTime = datetime_formatted, data_all_filled[, ..cols_use])

dhsvm_data[, `PRCP.H-1 (in)` := `PRCP.H-1 (in)` * 0.0254]
dhsvm_data[, `WSPDX.H-1 (mph)` := `WSPDX.H-1 (mph)` * 0.44704]

fwrite(dhsvm_data, 'project/weather_data/dhsvm_forcing_imputed.txt', sep = ' ', quote = FALSE, col.names = FALSE)
