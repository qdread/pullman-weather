# First, get a moving average of all the variables other than precipitation, using the adjacent two values.

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

# For precipitation, regardless of the length of the missing run, we will randomly sample an hourly value from +/- 2 hours and +/- 2 days in the current year and all other years.

random_precip_value <- function(Date, Time, ...) {
  days_use <- Date + -2:2
  hr <- as.numeric(substr(Time, 1, 2))
  times_use <- str_pad(paste0(hr + -2:2, ':00'), width = 5, pad = '0')
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
fiveday_means <- cbind(fiveday_means, `PRCP.H-1 (in)` = fiveday_precip)
for (v in obs_vars) {
  data_all_filled[[v]] <- ifelse(is.na(data_all_filled[[v]]), fiveday_means[[v]], data_all_filled[[v]])
}

# Calculate the longwave radiation column using the Stefan-Boltzmann law
# j = emissivity * sigma * (temp_C + 273.15)^4
# where emissivity is set at 0.9 (unitless), and sigma is the Stefan-Boltzmann constant 5.670374e-8 W m-2 K-4 
sigma <- 5.670374e-8
emissivity <- 0.9
data_all_filled[, `LWR (W m-2)` := emissivity * sigma * (`TOBS.I-1 (degC)` + 273.15)^4]

# Make flags saying which values were imputed and which were measured. LWR is imputed if temperature is imputed.
imputation_flags <- needed_raw_data[, lapply(.SD, is.na), .SDcols = obs_vars]
setnames(imputation_flags, paste0('imputed_', names(imputation_flags)))
imputation_flags[, `imputed_LWR (W m-2)` := `imputed_TOBS.I-1 (degC)`]

# Combine flags with imputed data and write to a file.
data_all_filled <- cbind(data_all_filled, imputation_flags)
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
