library(data.table)
library(purrr)
library(glue)
library(stringr)
library(lubridate)

years <- 2014:2021

# Files have different number of rows to skip 
skip_n_scan <- c(2, 0, 4, 4, 4, 4, 4, 4)
skip_n_solar <- c(2, 4, 4, 4, 4, 4, 4, 4)

# Read data
scan_raw <- map2(years, skip_n_scan, ~ fread(glue('project/weather_data/2198_SCAN_WATERYEAR={.x}.csv'), skip = .y)) 
solar_raw <- map2(years, skip_n_solar, ~ fread(glue('project/weather_data/2198_SOLAR_WATERYEAR={.x}.csv'), skip = .y)) 

# Convert Date column to Date format for all, if it is not already
for (i in 1:8) {
  if (!'Date' %in% class(scan_raw[[i]][['Date']])) {
    scan_raw[[i]][, Date := as.Date(Date, format = '%m/%d/%Y')]
  } else {
    scan_raw[[i]][, Date := as.Date(Date)]
  }
  if (!'Date' %in% class(solar_raw[[i]][['Date']])) {
    solar_raw[[i]][, Date := as.Date(Date, format = '%m/%d/%Y')]
  } else {
    solar_raw[[i]][, Date := as.Date(Date)]
  }
}

scan_data <- rbindlist(scan_raw, fill = TRUE)
solar_data <- rbindlist(solar_raw, fill = TRUE)

# Get rid of bad column
scan_data[, V32 := NULL]
solar_data[, V5 := NULL]

# Get rid of bad rows that all have 23:59 as the time
scan_data <- scan_data[!Time %in% '23:59']
solar_data <- solar_data[!Time %in% '23:59']

# Some of the times are formatted differently from others. Make sure all are padded with 0 if hour has only 1 digit.
scan_data[, Time := str_pad(Time, 5, pad = '0')]
solar_data[, Time := str_pad(Time, 5, pad = '0')]

# Sort and set keys for each data table, then do a full join.
setkey(scan_data, `Site Id`, Date, Time)
setkey(solar_data, `Site Id`, Date, Time)

unique_times <- unique(rbind(scan_data[,.(`Site Id`, Date, Time)], solar_data[, .(`Site Id`, Date, Time)]))

all_raw_data <- scan_data[solar_data[unique_times]]

# Create data.frame of all the dates and times with no gaps and join the data to it, to show how many are missing.
date_range <- range(all_raw_data[['Date']])
all_date_time <- CJ(Date = seq(from = date_range[1], to = date_range[2], by = 1), Time = glue('{sprintf("%02d", 0:23)}:00'))

all_raw_data <- all_raw_data[all_date_time, on = .(Date, Time)]

# Create data frame with only the desired variables
needed_raw_data <- all_raw_data[, .(Date, Time, `TOBS.I-1 (degC)`, `WSPDX.H-1 (mph)`, `RHUM.I-1 (pct)`, `SRADV.H-1 (watt)`, `PRCP.H-1 (in)`)]

obs_vars <- c('TOBS.I-1 (degC)', 'WSPDX.H-1 (mph)', 'RHUM.I-1 (pct)', 'SRADV.H-1 (watt)', 'PRCP.H-1 (in)')

# Convert -99.9 codes to NA
needed_raw_data[, (obs_vars) := lapply(.SD, function(x) ifelse(x == -99.9, as.numeric(NA), x)), .SDcols = obs_vars]

# Add month, day, and time columns
needed_raw_data[, Month := month(Date)]
needed_raw_data[, Day := day(Date)]
needed_raw_data[, Time := as.numeric(substr(Time, 1, 2))]