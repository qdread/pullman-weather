# Script to replace missing values

# Algorithm:

# For temperature, radiation, wind speed, and humidity, if <5 consecutive values are missing, linearly interpolate.
# If >=5 consecutive values are missing, use statistical average of the same date and time from all other years but smooth the edges with the values from 
# immediately before or after the time.

# For precipitation, if <5 consecutive values are missing, randomly sample from a distribution based on the 24 hour period around when the values are missing
# If >=5 consecutive values are missing, randomly sample from a distribution based on a moving average of the days and times that the 

interpolate_variable <- function(date, time, x) {
  # generate run lengths of missing values
  rle_missing <- rle(is.na(x))
  run_lengths <- rep(rle_missing$lengths, rle_missing$lengths)
  
  for (i in 1:)
}

# Smoothing that will take the average of values from all other years, including a moving window of 5 days with weights of 1, 2, 3, 2, 1

library(lubridate)

needed_raw_data[, Year := year(Date)]
needed_raw_data[, Month := month(Date)]
needed_raw_data[, Day := day(Date)]


means_bydaytime <- needed_raw_data[, lapply(.SD, mean, na.rm = TRUE), by = .(Month, Day, Time), .SDcols = obs_vars[1:4]]

# Unfortunately Feb 29 has no data for any years. Both Feb 29s fall during the period of missing data.