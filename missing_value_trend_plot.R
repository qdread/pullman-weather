source('initial_processing.R')

library(ggplot2)

theme_set(
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank())
)

# Summarize number of missing values per 24-hour period (may range from 0-24) per response variable

missing_byday <- needed_raw_data[, lapply(.SD, function(x) sum(is.na(x))), by = .(Date), .SDcols = obs_vars] |>
  melt(id.vars = 'Date', value.name = 'n_missing')

# Make a plot, shading in alternating stripes by water year
wateryear_shading <- data.frame(d_min = as.Date(c('2013-10-01', '2015-10-01', '2017-10-01', '2019-10-01')),
                                d_max = as.Date(c('2014-09-30', '2016-09-30', '2018-09-30', '2020-09-30')))

ggplot(missing_byday) +
  geom_rect(data = wateryear_shading, aes(xmin = d_min, xmax = d_max, ymin = -Inf, ymax = Inf), fill = 'skyblue') +
  geom_line(aes(x = Date, y = n_missing)) + facet_wrap(~ variable, ncol = 1) + 
  scale_y_continuous(limits = c(0, 24), breaks = c(0, 6, 12, 18, 24), name = 'number of missing values') + scale_x_date(expand = c(0, 0)) +
  theme(axis.title.x = element_blank())

ggsave('project/figures/missing_value_trends.pdf')


# Run lengths of missing values -------------------------------------------

rle_na <- map_dfr(obs_vars, ~ do.call(cbind, c(variable = .x, rle(is.na(needed_raw_data[[.x]])))) |> as.data.table())
rle_na <- rle_na[values == TRUE]
rle_na[, lengths := as.integer(lengths)]

ggplot(rle_na, aes(x = lengths)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ variable) + scale_x_log10(name = 'run length of consecutive missing values') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)))

ggsave('project/figures/missing_value_runlength_loghistogram.pdf')

# The majority of the runs are only 1 hour missing at once, followed by a few dozen with <10 hours missing, then around half a dozen with 20-30 hours missing,
# then all variables have 2 places where about a week was missing, then the 3 runs of 3, 8, and 12 months missing.

# Precipitation RLE:
rle_precip <- rle(needed_raw_data$`PRCP.H-1 (in)` > 0)
rle_precip <- data.table(length = rle_precip$lengths, any_precip = rle_precip$values)

table(rle_precip[(any_precip)][['length']]) # Most precipitation events are 1 hour long but there are many that are longer.
