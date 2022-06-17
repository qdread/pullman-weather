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
