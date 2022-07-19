# Join in wide form.

join_vars = c('Date','Month','Day','Time')
env_vars = c('T_degC', 'WS_mph', 'RH_pct', 'SR_in_Wm2', 'PRCP_in')
env_vars_amf = c('T_degC', 'WS_mph', 'RH_pct', 'PPFD', 'PRCP_in')
setnames(scan_ts, old = env_vars, new = paste0('SCAN_', env_vars))
setnames(awn_ts, old = env_vars, new = paste0('AWN_', env_vars))
setnames(amfrc1_ts, old = env_vars_amf, new = paste0('AMFRC1_', env_vars))
setnames(amfrc2_ts, old = env_vars_amf, new = paste0('AMFRC2_', env_vars))
allts_wide <- Reduce(\(...) merge(..., by = join_vars, all = TRUE), list(scan_ts, awn_ts, amfrc1_ts, amfrc2_ts))

fwrite(allts_wide, 'project/weather_data/allts_wide.csv')

