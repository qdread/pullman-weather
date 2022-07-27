sr_dat <- all_long[variable == 'SR_in_Wm2' & !is.na(SCAN) & !is.na(AWN)]
sr_dat[, SCAN_tr := log(SCAN + 0.01)]
sr_dat[, AWN_tr := log(AWN + 0.01)]

set.seed(7271)

library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

sr_awn_rf <- train(
  form = SCAN ~ AWN,
  data = sr_dat,
  method = 'rf',
  trControl = trainControl(method = 'cv', number = 10, predictionBounds = c(0, NA), verboseIter = TRUE, allowParallel = TRUE)
)

summary(sr_awn_rf)

sr_awn_rf_pred_IS <- predict(sr_awn_rf)
