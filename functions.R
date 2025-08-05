
pred_save <- function(learner, test, running_number, inverse = TRUE) {
  prds <- learner$predict_newdata(test)
  if (inverse) risk <- 1 - prds$data$crank
  prds_df <- data.frame(CombinedID = test$CombinedID,
                        risk = risk)
  write.csv(prds_df, file = sprintf("submission_apacheII07_%s.csv", running_number))
}
