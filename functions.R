
pred_save <- function(model, test, running_number, inverse = TRUE) {
  preds <- predict(model, newdata = test, type = "risk")
  prds_df <- data.frame(CombinedID = test$CombinedID,
                        risk = preds)

  write.csv(prds_df, file = sprintf("submission_apacheII07_%s.csv", running_number))
}
