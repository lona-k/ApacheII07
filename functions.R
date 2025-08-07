
<<<<<<< HEAD
pred_save <- function(learner, test, running_number, inverse = TRUE) {
  prds <- learner$predict_newdata(test)
  if (inverse) risk <- - prds$crank
  prds_df <- data.frame(CombinedID = test$CombinedID,
                        risk = risk)
  readr::write_csv(prds_df, file = sprintf("submission_apacheII07_%s.csv", running_number))
=======
pred_save <- function(model, test, running_number, inverse = TRUE) {
  preds <- predict(model, newdata = test, type = "risk")
  prds_df <- data.frame(CombinedID = test$CombinedID,
                        risk = preds)

  write.csv(prds_df, file = sprintf("submission_apacheII07_%s.csv", running_number))
>>>>>>> 03cb7ee991a5de58760c444f164f2e4be2617bd0
}

