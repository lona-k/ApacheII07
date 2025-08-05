
source("functions.R")

library(mlr3verse)
library(mlr3proba)
library(dplyr)

# Preprocessing ####

data_df <- readRDS("icu_train.Rds")
test <- readRDS("icu_test.Rds")
icu <- readRDS("ICU.Rds")

data_df <- data_df %>%
  mutate(across(where(~ inherits(.x, "difftime")), as.numeric),
         surv_icu_death = ifelse(surv_icu_status == 2, 1, 0))

to_factor <- c(
  "Propofol2_4", "inMV2_4", "OralIntake2_4", "PN2_4", "EN2_4",
  "PatientDied", "PatientDischarged", "surv_icu_status",
  "comorbidities_yn", "blood_sugar_yn"
)

data_df <- data_df %>%
  mutate(across(all_of(to_factor), as.factor))


full_na <- names(Filter(function(col) all(is.na(col)), data_df))
any_na <- names(Filter(function(col) any(is.na(col)) & !all(is.na(col)), data_df))
setdiff_exclude <- setdiff(colnames(data_df), colnames(test))

# impute_features <- c("Surv0To60", "Disc0To60", "sistersite", "admission_type", "DiagID")

data <- as_task_surv(x = data_df,
                      time = "surv_icu0to60",
                      event = "surv_icu_death",
                      type = "right")


# Death vs. no Death ####

## baseline model ####

data_full <- as_task_surv(x = data_df,
                     time = "surv_icu0to60",
                     event = "surv_icu_death",
                     type = "right")

data_complete <- data_full$clone(deep = TRUE)
data_complete$select(setdiff(data_complete$feature_names, c(any_na, setdiff_exclude, "CombinedID")))

# cv_10 <- rsmp("cv", folds = 10)
lrn_cox <- lrn("surv.coxph")
lrn_cox$train(data_complete)

pred <- lrn_cox$predict_newdata(test)
pred_save(lrn_cox, test, running_number = "cox2")




