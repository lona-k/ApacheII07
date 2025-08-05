
source("functions.R")
# remotes::install_github("mlr-org/mlr3tuning@v1.3.0")  # for tuning when we get to that 

library(mlr3verse)
library(mlr3proba)
library(mlr3pipelines)
library(dplyr)
library(tidyverse)

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



## NA stragegy ####

dummy_cols <- grep("___", names(data_df), value = TRUE)
prefixes  <- unique(sub("___.*$", "", dummy_cols))

data_df <- data_df %>%
  # find all dummy columns (they all have "___" in their name)
  { . } %>%
  { 
    # for each prefix, create the collapsed column, then drop the dummies
    out <- .
    for(prefix in prefixes) {
      cols <- grep(paste0("^", prefix, "___"), names(out), value = TRUE)
      
      # create new integer column: the suffix of the one-hot that's 1
      out[[prefix]] <- apply(out[cols], 1, function(r) {
        i <- which(r == 1L)
        if (length(i) == 1L) {
          as.integer(sub("^.*___", "", cols[i]))
        } else {
          NA_integer_
        }
      })
      
      # drop the old dummy columns
      out <- select(out, -all_of(cols))
    }
    out
  }

data_na <- as_task_surv(x = data_df,
                          time = "surv_icu0to60",
                          event = "surv_icu_death",
                          type = "right")

data_na$select(setdiff(data_na$feature_names, c(setdiff_exclude, "CombinedID")))


graph = po("imputesample") %>>% lrn_logreg
graph$plot(horizontal = TRUE)


data_na$missings()

graph_na <- po("imputelearner", lrn("regr.rpart")) %>>% lrn("surv.coxph")
graph_na <- po("imputelearner") %>>% lrn("surv.coxph")

graph_na2 <- as_learner(po("imputesample") %>>% lrn("surv.coxph"))
res <- graph_na2$train(data_na)
graph_na2$predict_newdata(test)
graph_na$train(data_na)

pred_save(graph_na, test, running_number = "cox_na_samples")

