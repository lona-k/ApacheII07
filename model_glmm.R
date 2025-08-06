source("functions.R")
# remotes::install_github("mlr-org/mlr3tuning@v1.3.0") # do this on windows!!
# library(mlr3verse)
# library(mlr3proba)
# library(mlr3pipelines)
library(dplyr)
library(tidyverse)
# library(mlr3learners) 
library(survival)
library(mice)

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

test <- test %>%
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


# impute_features <- c("Surv0To60", "Disc0To60", "sistersite", "admission_type", "DiagID")


# merge patient and icu
# include time and icu hospital id

data_df <- data_df %>% 
  mutate(CombinedicuID = as.integer(CombinedicuID))
test <- test %>% 
  mutate(CombinedicuID = as.integer(CombinedicuID))

icu <- icu %>% 
  mutate(CombinedicuID = as.integer(CombinedicuID))

# Left-join ICU info onto the patient table
data_icu <- data_df %>%
  left_join(icu, by = c("CombinedicuID", "Year"))
test_icu <- test %>%
  left_join(icu, by = c("CombinedicuID", "Year"))

full_na <- names(Filter(function(col) all(is.na(col)), data_df))
any_na <- names(Filter(function(col) any(is.na(col)) & !all(is.na(col)), data_df))
setdiff_exclude <- setdiff(colnames(data_df), colnames(test))

features_icu_no_na <- setdiff(colnames(data_icu), c(any_na, setdiff_exclude, "CombinedID", "Year", "CombinedicuID"))
features_icu_na <- setdiff(colnames(data_icu), c(setdiff_exclude, "CombinedID", "Year", "CombinedicuID"))


# Lasso model #### 




# GLMM ####

# baseline
vars <- paste0(features_icu_no_na, collapse = " + ")
form_cox <- as.formula(sprintf("Surv(surv_icu0to60, surv_icu_death) ~  frailty(CombinedicuID) + as.numeric(as.character(Year))"))
frail_cox <- coxph(
  form_cox,
  # form_cox,
  data = data_icu
)

pred_save(frail_cox, test, running_number = "cox_icu_simple2", preds = predict(frail_cox, newdata = test_icu, type = "risk"))

# all no NA
vars <- paste0(features_icu_no_na, collapse = " + ")
form_cox <- as.formula(sprintf("Surv(surv_icu0to60, surv_icu_death) ~ %s + frailty(CombinedicuID) + as.numeric(as.character(Year))", vars))
frail_cox <- coxph(
  form_cox,
  # form_cox,
  data = data_icu
)

pred_save(frail_cox, test, running_number = "cox_icu_all_no_na", preds = predict(frail_cox, newdata = test_icu, type = "risk"))

# multiple imputation

## .####
surv_learners = c(
  "surv.akritas",
  "surv.aorsf",
  "surv.blackboost",
  "surv.cforest",
  "surv.coxboost",
  "surv.coxph",
  "surv.coxtime",
  "surv.ctree",
  "surv.cv_coxboost", # !
  "surv.cv_glmnet", # !
  "surv.deephit",
  "surv.deepsurv",
  "surv.dnnsurv",
  "surv.flexible",
  "surv.gamboost",
  "surv.gbm",
  "surv.glmboost",
  "surv.glmnet", # !
  "surv.kaplan",
  "surv.loghaz",
  "surv.mboost", # Boosted Generalized Additive Survival Learner
  # "surv.nelson",
  "surv.obliqueRSF",
  # "surv.parametric",
  # "surv.pchazard",
  "surv.penalized",
  # "surv.ranger"
)


