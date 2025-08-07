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

# XGboost
library(xgboost)
library(dplyr)
library(caret)
library(mlexperiments)
library(mlsurvlrnrs)
library(data.table)
library(mice)

all_cols <- c("surv_icu0to60", "surv_icu_death", features_icu_no_na)
all_cols
complete_idx <- complete.cases(data_icu[, all_cols])
complete_idx

df_cox <- data_icu[complete_idx, all_cols]

features <- features_icu_no_na

time_icu  <- data_icu$surv_icu0to60
event_icu <- data_icu$surv_icu_death

X_icu <- model.matrix(~ . -1, data = data_icu[, features, with = FALSE])

cat("Rows in X_icu:    ", nrow(X_icu), "\n")
cat("Length of time_icu:", length(time_icu), "\n")
cat("Length of event_icu:", length(event_icu), "\n")
params <- list(set.seed = 1502, eval_metric = "auc", objective = "binary:logistic")

<<<<<<< HEAD

model <- xgboost(data = X_icu, label = event_icu, params = params, nrounds = 20, verbose = 1)


## GLMM ####
=======
# GLMM ####
>>>>>>> 03cb7ee991a5de58760c444f164f2e4be2617bd0

# baseline
vars <- paste0(features_icu_no_na, collapse = " + ")
form_cox <- as.formula("Surv(surv_icu0to60, surv_icu_death) ~  frailty(CombinedicuID) + as.numeric(as.character(Year))")
cox_glmm_base <- coxph(
  form_cox,
  # form_cox,
  data = data_icu
)

pred_save(cox_glmm_base, test_icu, running_number = "cox_icu_simple3")

# all no NA
vars <- paste0(features_icu_no_na, collapse = " + ")
form_cox <- as.formula(sprintf("Surv(surv_icu0to60, surv_icu_death) ~ %s + frailty(CombinedicuID) + as.numeric(as.character(Year))", vars))
cox_glmm_all <- coxph(
  form_cox,
  # form_cox,
  data = data_icu
)

pred_save(cox_glmm_all, test_icu, running_number = "cox_icu_all_no_na")

# lasso -> mgcv

# GAM + RI
library(mgcv)
numeric_vars <- features_icu_no_na[
  sapply(data_icu[ , features_icu_no_na], is.numeric)
]

form_fact_vars <- paste0(setdiff(features_icu_no_na, numeric_vars), collapse = " + ")
form_splines <- paste(paste0("s(", numeric_vars, ", bs = \"ps\")"), collapse = " + ")

form_gam_pen <- as.formula(sprintf("surv_icu0to60 ~ %s + %s", form_fact_vars, form_splines))
form_gam_pen <- update(form_gam_pen, . ~ . + s(CombinedicuID, bs = "re"))
cox_gam_splines <- gam(form_gam_pen, data = data_icu, method = "REML", family = "cox.ph", weights = surv_icu_death)


# GAM + RI + penalization


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


