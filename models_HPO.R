source("functions.R")
remotes::install_github("mlr-org/mlr3tuning@v1.3.0") # do this on windows!!
library(mlr3verse)
library(mlr3proba)
library(mlr3pipelines)
library(dplyr)
library(tidyverse)
library(mlr3learners) 

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

data_full <- as_task_surv(x = data_df,
                          time = "surv_icu0to60",
                          event = "surv_icu_death",
                          type = "right")

data_complete <- data_full$clone(deep = TRUE)
data_complete$select(setdiff(data_complete$feature_names, c(any_na, setdiff_exclude, "CombinedID")))


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

data_na <- as_task_surv(x = data_df,
                        time = "surv_icu0to60",
                        event = "surv_icu_death",
                        type = "right")

data_na$select(setdiff(data_na$feature_names, c(setdiff_exclude, "CombinedID")))



# TODO: LEARNER #### (kopieren)

# mögliche learner
surv_learners = c(
  "surv.akritas",
  "surv.aorsf",
  "surv.blackboost",
  "surv.cforest", #
  "surv.coxboost", #
  "surv.coxph",
  "surv.coxtime",
  "surv.ctree",
  "surv.cv_coxboost", #
  "surv.cv_glmnet",
  "surv.deephit",
  "surv.deepsurv", #
  "surv.dnnsurv",
  "surv.flexible", #
  "surv.gamboost",
  "surv.gbm", #
  "surv.glmboost",
  "surv.glmnet",
  "surv.kaplan",
  "surv.loghaz",
  "surv.mboost", #
  "surv.nelson",
  "surv.obliqueRSF",
  "surv.parametric", #
  "surv.pchazard", #
  "surv.penalized",
  "surv.ranger", # 
  "surv.rfsrc",
  "surv.rpart",
  "surv.svm",
  "surv.xgboost" #
)

surv_learners

# lst_lrns_proba <- lapply(surv_learners, function(l) {
#   cat("Trainiere:", l, "\n")
#   learner <- lrn(l)
#   learner$train(data_complete)
#   
#   pred <- learner$predict_newdata(test)
#   pred_save(learner, test, running_number = l)
#   
#   list(learner = learner, pred = pred)
# })



# HPO loop ####
# Kopieren!! + learner eintragen

l <- "surv.rpart" # TODO: specify surv learner
learner <- lrn(l)
# learner$train(data_complete)

# Zeigt Hyperparameter
as.data.table(learner$param_set)[,.(id, class, lower, upper, nlevels)]

search_space = ps(
  # TODO: ?learner type und dann die Hyperparameter da eintragen, wo sie sinnvoll sind
  # ... (Dokumentation des Learners in Chatgpt pasten und fragen, welche parameter und werte sinnvoll sind)
  # integer: number of trees
  # num.trees        = p_int(lower = 100L, upper = 1000L),
  # integer: number of variables to possibly split at in each node
  minsplit = p_int(lower = 1L, upper = 50),
  # double: fraction of observations to sample
  cp  = p_dbl(lower = 0.01,  upper = 0.5)
  # factor: type of variable importance
  # importance       = p_fct(levels = c("none", "impurity", "permutation"))
)

# Inspect the ParamSet
search_space


instance = ti(
  task         = data_complete,
  learner      = learner,
  resampling   = rsmp("cv", folds = 5),
  measure      = msr("surv.cindex"),
  search_space = search_space,
  terminator   = trm("evals", n_evals = 50),
  store_models = TRUE
)

tuner = tnr("random_search")
tuner$optimize(instance)

# Extract results
instance$result$learner_param_vals

lrn_tuned = lrn(l)
lrn_tuned$param_set$values = instance$result_learner_param_vals
lrn_tuned$train(data_complete)
# pred <- tuned_lrn$predict_newdata(test)
pred_save(lrn_tuned, test, running_number = sprintf("hpo_%s", l))


