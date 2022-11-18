# This script contains all functions that apply weighting methods

#' Apply ensemble weighting
#'
#' Apply user-defined metric and method to weight ensembles
#'
#' @param df_ens data.frame
#' @param target_var character; column name in df_ens containing the target variable
#' @param method character; 
#' @param group_vars character; column names in df_ens to group over
#' @param ...; arguments fed into the function specified by 'method'
#'
#' @source 
#' https://www.tmwr.org/ensembles.html#blend-predictions
#' https://github.com/tidymodels/stacks/blob/main/R/blend_predictions.R
#'
#' @examples
#' 
#' @import stacks
#' @import parsnip
#' @import workflows
#' @import tune
#' @importFrom reshape2 dcast
#'
#' @export

reg_glm = function(df_ens, target_var, group_vars,
                   times = 25, penalty = 10 ^ (-6:-1),
                   mixture = 1){
  if(!is.null(group_vars)){
    warning("Grouping variables cannot yet be applied to this function")
  }
  
  all_models = unique(df_ens[["model"]])
  all_models = all_models[!grepl("[Oo]bs", all_models)]
  
  # Transform data to wide format
  left_hand_cols = names(df_ens)[!(names(df_ens) %in% c("model", target_var))]
  inp = reshape2::dcast(df_ens, as.formula(paste0(paste0(left_hand_cols, collapse = " + "),
                                                  " ~ model")),
                        value.var = target_var)
  
  inp = inp[, -c(1, 2)]
  
  # NAs seem to cause issues for this function
  inp = inp[complete.cases(inp),]
  
  # This part of the code comes from the stacks::blend_predictions function
  tune_quo <- rlang::new_quosure(tune::tune(), env = rlang::empty_env())
  model_spec <- 
    parsnip::linear_reg(penalty = !!tune_quo, mixture = !!tune_quo) %>%
    parsnip::set_engine("glmnet", lower.limits = !!0, lambda.min.ratio = 0, intercept = F)
  preds_formula <-
    rlang::new_formula(as.name("Obs"), as.name("."), env = rlang::base_env())
  preds_wf <-
    workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_formula(preds_formula)
  
  candidates <- 
    preds_wf %>%
    tune::tune_grid(
      resamples = rsample::bootstraps(inp, times = times),
      grid = purrr::cross_df(
        list(
          penalty = penalty,
          mixture = mixture
        )
      ),
      metrics = NULL
    )
  
  metric <- tune::.get_tune_metric_names(candidates)[1]
  best_param <- tune::select_best(candidates, metric = metric)
  coefs <-
    model_spec %>%
    tune::finalize_model(best_param) %>%
    parsnip::fit(formula = preds_formula, data = inp)
  # End of blend_predictions code
  
  # Extract weights from the coefs
  model_weights = extract_weights_reg_glm(paste0(as.character(get_expressions(coefs)$numeric$.pred),
                                                 collapse = ""))
  
  model_weights = merge(model_weights, list(model = all_models), by = "model", all = T)
  model_weights$weight[is.na(model_weights$weight)] = 0.0
  
  # Standardise weights to sum up to 1
  model_weights$weight = model_weights$weight / sum(model_weights$weight)
  
  return(model_weights)
}
