
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
#' @importFrom stringr

extract_weights_reg_glm = function(char){
  # Make a function that returns a data.frame with the weights per model
  # Perhaps even add an argument with all the models
  # So that a 0 is added where necessary
  # Then in the main function it can be standardised to sum up to 1
  
  # Parse the output from the stacks::get_expressions function
  parsed_char = str_match_all(char, "\\(([[:alnum:]]+) \\* ([[:graph:]]+)\\)")
  
  return(data.frame(model = parsed_char[[1]][, 2],
                    weight = as.numeric(parsed_char[[1]][, 3])))
}
