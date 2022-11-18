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
#' @examples
#' 
#' @importFrom dplyr group_by
#'
#' @export

# # Test
# df_ens = read.csv("C:/Users/jorme194/Documents/Projects/2022/GLEON Ensemble Weighting Project/Test cases/LER_temp_example.csv")
# target_var = "Water_Temperature_celsius"
# group_vars = c("model")
# method = "default"
# library(stacks)

weight_ensemble <- function(df_ens, target_var, method = "default",
                            group_vars = NULL, ...){
  
  # Check input variables, including if method is listed in our list of methods
  # Maybe also create a list_methods function, which prints the csv file
  # where we store the included methods and the functions that they link to
  
  if(method == "default"){
    method_func = method_list$function.[which(method_list$default)]
  }else if(method %in% method_list$method){
    method_func = method_list$function.[which(method_list$method == method)]
  }else{
    stop("Method ", method, " is unknown. Should be one of:\n",
         paste(method_list$method, collapse = "\n"))
  }
  
  output = get(method_func)(df_ens, target_var, group_vars, ...)
}
