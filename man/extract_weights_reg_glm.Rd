% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{extract_weights_reg_glm}
\alias{extract_weights_reg_glm}
\title{Apply ensemble weighting}
\source{
https://www.tmwr.org/ensembles.html#blend-predictions
https://github.com/tidymodels/stacks/blob/main/R/blend_predictions.R
}
\usage{
extract_weights_reg_glm(char)
}
\arguments{
\item{df_ens}{data.frame}

\item{target_var}{character; column name in df_ens containing the target variable}

\item{method}{character;}

\item{group_vars}{character; column names in df_ens to group over}

\item{...;}{arguments fed into the function specified by 'method'}
}
\description{
Apply user-defined metric and method to weight ensembles
}
\examples{

}
