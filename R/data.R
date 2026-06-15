#' Demonstration data set with continous outcomes
#'
#' Simulated summary data from 10 outcomes to be presented in a forest plot.
#' Each row represents the summary of one outcome variable for two treatment groups with 
#' columns for the variable name (vlabel), 
#' number of observations and mean (sd) for each group (n1, n2, n3 and n4),
#' the difference between groups (beta), with 95% CI (beta_lci and beta_uci), and a p-value (p1)
#'
#' @format A data frame with 9 columns: \code{vlabel}, \code{n1}, \code{n2}, \code{n3}, \code{n4},
#'	\code{beta}, \code{beta_lci}, \code{beta_uci} and \code{p1}.
#'
"forplotdata"

#' Raw data for continous outcomes
#'
#' Simulated raw data from 10 outcomes in a long format.
#' Includes columns for the outcome values (out), 
#'	the outcome variable (var) as factor,
#' and the treatment arm (arm) as a factor.
#'
#' @format A data frame with 3 columns: \code{out}, \code{var} and \code{arm}.
#'
"forplotdata_bp"

#' Demonstration data set with binary outcomes
#'
#' Simulated summary data from 10 outcomes to be presented in a forest plot.
#' Each row represents the summary of one outcome variable for two treatment groups with 
#' columns for the variable name (vlabel), 
#' number of observations and n (%) for each group (n1, n2, n3 and n4),
#' the crude proporion in each group (prop1, prop2)
#' the risk difference between groups (beta), with 95% CI (beta_lci and beta_uci), the formatted 
#'	risk differnce (beta_format) and a p-value (p1)
#'
#' @format A data frame with 12 columns: \code{vlabel}, \code{n1}, \code{n2}, \code{n3}, \code{n4},
#' \code{prop1}, \code{prop2}, \code{beta}, \code{beta_lci}, \code{beta_uci}, \code{beta_format} and \code{p1}.
#'
"forplotdata_prop"
