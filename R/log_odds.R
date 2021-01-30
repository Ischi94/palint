#'Calculate the log odds ratio with wald confidence intervals
#'
#'\code{log_odds} computes the log odds ratio between two binary outcomes.
#'
#'
#'@param n00 (numeric) A scalar for the number of observations exposed with the
#'  condition.
#'@param n01 (numeric) A scalar for the number of observations exposed without
#'  the condition.
#'@param n10 (numeric) A scalar for the number of observations not exposed with
#'  the condition.
#'@param n11 (numeric) A scalar for the number of observations not exposed
#'  without the condition.
#'@param alpha (numeric) A value defining the probability for the wald
#'  confidence interval. Default is an alpha of 0.05, corresponding to a 0.95 %
#'  confidence interval.
#'@param print.result (character) Either "vector" for output given in a vector
#'  or "df" for output given within a tibble (default).
#'
#'@export
#'
#'@examples # If we have a hypothetical group of smokers (exposed) and non-smokers (not exposed),
#' # then we can look for the rate of lung cancer (condition).
#' # 20 smokers have lung cancer (n00),
#' # 80 smokers do not have lung cancer (n01),
#' # one non-smoker has lung cancer (n10),
#' # and 99 non-smokers do not have lung cancer (n11).
#' log_odds(20, 80, 1, 99)
log_odds <- function(n00, n01, n10, n11, alpha = 0.05, print.result = "df") {
  if(!is.numeric(n00) || !is.numeric(n01) || !is.numeric(n10) || !is.numeric(n11)){
    stop("Input must be numeric")
  }

  if(!is.numeric(alpha)|| alpha < 0 || alpha > 1){
    stop("Alpha must be numeric and between 0 and 1")
  }

  OR <- (n00 * n11) / (n01 * n10)

  siglog <- sqrt((1 / n00) + (1 / n01) + (1 / n10) + (1 / n11))
  zalph <- stats::qnorm(1 - alpha / 2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog

  log_odds_output <-
    tibble::tibble(
      log_odds_ratio = round(logOR, 3),
      lower_ci = round(loglo, 3),
      upper_ci = round(loghi, 3)
    )

  if(print.result == "vector"){
    return(c("log_odds_ratio" = round(logOR, 3),
             "lower_ci" = round(loglo, 3),
             "upper_ci" = round(loghi, 3)))
  }

  if(print.result == "df"){
    return(log_odds_output)
  }

}
