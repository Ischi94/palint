#' Calculate percentage change
#'
#'\code{perc_change} returns the percentage change between two values, where one value (\code{old.value}) is
#'used as a baseline to compare the change with.
#'
#'The output can be specified via the \code{print.result} parameter.
#'Options are the raw value, transformed to percentage, or a text description.
#'Default settings return the raw value.
#'
#'@section Warning:
#'\code{perc_change} is vectorized and shorter values are recycled.
#'If the length of \code{old.value} differs from the length of
#'\code{print.result}, please confirm that the code is doing the desired task by double-checking with
#'print.result = "text".
#'
#' @param old.value A numeric value or vector. This is the baseline for the comparison.
#' @param new.value A numeric value or vector. This value is compared to the baseline.
#' @param print.result Character. Either "value", "percentage", or "text".
#'
#' @export
#'
#' @examples
#' perc_change(7, 5, print.result = "text")
perc_change <- function(old.value, new.value, print.result = "value"){
  if(!is.numeric(old.value) || !is.numeric(new.value)){
    stop("Input must be numeric")
  }

  perc.change <- list()
  change <- new.value - old.value
  perc.change$value <- change/old.value
  perc.change$perc <- perc.change$value*100
  perc.change$text <- paste0("The percentage change from ", old.value,
                             " to ", new.value,
                             " is ", round(perc.change$perc, 2), "%")
  if(print.result == "value"){
    return(perc.change$value)
  }

  if(print.result == "percentage"){
    return(perc.change$perc)
  }

  if(print.result == "text"){
    print(perc.change$text)
  }
}
