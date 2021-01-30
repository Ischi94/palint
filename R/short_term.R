#' Calculate short-term changes for binned data.
#'
#' @param data (data.frame) A data frame or tibble containing the value column
#'   and the bin column
#' @param value (unquoted expression) Variable name of the value for which the
#'   short-term changes should be calculated.
#' @param bin (unquoted expression) Variable name of the discrete bin numbers.
#' @param bin.one (character) Either "oldest" when the first bin (normally 1) is
#'   the oldest out of all bins or "youngest" when the first bin is the most
#'   recent one. "oldest" is the default and corresponds to the convention that
#'   stage 1 is the oldest geologic stage.
#' @param mult.observations (logical) Are there more observations per bin?
#'   Defaults to one observation per bin.
#'
#' @section Details:
#' To calculate the short-term change for each row, the coefficient of a linear
#' regression between the value and bin of interest and each corresponding lag
#' (the value and bin before the focal bin) is used. This corresponds to a unit
#' change of the value compared to the previous bin. For example, if we have
#' 20°C at bin 10, and 22°C at bin 11, the short-term temperature change from
#' bin 10 to 11 is 2. If bin 11 is older than bin 10, one needs to specify this
#' by setting bin.one = "youngest" and the corresponding short-term temperature
#' change would hence be -2. If there is more than one observation per bin, and
#' mult.observations is set to TRUE, the regression is based on all values per
#' bin.
#'
#' @return The output is a tibble data frame containing the value column, the
#'   bin column and the calculated short-term change.
#' @export
#'
#' @examples
#' dfr <- data.frame(x = rnorm(10), stg = 1:10)
#' short_term(data = dfr, value = x, bin = stg)
short_term <- function(data, value, bin, bin.one = "oldest", mult.observations = FALSE){

  lag.bin = NULL
  lag.val = NULL
  comb.val = NULL
  comb.bin = NULL
  model = NULL
  stg = NULL

  if(!tibble::is_tibble(data)){
    data <- data %>%
      tibble::as_tibble()
  }

  ori.data <- data

  if(bin.one != "oldest" && bin.one != "youngest"){
    stop("bin.one can either be 'earliest' or 'latest'")
  } else if(bin.one == "oldest") {
    data <- data %>%
      dplyr::arrange({{bin}})
  } else if(bin.one == "youngest") {
    data <- data %>%
      dplyr::arrange(dplyr::desc({{bin}}))
  }

  if(mult.observations == FALSE){
    suppressMessages(
      data %>%
        dplyr::mutate(lag.val = dplyr::lag({{value}}),
                      lag.bin = dplyr::lag({{bin}})) %>%
        dplyr::group_by({{value}}) %>%
        dplyr::mutate(comb.val = list(c({{value}}, lag.val)),
                      comb.bin = list(c({{bin}}, lag.bin))) %>%
        dplyr::select({{bin}}, comb.val, comb.bin) %>%
        tidyr::nest(data = c(comb.val, comb.bin)) %>%
        dplyr::mutate(model = list(stats::lm(comb.val[[1]] ~ comb.bin[[1]], data = data[[1]])),
                      short_term = purrr::map(model, "coefficients"),
                      short_term = purrr::map_dbl(short_term, purrr::pluck, 2)) %>%
        dplyr::select({{value}}, {{bin}}, short_term) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(ori.data)
    )
  }else if(mult.observations == TRUE){
    data.mult <- data %>%
      dplyr::group_by(stg) %>%
      tidyr::nest()

    data.mult <- data.mult %>%
      dplyr::mutate(x = purrr::map(data, rlang::as_label(rlang::enquo(value)))) %>%
      dplyr::ungroup() %>%
      dplyr::select({{value}}, {{bin}}) %>%
      dplyr::mutate(lag.val = dplyr::lag({{value}}),
                    lag.bin = dplyr::lag({{bin}})) %>%
      dplyr::group_by({{bin}}) %>%
      dplyr::mutate(comb.val = purrr::map2({{value}}, lag.val, c),
                    comb.bin = list(rep(c({{bin}}, lag.bin), 2))) %>%
      tidyr::drop_na() %>%
      dplyr::select({{bin}}, comb.val, comb.bin) %>%
      tidyr::nest(data = c(comb.val, comb.bin)) %>%
      dplyr::mutate(model = list(stats::lm(comb.val[[1]] ~ comb.bin[[1]], data = data[[1]])),
                    short_term = purrr::map(model, "coefficients"),
                    short_term = purrr::map_dbl(short_term, purrr::pluck, 2)) %>%
      dplyr::select({{bin}}, short_term) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(data.mult)

    if(bin.one == "oldest"){
      data.mult %>%
        dplyr::arrange({{bin}})
    }else if(bin.one == "youngest"){
      data.mult %>%
        dplyr::arrange(dplyr::desc({{bin}}))
    }
  }
}
