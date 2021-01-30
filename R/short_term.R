short_term <- function(data, value, bin, bin.one = "oldest", mult.observations = FALSE){

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
        dplyr::mutate(model = list(lm(comb.val[[1]] ~ comb.bin[[1]], data = data[[1]])),
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
      dplyr::mutate(model = list(lm(comb.val[[1]] ~ comb.bin[[1]], data = data[[1]])),
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
