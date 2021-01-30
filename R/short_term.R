short_term <- function(data, value, bin, bin.one = "earliest"){

  if(!tibble::is_tibble(data)){
    data <- data %>%
      tibble::as_tibble()
  }

  ori.data <- data

  if(bin.one != "earliest" && bin.one != "latest"){
    stop("bin.one can either be 'earliest' or 'latest'")
  } else if(bin.one == "earliest") {
    data <- data %>%
      dplyr::arrange({{bin}})
  } else if(bin.one == "latest") {
    data <- data %>%
      dplyr::arrange(dplyr::desc({{bin}}))
  }

  suppressMessages(
    data %>%
      dplyr::mutate(lag.val = dplyr::lead({{value}}),
                    lag.bin = dplyr::lead({{bin}})) %>%
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
}
