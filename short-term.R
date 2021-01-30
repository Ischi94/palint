dat3 <- data.frame(x = rnorm(10), stg = 1:10)


short_term <- function(data, value, bin){
  data %>%
    tibble::as_tibble() %>%
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
    dplyr::ungroup()
}

short_term(data = dat3, value = x, bin = stg)
