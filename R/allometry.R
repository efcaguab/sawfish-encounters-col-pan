get_allometric_model_data <- function(encounters){
  encounters %>%
    dplyr::filter(rostrum_length_cm < total_length_cm) %>%
    dplyr::filter(!is.na(rostrum_length_cm), !is.na(total_length_cm)) %>%
    dplyr::mutate(dplyr::across(c(rostrum_length_cm, total_length_cm), log))
}

model_allometry <- function(allometric_model_data){
  suppressPackageStartupMessages({
    library(brms)
  })

  brm(total_length_cm ~ rostrum_length_cm + (1 | species),
      data = allometric_model_data,
      control = list(adapt_delta = 0.99,
                     max_treedepth = 15),
      iter = 3000,
      warmup = 2000,
      cores = 4)
}
