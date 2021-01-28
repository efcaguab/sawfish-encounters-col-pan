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
      prior = c(set_prior("student_t(3, 1, 1)", class = "b"),
                set_prior("student_t(3, 0, 2)", class = "Intercept")),
      iter = 5000,
      warmup = 2500,
      cores = 4)
}
