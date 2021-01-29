get_length_model_data <- function(encounters, allometric_model){

  suppressPackageStartupMessages({
    library(dplyr)
  })

  measured <- encounters %>%
    filter(!is.na(reported_collection_year), !is.na(total_length_cm)) %>%
    mutate(total_length_cm_se = 0)

  length_pred_data <- encounters %>%
    filter(!is.na(reported_collection_year), !is.na(rostrum_length_cm),
           is.na(total_length_cm)) %>%
    mutate(rostrum_length_cm = log(rostrum_length_cm))

  estimated <- tidybayes::add_fitted_draws(length_pred_data,
                      allometric_model) %>%
    mutate(.prediction = exp(.value)) %>%
    summarise(.prediction_se = sqrt(stats::var(.prediction)),
              .prediction = mean(.prediction), .groups = "drop") %>%
    mutate(total_length_cm = round(.prediction),
           total_length_cm_se = .prediction_se)

  bind_rows(measured, estimated) %>%
    mutate(across(c(total_length_cm, total_length_max), round),
           scaled_year = scale(reported_collection_year)) %>%
    filter(species == "Pristis pristis")

}

get_max_length_model_data <- function(length_model_data){
  length_model_data %>%
    dplyr::group_by(reported_collection_year, source_type) %>%
    dplyr::filter(total_length_cm == max(total_length_cm)) %>%
    dplyr::ungroup()
}

model_length <- function(length_model_data){
  suppressPackageStartupMessages({
    library(brms)
  })

  length_model_data <- length_model_data %>%
    dplyr::mutate(max_length = round(max(total_length_cm) * 1.2))

  # m1 <-
  brm(total_length_cm | trials(max_length) ~ scaled_year +
        (1 | source_type) +
        (1 | observation),
      family = "binomial",
      save_pars = save_pars(all = T),
      prior = c(set_prior("student_t(3, 0, 1)", class = "b"),
                set_prior("student_t(3, 0, 2)", class = "Intercept")),
      data = length_model_data,
      control = list(adapt_delta = 0.99,
                     max_treedepth = 12),
      iter = getOption("proj_config")$models$iter,
      warmup = getOption("proj_config")$models$warmup,
      cores = 4)

}

fit_length_time_draws <- function(max_length_model, max_length_model_data){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidybayes)
    library(ggdist)
    library(patchwork)
  })

  year_range <- max_length_model_data %>%
    group_by(source_type) %>%
    summarise(max_year = max(reported_collection_year),
              min_year = min(reported_collection_year))

  nd <- expand_grid(scaled_year = seq(min(max_length_model$data$scaled_year), max(max_length_model$data$scaled_year), length.out = max(max_length_model_data$reported_collection_year) - min(max_length_model_data$reported_collection_year) + 1),
                    source_type = c("Any", "Non-interview observation", "Literature", "Museum Collection"),
                    country = "Panama",
                    species = c("Pristis pristis"),
                    sex = "unknown",
                    max_length = max_length_model$data$max_length[1]) %>%
    mutate(year = (scaled_year * attr(max_length_model_data$scaled_year, "scaled:scale")) + attr(max_length_model_data$scaled_year, "scaled:center")) %>%
    left_join(year_range) #%>%
  # filter((source_type != "Any" & year <= max_year) | source_type == "Any", (source_type != "Any" & year >= min_year) | source_type == "Any")

  add_fitted_draws(nd, max_length_model,
                                         re_formula = ~ (1 | source_type),
                                         # n = 1000,
                                         allow_new_levels = T) %>%
    mutate(year = (scaled_year * attr(max_length_model_data$scaled_year, "scaled:scale")) + attr(max_length_model_data$scaled_year, "scaled:center")) %>%
    mutate(source_type = case_when(source_type == "Any" ~ "Overall", TRUE ~ source_type)) %>%
    mutate(source_type = fct_relevel(source_type, "Overall"))
}

plot_length_vs_time <- function(model_fitted_draws, max_length_model_data){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidybayes)
    library(ggdist)
    library(patchwork)
  })

  p1 <- model_fitted_draws %>%
    filter(source_type == "Overall") %>%
    ggplot(aes(x = year)) +
    stat_lineribbon(aes(y = .value, alpha = forcats::fct_rev(ordered(stat(.width)))),
                    .width = c(0.05, 0.66, 0.9), size = 0.5, fill = "black", colour = "black") +
    geom_point(data = max_length_model_data,
               aes(x = reported_collection_year, y = total_length_cm),
               shape = 21) +
    geom_linerange(data = max_length_model_data,
                   aes(x = reported_collection_year,
                       ymax = total_length_cm + total_length_cm_se,
                       ymin = total_length_cm -total_length_cm_se)) +
    scale_y_continuous(labels = scales::number_format(scale = 1/100)) +
    scale_x_continuous() +
    theme_minimal() +
    # facet_grid(. ~ source_type) +
    theme(legend.position = "none") +
    labs(x = "Year",
         y = "Total length (m)")

  p2 <- model_fitted_draws %>%
    group_by(.draw, source_type) %>%
    mutate(ten_year_diff = (.value - lead(.value, 10))/.value) %>%
    filter(!is.na(ten_year_diff),
           source_type == "Overall") %>%
    ggplot(aes(x = ten_year_diff)) +
    stat_halfeye(slab_alpha = 0.33, fill = "grey30", .width = c(0.66, 0.9)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    scale_x_continuous(labels = scales::label_percent()) +
    coord_cartesian(xlim = c(0, NA), clip = "off") +
    labs(x = "Rate of decrease (per decade)",
         y = "Density") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

  p <- p1 + p2 + plot_layout(ncol = 1, heights = c(1,0.2))
  return(p)
}
