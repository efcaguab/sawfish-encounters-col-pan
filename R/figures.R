plot_map <- function(encounters, path){
  suppressPackageStartupMessages({
    library(rnaturalearth)
    library(rnaturalearthhires)
    library(tidyverse)
    library(patchwork)
  })

  world <- ne_countries(country = c("Colombia", "Panama", "Costa Rica"), scale = "large", returnclass = "sf")

  map_data <-
    encounters %>%
    filter(reported_collection_year > 1800) %>%
    mutate(source_type = fct_recode(source_type,
                                    "Museum" = "Museum Collection",
                                    "Non-interview" = "Non-interview observation"),
           source_type = fct_reorder(source_type, reported_collection_year, median),
           across(c(longitude, latitude), as.numeric)) %>%
    filter(latitude > 0)

  country_names <- tibble::tribble(
    ~ name, ~ lat, ~ lon,
    "Colombia", 6, -74.5,
    "Panama", 9.75, -81
  )

  p1 <-
    ggplot(data = map_data) +
    geom_sf(data = world, inherit.aes = F, alpha = 0.5, colour = "grey30") +
    geom_point(data = map_data, aes(x = longitude, y = latitude, fill = source_type), shape = 21) +
    geom_text(data = country_names,
              aes(label = name, x = lon, y = lat),
              colour = "grey50", size = 2.7, fontface = "italic") +
    scale_fill_viridis_d() +
    guides(fill = guide_legend(override.aes = list(colour = NA, alpha = 1),
                               nrow = 2, keywidth = 0.5)) +
    theme_minimal(base_size = 10) +
    theme(axis.title = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          title = element_text(size = 9)) +
    coord_sf(xlim = c(-83.9, -73), ylim = c(4.1, 11.9), expand = FALSE) +
    scale_x_continuous(breaks = seq(-90, 90, by = 2)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 2)) +
    labs(fill = "Source",
         tag = "a)")

  p2 <- ggplot(data = map_data) +
    geom_sf(data = world, inherit.aes = F, alpha = 0.5, colour = "grey30") +
    geom_point(data = map_data,
               aes(x = longitude, y = latitude, fill = reported_collection_year),
               shape = 21, size = 1.5) +
    geom_text(data = country_names,
              aes(label = name, x = lon, y = lat),
              colour = "grey50", size = 2.7, fontface = "italic") +
    scale_fill_stepsn(breaks = seq(1000, 3000, by = 25),
                      colours = viridis::viridis(2),
                      guide = guide_coloursteps(barwidth = 11, barheight = 0.5,
                                                override.aes = list(colour = NA, alpha = 1))) +
    theme_minimal(base_size = 10) +
    theme(axis.title = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          title = element_text(size = 9)) +
    coord_sf(xlim = c(-83.9, -73), ylim = c(4.1, 11.9), expand = FALSE) +
    scale_x_continuous(breaks = seq(-90, 90, by = 2)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 2)) +
    labs(fill = "Year",
         tag = "b)")

  p1 + p2 + plot_layout(ncol = 1, heights = c(1, 0.95))

  ggsave(path, width = 8, height = 15, units = "cm")
}

plot_histogram <- function(encounters, path){

  suppressPackageStartupMessages({
    require(tidyverse)
  })

  enc_data <- encounters %>%
    filter(reported_collection_year > 1800) %>%
    mutate(source_type = fct_recode(source_type,
                                    "Museum" = "Museum Collection",
                                    "Non-interview" = "Non-interview observation"),
           source_type = fct_reorder(source_type, reported_collection_year, median),
           country = fct_infreq(country))

  enc_data %>%
    ggplot(aes(x = reported_collection_year, fill = source_type)) +
    geom_histogram(binwidth = 10, center = 2000) +
    facet_wrap("country", ncol = 2) +
    scale_fill_viridis_d(guide = guide_legend(keywidth = 0.7, keyheight = 0.7)) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          title = element_text(size = 9))  +
    labs(x = "Reported collection year",
         y = "Number of encounters")

  ggsave(path, width = 16, height = 6, units = "cm")
}

plot_allometry <- function(allometric_model, path){

  suppressPackageStartupMessages({
    require(tidyverse)
    require(tidybayes)
    require(ggdist)
  })

  add_fitted_draws(
    newdata = tidyr::expand_grid(
      rostrum_length_cm = seq(min(allometric_model$data$rostrum_length_cm),
                              max(allometric_model$data$rostrum_length_cm),
                              length.out = 10),
      species = "Pristis pristis"),
    model = allometric_model) %>%
    ungroup() %>%
    mutate(across(c(rostrum_length_cm, .value), exp)) %>%
    ggplot(aes(x = rostrum_length_cm/100, y = .value/100)) +
    stat_lineribbon(aes(alpha = forcats::fct_rev(ordered(stat(.width)))),
                    .width = c(0.05, 0.66, 0.95), size = 0.5, fill = "grey30") +
    geom_point(data = filter(allometric_model$data, species == "Pristis pristis"),
               aes(y = exp(total_length_cm)/100,
                   x = exp(rostrum_length_cm)/100),
               shape = 21, size = 1.5) +
    # scale_x_log10(breaks = c(0.25, 0.5, 1, 2)) +
    # scale_y_log10(breaks = c(2.5/4, 2.5/2, 2.5, 5, 10)) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none",
          title = element_text(size = 9)) +
    labs(x = "Rostrum length (m)",
         y = "Total length (m)")

  ggsave(path, width = 8, height = 6, units = "cm")
}


plot_random_intercepts <- function(max_length_model, path){

  suppressPackageStartupMessages({
    require(tidyverse)
    require(tidybayes)
    require(ggdist)
  })

  max_length_model %>%
    gather_draws(r_source_type[source_type,Intercept]) %>%
    ungroup() %>%
    mutate(source_type = fct_recode(source_type,
                                    "Museum" = "Museum.Collection",
                                    "Non-interview" = "Non-interview.observation"),
           source_type = fct_reorder(source_type, .value, .desc = T)) %>%
    ggplot(aes(x = .value, y = source_type)) +
    stat_pointinterval(.width = c(0.66, 0.9),
                       interval_size_range = c(0.5,1), fatten_point = 1.2) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    theme_minimal(base_size = 10) +
    theme(title = element_text(size = 9),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(x = "Intercept")

  ggsave(path, width = 8, height = 3.5, units = "cm")
}
