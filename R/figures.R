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