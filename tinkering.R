library(tidyverse)

d <- tibble(x = runif(100, -2, 2),
       y = runif(100, -2, 2),
       type = rep(c("Longer period (n=90)", "Shoter period (n=10)"), times = c(90, 10)),
       points = "Random (2D)")

n <- tibble(x = rnorm(100),
            y = rnorm(100),
            type = rep(c("Longer period (n=90)", "Shoter period (n=10)"), times = c(90, 10)),
            points = "Gaussian (2D)")

l <- tibble(x = runif(100, -2, 2),
            y = 1,
            type = rep(c("Longer period (n=90)", "Shoter period (n=10)"), times = c(90, 10)),
            points = "Random (1D)")

nbins <- 10

bind_rows(d, n) %>%
  bind_rows(l) %>%
  ggplot(aes(x, y)) +
  geom_density2d_filled( h = c(1, 1), contour_var = "ndensity", bins = nbins) +
  geom_point(size = 0.5, shape = 21) +
  facet_grid(points ~ type) +
  coord_cartesian(expand = F) +
  scale_fill_manual(values = c("white", RColorBrewer::brewer.pal(nbins-1, 'Blues'))) +
  theme(legend.position = "none") +
  labs(title =  "Kernel density")
