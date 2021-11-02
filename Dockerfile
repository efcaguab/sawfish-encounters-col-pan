FROM rocker/geospatial:4.0.3

# Extra R packages
RUN install2.r drake here janitor skimr brms ggdist inspectdf readxl tidybayes broom.mixed config english patchwork rnaturalearth
RUN installGithub.r ropensci/rnaturalearthhires

# Rstudio interface preferences
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
