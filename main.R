# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)
library(patchwork)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)
options(proj_config = config::get())

# Plan analysis ------------------------------------------------------------

full_plan <- drake_plan(
  encounters = clean_encounters(file_in("data/raw/sawfish-data.xlsx")),
  allometric_model_data = get_allometric_model_data(encounters),
  allometric_model = model_allometry(allometric_model_data),
  length_model_data = get_length_model_data(encounters, allometric_model),
  max_length_model_data = get_max_length_model_data(length_model_data),
  max_length_model = model_length(max_length_model_data),
  max_length_model_fitted_draws = fit_length_time_draws(max_length_model, max_length_model_data),
  fig_max_length_vs_time = plot_length_vs_time(max_length_model_fitted_draws, max_length_model_data),
  length_model = model_length(length_model_data),
  length_model_fitted_draws = fit_length_time_draws(length_model, length_model_data),
  fig_length_vs_time = plot_length_vs_time(length_model_fitted_draws, length_model_data),
  data_exp_notebook = target(rmarkdown::render(knitr_in("notebooks/data-exploration.Rmd"))),
  paper_notebook = target(rmarkdown::render(knitr_in("notebooks/paper-document.Rmd"))),
  fig_map_pdf = plot_map(encounters, file_out("figures/map.pdf")),
  fig_length_pdf =  ggplot2::ggsave(plot = fig_max_length_vs_time,
                                    file_out("figures/length-time.pdf"), width = 8, height = 8, units = "cm")
)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan, lock_envir = FALSE)
}
