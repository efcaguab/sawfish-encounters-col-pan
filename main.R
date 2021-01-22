# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)

# Plan analysis ------------------------------------------------------------

full_plan <- drake_plan(
  encounters = clean_encounters(file_in("data/raw/sawfish-data.xlsx")),
  allometric_model_data = get_allometric_model_data(encounters),
  allometric_model = model_allometry(allometric_model_data),
  length_model_data = get_length_model_data(encounters, allometric_model),
  max_length_model_data = get_max_length_model_data(length_model_data),
  length_model = model_length(length_model_data),
  max_length_model = model_length(max_length_model_data),
  fig_length_vs_time = plot_length_vs_time(length_model, length_model_data),
  fig_max_length_vs_time = plot_length_vs_time(max_length_model, max_length_model_data),
)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan, lock_envir = FALSE)
}
