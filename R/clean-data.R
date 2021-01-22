clean_encounters <- function(x){
  readxl::read_excel(x, sheet = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate(ocean = tidyr::replace_na(ocean, "unknown"),
                  total_length_censoring = dplyr::if_else(stringr::str_detect(total_length_cm, "missing"),
                                                          "interval",
                                                          "none"),
                  total_length_cm = stringr::str_extract(total_length_cm, "[0-9]+\\.?[0-9]+"),
                  year_censoring = dplyr::if_else(stringr::str_detect(reported_collection_year, "<"),
                                                  "right",
                                                  "none"),
                  reported_collection_year = stringr::str_extract(reported_collection_year, "[0-9]+"),
                  dplyr::across(c(total_length_cm, reported_collection_year, rostrum_length_cm), as.numeric),
                  total_length_max = dplyr::if_else(total_length_censoring == "interval", total_length_cm * 1.5, total_length_cm),
                  sex = dplyr::case_when(sex == "Male" ~ "male",
                                         sex == "Female" ~ "female",
                                         TRUE ~ "unknown"),
                  ocean = dplyr::case_when(ocean == "Pacific" ~ "pacific",
                                           ocean == "Caribean" ~ "caribean",
                                           TRUE ~ "unknown"),
                  observation = as.character(number))
}

function(){
  encounters %>%
    dplyr::filter(!is.na(reported_collection_year),
                  !is.na(total_length_cm)) %>%
    View()
}

