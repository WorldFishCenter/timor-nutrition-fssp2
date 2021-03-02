lookup_choice <- function(x, list_name, metadata, language = "English"){

  language_index <- which(unlist(metadata$summary$languages) == language)

  label_dat <- purrr::keep(.x = metadata$content$choices,
                           .p = ~.$list_name == list_name) %>%
    purrr::map_dfr(~ tibble::tibble(name = .$name,
                                    label = .$label[[language_index]][1]))

  labels <- tibble::tibble(name = as.character(x)) %>%
    dplyr::left_join(label_dat, by = "name")

  labels$label
}

lookup_district_eng <- function(x){
  x %>%
    lookup_choice(list_name = "district",
                  metadata = drake::readd(survey_metadata),
                  language = "English")
}


lookup_village_eng <- function(x){
  x %>%
    lookup_choice(list_name = "aldeia_suco",
                  metadata = drake::readd(survey_metadata),
                  language = "English")
}
