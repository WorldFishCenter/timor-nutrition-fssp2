# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)

pars <- config::get()

# Plan analysis ------------------------------------------------------------

data_ingestion <- drake_plan(

  # Get survey information
  survey_metadata = target(
    command = get_survey_metadata(
      id = pars$survey_id,
      token = pars$token),
    # Always get this info
    trigger = trigger(condition = TRUE)),

  # Download csv of the surveys responses
  csv_path = target(
    command = download_kobo_data(
      filename = pars$csv_path,
      id = pars$survey_id,
      token = pars$token,
      format = "csv"),
    # Download it only if the new submissions are included
    trigger = trigger(change = survey_metadata$deployment__submission_count),
    format = "file"),

  # Download json of the survey responses
  json_path = target(
    command = download_kobo_data(
      filename = pars$json_path,
      id = pars$survey_id,
      token = pars$token,
      format = "json"),
    # Download it only if the new submissions are included
    trigger = trigger(change = survey_metadata$deployment__submission_count),
    format = "file"),

  # Load data into memory
  submissions_table = readr::read_csv(file = csv_path, col_types = readr::cols()),
  submissions_list = jsonlite::read_json(path = json_path),
)

data_checks <- drake_plan(
  data_validation_exploration = rmarkdown::render(
    input = knitr_in("reports/validation.Rmd"),
    quiet = T
  )

)

full_plan <- bind_plans(
  data_ingestion,
  data_checks,
)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan)
}
