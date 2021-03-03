get_survey_metadata <- function(id = NULL, token = NULL, api = "kobohr"){

  stopifnot(length(token) > 0)

  cat(token)
  cat(id)

  assets_v1_raw <- httr::GET(
    url = paste(kobo_host(api, version = "v1"), "data", sep = "/"),
    config = httr::add_headers(Authorization = token))

  message(assets_v1_raw)

  survey_basic_metadata <- purrr::keep(httr::content(assets_v1_raw),
                                       ~.$id == id)[[1]]

  # the version 2 of the api returns much richer information about the surveys
  httr::GET(
    url = paste(kobo_host(api, version = "v2"), "assets",
                survey_basic_metadata$id_string, sep = "/"),
    config = httr::add_headers(Authorization = token)) %>%
    httr::content() %>%
    c(survey_basic_metadata)
}


#' Download kobo data in as csv or json
#'
#' @param filename string with path to file
#' @param id survey id
#' @param token access token for the account e.g. "Token XXXXXXX"
#' @param api Either "kobo", "kobohr", "ona", or a custom (full) URL. API URLs
#'   are made available for KoBo Toolbox ("kobo",
#'   \url{https://kc.kobotoolbox.org/api/v1/}), KoBo Humanitarian Response
#'   ("kobohr", \url{https://kc.humanitarianresponse.info/api/v1/}), Ona ("ona",
#'   \url{https://ona.io/api/v1/}) and Unhcr ("unhcr",
#'   \url{https://kobocat.unhcr.org/api/v1/}) . For your own installation, or
#'   other installations using the same API but accessed at a different URL,
#'   enter the full URL.
#' @param format Either "csv" or "json"
#'
#' @return 0 zero on success
#' @author Fernando Cagua
#' @export
#'
#' @examples
#'
download_kobo_data <- function(filename, id = NULL, token = NULL,
                               api = "kobohr", format = "csv",
                               overwrite = TRUE){

  request_url <- paste(kobo_host(api),
                       "data", as.character(id), sep = "/")

  httr::GET(url = request_url,
            config = httr::add_headers(Authorization = token),
            query = list(format = format),
            httr::write_disk(filename, overwrite = overwrite))

  filename
}

#' @name kobo_host
#' @rdname kobo_host
#' @title  Select server
#'
#' @description A helper function to conveniently switch different APIs.
#'
#' Specifies the Host URL of the API to Use
#'
#'
#' @param api Either "kobo", "kobohr", "ona", or a custom (full) URL.
#' @return A single string with the URL to use.
#' @note API URLs are made available for KoBo Toolbox ("kobo",
#' \url{https://kc.kobotoolbox.org/api/v1/}), KoBo Humanitarian Response
#' ("kobohr", \url{https://kc.humanitarianresponse.info/api/v1/}), Ona
#' ("ona", \url{https://ona.io/api/v1/}) and Unhcr ("unhcr", \url{https://kobocat.unhcr.org/api/v1/}) . For your own installation, or other
#' installations using the same API but accessed at a different URL,
#' enter the full URL.
#' @author Ananda Mahto
#' @note This function is not intended to be called directly.
#' It is used in other functions.
#'
#' @examples#'
#' \dontrun{
#' kobo_host("unhcr")
#' kobo_host("ttps://kobocat.unhcr.org/api/v1/")
#' }
#'

kobo_host <- function(api, version = "v1") {
  if (api %in% c("kobo", "kobohr", "ona","unhcr")) {
    if (version == "v1") {
      switch(api,
             kobo = "https://kc.kobotoolbox.org/api/v1",
             kobohr = "https://kc.humanitarianresponse.info/api/v1",
             ona = "https://ona.io/api/v1",
             unhcr = "https://kobocat.unhcr.org/api/v1")
    } else if (version == "v2") {
      switch(api,
             kobo = "https://kobo.kobotoolbox.org/api/v2",
             kobohr = "https://kobo.humanitarianresponse.info/api/v2")
    }
  } else {
    api
  }
}
