#' Get country codes for a vector of country names
#'
#' Finds country code for most countries using alpha-3 (ISO 3166) codes for most countries, and
#' custom code for the others.
#'
#' @details
#' Yugoslavia and 'Serbia and Montenegro' are exceptions to alpha-3 as they are countries
#' that no longer exists. In addition, 'Rest of the World' voting has its own
#' custom country code of ROW.
#'
#' Uses [countrycode::codelist].
#'
#' @param name A character vector with country names
#'
#' @returns A character vector with country codes
#'
#' @examples
#' countries <- c("Sweden", "Italy", "Yugoslavia", "Rest of the World")
#'
#' get_countrycode(countries)
#'
#' @export
get_countrycode <- function(name) {
  stopifnot(is.character(name), length(name) >= 1)

  codes <- name |>
    data.frame() |>
    dplyr::mutate(
      code = dplyr::case_when(
        name == "Yugoslavia" ~ "YUG",
        name == "Serbia and Montenegro" ~ "SCG",
        name == "Rest of the World" ~ "ROW",
        .default = countrycode::countryname(
          sourcevar = .data$name,
          destination = "iso3c",
          warn = FALSE
        )
      )
    ) |>
    dplyr::pull(.data$code)

  return(codes)
}

#' Get country names from country codes
#'
#' Checks for countries participated in Eurovision Song Contest. Finds country
#' name for most countries using alpha-3 (ISO 3166) codes, and custom code
#' for the others.
#'
#' @details
#' Yugoslavia and 'Serbia and Montenegro' are exceptions to alpha-3 as they are countries
#' that no longer exists. In addition, 'Rest of the World' voting has its own
#' country code of ROW.
#'
#' Uses [countrycode::codelist].
#'
#' @param code A character vector with country codes
#'
#' @returns A character vector with country names
#'
#' @examples
#' country_codes <- c("SWE", "ITA", "YUG", "ROW")
#'
#' get_countryname(country_codes)
#'
#' @export
get_countryname <- function(code) {
  stopifnot(is.character(code), length(code) >= 1)

  names <- code |>
    data.frame() |>
    dplyr::mutate(
      name = dplyr::case_when(
        code == "YUG" ~ "Yugoslavia",
        code == "SCG" ~ "Serbia and Montenegro",
        code == "ROW" ~ "Rest of the World",
        .default = countrycode::countrycode(
          sourcevar = .data$code,
          origin = "iso3c",
          destination = "country.name",
          warn = FALSE
        )
      )
    ) |>
    dplyr::pull(.data$name)

  return(names)
}

#' Is a country name or code valid within Eurovision
#'
#' @param x A character vector containing either country names or codes
#'
#' @returns A logical vector
#'
#' @seealso [get_countrycode()] [get_countryname()]
#'
#' @examples
#' is_valid_country("Sweden")
#'
#' country_mix <- c("Sweden", "ITA", "Artist", NA, "ROW")
#'
#' is_valid_country(country_mix)
#'
#' @export
is_valid_country <- function(x) {
  stopifnot(is.character(x), length(x) >= 1)

  valid_codes <- !is.na(get_countrycode(x))
  valid_names <- !is.na(get_countryname(x))

  valid_codes | valid_names
}
