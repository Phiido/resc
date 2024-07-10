#' Get country codes for a vector of country names
#'
#' Returns the corresponding country code for a vector of country names.
#' Most countries are identified using alpha-3 (ISO 3166) codes.
#' Exceptions are Yugoslavia, 'Serbia and Montenegro', and 'Rest of the World',
#' which have custom codes.
#'
#' @details
#' Yugoslavia and 'Serbia and Montenegro' are countries that no longer exist.
#' 'Rest of the World' is a special case with its own country code, ROW.
#'
#' Uses [countrycode::codelist].
#'
#' @param name A character vector with country names
#'
#' @returns A character vector with country codes
#' 
#' @seealso [get_countryname()]
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
#' Returns the corresponding country name for a vector of country codes.
#' Most countries are identified using alpha-3 (ISO 3166) codes.
#' Exceptions are Yugoslavia, 'Serbia and Montenegro', and 'Rest of the World',
#' which have custom codes.
#'
#' @details
#' Yugoslavia and 'Serbia and Montenegro' are countries that no longer exist.
#' 'Rest of the World' is a special case with its own country code, ROW.
#'
#' Uses [countrycode::codelist].
#'
#' @param code A character vector with country codes
#'
#' @returns A character vector with country names
#' 
#' @seealso [get_countrycode()]
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

#' Determine if a country name or code is valid within Eurovision
#'
#' This function checks if each input is a valid country name or code.
#'
#' @param x A character vector containing either country names or codes
#' @return A logical vector indicating which inputs are valid
#'
#' @details
#' Validity is determined by checking if the input can be converted to a
#' country code or name using the functions [get_countrycode()] and
#' [get_countryname()].
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
