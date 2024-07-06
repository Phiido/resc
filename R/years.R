#' Get years pertaining to Eurovision Song Contest
#'
#' Builds a list with potential sets of years starting from contest start in
#' 1956 up until the current year.
#'
#' @details
#' The following are possible to subset:
#'
#' * `all`          - Returns all years from contest start up until current year
#' * `cancelled`    - Returns years when the event was cancelled
#' * `one_round`    - Returns years that only had a Final round
#' * `two_round`    - Returns years that had one Semi-Final and one Final round
#' * `three_round`  - Returns years that had two Semi-Finals and one Final round
#'
#' See examples for detailed usage.
#'
#' @returns a list of numeric vectors
#'
#' @examples
#' esc_years <- get_years()
#'
#' # Which years have had all three rounds?
#' esc_years$three_round
#'
#' # Which years were cancelled?
#' esc_years$cancelled
#'
#' @export
get_years <- function() {
  starting_year <- 1956
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  list(
    all         = starting_year:current_year,
    cancelled   = 2020,
    one_round   = starting_year:2003,
    two_round   = 2004:2007,
    three_round = 2008:current_year
  )
}
