#' Get years pertaining to Eurovision Song Contest
#'
#' The function returns a list with vectors of years pertaining to Eurovision
#' Song Contest. The list contains the following vectors:
#'
#' * `all`          - All years from contest start up until current year
#' * `cancelled`    - Years when the event was cancelled
#' * `one_round`    - Years that only had a Final round
#' * `two_round`    - Years that had one Semi-Final and one Final round
#' * `three_round`  - Years that had two Semi-Finals and one Final round
#'
#' @returns a list of named numeric vectors
#'
#' @examples
#' # Get years that have had all three rounds
#' esc_years <- get_years()
#' esc_years$three_round
#'
#' # Get years when the event was cancelled
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
