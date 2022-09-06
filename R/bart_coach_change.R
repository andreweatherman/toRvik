#' Get Head Coach Changes
#'
#' Returns head coaching changes at the Division 1 level by season.
#'
#' @returns Returns a tibble with four columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{old_coach}}{character.}
#'   \item{\code{new_coach}}{character.}
#' }
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom magrittr %>%
#' @examples
#' bart_coach_change(year=2015)
#'
#' @export
bart_coach_change <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number. Data only goes back to 2008!")
    } else {
      x <- httr::GET(paste0("https://barttorvik.com/coaching_moves.php?year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table(header = FALSE) %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row = 1) %>%
        janitor::clean_names()
      return(x)
    }
  })
}
