#' Get NCAA Tournament Results
#'
#' Returns a tibble with raw and adjusted NCAA tournament results by team,
#' coach, conference, or seed back to 2000.
#'
#' \itemize{\item PASE is the number of wins above or below the expected value
#' given a seed. \item PAKE is the number of wins above or below the expected
#' value given a KenPom rating.}
#'
#' @returns Returns a tibble of adjusted and raw tournament results.
#' @param min_year Minimum year to pull -- defaults to 2000 (YYYY).
#' @param max_year Maximum year to pull -- defaults to current season (YYYY).
#' @param type Data split value, defaults to team ('team', 'coach', 'conf',
#'   'seed').
#' @import dplyr
#' @import httr
#' @import janitor
#' @import readr
#' @importFrom withr local_options
#' @importFrom rvest read_html html_table
#' @importFrom cli cli_abort
#' @importFrom purrr pluck
#' @importFrom magrittr %>%
#' @examples
#' bart_tourney_results(min_year=2010, max_year=2015, type='conf')
#'
#' @export
bart_tourney_results <- function(min_year = 2000, max_year = current_season(), type = "team") {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(min_year) && nchar(min_year) == 4 && min_year >=
          2000)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2000!")
    }
    if (!(is.numeric(max_year) && nchar(max_year) == 4 && max_year <=
          toRvik::current_season())) {
      cli::cli_abort("Enter a valid year as a number (YYYY). The maximum year cannot exceed the current season.")
    }
    if (!(type %in% c("team", "coach", "conf", "seed"))) {
      cli::cli_abort("Please input a valid type command ('team','coach','conf', or 'seed')")
    } else {
      x <- httr::GET(paste0("https://barttorvik.com/cgi-bin/ncaat.cgi?conlimit=&yrlow=", min_year, "&yrhigh=", max_year, "&type=", type)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        dplyr::select(-1) %>%
        janitor::clean_names() %>%
        filter(pake != "PAKE") %>%
        dplyr::mutate(across(15:16, readr::parse_number),
          across(2:14, as.numeric),
          from = min_year,
          to = max_year
        )
      return(x)
    }
  })
}
