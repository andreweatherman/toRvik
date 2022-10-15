#' Get NCAA Tournament Odds
#'
#' Returns a tibble with round-to-round tournament probabilities. Data runs back
#' to the 2018 tournament; region is only avaliable for the 2022 tournament.
#'
#' \code{odds} splits the data by four variables: \describe{ \item{current}{Returns
#' to-date odds.} \item{pre}{Returns pre-tournament odds} \item{recent}{Returns
#' adjusted odds over the last ten games} \item{t100}{Returns odds against top
#' 100 opponents.}}
#'
#' @returns Returns a tibble with 10 to 11 columns (depending on year input):
#' #' \describe{
#'   \item{\code{seed}}{double.}
#'   \item{\code{region}}{character. Only avaliable for year >= 2022}
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{r64}}{double.}
#'   \item{\code{r32}}{double.}
#'   \item{\code{s16}}{double.}
#'   \item{\code{e8}}{double.}
#'   \item{\code{f4}}{double.}
#'   \item{\code{f2}}{double.}
#'   \item{\code{champ}}{double.}
#' }
#' @param year Defaults to current season (YYYY).
#' @param odds Filters results by odds, defaults to current (see details).
#' @import dplyr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom rvest read_html html_table
#' @importFrom cli cli_abort
#' @importFrom purrr pluck
#' @importFrom stringr str_replace
#' @importFrom magrittr %>%
#' @examples
#' \donttest{try(bart_tourney_odds(year=2022, odds='pre'))}
#'
#' @export
bart_tourney_odds <- function(year = current_season(), odds = "current") {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    lookup <- list(
      "current" = "cur",
      "pre" = "pre",
      "recent" = "l10",
      "t100" = "t100"
    )
    odds <- lookup[odds]
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2018)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2018!")
    }
    if (!(odds %in% c("cur", "pre", "l10", "t100"))) {
      cli::cli_abort("Please input a valid odds variable ('current', 'pre', 'recent', or 't100')")
    }
    if (year >= 2022) {
      x <- httr::GET(paste0("https://barttorvik.com/tourneytime.php?conlimit=All&src=", odds, "&year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table(header = FALSE) %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row = 1) %>%
        janitor::clean_names() %>%
        dplyr::mutate_all(funs(stringr::str_replace(., "\\u2713", "100"))) %>%
        dplyr::mutate(across(c(1, 5:11), as.numeric))
      return(x)
    }
    if (year < 2022) {
      x <- httr::GET(paste0("https://barttorvik.com/tourneytime.php?conlimit=All&src=", odds, "&year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table(header = FALSE) %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row = 1) %>%
        janitor::clean_names() %>%
        dplyr::select(-2) %>%
        dplyr::mutate_all(funs(stringr::str_replace(., "\\u2713", "100"))) %>%
        dplyr::mutate(across(c(1, 4:10), as.numeric))
      return(x)
    }
  })
}
