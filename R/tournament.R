#' Get Committee Tournament Sheets
#'
#' Returns a tibble with similar quality and resume metrics used by the NCAA
#' seeding committee. Data runs back to the 2019 tournament.
#'
#' @returns Returns a tibble with 16 columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{seed}}{double.}
#'   \item{\code{net}}{double.}
#'   \item{\code{kpi}}{double.}
#'   \item{\code{sor}}{double.}
#'   \item{\code{res_avg}}{double. Average of resume rankings (NET, KPI, and
#'   SOR).}
#'   \item{\code{bpi}}{double.}
#'   \item{\code{kp}}{double.}
#'   \item{\code{sag}}{double.}
#'   \item{\code{qual_avg}}{double. Average of quality rankings (BPI, KP, and
#'   Sag).}
#'   \item{\code{q1a}}{character.}
#'   \item{\code{q1}}{character.}
#'   \item{\code{q2}}{character.}
#'   \item{\code{q1_2}}{character.}
#'   \item{\code{q3}}{character.}
#'   \item{\code{q4}}{character.}
#' }
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' bart_tourney_sheets(year=2019)
#'
#' @export
bart_tourney_sheets <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2019)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2019!")
    } else {
      x <- httr::GET(paste0("https://barttorvik.com/teamsheets.php?year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row = 1) %>%
        janitor::clean_names() %>%
        tidyr::separate(team, into = c("team", "seed"), sep = "(?<=\\D) (?=[0-9])")
      x <- x %>%
        dplyr::select(-1) %>%
        dplyr::mutate(across(c(2:10), as.numeric)) %>%
        dplyr::rename("res_avg" = 6, "qual_avg" = 10)
      return(x)
    }
  })
}


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
#' bart_tourney_odds(year=2022, odds='pre')
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
