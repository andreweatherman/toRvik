#' Get Player of the Year Ratings
#'
#' Returns Barttorvik Player of the Year ratings on a variety of splits.
#'
#' Accepted conference abbreviations for the `conf` argument are: \itemize{\item
#' ‘A10’, ‘ACC’, ‘AE’, ‘ASun’, ‘Amer’, ‘B10’, ‘B12’, ‘BE’, ‘BSky’, ‘BSth’, ‘BW’,
#' ‘CAA’, ‘CUSA’, ‘Horz’, ‘Ivy’, ‘MAAC’, ‘MAC’, ‘MEAC’, ‘MVC’, ‘MWC’, ‘NEC’,
#' ‘OVC’, ‘P12’, ‘Pat’, ‘SB’, ‘SC’, ‘SEC’, ‘SWAC’, ‘Slnd’, ‘Sum’, ‘WAC’, ‘WCC’ }
#'
#' @returns Returns a tibble with four columns:
#' \describe{
#'   \item{\code{rk}}{integer.}
#'   \item{\code{player}}{character.}
#'   \item{\code{team}}{character.}
#'   \item{\code{score}}{double.}
#'   }
#' @param year Defaults to current season (YYYY).
#' @param conf Filters results by conference; defaults to all (see details).
#' @param class Filters results by class ('fr', 'so', 'jr', 'sr'); defaults to
#'   no filter.
#' @param conf_only Logical. Filters data by conference-only play; defaults to
#'   `FALSE`.
#' @import dplyr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom magrittr %>%
#' @examples
#' bart_poy(year=2019, class='fr')
#'
#' @export
bart_poy <- function(year = current_season(), conf = "All", class = NULL, conf_only = FALSE) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    }
    if(!is.null(class) && !(class %in% c('fr', 'so', 'jr', 'sr'))) {
      cli::cli_abort("Please input correct class value (see details)")
    }
    if (!(conf %in% c('All', 'A10', 'ACC', 'AE', 'ASun', 'Amer', 'B10', 'B12', 'BE', 'BSky', 'BSth', 'BW',
                      'CAA', 'CUSA', 'Horz', 'Ivy', 'MAAC', 'MAC', 'MEAC', 'MVC', 'MWC', 'NEC', 'OVC',
                      'P12', 'Pat', 'SB', 'SC', 'SEC', 'SWAC', 'Slnd', 'Sum', 'WAC', 'WCC'))) {
      cli::cli_abort("Please enter valid conference code (see details)")
    }

    class_lookup <- list(
      "fr" = "Fr",
      "so" = "So",
      "jr" = "Jr",
      "sr" = "Sr"
    )
    class <- class_lookup[class]
    if (conf_only == FALSE) {
      x <- httr::GET(paste0("https://barttorvik.com/poy.php?conlimit=", conf, "&year=", year, "&yr=", class)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        janitor::clean_names()
      return(x)
    }
    if (conf_only == TRUE) {
      x <- httr::GET(paste0("https://barttorvik.com/conpoy.php?conlimit=", conf, "&year=", year, "&yr=", class)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        janitor::clean_names()
      return(x)
    }
  })}
