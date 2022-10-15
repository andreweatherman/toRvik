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
#' \donttest{try(bart_tourney_sheets(year=2019))}
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
