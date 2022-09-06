#' Get Team Shooting Splits
#'
#' Returns team shooting totals and shares by shot location. Data returns back
#' to 2010.
#'
#' `x_share` represents the percentage of made FGs that fall under that
#' category.
#'
#' @returns Returns a tibble with 20 columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{seed}}{double.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{dunk_fg}}{character.}
#'   \item{\code{dunk_share}}{double.}
#'   \item{\code{dunk_fg_d}}{character.}
#'   \item{\code{dunk_share_d}}{double.}
#'   \item{\code{close_fg}}{character.}
#'   \item{\code{close_share}}{double.}
#'   \item{\code{close_fg_d}}{character.}
#'   \item{\code{close_share_d}}{double.}
#'   \item{\code{far_fg}}{character.}
#'   \item{\code{far_share}}{double.}
#'   \item{\code{far_fg_d}}{character.}
#'   \item{\code{far_share_d}}{double.}
#'   \item{\code{three_fg}}{character.}
#'   \item{\code{three_share}}{double.}
#'   \item{\code{three_fg_d}}{character.}
#'   \item{\code{three_share_d}}{double.}
#'   \item{\code{year}}{double.}
#'}
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import httr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' bart_team_shooting(year=2019)
#' @export
bart_team_shooting <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    names <- c("team", "conf", "dunk_fg", "dunk_share", "dunk_fg_d", "dunk_share_d", "close_fg", "close_share", "close_fg_d", "close_share_d", "far_fg", "far_share", "far_fg_d", "far_share_d", "three_fg", "three_share", "three_fg_d", "three_share_d")
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2010)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2010!")
    } else {
      x <- httr::GET(paste0("https://barttorvik.com/teampbptot.php?year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        subset(select = -c(1, seq(7, 22, 5)))
      colnames(x) <- names
      x <- x[!(x$team == ""), ]
      x <- x %>%
        tidyr::separate(team, into = c("team", "seed"), sep = "(?<=\\D) (?=[0-9])") %>%
        dplyr::mutate(across(c(2, seq(5, 19, 2)), as.numeric), year = year)
      return(x)
    }
  })
}
