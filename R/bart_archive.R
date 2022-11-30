#' Get T-Rank Archive Ratings
#'
#' Returns T-Rank ratings and efficiency metrics from the morning of the
#' specified day. Data goes back to 2014-15.
#'
#' @returns Returns a tibble with 16 columns:
#' \describe{
#'   \item{\code{rk}}{double.}
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{rec}}{character.}
#'   \item{\code{barthag}}{double. The estimation of a team's win probability
#'   against the average Division 1 team on a neutral court.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{adj_o_rk}}{double.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{adj_d_rk}}{double.}
#'   \item{\code{adj_tempo}}{double.}
#'   \item{\code{adj_tempo_rk}}{double.}
#'   \item{\code{proj_rec}}{character.}
#'   \item{\code{proj_conf_rec}}{character.}
#'   \item{\code{wab}}{double. The number of wins above or below the expected
#'   total from a bubble team against the same schedule.}
#'   \item{\code{wab_rk}}{double.}
#'   \item{\code{date}}{double.}
#' }
#' @param date Date to filter for (YYYYMMDD).
#' @param team Team to filter for.
#' @param year Year to filter for.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_archive(date='20220113'))}
#'
#' @export
bart_archive <- function(date = NULL, team = NULL, year = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2015)) {
    cli::cli_abort(c(
      "{.var year} must be 2015 or later",
      "x" = "You passed through {year}"
    ))
  }

  # remove hyphens if present in date
  date <- gsub('-', '', date)

  data <- read.csv(paste0('https://github.com/andreweatherman/toRvik-data/raw/main/ratings/archive/by_date/ratings_archive_', date, '.csv')) %>%
    arrange(rank)

  tryCatch(
    expr = {
      data  <- data %>%
        drop_index() %>%
        make_toRvik_data('Archive Ratings', Sys.time())
    },
    error = function(e) {
      check_docs_error()
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(data)
}
