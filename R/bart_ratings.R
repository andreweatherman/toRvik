#' Get T-Rank Ratings
#'
#' Returns current T-Rank ratings and two forms of strength of schedule.
#'
#' \itemize{\item `x_elite_sos` is the percentage of games that an 'elite' team
#' would project to lose against this team's non-conference or overall schedule.
#' \item `x_cur_sos` is the current average Barthag rating of opponents. \item
#' `x_fut_sos` is the projected average Barthag rating of opponents.}
#'
#' @returns Returns a tibble with 19 columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{barthag}}{double. The estimation of a team's win probability
#'   against the average Division 1 team on a neutral court.}
#'   \item{\code{barthag_rk}}{integer.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{adj_o_rk}}{integer.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{adj_d_rk}}{integer.}
#'   \item{\code{adj_t}}{double.}
#'   \item{\code{adj_t_rk}}{integer.}
#'   \item{\code{wab}}{double. The number of wins above or below the expected
#'   total from a bubble team against the same schedule.}
#'   \item{\code{nc_elite_sos}}{double.}
#'   \item{\code{nc_fut_sos}}{double.}
#'   \item{\code{nc_cur_sos}}{double.}
#'   \item{\code{ov_elite_sos}}{double.}
#'   \item{\code{ov_fut_sos}}{double.}
#'   \item{\code{ov_cur_sos}}{double.}
#'   \item{\code{seed}}{integer.}
#'   \item{\code{year}}{double.}
#' }
#' @param year Defaults to current season (YYYY).
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{bart_ratings(year=2020)}
#'
#' @export
bart_ratings <- function(year=current_season()) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/ratings?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year
    )
  )

  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data(sprintf('Team Ratings: %i', year), Sys.time())
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

