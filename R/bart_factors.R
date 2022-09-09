#' Get Four Factor Statistics
#'
#' Returns four factor data and team records on a variety of splits, including
#' date range, quadrant level, opponent ranking, game location, and game type.
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}.
#'
#' @returns Returns a tibble with 22 columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{rating}}{double. Expected scoring margin against an average
#'   team on a neutral court.}
#'   \item{\code{rank}}{double.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{adj_o_rank}}{double.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{adj_d_rank}}{double.}
#'   \item{\code{tempo}}{double.}
#'   \item{\code{off_ppp}}{double. Raw points scored per possession.}
#'   \item{\code{off_efg}}{double. Team effective FG\%.}
#'   \item{\code{off_to}}{double. Offensive turnover rate.}
#'   \item{\code{off_or}}{double. Offensive rebound rate.}
#'   \item{\code{off_ftr}}{double. Offensive free throw rate.}
#'   \item{\code{def_ppp}}{double. Raw points allowed per possession.}
#'   \item{\code{def_efg}}{double. Effective FG\% allowed.}
#'   \item{\code{def_to}}{double. Turnover rate forced.}
#'   \item{\code{def_or}}{double. Defensive rebound rate.}
#'   \item{\code{def_ftr}}{double. Free throw rate allowed.}
#'   \item{\code{wins}}{integer.}
#'   \item{\code{losses}}{integer.}
#'   \item{\code{games}}{integer.}
#' }
#'
#' @param year Defaults to current season (YYYY).
#' @param result Filters by result ('W' or 'L')
#' @param type Filter by game type ('nc', 'conf', or 'post')
#' @param start Filters by starting date (YYYY-MM-DD)
#' @param end Filters by ending date (YYYY-MM-DD)
#' @param location Filters by game location ('H', 'A', or 'N')
#' @param last Filters by last x games played
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble arrange
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' bart_factors(year=2022, start='2022-01-13', type='conf')
#'
#' @export
bart_factors <- function(year = current_season(), result = NULL, type = NULL, start = NULL, end = NULL, location = NULL, last = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/games/factors/splits?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      result = result,
      type = type,
      start = start,
      end = end,
      location = location,
      last = last
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Team Factors', Sys.time()) %>%
        dplyr::arrange(rank)
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
