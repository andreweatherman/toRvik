#' Get Four Factor Statistics
#'
#' Returns four factor data and team records on a variety of splits, including
#' date range, quadrant level, opponent ranking, game location, and game type.
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}. Data can be
#' split on three variables: \describe{ \item{result}{Splits on game result: 'W'
#' or 'L'.} \item{type}{Splits on game type: 'nc' (non-conference), 'conf'
#' (conference), or 'post' (post-season tournaments).} \item{quad}{Splits by
#' quadrant level; 1-4 with 0 indicating 1-A games.} \item{top}{Splits by
#' opponent T-Rank position, adjusted for game location.}
#' \item{start/end}{Splits by date range (YYYYMMDD).}}
#'
#' @returns Returns a tibble with 22 columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{rating}}{double. Estimation of margin of victory against a
#'   statistically average team on a neutral court.}
#'   \item{\code{rank}}{double.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{adj_o_rank}}{double.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{adj_d_rank}}{double.}
#'   \item{\code{tempo}}{double.}
#'   \item{\code{off_ppp}}{double. Raw points/possession.}
#'   \item{\code{off_efg}}{double. Effective FG%}
#'   \item{\code{off_to}}{double. Turnover rate.}
#'   \item{\code{off_or}}{double. Offensive rebound rate.}
#'   \item{\code{off_ftr}}{double. Free throw rate.}
#'   \item{\code{def_ppp}}{double. Raw points/possession.}
#'   \item{\code{def_efg}}{double. Effective FG% allowed.}
#'   \item{\code{def_to}}{double. Forced urnover rate.}
#'   \item{\code{def_or}}{double. Offensive rebound rate allowed.}
#'   \item{\code{def_ftr}}{double. Free throw rate allowed.}
#'   \item{\code{wins}}{integer.}
#'   \item{\code{losses}}{integer.}
#'   \item{\code{games}}{integer}}
#'
#' @param year Defaults to current season (YYYY).
#' @param result Filters by result ('W' or 'L')
#' @param type Filter by game type ('nc', 'conf', or 'post')
#' @param start Filters by starting date (YYYY-MM-DD)
#' @param end Filters by ending date (YYYY-MM-DD)
#' @param location Filters by game location ('H', 'A', or 'N')
#' @param last Filters by last x games played
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @examples
#' \donttest{bart_factors(start='2022-01-13', type='conf')}
#'
#' @export
bart_factors <- function(year = current_season(), result = NULL, type = NULL, start = NULL, end = NULL, location = NULL, last = NULL) {
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
  data <- jsonlite::fromJSON(parsed) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange(rank)
  return(data)
}
