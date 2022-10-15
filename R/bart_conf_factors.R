#' Get Conference Four Factor Statistics
#'
#' Returns conference-wide four factor data on a variety of splits, including
#' date range, quadrant level, opponent ranking, game location, and game type.
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}. Data can be
#' split on five variables: \describe{ \item{venue}{Splits on game location;
#' 'all', 'home', 'away', 'neutral', and 'road' (away + neutral).}
#' \item{type}{Splits on game type; 'all', 'nc' (non-conference), 'conf'
#' (conference), 'reg' (regular season), 'post' (post-season tournaments),
#' 'ncaa' (NCAA tournament).} \item{quad}{Splits by quadrant level; 1-4 with 0
#' indicating 1-A games.} \item{top}{Splits by opponent T-Rank position,
#' adjusted for game location.} \item{start/end}{Splits by date range
#' (YYYYMMDD).}}
#'
#' @returns Returns a tibble with 21 columns:
#' \describe{
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
#' @param year Filters to year (YYYY)
#' @param conf Filters to conference
#' @param opp_conf Filters to opponent conference
#' @param type Filters to game type ('nc', 'conf', or 'post')
#' @param location Filters to game location ('H', 'A', or 'N')
#' @param start Filters by starting date (YYYY-MM-DD)
#' @param end Filters by ending date (YYYY-MM-DD)
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble arrange
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_conf_factors(type='nc'))}
#'
#' @export

bart_conf_factors <- function(year = current_season(), conf = NULL, opp_conf = NULL,type = NULL, location = NULL, start = NULL, end = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/conferences/factors?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      conf = conf,
      opp_conf = opp_conf,
      type = type,
      location = location,
      start = start,
      end = end
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Conference Factors', Sys.time()) %>%
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
