#' Get T-Rank Archive Ratings
#'
#' Returns T-Rank ratings and efficiency metrics from the morning of the
#' specified day. Data goes back to 2014-15.
#'
#' @returns Returns a tibble with 16 columns:
#' #' \describe{
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
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @examples
#' \donttest{bart_archive(date='20220113')}
#'
#' @export
bart_archive <- function(date = NULL, team = NULL, year = NULL) {
  base_url <- 'https://api.cbbstat.com/ratings/archive?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      date = date,
      team = team,
      year = year
    )
  )
  data <- jsonlite::fromJSON(parsed) %>% dplyr::as_tibble()
  return(data)
}
