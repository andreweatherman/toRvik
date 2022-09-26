#' Get Team Recruiting Rankings
#'
#' Returns team rankings for 247Sports
#'
#' Function pulls national and conference ranking for 247Sports recruiting back
#' to 2011
#'
#' @returns Returns a tibble with 31 columns: \describe{
#'   \item{\code{year}}{integer.}
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{rank}}{integer.}
#'   \item{\code{comp_rank}}{integer. Composite national team ranking.}
#'   \item{\code{conf_rank}}{double.}
#'   \item{\code{comp_conf_rank}}{double. Composite conference team ranking.}
#'   \item{\code{avg_rating}}{double. Average commit rating.}
#'   \item{\code{avg_comp_rating}}{double. Average commit composite rating.}
#'   \item{\code{rating}}{double. 247Sports class rating.}
#'   \item{\code{comp_rating}}{double. 247Sports composite class rating.}
#'   \item{\code{five_stars}}{integer.}
#'   \item{\code{comp_five_stars}}{double.}
#'   \item{\code{four_stars}}{integer.}
#'   \item{\code{comp_four_stars}}{double.}
#'   \item{\code{three_stars}}{integer.}
#'   \item{\code{comp_three_stars}}{double.}
#'   \item{\code{commits}}{integer.}}
#' @param year Freshman season for class (class year + 1)
#' @param conf Conference to filter for
#' @param team Team to filter for
#' @param ... Any other parameter accepted by the API endpoint
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' team_recruiting_rankings(year=2022)
#'
#' @export

team_recruiting_rankings <- function(year = NULL, conf = NULL, team = NULL, ...) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2011)) {
    cli::cli_abort(c(
      "{.var year} must be 2011 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/teams/recruits?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      conf = conf,
      team = team,
      ...
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Team Recruiting Rankings', Sys.time())
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
