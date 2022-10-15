#' Get Head Coach Changes
#'
#' Returns head coaching changes at the Division 1 level by season.
#'
#' @returns Returns a tibble with four columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{old_coach}}{character.}
#'   \item{\code{new_coach}}{character.}
#' }
#' @param year Year to filter
#' @param conf Conference to filter
#' @param team Team to filter
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_coach_change(year=2015))}
#'
#' @export
bart_coach_change <- function(year = NULL, conf = NULL, team = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/coaches/changes?'

  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      conf = conf,
      team = team
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Coaching Changes', Sys.time())
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
