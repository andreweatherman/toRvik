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
#' @param year Filters to year
#' @param conf Filters to conference
#' @param team Filters to team
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_team_shooting(year=2019))}
#' @export
bart_team_shooting <- function(year = current_season(), conf = NULL, team = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2010)) {
    cli::cli_abort(c(
      "{.var year} must be 2010 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/teams/shooting?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      team = team,
      conf = conf
    )
  )

  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Team Shooting', Sys.time())
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
