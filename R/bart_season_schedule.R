#' Get Full Season Schedule
#'
#' Returns full season schedule for all D-1 teams by year
#'
#' Function pulls all games involving at least one D-1 team. Under the `type`
#' column, games are classified by one of four indicators, explained below:
#' \describe{ \item{nc}{Non-conference games between two D-1 teams.}
#' \item{conf}{In-conference games.} \item{post}{Post-conference tournament
#' games.} \item{nond1}{Games involving one non-D1 team.} }
#'
#' @returns Returns a tibble with six columns:
#' \describe{
#'   \item{\code{date}}{double.}
#'   \item{\code{type}}{character. See details.}
#'   \item{\code{neutral}}{logical.}
#'   \item{\code{home}}{character.}
#'   \item{\code{away}}{character.}
#'   \item{\code{game_id}}{character.}
#' }
#' @param year Defaults to current season (YYYY).
#' @param type Filters for game type (see details).
#' @param conf Filters for conference.
#' @param team Filters for team.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_season_schedule(year=2022))}
#'
#' @export
bart_season_schedule <- function(year = current_season(), type = NULL, conf = NULL, team = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/games?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      type = type,
      conf = conf,
      team = team
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data(sprintf('%i Schedule', year), Sys.time())
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
