#' Get Pregame Probabilities
#'
#' Returns pregame win probabilities, expected scores, and thrill quotients for
#' all games.
#'
#' Under the `type` column, games are classified by one of five indicators,
#' explained below: \describe{ \item{nc}{Non-conference games between two D-1
#' teams.} \item{conf}{In-conference games.} \item{conf_t}{Conference tournament
#' games.} \item{post}{Post-conference tournament games.} \item{nond1}{Games
#' involving one non-D1 team.} }
#'
#' @returns Returns a tibble with 13 columns: \describe{
#'   \item{\code{date}}{double.} \item{\code{conf}}{character.}
#'   \item{\code{line}}{character.} \item{\code{ttq}}{double. Torvik Thrill
#'   Quotient -- measures how good the teams are, how close the game is
#'   projected to be, and how fast the tempo is projected to be.}
#'   \item{\code{type}}{character. See details.} \item{\code{team1}}{character.}
#'   \item{\code{team1_wp}}{double. Estimated win percentage.}
#'   \item{\code{team1_pts}}{double. Estimated total points.}
#'   \item{\code{team2}}{character.} \item{\code{team2_wp}}{double. Estimated
#'   win percentage.} \item{\code{team2_pts}}{double. Estimated total points.}
#'   \item{\code{game_id}}{character.} \item{\code{year}}{double.} }
#' @param year Defaults to current season (YYYY).
#' @param team Team to filter for.
#' @param type Game type to filter for ('nc', 'conf', 'conf_t', 'post', 'nond1')
#' @param game_id Game ID to filter for.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_pregame(year=2022))}
#'
#' @export
bart_pregame <- function(year=current_season(), team = NULL, type = NULL, game_id = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/games/pregame?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      team = team,
      type = type,
      game_id = game_id
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Pregame Probabilities', Sys.time())
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
