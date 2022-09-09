#' Get Team Game Stats
#'
#' Returns box score data by team for all games.
#'
#' The home team is coded as `team2`. Neutral site games may contain errors as
#' to whom is the home team.
#'
#' @returns Returns a tibble of box score statistics
#' @param year Filters to year (YYYY)
#' @param date Filters to date (YYYY-MM-DD)
#' @param team Filters to team
#' @param conf Filters to conference
#' @param game_id Filters to game
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{bart_game_box(year=2022)}
#'
#' @export
bart_game_box <- function(year = current_season(), date = NULL, team = NULL, conf = NULL, game_id = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/games/box?'
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
        make_toRvik_data('Game Box', Sys.time())
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
