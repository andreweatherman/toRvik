#' Get Game Four Factors
#'
#' Returns game-by-game four factor statistics.
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}. `avg_marg` and
#' `opp_avg_marg` is the the average lead or deficit during a game.
#'
#' @returns Returns a tibble of four factor statistics
#' @param year Defaults to current season (YYYY).
#' @param team Filters to team.
#' @param conf Filters to conference.
#' @param opp_conf Filters to opponent's conference.
#' @param type Filters for game type ('nc', 'conf', 'conf_t', 'post')
#' @param location Filters for game location ('H', 'A', 'N')
#' @param result Filters for game result.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_game_factors(year=2022))}
#'
#' @export
bart_game_factors <- function(year = current_season(), team = NULL, conf = NULL, opp_conf = NULL, type = NULL, location = NULL, result = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/games/factors?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      team = team,
      conf = conf,
      opp_conf = opp_conf,
      type = type,
      location = location,
      result = result
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        drop_index() %>%
        mutate(date = as.Date(date)) %>%
        make_toRvik_data('Game Factors', Sys.time())
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
