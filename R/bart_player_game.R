#' Get Player Game Stats
#'
#' Returns detailed game-by-game player statistics on a variety of splits.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats; sorts by ppg.}
#' \item{shooting}{Returns play-by-play shooting splits; sorts by ppg.}
#' \item{advanced}{Returns advanced metrics and possession-adjusted box score
#' statistics; sorts by recruiting rank.}}
#'
#' @returns Returns a tibble with the number of columns dependent on the value
#'   supplied to the `stat` argument.
#' @param year Year to filter.
#' @param stat Indicates statistical split (see details).
#' @param game_id Game to filter.
#' @param player_id Player to filter.
#' @param exp Player experience to filter.
#' @param team Team to filter.
#' @param conf Conference to filter.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{bart_player_game(year=2022, stat='box')}
#'
#' @export
bart_player_game <- function(year = current_season(), stat = NULL, game_id = NULL, player_id = NULL, exp = NULL, team = NULL, conf = NULL, load_all = FALSE, ...) {


  # load all data if requested
  if (load_all) {

    switch(stat,
           'box' = 'pg_box',
           'shooting' = 'pg_shooting',
           'advanced' = 'pg_adv',
           'all' = 'pg_all')

    data <- load_gh_data(stat)

    # filter with parameters
    if (any(!is.null(c(game_id, player_id, exp, team, conf)))) {

      data <- data %>%
        dplyr::filter(
          year %==% !!year &
          game_id %==% !!game_id &
          player_id %==% id &
          exp %==% !!exp &
          team %==% !!team &
          conf %==% !!conf
        )

    }

    else {

    }

    tryCatch(
      expr = {
        data  <- data %>%
          make_toRvik_data('Player Game Stats', Sys.time())
      },
      error = function(e) {
        check_docs_error()
      },
      warning = function(w) {
      },
      finally = {
      }
    )

  }

  else {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/players/game/stats?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      type = stat,
      game_id = game_id,
      player_id = player_id,
      exp = exp,
      team = team,
      conf = conf
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Player Game Stats', Sys.time())
    },
    error = function(e) {
      check_docs_error()
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  }
  return(data)
}
