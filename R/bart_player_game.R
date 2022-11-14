#' Get Player Game Stats
#'
#' Returns detailed game-by-game player statistics on a variety of splits.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats; sorts by ppg.}
#' \item{shooting}{Returns play-by-play shooting splits; sorts by ppg.}
#' \item{advanced}{Returns advanced metrics and possession-adjusted box score
#' statistics; sorts by recruiting rank.}
#' \item{all}{Used when `load_all` is TRUE to return all data}}
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
#' @param load_all Load all available data (boolean); defaults to FALSE.
#' @param ... Acceptable parameters for API. Used for future development
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_player_game(year=2023, stat='box'))}
#'
#' @export
bart_player_game <- function(year = current_season(), stat = NULL, game_id = NULL, player_id = NULL, exp = NULL, team = NULL, conf = NULL, load_all = FALSE, ...) {

  if (is.null(stat)) {
    cli::cli_abort(c(
      "x" = "You forgot to include {.var stat}!"
    ))
  }

  stat <- switch(stat,
                 'box' = 'pg_box',
                 'shooting' = 'pg_shooting',
                 'advanced' = 'pg_adv',
                 'all' = 'pg_all')

  # load all data if requested
  if (load_all) {

    data <- load_gh_data(stat, load_all = TRUE)

    # filter with parameters
    if (any(!is.null(c(game_id, player_id, exp, team, conf)))) {

      data <- data %>%
        dplyr::filter(
          game_id %==% !!game_id &
          id %==% player_id &
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

    data <- load_gh_data(stat, year = year)

    # filter with parameters
    if (any(!is.null(c(game_id, player_id, exp, team, conf)))) {

      data <- data %>%
        dplyr::filter(
          game_id %==% !!game_id &
            id %==% player_id &
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

  return(data)
}
