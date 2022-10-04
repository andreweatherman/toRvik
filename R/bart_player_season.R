#' Get Player Season Stats
#'
#' Returns detailed, season-long player statistics on a variety of splits.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats; sorts by ppg.}
#' \item{shooting}{Returns play-by-play shooting splits; sorts by ppg.}
#' \item{advanced}{Returns advanced metrics and possession-adjusted box score
#' statistics; sorts by recruiting rank.} \item{all}{Used when `load_all` is
#' TRUE to return all data}}
#'
#' @returns Returns a tibble with the number of columns dependent on the value
#'   supplied to the `stat` argument.
#' @param year Defaults to current season (YYYY).
#' @param id Filters to player ID
#' @param team Filters to team
#' @param stat Stat to return ('advanced', 'box', 'shooting')
#' @param load_all Load all available data (boolean); defaults to FALSE.
#' @param ... Acceptable parameters for API. Used for function development
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{bart_player_season(year=2019, stat='advanced')}
#'
#' @export
bart_player_season <- function(year = current_season(), id = NULL, team = NULL, stat = NULL, load_all = FALSE, ...) {

  if (is.null(stat)) {
    cli::cli_abort(c(
      "x" = "You forgot to include {.var stat}!"
    ))
  }

  # load all data if requested
  if (load_all) {

    stat <- switch(stat,
                   'box' = 'ps_box',
                   'shooting' = 'ps_shooting',
                   'advanced' = 'ps_adv',
                   'all' = 'ps_all')

    data <- load_gh_data(stat)

    # filter with parameters
    if (any(!is.null(c(id, team)))) {

      data <- data %>%
        dplyr::filter(
            id %==% player_id &
            team %==% !!team
        )

    }

    else {

    }

    tryCatch(
      expr = {
        data  <- data %>%
          make_toRvik_data('Player Season Stats', Sys.time())
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

    base_url <- 'https://api.cbbstat.com/players/season/stats?'
    parsed <- httr::modify_url(
      base_url,
      query = list(
        year = year,
        id = id,
        team = team,
        type = stat
      )
    )
    data <- data.frame()

    tryCatch(
      expr = {
        data  <- jsonlite::fromJSON(parsed) %>%
          make_toRvik_data('Player Season Stats', Sys.time())
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

