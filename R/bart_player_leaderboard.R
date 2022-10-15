#' Player Leaderboard
#'
#' Returns season or career stat leaders for a given statistic
#'
#' Available statistics to pass through to `stat`:
#' |stat    |agg  |
#' |:-------|:----|
#' |min     |both |
#' |pts     |both |
#' |fg_pct  |mean |
#' |efg     |mean |
#' |ts      |mean |
#' |two_m   |both |
#' |two_a   |both |
#' |three_m |both |
#' |three_a |both |
#' |ftm     |both |
#' |fta     |both |
#' |dunk_m  |both |
#' |dunk_a  |both |
#' |rim_m   |both |
#' |rim_a   |both |
#' |mid_m   |both |
#' |mid_a   |both |
#' |oreb    |both |
#' |dreb    |both |
#' |reb     |both |
#' |ast     |both |
#' |tov     |both |
#' |ast_tov |mean |
#' |stl     |both |
#' |blk     |both |
#' |pf      |both |
#' |ortg    |mean |
#' |usg     |mean |
#' |or_pct  |mean |
#' |dr_pct  |mean |
#' |ast_pct |mean |
#' |to_pct  |mean |
#' |stl_pct |mean |
#' |blk_pct |mean |
#' |bpm     |mean |
#' |obpm    |mean |
#' |dbpm    |mean |
#' |net     |mean |
#'
#' @returns Returns a tibble.
#' @param stat Stat to return (see details)
#' @param year Filters to yer -- only with `season` span
#' @param agg Return `mean` or `sum` data
#' @param span Return `season` or `career` data
#' @param num_players Number of players to return (defaults to 25)
#' @param min_games Minimum games
#' @param min_minutes Minimum minutes
#' @param exp Experience to filter
#' @param team Team to filter
#' @param conf Conference to filter
#' @param id Player id
#' @param ... Other API parameters. Used for future expansion.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble arrange
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_player_leaderboard('pts'))}
#' @md
#' @export
bart_player_leaderboard <- function(stat = NULL, year = NULL, agg = 'mean', span = 'career', num_players = 25, min_games = NULL, min_minutes = NULL, exp = NULL, team = NULL, conf = NULL, id = NULL, ...) {

  # test that stat was passed through
  if (is.null(stat)) {
    cli::cli_abort(c(
      "x" = "{.var stat} must be declared. See documentation for a full list."
    ))
  }

  # test that year wasn't used with career
  if (span == 'career' & !is.null(year)) {
    cli::cli_abort(c(
      "x" = "{.var year} cannot be used when {.var span} is set to career"
    ))
  }

  # test stat passed through
  accepted_stats <- c('min', 'pts', 'fg_pct', 'efg', 'ts', 'two_m', 'two_a', 'three_m',
                      'three_a', 'ftm', 'fta', 'dunk_m', 'dunk_a', 'rim_m', 'rim_a',
                      'mid_m', 'mid_a', 'oreb', 'dreb', 'reb', 'ast', 'tov', 'ast_tov',
                      'stl', 'blk', 'pf', 'ortg', 'usg', 'or_pct', 'dr_pct', 'ast_pct',
                      'to_pct', 'stl_pct', 'blk_pct', 'bpm', 'obpm', 'dbpm', 'net')

  if (!(stat %in% accepted_stats)) {
    cli::cli_abort(c(
      "x" = "{stat} is not supported. Please check function details for a full list of accepted
      stats."
    ))
  }

  base_url <- 'https://api.cbbstat.com/players/leaders?'

  parsed <- httr::modify_url(
    base_url,
    query = list(
      stat = stat,
      year = year,
      agg = agg,
      span = span,
      n = num_players,
      min_games = min_games,
      min_minutes = min_minutes,
      exp = exp,
      conf = conf,
      team = team,
      id = id,
      ...
    )
  )

  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data(sprintf('Player Leaders: %s', stat), Sys.time())
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
