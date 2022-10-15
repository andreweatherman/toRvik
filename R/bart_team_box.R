#' Get Team Box Stats
#'
#' Returns aggregate team stats back to 2008
#'
#' The `split` argument filters the results by split, explained below:
#' \describe{ \item{result}{Wins and losses} \item{location}{Game location}
#' \item{month}{Game month} \item{type}{Game type}}
#'
#' @returns Returns a tibble with the number of columns dependent on the year.
#' @param year Filters to year.
#' @param start Filters to starting year
#' @param end Filters to ending year
#' @param team Filters to team.
#' @param conf Filters to conf.
#' @param split Split to filter (see details).
#' @param stat Filters for agg. stats (total) or avg. stats (avg)
#' @param aggregate Aggregate all years (TRUE) or return per-year stats when
#'   passing a range of years (FALSE; default)
#' @param ... Any other parameter accepted by the API endpoint
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble arrange
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_team_box(split='month', team='Duke'))}
#' @export
bart_team_box <- function(year = NULL, start = NULL, end = NULL, team = NULL, conf = NULL, split = NULL, stat = 'total', aggregate = FALSE, ...) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  # test starting year
  if (!is.null(start) & !(is.numeric(start) && nchar(start) == 4 && start >= 2008)) {
    cli::cli_abort(c(
      "{.var start} must be 2008 or later",
      "x" = "You passed through {start}"
    ))
  }

  # test ending year
  if (!is.null(end) & !(is.numeric(end) && nchar(end) == 4 && end >= 2008)) {
    cli::cli_abort(c(
      "{.var end} must be 2008 or later",
      "x" = "You passed through {end}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/teams/stats?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      start = start,
      end = end,
      team = team,
      conf = conf,
      split = split,
      stat = stat,
      aggregate = aggregate,
      ...
    )
  )

  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Team Stats', Sys.time())
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
