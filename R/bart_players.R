#' Get All Players
#'
#' Returns players and player IDs for a given season
#'
#' @returns Returns a tibble with three columns:
#' \item{\code{player}}{character.}
#' \item{\code{team}}{character.}
#' \item{\code{id}}{double.}
#' \item{\code{year}}{double.}
#'
#' @param year Year to filter for; defaults to current season (YYYY)
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' bart_players(year=2022)
#'
#' @export
bart_players <- function(year = current_season()) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
    cli::cli_abort(c(
      "{.var year} must be 2008 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/players?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data(sprintf('Player Database: %i', year), Sys.time())
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
