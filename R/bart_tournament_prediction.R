#' T-Rank Tournament Prediction
#'
#' Returns single-elimination tournament predictions using Barttorvik's
#' model
#'
#' Given a list of teams arranged in scheduled order, function returns
#' tournament predictions for single-elimination tournaments. Teams must be
#' supplied as a character vector and in bracket order. In other words, if team
#' A plays team B and team C plays team D in round one, the function should be
#' supplied `teams=c('A', 'B', 'C', 'D')`.
#'
#' @returns Returns a tibble with four columns: \describe{
#'   \item{\code{team}}{character.} \item{\code{wins}}{integer.}
#'   \item{\code{finals}}{integer.} \item{\code{champ}}{integer.}}
#' @param teams List of teams arranged in scheduled order
#' @param date Date for tournament
#' @param sims Number of simulations to play
#' @param seed Seed to set for reproducible results
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' \donttest{try(bart_tournament_prediction(teams=c('Duke', 'North Carolina',
#' 'Kansas', 'Villanova'), date='20220402', sims=10, seed=1))}
#'
#' @export

bart_tournament_prediction <- function(teams = NULL, date = NULL, sims = NULL, seed = NULL) {

  base_url <- 'https://api.cbbstat.com/games/predictions/tournaments?'
  count <- 0
  for(team in teams) {
    if(count == 0) {
      base_url <- paste0(base_url, 'teams=', team)
    }
    else {
    base_url <- paste0(base_url, '&teams=', team)
    }
    count <- count + 1
  }
  parsed <- httr::modify_url(
    base_url,
    query = list(
      date = date,
      sims = sims,
      seed = seed
    )
  )

  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data(sprintf('Tournament Prediction: %i Sims', sims), Sys.time())
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
