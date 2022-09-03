#' T-Rank Tournament Prediction
#'
#' Returns single-elimination tournament predictions using Barttorvik's
#' model
#'
#' Given a list of teams arranged in scheduled order, function returns
#' tournament predictions for single-elimination tournaments
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
#' @examples
#' \donttest{bart_tournament_predicition(teams=c('Duke', 'North Carolina',
#' 'Kansas', 'Villanova'), date='20220402', sims=10, seed=1)}
#'
#' @export

bart_tournament_predicition <- function(teams = NULL, date = NULL, sims = NULL, seed = NULL) {
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
  data <- jsonlite::fromJSON(parsed) %>% dplyr::as_tibble()
  return(data)
}
