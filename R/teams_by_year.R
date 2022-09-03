#' Teams and Conferences
#'
#' Returns teams and their conference affiliation by year
#'
#' Function pulls teams and their men's basketball conference affiliation
#' by year back to 2008
#'
#' @returns Returns a tibble with three columns:
#' \describe{
#'   \item{\code{team}}{character.} \item{\code{year}}{integer.}
#'   \item{\code{conf}}{character.}}
#'
#' @param year Year to filter
#' @param conf Conference to filter
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @examples
#' teams_by_year()
#'
#' @export

teams_by_year <- function(year = NULL, conf = NULL) {
  base_url <- 'https://api.cbbstat.com/teams'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      conf = conf
    )
  )
  data <- jsonlite::fromJSON(parsed) %>% dplyr::as_tibble()
  return(data)
}
