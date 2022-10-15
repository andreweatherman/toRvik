#' Get Estimated Injury Impact
#'
#' Returns estimated adjusted ratings in a one-player injury scenario by team.
#'
#' Estimations are experimental and based on
#' \href{https://twitter.com/totally_t_bomb/status/973731719479201792}{'highly
#' dubious analysis.'}
#'
#' @returns Returns a tibble with five columns:
#' \describe{
#'   \item{\code{situation}}{character.}
#'   \item{\code{adj_oe}}{double.}
#'   \item{\code{adj_de}}{double.}
#'   \item{\code{barthag}}{double.}
#'   \item{\code{rk}}{double.}
#'   }
#' @param year Defaults to current season (YYYY).
#' @param team Indicates team.
#' @param player Indicates player to remove.
#' @import dplyr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom rvest read_html html_table
#' @importFrom cli cli_abort
#' @importFrom purrr pluck
#' @importFrom magrittr %>%
#' @examples
#' \donttest{try(bart_injuryimpact(year=2019, team='Duke', player='Zion Williamson'))}
#'
#' @export
bart_injuryimpact <- function(year = current_season(), team = NULL, player = NULL) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (is.null(team) | is.null(player)) {
      cli::cli_abort("Please enter a team or player value!")
    }
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    }
    if(grepl('  ', team) | grepl('  ', player)){
      cli::cli_abort("Check spacing in name; use just one space to separate.")
    }
    else {
      player <- gsub(" ", "+", player)
      team <- gsub(" ", "+", team)
      x <- httr::GET(paste0("https://barttorvik.com/missing_player.php?team=", team, "&player=", player, "&year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table(header = FALSE) %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row = 1) %>%
        janitor::clean_names() %>%
        dplyr::rename("situation" = 1) %>%
        dplyr::mutate(across(c(2:5), as.numeric))
      return(x)
    }
  })
}
