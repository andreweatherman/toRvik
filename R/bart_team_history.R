#' Get Team History
#'
#' Returns efficiency and four factor finish, conference and overall records,
#' and basic shooting percentages by year back to 2008.
#'
#' `team` input must be an exact match: 'State' is abbreviated to 'St.' and full
#' names are used where applicable (e.g. 'North Carolina'). For complete list,
#' see `team` column of \code{\link{bart_ratings}}.
#'
#' @returns Returns a tibble with 23 columns:
#' \describe{
#'   \item{\code{year}}{double.}
#'   \item{\code{t_rk}}{double.}
#'   \item{\code{coach}}{character.}
#'   \item{\code{seed}}{double.}
#'   \item{\code{finish}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{ov_rec}}{character.}
#'   \item{\code{conf_rec}}{character.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{efg_o}}{double.}
#'   \item{\code{efg_d}}{double.}
#'   \item{\code{to_percent}}{double.}
#'   \item{\code{tod_percent}}{double.}
#'   \item{\code{or_percent}}{double.}
#'   \item{\code{dr_percent}}{double.}
#'   \item{\code{ftr}}{double.}
#'   \item{\code{ftrd}}{double.}
#'   \item{\code{ft_percent}}{double.}
#'   \item{\code{two_pct}}{double.}
#'   \item{\code{two_pct_d}}{double.}
#'   \item{\code{three_pct}}{double.}
#'   \item{\code{adj_t}}{double.}
#' }
#' @param team Indicates team to return.
#' @import dplyr
#' @import httr
#' @import janitor
#' @import  readr
#' @importFrom withr local_options
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' \donttest{try(bart_team_history(team='Charlotte'))}
#' @export
bart_team_history <- function(team) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if(grepl('  ', team)){
      cli::cli_abort("Check spacing in team name; use just one space to separate.")
    }
    team <- gsub(" ", "+", team)
    x <- httr::GET(paste0("https://barttorvik.com/team-history.php?team=", team)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::clean_names() %>%
      tidyr::separate(coach, into = c("coach", "more"), sep = "(?<=[a-z.])\\s*(?=[0-9])") %>%
      tidyr::separate(more, into = c("seed", "finish"), sep = ",") %>%
      tidyr::separate(rec, into = c("ov_rec", "conf_rec"), sep = "[\\(\\)]") %>%
      dplyr::mutate_at(c(5, 7), funs(trimws(.))) %>%
      dplyr::mutate(
        seed = readr::parse_number(seed),
        across(c(1, 2, 4, 9:22), as.numeric)
      ) %>%
      dplyr::rename(
        "two_pct" = 20,
        "two_pct_d" = 21,
        "three_pct" = 22
      )
    return(x)
  })
}
