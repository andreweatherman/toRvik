#' Get Coaching History and Notable Recruits
#'
#' Returns coaching history with four factor and tournament finish (tibble one)
#' along with notable recruits (tibble two).
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}. `rec_rank` is a
#' scale of recruiting ranking, where 100 is a consensus #1 recruit and 0 is
#' unrecruited.
#'
#' @returns Returns a list of tibbles: 'Coach History' and 'Recruiting History'
#' @param coach Coach to return.
#' @import dplyr
#' @import httr
#' @import janitor
#' @import readr
#' @importFrom withr local_options
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' bart_coach('Mike Krzyzewski')
#'
#' @export
bart_coach <- function(coach) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if(grepl('  ', coach)){
      cli::cli_abort("Check spacing in coach's name; use just one space to separate first and last.")
    }
    coach <- gsub(" ", "+", coach)
    y_names <- c("year", "player", "team", "rec_rank")
    tabs <- httr::GET(paste0("https://barttorvik.com/coach-history.php?coach=", coach)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table()
    x <- tabs %>%
      purrr::pluck(1) %>%
      janitor::clean_names() %>%
      tidyr::separate(team, into = c("team", "more"), sep = "(?<=[a-zA-QS-Z.])\\s*(?=[0-9])") %>%
      tidyr::separate(more, into = c("seed", "finish"), sep = ",") %>%
      tidyr::separate(rec, into = c("ov_rec", "conf_rec"), sep = "[\\(\\)]") %>%
      dplyr::mutate_all(funs(stringr::str_replace(., "No Tourney - COVID-19", ""))) %>%
      dplyr::mutate_at(c(3, 5, 7), funs(trimws(.))) %>%
      dplyr::mutate(
        seed = readr::parse_number(seed),
        across(c(1, 2, 4, 9:22), as.numeric)
      ) %>%
      dplyr::rename(
        "two_pct" = 20,
        "two_pct_d" = 21,
        "three_pct" = 22,
        "three_pct_d" = 23
      )
    y <- tabs %>%
      purrr::pluck(2)
    colnames(y) <- y_names
    z <- c(list(x), list(y))
    return(z)
  })
}
