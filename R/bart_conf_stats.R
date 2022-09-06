#' Get Conference Team Statistics
#'
#' Returns conference-only metrics, strengths of schedule, and bid probabilities
#' (auto and at-large) for all teams in a conference.
#'
#' Accepted conference abbreviations for the `conf` argument are: \itemize{\item ‘A10’, ‘ACC’,
#' ‘AE’, ‘ASun’, ‘Amer’, ‘B10’, ‘B12’, ‘BE’, ‘BSky’, ‘BSth’, ‘BW’, ‘CAA’,
#' ‘CUSA’, ‘Horz’, ‘Ivy’, ‘MAAC’, ‘MAC’, ‘MEAC’, ‘MVC’, ‘MWC’, ‘NEC’, ‘OVC’,
#' ‘P12’, ‘Pat’, ‘SB’, ‘SC’, ‘SEC’, ‘SWAC’, ‘Slnd’, ‘Sum’, ‘WAC’, ‘WCC’}
#'
#' @returns Returns a tibble with 23 columns:
#' \describe{
#'   \item{\code{rk}}{double.}
#'   \item{\code{team}}{character.}
#'   \item{\code{seed}}{double.}
#'   \item{\code{finish}}{character.}
#'   \item{\code{conf_rec}}{character.}
#'   \item{\code{adj_oe}}{double.}
#'   \item{\code{adj_de}}{double.}
#'   \item{\code{barthag}}{double. The estimation of a team's win probability
#'   against the average Division 1 team on a neutral court.}
#'   \item{\code{eff_marg}}{double.}
#'   \item{\code{con_oe}}{double.}
#'   \item{\code{con_oe_rk}}{double.}
#'   \item{\code{con_de}}{double.}
#'   \item{\code{con_de_rk}}{double.}
#'   \item{\code{conf_barthag}}{double.}
#'   \item{\code{proj_rec}}{character.}
#'   \item{\code{conf_cur_sos}}{double. The current average Barthag rating of
#'   conference opponents.}
#'   \item{\code{conf_cur_sos_rk}}{double.}
#'   \item{\code{conf_fut_sos}}{double. The projected average Barthag rating of
#'   conference opponents}
#'   \item{\code{conf_fut_sos_rk}}{double.}
#'   \item{\code{conf_sos}}{double.}
#'   \item{\code{conf_sos_rk}}{double.}
#'   \item{\code{auto_prob}}{double. Probability of winning the conference
#'   tournament (automatic bid).}
#'   \item{\code{bid_prob}}{double. Probability of securing a tournament bid
#'   (auto or at-large)}
#'}
#' @param year Defaults to current season (YYYY).
#' @param conf Indicates conference (see details).
#' @import dplyr
#' @import httr
#' @import janitor
#' @import readr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' bart_conf_stats(year=2021, conf='ACC')
#'
#' @export
bart_conf_stats <- function(year = current_season(), conf = NULL) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (is.null(conf) || !(conf %in% c('A10', 'ACC', 'AE', 'ASun', 'Amer', 'B10', 'B12', 'BE', 'BSky', 'BSth', 'BW',
                                      'CAA', 'CUSA', 'Horz', 'Ivy', 'MAAC', 'MAC', 'MEAC', 'MVC', 'MWC', 'NEC', 'OVC',
                                      'P12', 'Pat', 'SB', 'SC', 'SEC', 'SWAC', 'Slnd', 'Sum', 'WAC', 'WCC'))) {
      cli::cli_abort("Please enter valid conference code (see details)")
    }
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number. Data only goes back to 2008!")
    }
    if(year==current_season()) {
      x <- httr::GET(paste0("https://barttorvik.com/conf.php?conf=", conf, "&year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row_number = 1) %>%
        janitor::clean_names() %>%
        tidyr::separate(team, into = c("team", "more"), sep = "(?<=[a-zA-QS-Z.])\\s*(?=[0-9])") %>%
        tidyr::separate(more, into = c("seed", "finish"), sep = ",") %>%
        dplyr::mutate_at(4, funs(trimws(.))) %>%
        dplyr::select(-9) %>%
        dplyr::rename(
          "conf_rec" = 5,
          "eff_marg"=9,
          "con_oe_rk" = 11,
          "con_de_rk" = 13,
          "conf_barthag" = 14,
          "conf_cur_sos" = 16,
          "conf_cur_sos_rk" = 17,
          "conf_fut_sos" = 18,
          "conf_fut_sos_rk" = 19,
          "conf_sos" = 20,
          "conf_sos_rk" = 21,
          "auto_prob"=22,
          "bid_prob"=23
        ) %>%
        dplyr::mutate(
          year=year,
          seed = readr::parse_number(seed),
          auto_prob=readr::parse_number(auto_prob),
          bid_prob=readr::parse_number(bid_prob),
          across(c(1, 6:14, 16:23), as.numeric)
        )
      return(x)
    }
  else {
    x <- httr::GET(paste0("https://barttorvik.com/conf.php?conf=", conf, "&year=", year)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::row_to_names(row_number = 1) %>%
      janitor::clean_names() %>%
      tidyr::separate(team, into = c("team", "more"), sep = "(?<=[a-zA-QS-Z.])\\s*(?=[0-9])") %>%
      tidyr::separate(more, into = c("seed", "finish"), sep = ",") %>%
      dplyr::mutate_at(4, funs(trimws(.))) %>%
      dplyr::select(-9) %>%
      dplyr::rename(
        "conf_rec" = 5,
        "eff_marg"=9,
        "con_oe_rk" = 11,
        "con_de_rk" = 13,
        "conf_barthag" = 14,
        "conf_cur_sos" = 16,
        "conf_cur_sos_rk" = 17,
        "conf_fut_sos" = 18,
        "conf_fut_sos_rk" = 19,
        "conf_sos" = 20,
        "conf_sos_rk" = 21
      ) %>%
      dplyr::mutate(
        year=year,
        seed = readr::parse_number(seed),
        across(c(1, 6:14, 16:21), as.numeric)
      )
    return(x)
  }
  })
}
