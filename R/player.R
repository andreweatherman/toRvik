#' Current Season
#'
#' Returns current season in YYYY format; used as default season in package
#' functions.
#'
#' @export
current_season <- function() {
  dplyr::if_else(as.double(substr(Sys.Date(), 6, 7)) >= 10,
    as.double(substr(Sys.Date(), 1, 4)) + 1, as.double(substr(
      Sys.Date(),
      1, 4
    ))
  )
}

#' Get Player Season Stats
#'
#' Returns detailed, season-long player statistics on a variety of splits.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats; sorts by ppg.}
#' \item{shooting}{Returns play-by-play shooting splits; sorts by ppg.}
#' \item{adv}{Returns advanced metrics and possession-adjusted box score
#' statistics; sorts by recruiting rank.}}
#'
#' @param year Defaults to current season (YYYY).
#' @param stat Indicates statistical split (see details).
#' @param conf_only Logical. Filters data by conference-only play; defaults to
#'   `FALSE`.
#' @import dplyr
#' @import readr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{bart_player_season(year=2019, stat='adv', conf_only=TRUE)}
#'
#' @export
bart_player_season <- function(year = current_season(), stat = NULL, conf_only = F) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008")
    }
    if (is.null(stat) || !(stat %in% c("box", "shooting", "adv"))) {
      cli::cli_abort("Please input a valid stat command ('box', 'shooting', or 'adv')")
    }
    c_only <- as.integer(conf_only)
    if (stat == "box") {
      names <- c(
        "player", "pos", "exp", "hgt", "team", "conf", "g", "mpg", "ppg", "oreb",
        "dreb", "rpg", "apg", "ast_to", "spg", "bpg", "num", "year", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=", year, "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE)
      y <- x %>% dplyr::mutate(across(c(17, 18, 20, 21, 43, 44), as.numeric))
      y <- y %>%
        dplyr::group_by(X33) %>%
        dplyr::summarize(
          fga = sum(X18, X21, X44, na.rm = TRUE),
          fgm = sum(X17, X20, X43, na.rm = TRUE),
          fg_pct = fgm / fga
        ) %>%
        dplyr::rename("id" = 1)

      x <- x %>% dplyr::select(1, 65, 26, 27, 2:4, 55, 64, 58:61, 36, 62, 63, 28, 32, 33)
      colnames(x) <- names
      x <- dplyr::left_join(x, (y %>% dplyr::select(1, 4)), by = "id") %>%
        dplyr::relocate(fg_pct, .before = oreb) %>%
        dplyr::arrange(desc(ppg))
      return(x)
    }

    if (stat == "shooting") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "mpg", "ppg", "usg", "ortg", "efg", "ts",
        "ftm", "fta", "ft_pct", "two_m", "two_a", "two_pct", "three_m", "three_a",
        "three_pct", "dunk_m", "dunk_a", "dunk_pct", "rim_m", "rim_a", "rim_pct",
        "mid_m", "mid_a", "mid_pct", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=", year, "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(
          1, 65, 26, 2:4, 55, 64, 7, 6, 8, 9, 14:22, 43:45, 37, 38, 41, 39, 40,
          42, 33
        )
      colnames(x) <- names
      x <- x %>%
        dplyr::mutate(p_per = ((40 * ppg) / mpg), .after = ppg) %>%
        dplyr::arrange(desc(ppg))
      return(x)
    }
    if (stat == "adv") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "min", "porpag", "dporpag", "ortg", "adj_oe", "drtg", "adj_de",
        "stops", "obpm", "dbpm", "bpm", "oreb", "dreb", "ast", "to", "blk", "stl", "ftr", "pfr",
        "rec", "pick", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=", year, "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 65, 26, 2:5, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 46, 33)
      colnames(x) <- names
      x <- x %>% dplyr::arrange(desc(rec))
      return(x)
    }
  })
}


#' Get Player Game Stats
#'
#' Returns detailed game-by-game player statistics on a variety of splits.
#'
#' #' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats; sorts by ppg.}
#' \item{shooting}{Returns play-by-play shooting splits; sorts by ppg.}
#' \item{adv}{Returns advanced metrics and possession-adjusted box score
#' statistics; sorts by recruiting rank.}}
#'
#' @param year Defaults to current season (YYYY).
#' @param stat Indicates statistical split (see details).
#' @import dplyr
#' @import jsonlite
#' @import lubridate
#' @importFrom curl curl_download
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{bart_player_game(year=2022, stat='box')}
#'
#' @export
bart_player_game <- function(year = current_season(), stat = NULL) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number. Data only goes back to 2008!")
    }
    if (!(is.character(stat) && stat %in% c("box", "shooting", "adv"))) {
      cli::cli_abort("Please input a valid stat command ('box,' 'shooting', or 'adv')")
    }
    curl::curl_download(paste0('https://barttorvik.com/', year, '_all_advgames.json.gz'), 'games.json')
    if (stat == "box") {
      names <- c(
        "date", "player", "exp", "team", "opp", "result", "min", "pts", "two_m", "two_a", "three_m",
        "three_a", "ftm", "fta", "oreb", "dreb", "ast", "tov", "stl", "blk", "pf", "id", "game_id"
      )
      x <- jsonlite::fromJSON('games.json') %>%
        dplyr::as_tibble() %>%
        select(1, 49, 51, 48, 6, 5, 9, 34, 24:29, 35, 36, 37, 38, 39, 40, 43, 52, 7)
      colnames(x) <- names
      x <- x %>%
        dplyr::mutate(
          date = lubridate::ymd(date),
          across(c(7:16), as.numeric),
          reb = oreb + dreb, .after = dreb,
          result = case_when(
            result == "1" ~ "W",
            TRUE ~ "L"
          ),
          year = year
        ) %>%
        dplyr::mutate(
          fgm = two_m + three_m,
          fga = two_a + three_a,
          .after = three_a
        ) %>%
        dplyr::relocate(year, .after = date)
    }
    if (stat == "shooting") {
      names <- c(
        "date", "player", "exp", "team", "opp", "result", "min", "pts", "usg", "efg", "ts", "dunk_m", "dunk_a",
        "rim_m", "rim_a", "mid_m", "mid_a", "two_m", "two_a", "three_m", "three_a", "ftm", "fta", "id", "game_id"
      )
      x <- jsonlite::fromJSON('games.json') %>%
        dplyr::as_tibble() %>%
        select(1, 49, 51, 48, 6, 5, 9, 34, 11:13, 18:29, 52, 7)
      colnames(x) <- names
      x <- x %>%
        dplyr::mutate(
          date = lubridate::ymd(date),
          across(c(7:24), as.numeric),
          fg = (two_m + three_m) / (two_a + three_a) * 100, .before = efg,
          result = case_when(
            result == "1" ~ "W",
            TRUE ~ "L"
          ),
          year = year
        ) %>%
        dplyr::relocate(year, .after = date)
    }
    if (stat == "adv") {
      names <- c(
        "date", "player", "exp", "team", "opp", "result", "min", "pts", "usg", "ortg", "or_pct", "dr_pct",
        "ast_pct", "to_pct", "stl_pct", "blk_pct", "bpm", "obpm", "dbpm", "net", "poss", "id", "game_id"
      )
      x <- jsonlite::fromJSON('games.json') %>%
        dplyr::as_tibble() %>%
        select(1, 49, 51, 48, 6, 5, 9, 34, 11, 10, 14:17, 41:42, 30:33, 44, 52, 7)
      colnames(x) <- names
      x <- x %>% dplyr::mutate(
        date = lubridate::ymd(date),
        across(c(7:22), as.numeric),
        result = case_when(
          result == "1" ~ "W",
          TRUE ~ "L"
        ),
        year = year, .after = date
      )
    }
    unlink('games.json')
    return(x)
  })
}

#' Get Transfer Portal Statistics
#'
#' Returns detailed, season-long player statistics on a variety of splits for
#' all portal players for 2022-23. Transfer information is sourced from
#' \href{https://verbalcommits.com/transfers/2022}{Verbal Commits} and is
#' updated every 20 minutes. Commit information is updated roughly every 18
#' hours.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats.} \item{shooting}{Returns
#' play-by-play shooting splits.} \item{adv}{Returns advanced metrics and
#' possession-adjusted box score statistics.}}
#'
#' @param stat Indicates statistical split (see details).
#' @param conf_only Logical. Filters data by conference-only play; defaults to
#'   `FALSE`.
#' @param active Logical. Filters players by portal status -- active vs.
#'   committed; defaults to TRUE (active).
#' @import dplyr
#' @import jsonlite
#' @import readr
#' @import httr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom stringr str_match
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{bart_transfers(stat='box')}
#'
#' @export
bart_transfers <- function(stat = NULL, conf_only = F, active=T) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (is.null(stat) || !(stat %in% c("box", "shooting", "adv"))) {
      cli::cli_abort("Please input a valid stat command ('box', 'shooting', or 'adv')")
    }
    c_only <- as.integer(conf_only)
    txt <- httr::GET("https://barttorvik.com/playerstat.php?link=y&xvalue=trans&year=2022")
    txt <- httr::content(txt)
    t <- stringr::str_match(txt, "var transfers\\s*(.*?)\\s*var trandict")[1, 2]
    t <- gsub("=", "", t)
    t <- gsub(";", "", t)
    all <- jsonlite::fromJSON(t) %>%
      dplyr::as_tibble()
    portal <- all %>%
              dplyr::filter(V3 == "") %>%
              dplyr::select(1, 2) %>%
              dplyr::rename("player" = 1,
                            "team"=2)
    commit <- all %>%
              dplyr::filter(V3 != "") %>%
              dplyr::select(1, 2, 3) %>%
              dplyr::rename("player" = 1,
                            "team"=2,
                            "new_school"=3)
    if (stat == "box") {
      names <- c(
        "player", "pos", "exp", "hgt", "team", "conf", "g", "mpg", "ppg", "oreb",
        "dreb", "rpg", "apg", "ast_to", "spg", "bpg", "num", "year", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE)
      y <- x %>% dplyr::mutate(across(c(17, 18, 20, 21, 43, 44), as.numeric))
      y <- y %>%
        dplyr::group_by(X33) %>%
        dplyr::summarize(
          fga = sum(X18, X21, X44, na.rm = TRUE),
          fgm = sum(X17, X20, X43, na.rm = TRUE),
          fg_pct = fgm / fga
        ) %>%
        dplyr::rename("id" = 1)
      x <- x %>% dplyr::select(1, 65, 26, 27, 2:4, 55, 64, 58:61, 36, 62, 63, 28, 32, 33)
      colnames(x) <- names
      x <- dplyr::left_join(x, (y %>% dplyr::select(1, 4)), by = "id") %>%
        dplyr::relocate(fg_pct, .before = oreb) %>%
        dplyr::arrange(desc(ppg))
          if(active==T) {
              x <- merge(x, portal, by = c("player", "team")) %>%
                dplyr::as_tibble() }
          if(active==F) {
              x <- merge(x, commit, by = c("player", "team")) %>%
                  dplyr::as_tibble() }
      }
      if (stat == "shooting") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "mpg", "ppg", "usg", "ortg", "efg", "ts",
        "ftm", "fta", "ft_pct", "two_m", "two_a", "two_pct", "three_m", "three_a",
        "three_pct", "dunk_m", "dunk_a", "dunk_pct", "rim_m", "rim_a", "rim_pct",
        "mid_m", "mid_a", "mid_pct", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022", "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(
          1, 65, 26, 2:4, 55, 64, 7, 6, 8, 9, 14:22, 43:45, 37, 38, 41, 39, 40,
          42, 33
        )
      colnames(x) <- names
      x <- x %>%
        dplyr::mutate(p_per = ((40 * ppg) / mpg), .after = ppg) %>%
        arrange(desc(ppg))
            if(active==T) {
                x <- merge(x, portal, by = c("player", "team")) %>%
                     dplyr::as_tibble() }
            if(active==F) {
                x <- merge(x, commit, by = c("player", "team")) %>%
                     dplyr::as_tibble() }
      }
    if (stat == "adv") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "min", "porpag", "dporpag", "ortg", "adj_oe", "drtg", "adj_de",
        "stops", "obpm", "dbpm", "bpm", "oreb", "dreb", "ast", "to", "blk", "stl", "ftr", "pfr",
        "rec", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022", "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 65, 26, 2:5, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 33)
      colnames(x) <- names
      x <- x %>% arrange(desc(rec))
          if(active==T) {
              x <- merge(x, portal, by = c("player", "team")) %>%
                   dplyr::as_tibble() }
          if(active==F) {
             x <- merge(x, commit, by = c("player", "team")) %>%
                  dplyr::as_tibble() }
    }
    return(x)
}
  )
}

#' Get Player of the Year Ratings
#'
#' Returns Barttorvik Player of the Year ratings on a variety of splits.
#'
#' Accepted conference abbreviations for the `conf` argument are: \itemize{\item ‘A10’, ‘ACC’,
#' ‘AE’, ‘ASun’, ‘Amer’, ‘B10’, ‘B12’, ‘BE’, ‘BSky’, ‘BSth’, ‘BW’, ‘CAA’,
#' ‘CUSA’, ‘Horz’, ‘Ivy’, ‘MAAC’, ‘MAC’, ‘MEAC’, ‘MVC’, ‘MWC’, ‘NEC’, ‘OVC’,
#' ‘P12’, ‘Pat’, ‘SB’, ‘SC’, ‘SEC’, ‘SWAC’, ‘Slnd’, ‘Sum’, ‘WAC’, ‘WCC’ }
#'
#' @param year Defaults to current season (YYYY).
#' @param conf Filters results by conference; defaults to all (see details).
#' @param class Filters results by class ('fr', 'so', 'jr', 'sr'); defaults to
#'   no filter.
#' @param conf_only Logical. Filters data by conference-only play; defaults to
#'   `FALSE`.
#' @import dplyr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{bart_poy(year=2019, class='fr')}
#'
#' @export
bart_poy <- function(year = current_season(), conf = "All", class = NULL, conf_only = F) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
  if (!(is.numeric(year) && nchar(year) == 4 && year >=
    2008)) {
    cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
  }
  if(!is.null(class) && !(class %in% c('fr', 'so', 'jr', 'sr'))) {
    cli::cli_abort("Please input correct class value (see details)")
  }
  if (!(conf %in% c('All', 'A10', 'ACC', 'AE', 'ASun', 'Amer', 'B10', 'B12', 'BE', 'BSky', 'BSth', 'BW',
                                     'CAA', 'CUSA', 'Horz', 'Ivy', 'MAAC', 'MAC', 'MEAC', 'MVC', 'MWC', 'NEC', 'OVC',
                                     'P12', 'Pat', 'SB', 'SC', 'SEC', 'SWAC', 'Slnd', 'Sum', 'WAC', 'WCC'))) {
    cli::cli_abort("Please enter valid conference code (see details)")
  }
  class_lookup <- list(
    "fr" = "Fr",
    "so" = "So",
    "jr" = "Jr",
    "sr" = "Sr"
  )
  class <- class_lookup[class]
  if (conf_only == F) {
    x <- httr::GET(paste0("https://barttorvik.com/poy.php?conlimit=", conf, "&year=", year, "&yr=", class)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::clean_names()
    return(x)
  }
  if (conf_only == T) {
    x <- httr::GET(paste0("https://barttorvik.com/conpoy.php?conlimit=", conf, "&year=", year, "&yr=", class)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::clean_names()
    return(x)
  }
})}

#' Get Estimated Injury Impact
#'
#' Returns estimated adjusted ratings in a one-player injury scenario by team.
#'
#' Estimations are experimental and based on
#' \href{https://twitter.com/totally_t_bomb/status/973731719479201792}{'highly
#' dubious analysis.'}
#'
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
#' \dontrun{bart_injuryimpact(year=2019, team='Duke', player='Zion Williamson')}
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
