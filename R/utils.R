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

my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")

# custom operator for use in filter with NULL variables
`%==%` <- function (e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return(e1 == e2)
  }
}

# Helper function to match GitHub url path
gh_data_path <- function(stat = ...) {

  switch(stat,
         'pg_box' = 'player_game/box.rds',
         'pg_shooting' = 'player_game/shooting.rds',
         'pg_adv' = 'player_game/advanced.rds',
         'pg_all' = 'player_game/all.rds')

}

# Function to load full data from GitHub
# This function is adapted from hoopR
load_gh_data <- function(stat = NULL, dbConnection = NULL, tablename = NULL, ...) {
  if (!is.null(dbConnection) && !is.null(tablename))
    in_db <- TRUE
  else in_db <- FALSE
  url <- paste0('https://github.com/andreweatherman/toRvik-data/raw/main/', gh_data_path(stat))
  out <- rds_from_url(url) %>% make_toRvik_data('Player Game Stats', Sys.time())
  if (in_db) {
    DBI::dbWriteTable(dbConnection, tablename, out, append = TRUE)
    out <- NULL
  }
  else {
    class(out) <- c("toRvik_data", "tbl_df", "tbl", "data.table", "data.frame")
  }
  out
}


# helper function to download .rds from url
rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    warning('Failed to load data', call. = FALSE)
    return(data.table::data.table())
  }

  data.table::setDT(load)
  return(load)
}


# Functions for custom class
# turn a data.frame into a tibble/toRvik_data
make_toRvik_data <- function(df,type,timestamp){
  out <- df %>%
    tidyr::as_tibble()

  class(out) <- c("toRvik_data","tbl_df","tbl","data.table","data.frame")
  attr(out,"toRvik_timestamp") <- timestamp
  attr(out,"toRvik_type") <- type
  return(out)
}

# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots
message_completed <- function(x, in_builder = FALSE) {
  if (isFALSE(in_builder)) {
    str <- paste0(my_time(), " | ", x)
    cli::cli_alert_success("{{.field {str}}}")
  } else if (in_builder) {
    cli::cli_alert_success("{my_time()} | {x}")
  }
}

check_docs_error <- function() {
  cli::cli_alert_warning('Something went wrong. Check documentation.')
}

# helper function to define year errors
year_out_of_bounds <- function(year) {
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
  cli::cli_abort(c(
    "{.var year} must be 2008 or later",
    "x" = "You passed through {year}"
  ))
  }
}


rule_header <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      right = paste0("toRvik version ", utils::packageVersion("toRvik")),
      width = getOption("width")
    )
  )
}

rule_footer <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      width = getOption("width")
    )
  )
}

#' @export
#' @noRd
print.toRvik_data <- function(x,...) {
  cli::cli_rule(left = "{attr(x,'toRvik_type')}",right = "{.emph toRvik {utils::packageVersion('toRvik')}}")

  if(!is.null(attr(x,'toRvik_timestamp'))) {
    cli::cli_alert_info(
      "Data updated: {.field {format(attr(x,'toRvik_timestamp'), tz = Sys.timezone(), usetz = TRUE)}}"
    )
  }

  NextMethod(print,x)
  invisible(x)
}

# rbindlist but maintain attributes of last file, taken from nflreadr
rbindlist_with_attrs <- function(dflist){

  toRvik_timestamp <- attr(dflist[[length(dflist)]], "toRvik_timestamp")
  toRvik_type <- attr(dflist[[length(dflist)]], "toRvik_type")
  out <- data.table::rbindlist(dflist, use.names = TRUE, fill = TRUE)
  attr(out,"toRvik_timestamp") <- toRvik_timestamp
  attr(out,"toRvik_type") <- toRvik_type
  out

}
