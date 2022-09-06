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
