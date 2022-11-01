#' Get WR metrics from FiveThirtyEight
#'
#'
#' @return tibble
#' @export
#'
#'
get_538_wr_metrics <- function(){

  in_url <- glue::glue("https://projects.fivethirtyeight.com/nfl-receiver-rankings/data.json")

  raw_json <- jsonlite::fromJSON(in_url, simplifyVector = FALSE)

  listviewer::jsonedit(raw_json)

  dplyr::tibble(data = raw_json[["rtm_data"]]) |>
    tidyr::unnest_wider(data)

}
