#' Get ESPN's WR metrics from FiveThirtyEight
#'
#'
#' @return tibble
#'
#' @examples
#' raw_metrics <- espnscrapeR::get_espn_wr_metrics()
#'
#' dplyr::glimpse(raw_metrics)
#' @export

get_espn_wr_metrics <- function(){

  in_url <- "https://projects.fivethirtyeight.com/nfl-receiver-rankings/data.json"

  raw_json <- jsonlite::fromJSON(in_url, simplifyVector = FALSE)

  dplyr::tibble(data = raw_json[["rtm_data"]]) |>
    tidyr::unnest_wider(data)

}
