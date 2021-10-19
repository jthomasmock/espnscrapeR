#' Scrape weekly NFL leaders stats from ESPN
#'
#' @param stats character - either receiving, passing, or rushing
#' @param season character or numeric - greater than 2002
#' @param week character or numeric - 1 to 17 for regular season
#' @import purrr tidyr dplyr stringr
#' @importFrom dplyr %>%
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @importFrom glue glue
#' @importFrom readr parse_number
#' @return tibble
#' @export
#'
#' @examples
#' scrape_weekly_leaders(season = 2002, stats = "passing", week = 1)
scrape_weekly_leaders <- function(season = 2019, week = 1, stats = "passing") {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  if (!stats %in% c("receiving", "rushing", "passing")) {
    stop("Please choose stats of 'receiving', 'rushing', or 'passing'!")
  }

  if (!dplyr::between(as.numeric(season), 2002, current_year)) {
    stop(paste("Please choose season between 2002 and", current_year))
  }

  if (!dplyr::between(as.numeric(week), 1, 18)) {
    stop("Please choose a week between 1 and 18")
  }

  message(
    glue::glue("Scraping {stats} stats for week {week} from {season} season!")
  )

  url <- glue::glue("http://www.espn.com/nfl/weekly/leaders/_/week/{week}/year/{season}/seasontype/2/type/{stats}")

  pass_n <- c(
    "rank", "name", "team", "result", "pass_comp", "pass_att",
    "pass_yds", "pass_td", "pass_int", "sack", "pass_fumbles", "pass_rating"
  )

  rush_n <- c(
    "rank", "name", "team", "result", "rush_att", "rush_yds",
    "rush_avg", "rush_td", "rush_long", "rush_fumbles", "drop"
  )


  rec_n <- c(
    "rank", "name", "team", "result", "rec", "rec_yds", "rec_avg",
    "rec_td", "rec_long", "rec_fumbles", "drop"
  )


  fix_names <- dplyr::case_when(
    stats == "passing" ~ list(pass_n),
    stats == "rushing" ~ list(rush_n),
    stats == "receiving" ~ list(rec_n)
  )[[1]]

  raw_df <- url %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//*[@id="my-players-table"]/div[1]/div[1]/table[2]') %>%
    rvest::html_table(fill = TRUE) %>%
    purrr::set_names(nm = fix_names) %>%
    slice(3:n()) %>%
    dplyr::as_tibble()

  clean_df <- raw_df %>%
    dplyr::mutate(
      position = stringr::str_extract(name, stringr::str_sub(name, -2)),
      name = stringr::str_remove(name, position),
      name = stringr::str_remove(name, ",")
    ) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      season = season,
      week = week
    ) %>%
    dplyr::select(season, week, rank, name, team:dplyr::contains("fumbles"))

  suppressMessages(readr::type_convert(clean_df))
}
