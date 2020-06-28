#' Scrape ESPN QBR for NFL football
#'
#' Scrapes QBR tables for NFL QBs by specific week or at season level.
#'
#' This function supercedes `get_nfl_qbr` due to the ESPN API altering past data.
#'
#' @param season Either numeric or character. Must be between 2006 and current season.
#' @param week Either NA to return season, week 1 to 4 for playoffs, or 1 to 17 for regular season.
#' @param season_type Character - either "Regular" or "Playoffs"
#' @return Returns a tibble
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @examples
#' # Scrape ALL Playoff QBR from 2016 season
#' scrape_nfl_qbr("2016", season_type = "Playoffs", week = NA)
#'
#' # Scrape Regular season QBR for week 4 of 2019
#' scrape_nfl_qbr("2019", season_type = "Regular", week = 4)

scrape_nfl_qbr <- function(season = 2019, week = NA, season_type = "Regular") {

  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Error handling to correct season type
  if (!season_type %in% c("Regular", "Playoffs")) {
    stop("Please choose season_type of 'Regular' or 'Playoffs'")
  }

  # Error handling for limits on season
  if (!dplyr::between(as.numeric(season), 2006, current_year)) {
    stop(paste("Please choose season between 2006 and", current_year))
  }

  # Error handling for limits on regular season weeks
  if (!is.na(week) & season_type == "Regular" & !dplyr::between(as.numeric(week), 1, 17)) {
    stop("Please choose regular season week between 1 and 17")
  }

  # Error handling for limits on playoff weeks
  if (!is.na(week) & season_type == "Playoffs" & !dplyr::between(as.numeric(week), 1, 4)) {
    stop("Please choose Playoff week between 1 and 4")
  }

  # Error handling for missing data from ESPN
  if (!is.na(week) & season_type == "Playoffs" & as.numeric(season) == 2017) {
    stop("ESPN has missing Playoff data for 2017")
  }

  week_current <- dplyr::if_else(
    # Logic check to fix years where superbowl = 5
    season_type == "Playoffs" & as.numeric(week) == 4 & season >= 2009,
    # outcome = 5
    5,
    # default to normal week
    as.numeric(week)
  )

  # Add useful messages - separated by week
  # message(
  #   dplyr::if_else(
  #     is.na(week),
  #     glue::glue("Scraping QBR totals for {season}!"),
  #     glue::glue("Scraping weekly QBR for week {week} of {season}!")
  #     )
  # )

  # Build up URL
  url <- dplyr::case_when(
     season_type == "Playoffs" & is.na(week) ~ glue::glue("https://www.espn.com/nfl/qbr/_/season/{season}/seasontype/3"),
     season_type == "Playoffs" & !is.na(week) ~ glue::glue("https://www.espn.com/nfl/qbr/_/view/weekly/season/{season}/seasontype/3/week/{week_current}"),
     season_type == "Regular" & is.na(week) ~ glue::glue("https://www.espn.com/nfl/qbr/_/season/{season}/seasontype/2"),
     season_type == "Regular" & !is.na(week) ~ glue::glue("https://www.espn.com/nfl/qbr/_/view/weekly/season/{season}/seasontype/2/week/{week}"),
     TRUE ~ NA_character_
  )

  raw_tables <- url %>%
    xml2::read_html() %>%
    rvest::html_table()

  comb_df <- raw_tables[[1]]

  # comb_df <- cbind(raw_tables[[1]], raw_tables[[2]])

  comb_df %>%
    janitor::clean_names() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(season = season) %>%
    dplyr::mutate(
      team = dplyr::if_else(
        stringr::str_detect(name, "/"),
        stringr::str_sub(name, 7),
        stringr::str_sub(name, -3)
        )
      ) %>%
    dplyr::select(name, team) %>%
    dplyr::mutate(team = stringr::str_remove(team, "[:lower:]+"),
           name = stringr::str_remove(name, team))
}


