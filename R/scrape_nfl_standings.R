#' Scrape NFL standings for a specific season from ESPN's site
#'
#' @param season Either numeric or character
#' @param add_superbowls Join superbowl winners (appropriate for historical data or after season over)
#'
#' @return Returns a tibble
#' @export
#' @import dplyr
#' @importFrom dplyr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_nodes html_text html_table html_attr
#' @importFrom glue glue
#' @examples
#' # Get standings from 2018 season
#' scrape_nfl_standings(season = "2018")
#'
#' # Get standings from 2010 season
#' scrape_nfl_standings(2000, add_superbowls = TRUE)
scrape_nfl_standings <- function(season, add_superbowls = FALSE) {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Small error handling to guide the limits on years
  if (!between(as.numeric(season), 1994, current_year)) {
    stop(paste("Please choose season between 1994 and", current_year))
  }

  message(glue::glue("Scraping {season}"))

  season_url <- glue::glue("https://www.espn.com/nfl/standings/_/season/{season}/group/conference")

  raw_season <- season_url %>%
    xml2::read_html()

  # Small error handling for failure
  if (length(html_table(raw_season)) == 0) {
    stop(paste("Table not found for", current_year))
  }

  team_names <- raw_season %>%
    rvest::html_node("div.tabs__content") %>%
    rvest::html_nodes("span.pr4.TeamLink__Logo") %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr("alt")

  team_abb <- raw_season %>%
    rvest::html_nodes("abbr") %>%
    rvest::html_text()

  raw_tables <- raw_season %>%
    rvest::html_nodes("div.Table__Scroller") %>%
    rvest::html_nodes("table") %>%
    rvest::html_table()

  nfc_table <- raw_tables[[1]] %>%
    dplyr::mutate(conf_name = "AFC")


  afc_table <- raw_tables[[2]] %>%
    dplyr::mutate(conf_name = "NFC")

  raw_standings <- dplyr::bind_rows(nfc_table, afc_table) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      conf_rank = dplyr::row_number(),
      playoffs = dplyr::if_else(conf_rank <= 6, "Made Playoffs", "Missed Playoffs"),
      playoff_rank = dplyr::case_when(
        conf_rank == 1 ~ "Division and Home Field",
        conf_rank == 2 ~ "Division and Bye",
        conf_rank == 3 ~ "Division",
        conf_rank == 4 ~ "Division",
        conf_rank == 5 ~ "Wild Card",
        conf_rank == 6 ~ "Wild Card",
        TRUE ~ "Missed Playoffs"
      )
    ) %>%
    dplyr::mutate(
      team_abb = team_abb,
      team = team_names,
      season = as.integer(season)
    ) %>%
    dplyr::mutate(team_logo = glue::glue(
      "https://a.espncdn.com/combiner/i?img=/i/teamlogos/nfl/500/scoreboard/{team_abb}.png&h=500&w=500"
    )) %>%
    dplyr::select(team, team_abb, season, conf_name, conf_rank, dplyr::everything())

  if (add_superbowls == TRUE) {
    message("Adding Superbowls!")

    sb_out <- scrape_superbowls()

    raw_standings %>%
      purrr::set_names(nm = tolower(names(raw_standings))) %>%
      dplyr::left_join(sb_out, by = c("team", "season"))
  } else {
    raw_standings %>%
      purrr::set_names(nm = tolower(names(raw_standings)))
  }
}
