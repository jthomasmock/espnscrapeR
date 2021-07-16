#' Get ESPN athlete info for NFL players
#'
#' @param athlete_id The player's unique athlete id
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr httr
#' @importFrom dplyr %>%
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @examples
#' # Get ALL Playoff QBR from 2016 season
#' get_athlete("2580")
#'
get_athlete <- function(athlete_id){

  season <- Sys.Date() %>% substr(1, 4)

  base_url <- "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/{season}/athletes/{athlete_id}"

  raw_get  <- base_url %>%
    glue::glue() %>%
    httr::GET()

  httr::stop_for_status(raw_get)

  raw_json <- content(raw_get)

  athlete_df <- raw_json %>%
    tibble::enframe() %>%
    dplyr::filter(name != "$ref") %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    janitor::clean_names() %>%
    tidyr::hoist(position, pos = "abbreviation") %>%
    tidyr::hoist(headshot, headshot_url = "href") %>%
    tidyr::hoist(
      draft,
      draft_txt = "displayText",
      draft_year = "year",
      draft_round = "round",
      draft_slot = "selection",
      draft_team_id = list("team", "$ref")
    ) %>%
    tidyr::hoist(experience, nfl_exp = "years") %>%
    tidyr::hoist(team, team_id = list(1, "$ref")) %>%
    dplyr::select(
      player_id = id,
      player_guid = guid,
      team_id,
      pos,
      player_first_name = first_name,
      player_last_name = last_name,
      player_full_name = full_name,
      player_short_name = short_name,
      weight,
      height,
      age,
      dob = date_of_birth,
      debut_year,
      headshot_url,
      jersey,
      nfl_exp,
      dplyr::contains("draft"),
      -draft
    ) %>%
    dplyr::mutate(
      draft_team_id = stringr::str_remove(draft_team_id, "http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/[:digit:]+/teams/"),
      draft_team_id = stringr::str_remove(draft_team_id, "\\?lang=en&region=us"),
      team_id = stringr::str_remove(team_id, "http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/[:digit:]+/teams/"),
      team_id = stringr::str_remove(team_id, "\\?lang=en&region=us")
    ) %>%
    tidyr::unchop(tidyselect:::where(is.list))

  athlete_df

}
