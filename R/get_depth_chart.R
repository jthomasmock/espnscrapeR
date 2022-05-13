#' Get ESPN NFL depth chart by year and team
#'
#' @param season Either numeric or character
#' @param team team_id or team_abb, can be retrieved via get_nfl_teams()
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr httr
#' @importFrom dplyr %>%
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @examples
#' # Get depth chart for 2017 for Pittsburgh
#' get_depth_chart("2016", team = "PIT")
#'

get_depth_chart <- function(season = 2020, team = 23){

  team_ids <- c(
    "22", "1", "33", "2", "29", "3", "4", "5", "6",
    "7", "8", "9", "34", "11", "30", "12", "13", "24",
    "14", "15", "16", "17", "18", "19", "20", "21",
    "23", "25", "26", "27", "10", "28"
  )

  team_abb <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
    "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
    "LV", "LAC", "LAR", "MIA", "MIN", "NE", "NO", "NYG",
    "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WSH")

  stopifnot("Please use a valid numeric team id or a team abbreviation - these can be found with get_nfl_teams()" =
      any(team %in% team_ids, team %in% team_abb))

  if(team %in% team_abb){
    team <- team_ids[team_abb == team]
  }

  base_url <- "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/{season}/teams/{team}/depthcharts"

  raw_get  <- base_url %>%
    glue::glue() %>%
    httr::GET()

  httr::stop_for_status(raw_get)

  raw_json <- httr::content(raw_get)

  depth_chart <- raw_json$items %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unnest_longer(positions) %>%
    unnest_wider(positions) %>%
    unnest_longer(athletes) %>%
    unnest_wider(athletes) %>%
    hoist(athlete, athlete_id = "$ref") %>%
    mutate(
      athlete_id = str_remove(athlete_id, "http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/[:digit:]+/athletes/"),
      athlete_id = str_remove(athlete_id, "\\?lang=en&region=us")
    ) %>%
    # glimpse()
    rename(
      pos_grp_id = id, pos_grp = name, pos_slot = slot, pos_rank = rank,
      pos_id = positions_id
    ) %>%
    hoist(
      position,
      pos_id = "id",
      pos_name = "name",
      pos_abb = "abbreviation"
    ) %>%
    select(-position) %>%
    mutate(season = season,
           team_id = team) %>%
    select(season, team_id, athlete_id, contains("pos"))

  depth_chart

}
