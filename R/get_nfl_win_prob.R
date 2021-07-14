#' Get NFL in-game win probabilities for a specific game from ESPN
#'
#' @param game_id Character string - can be acquired from the website of a specific game, or from espnscrapeR::get_nfl_schedule()
#'
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom httr GET content stop_for_status
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @importFrom tibble enframe
#' @examples
#' # Get win prob from specific game
#' get_espn_win_prob(game_id = "401030956")


get_espn_win_prob <- function(game_id){

  raw_url <- glue::glue("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/{game_id}/competitions/{game_id}/probabilities?limit=1000")

  extract_play_text <- function(text_in) {
    text_in %>%
      str_remove("\\?lang=en&region=us") %>%
      str_remove("http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/[:digit:]+/competitions/[:digit:]+/probabilities/")

  }

  extract_team_text <- function(text_in){
    text_in %>%
      str_remove("http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/[:digit:]+/teams/") %>%
      str_remove("\\?lang=en&region=us")
  }

  raw_get <- httr::GET(raw_url)

  httr::stop_for_status(raw_get)

  raw_json <- httr::content(raw_get)

  raw_df <- raw_json[["items"]] %>%
    enframe() %>%
    unnest_wider(value) %>%
    rename(row_id = name, play_url_ref = `$ref`) %>%
    mutate(play_id = extract_play_text(play_url_ref)) %>%
    hoist(homeTeam, home_team_id = "$ref") %>%
    hoist(awayTeam, away_team_id = "$ref") %>%
    select(-play_url_ref, -where(is.list), -any_of(c("lastModified", "secondsLeft"))) %>%
    mutate(
      home_team_id = extract_team_text(home_team_id),
      away_team_id = extract_team_text(away_team_id)
    ) %>%
    janitor::clean_names() %>%
    mutate(game_id = game_id)

  raw_df

}
