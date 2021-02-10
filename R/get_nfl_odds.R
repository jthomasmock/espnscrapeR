#' Get NFL odds for a specific game from ESPN's API
#'
#' @param game_id character
#'
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @examples
#' # Get odds from a specific game
#' get_nfl_odds(game_id = "400791550")

get_nfl_odds <- function(game_id) {

  game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={game_id}&enable=ranks,odds,linescores")

  raw_json <- fromJSON(game_url, simplifyVector = FALSE)

  team_df <- raw_json$boxscore$teams %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unnest_wider(team) %>%
    select(name:displayName, logo) %>%
    mutate(location = c("away", "home")) %>%
    rename(
      team_name = name, team_full = displayName,
      team_abb = abbreviation, team_logo = logo
      )

  odds_df <- raw_json$pickcenter %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    rename(over_under = overUnder, spread_team = details) %>%
    hoist(
      awayTeamOdds,
      away_ml = "moneyLine",
      away_odds = "spreadOdds"
    ) %>%
    hoist(
      homeTeamOdds,
      home_ml = "moneyLine",
      home_odds = "spreadOdds"
    ) %>%
    hoist(
      provider,
      odds_source = "name",
      odds_source_id = "id"
    ) %>%
    select(!where(is.list)) %>%
    pivot_longer(
      cols = c(away_ml, away_odds, home_ml, home_odds),
      names_to = "stat", values_to = "odds"
      ) %>%
    separate(stat, into = c("location", "stat"), sep = "_") %>%
    pivot_wider(names_from = stat, values_from = odds) %>%
    left_join(
      tibble(
        location = c("home", "away"),
        win_proj_fpi = c(
          as.double(raw_json$predictor$homeTeam$gameProjection),
          as.double(away_win_pct_fpi = raw_json$predictor$awayTeam$gameProjection)
        )
      ),
      by = "location"
    ) %>%
    left_join(team_df, by = "location") %>%
    mutate(game_id = game_id, win_proj_fpi = as.double(win_proj_fpi))

  odds_df
}
