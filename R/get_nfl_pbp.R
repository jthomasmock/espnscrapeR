#' Get NFL play-by-play for a specific game from ESPN's API
#'
#' @param game_id character
#'
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom httr GET content stop_for_status
#' @importFrom glue glue
#' @examples
#' # Get NFL play-by-play for a specific game
#' get_nfl_pbp(game_id = "300912027")


get_nfl_pbp <- function(game_id){

  game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary")

  raw_get <- httr::GET(game_url, query = list(event = game_id, enable = "ranks,odds,linescores,logos"))

  httr::stop_for_status(raw_get)

  raw_json <- httr::content(raw_get)

  nfl_pbp <- raw_json[["drives"]][["previous"]] %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    rename(drive_id = id) %>%
    unnest_wider(team) %>%
    select(-shortDisplayName) %>%
    hoist(logos, logo = list(1, "href")) %>%
    rename(
      pos_team_name = name, pos_team_abb = abbreviation,
      pos_team_full = displayName, drive_desc = description
    ) %>%
    unnest_wider(start) %>%
    rename(drive_start_yardline = yardLine, drive_start_text = text) %>%
    hoist(period,
          drive_start_qtr = "number"
    ) %>%
    select(-period, -logos) %>%
    hoist(clock,
          drive_start_clock = "displayValue"
    ) %>%
    unnest_wider(end) %>%
    hoist(period,
          drive_end_qtr = "number"
    ) %>%
    select(-period) %>%
    hoist(clock,
          drive_end_clock = "displayValue"
    ) %>%
    rename(drive_end_yardline = yardLine, drive_end_text = text) %>%
    hoist(timeElapsed, drive_time = "displayValue") %>%
    rename(
      drive_yds = yards, drive_result_score = isScore,
      drive_plays = offensivePlays, drive_result = result
    ) %>%
    select(-shortDisplayResult, -displayResult) %>%
    unnest_longer(plays) %>%
    unnest_wider(plays) %>%
    rename(
      play_id = id, play_desc = text, away_score = awayScore,
      home_score = homeScore, scoring_play = scoringPlay,
      yards_gained = statYardage, scoring_type = scoringType
    ) %>%
    hoist(type, play_type = "text") %>% select(-type) %>%
    hoist(period, quarter = "number") %>%
    hoist(clock, clock_text = "displayValue") %>%
    select(-priority) %>%
    mutate(across(c(drive_result_score, scoring_play), as.integer)) %>%
    hoist(
      start,
      start_posteam_id = list("team", "id"),
      start_down = "down",
      start_ydstogo = "distance",
      start_yardline = "yardLine",
      start_ydsto_ez = "yardsToEndzone",
      start_text = "downDistanceText",
      start_down_text = "shortDownDistanceText",
      start_possess_text = "possessionText",
    ) %>%
    hoist(
      end,
      end_posteam_id = list("team", "id"),
      end_down = "down",
      end_ydstogo = "distance",
      end_yardline = "yardLine",
      end_ydsto_ez = "yardsToEndzone",
      end_text = "downDistanceText",
      end_down_text = "shortDownDistanceText",
      end_possess_text = "possessionText",
    ) %>%
    hoist(scoring_type, score_type = "abbreviation") %>%
    relocate(yards_gained, .after = "play_type") %>%
    select(-start, -end, -modified, -scoring_type)

  game_header <- raw_json %>%
    keep(names(raw_json) %in% "header") %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unchop(competitions) %>%
    rename(game_id = id, game_uid = uid) %>%
    unnest_wider(competitions) %>%
    unnest_wider(season) %>%
    select(-id, -uid) %>%
    rename(season = year, season_type = type) %>%
    select(
      !any_of(
        c(
          "neutralSite", "conferenceCompetition", "boxscoreAvailable",
          "commentaryAvailable", "liveAvailable", "onWatchESPN", "recent",
          "boxscoreSource", "playByPlaySource"
        )
      )
    ) %>%
    hoist(
      competitors,
      home_team_name = list(1, "team", "name"),
      home_team_logo = list(1, "team", "logos", 1, "href"),
      home_team_abb = list(1, "team", "abbreviation"),
      home_team_id = list(1, "team", "id"),
      home_team_location = list(1, "team", "location"),
      home_team_full = list(1, "team", "displayName"),
      home_team_color = list(1, "team", "color"),
      home_team_color_alt = list(1, "team", "alternateColor"),
      home_score_final = list(1, "score"),
      home_win = list(1, "winner"),
      home_record = list(1, "record", 1, "summary"),
      # away team
      away_team_name = list(2, "team", "name"),
      away_team_logo = list(2, "team", "logos", 1, "href"),
      away_team_abb = list(2, "team", "abbreviation"),
      away_team_id = list(2, "team", "id"),
      away_team_location = list(2, "team", "location"),
      away_team_full = list(2, "team", "displayName"),
      away_team_color = list(2, "team", "color"),
      away_team_color_alt = list(2, "team", "alternateColor"),
      away_score_final = list(2, "score"),
      away_win = list(2, "winner"),
      away_record = list(2, "record", 1, "summary"),
    ) %>%
    mutate(home_win = as.integer(home_win),
           away_win = as.integer(away_win),) %>%
    select(!where(is.list), -timeValid)



  combo_df <- bind_cols(nfl_pbp, game_header) %>%
    select(
      contains("game"), contains("season"), date, week, drive_id,pos_team_abb,
      play_id, play_type, yards_gained, play_desc, everything()
      )
  if(raw_json[["header"]][["season"]][["year"]] >= 2015){

    wp_df <- raw_json[["winprobability"]] %>%
      tibble(data = .) %>%
      hoist(
        data,
        play_id = "playId",
        home_wp = "homeWinPercentage",
        tie_percentage = "tiePercentage",
        game_sec_remaining = "secondsLeft"
      )

    combo_df %>%
      left_join(wp_df, by = "play_id")

  } else {
    combo_df
  }


}
