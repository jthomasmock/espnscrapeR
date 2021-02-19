#' Get NFL boxscore for players for a specific game from ESPN's API
#'
#' @param game_id character
#'
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @examples
#' # Get NFL play-by-play for a specific game
#' get_nfl_boxscore_players(game_id = "300912027")
get_nfl_boxscore_players <- function(game_id) {
  game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={game_id}&enable=ranks,odds,linescores,logos")

  raw_json <- fromJSON(game_url, simplifyVector = FALSE)

  wide_game_raw <- raw_json[["boxscore"]][["players"]] %>%
    tibble(data = .) %>%
    unnest_wider(data)

  if ("statistics" %in% names(wide_game_raw)) {
    wide_game_summary <- raw_json[["boxscore"]][["players"]] %>%
      tibble(data = .) %>%
      unnest_wider(data) %>%
      unnest_longer(statistics) %>%
      unnest_wider(statistics) %>%
      unnest_longer(athletes) %>%
      unnest_wider(athletes) %>%
      unnest_wider(athlete) %>%
      unchop(cols = c(labels, descriptions, totals, stats)) %>%
      unchop(cols = c(labels, descriptions, totals, stats)) %>%
      select(-links) %>%
      mutate(
        name = case_when(
          name == "passing" ~ "pass",
          name == "rushing" ~ "rush",
          name == "receiving" ~ "rec",
          name == "fumbles" ~ "fum",
          name == "defensive" ~ "def",
          name == "interceptions" ~ "int",
          name == "kickReturns" ~ "kick_ret",
          name == "puntReturns" ~ "punt_ret",
          name == "kicking" ~ "kick",
          name == "punting" ~ "punt",
          TRUE ~ name
        )
      ) %>%
      unite(stat_labels, sep = "_", name, labels) %>%
      mutate(
        stat_labels = tolower(stat_labels) %>% gsub(x = ., pattern = "/", replacement = "p"),
        stat_labels = gsub(x = stat_labels, pattern = " ", replacement = "_"),
      ) %>%
      pivot_wider(names_from = stat_labels, values_from = stats, id_cols = c(id:displayName)) %>%
      rename(
        player_id = id,
        player_uid = uid,
        player_guid = guid,
        first_name = firstName,
        last_name = lastName,
        full_name = displayName,
        rush_att = rush_car,
        def_solo_tkl = def_solo,
        def_tkl = def_tot,
        rec_total = rec_rec,
        fum_total = fum_fum,
        int_total = int_int
      ) %>%
      separate(pass_cpatt, into = c("pass_att", "pass_cmp"), sep = "/", convert = TRUE) %>%
      separate(pass_sacks, into = c("pass_sacks", "pass_sack_yds"), sep = "-", convert = TRUE) %>%
      separate(kick_fg, into = c("kick_fg_att", "kick_fg_made"), sep = "/", convert = TRUE) %>%
      separate(kick_xp, into = c("kick_xp_att", "kick_xp_made"), sep = "/", convert = TRUE) %>%
      mutate(across(c(pass_yds:punt_long), ~ suppressWarnings(as.double(.x))))

    player_id_df <- raw_json[["boxscore"]][["players"]] %>%
      tibble(data = .) %>%
      unnest_wider(data) %>%
      unnest_wider(team) %>%
      select(
        team_id = id,
        team_uid = uid,
        team_city = location,
        team_name = name,
        team_abb = abbreviation,
        team_full = displayName,
        team_color = color,
        team_color_alt = alternateColor,
        team_logo = logo,
        statistics
      ) %>%
      unnest_longer(statistics) %>%
      unnest_wider(statistics) %>%
      unnest_longer(athletes) %>%
      unnest_wider(athletes) %>%
      unnest_wider(athlete) %>%
      select(team_id:team_logo, player_id = id) %>%
      distinct(player_id, .keep_all = TRUE)
  } else {
    wide_game_summary <- wide_game_raw %>%
      unnest_wider(team)
  }

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
          "neutralSite",
          "conferenceCompetition",
          "boxscoreAvailable",
          "commentaryAvailable",
          "liveAvailable",
          "onWatchESPN",
          "recent",
          "boxscoreSource",
          "playByPlaySource"
        )
      )
    ) %>%
    unchop(competitors) %>%
    unnest_wider(competitors) %>%
    rename(team_id = id, team_uid = uid, team_order = order, home_away = homeAway) %>%
    select(-links, -timeValid) %>%
    unnest_wider(team) %>%
    rename(
      team_name = name,
      team_location = location,
      team_full = displayName,
      team_abb = abbreviation,
      team_color = color,
      team_color_alt = alternateColor,
      team_score = score
    ) %>%
    hoist(logos, team_logo = list(1, "href")) %>%
    hoist(record, team_record = list(1, "displayValue")) %>%
    select(
      !any_of(c(
        "links",
        "logos",
        "possession",
        "broadcasts",
        "league",
        "linescores",
        "record",
        "status",
        "rank",
        "team_uid",
        "team_name",
        "team_abb",
        "team_full",
        "team_color",
        "team_color_alt",
        "team_logo",
        "nickname",
        "id",
        "uid"
      ))
    ) %>%
    select(game_id:date, any_of("week"), team_id:last_col())

  if ("statistics" %in% names(wide_game_raw)) {
    left_join(wide_game_summary, player_id_df, by = "player_id") %>%
      left_join(game_header, by = "team_id") %>%
      select(
        contains("game"),
        contains("season"),
        date,
        week,
        home_away,
        winner,
        contains("team"),
        everything(),
        -team_location
      ) %>%
      mutate(
        winner = as.integer(winner),
        team_score = as.integer(team_score)
      )
  } else {
    game_header %>%
      select(
        contains("game"),
        contains("season"),
        date,
        any_of("week"),
        home_away,
        winner,
        contains("team"),
        everything(),
        -team_location
      ) %>%
      mutate(
        winner = as.integer(winner),
        team_score = as.integer(team_score)
      )
  }
}
