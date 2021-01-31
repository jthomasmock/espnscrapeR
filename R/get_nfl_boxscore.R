#' Get NFL boxscore for a specific game from ESPN's API
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
#' get_nfl_boxscore(game_id = "300912027")
#'


get_nfl_boxscore <- function(game_id){

  game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={game_id}&enable=ranks,odds,linescores,logos")

  raw_json <- fromJSON(game_url, simplifyVector = FALSE)

  category_game_summary <- raw_json[["boxscore"]][["players"]] %>%
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
    unchop(cols = c(labels, descriptions, totals)) %>%
    unchop(cols = c(labels, descriptions, totals)) %>%
    mutate(name = substr(name, 1, 4)) %>%
    unite(col = "stat_labels", name, labels, sep = "_") %>%
    mutate(stat_labels = tolower(stat_labels) %>% str_replace("/| ", "_")) %>%
    mutate(
      stat_labels = str_replace(stat_labels, "rece", "rec"),
      stat_labels = str_replace(stat_labels, "defe", "def"),
      stat_labels = str_replace(stat_labels, "inte", "int"),
      stat_labels = case_when(
        stat_labels == "kick_long" & str_detect(descriptions, "Return") ~ "kick_long_return",
        stat_labels == "kick_long" & str_detect(descriptions, "Field") ~ "kick_long_fg",
        stat_labels == "punt_no" & str_detect(descriptions, "Return") ~ "punt_no_returns",
        stat_labels == "punt_no" & str_detect(descriptions, "Return", negate = TRUE) ~ "punt_no",
        stat_labels == "punt_avg" & str_detect(descriptions, "Return") ~ "punt_avg_return",
        stat_labels == "punt_avg" & str_detect(descriptions, "Return", negate = TRUE) ~ "punt_avg",
        stat_labels == "punt_long" & str_detect(descriptions, "Return") ~ "punt_long_return",
        stat_labels == "punt_long" & str_detect(descriptions, "Return", negate = TRUE) ~ "punt_long",
        stat_labels == "punt_yds" & str_detect(text, "Return", negate = TRUE) ~ "punt_yds",
        stat_labels == "punt_yds" & str_detect(text, "Return") ~ "punt_yds_return",
        TRUE ~ stat_labels
      )
    ) %>%
    pivot_wider(
      names_from = stat_labels,
      values_from = totals,
      id_cols = c(team_id)
    ) %>%
    separate(pass_sacks, into = c("pass_sacks", "pass_sack_yds"), sep = "-", convert = TRUE) %>%
    separate(pass_c_att, into = c("pass_att", "pass_comp"), sep = "/", convert = TRUE) %>%
    separate(kick_fg, into = c("kick_fg_made", "kick_fg_att"), sep = "/", convert = TRUE) %>%
    separate(kick_xp, into = c("kick_xp_made", "kick_xp_att"), sep = "/", convert = TRUE) %>%
    mutate(across(pass_att:punt_long, ~ suppressWarnings(as.double(.x)))) %>%
    rename(
      rec_total = rec_rec,
      int_total = int_int,
      fumb_total = fumb_fum,
      rush_att = rush_car,
      def_tkl = def_tot,
      def_solo_tkl = def_solo,
      kick_fg_pct = kick_pct
    )

  gen_boxscore <- raw_json[["boxscore"]][["teams"]] %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unnest_wider(team) %>%
    select(
      team_id = id, team_uid = uid, team_city = location, team_name = name,
      team_abb = abbreviation, team_full = displayName, team_color = color,
      team_color_alt = alternateColor, team_logo = logo, statistics
    ) %>%
    hoist(
      statistics,
      first_downs = list(1, "displayValue"),
      passing_1st_downs = list(2, "displayValue"),
      rushing_1st_downs = list(3, "displayValue"),
      first_downs_from_penalties = list(4, "displayValue"),
      third_down_efficiency = list(5, "displayValue"),
      fourth_down_efficiency = list(6, "displayValue"),
      total_plays = list(7, "displayValue"),
      total_yards = list(8, "displayValue"),
      yards_per_play = list(9, "displayValue"),
      total_drives = list(10, "displayValue"),
      red_zone_made_att = list(19, "displayValue"),
      penalties = list(20, "displayValue"),
      turnovers = list(21, "displayValue"),
      defensive_special_teams_tds = list(24, "displayValue"),
      possess_time = list(25, "displayValue")
    ) %>%
    separate(third_down_efficiency, c("third_down_conv", "third_down_att"), sep = "-", convert = TRUE) %>%
    separate(fourth_down_efficiency, c("fourth_down_conv", "fourth_down_att"), sep = "-", convert = TRUE) %>%

    separate(red_zone_made_att, c("redzone_att", "redzone_conv"), sep = "-", convert = TRUE) %>%
    separate(penalties, c("penalties", "pen_yards"), sep = "-", convert = TRUE) %>%
    rename(def_sp_tds = defensive_special_teams_tds) %>%
    mutate(across(first_downs:def_sp_tds, as.double)) %>%
    select(-statistics)

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
    unchop(competitors) %>%
    unnest_wider(competitors) %>%
    rename(team_id = id, team_uid = uid, team_order = order, home_away = homeAway) %>%
    select(-links, -timeValid) %>%
    unnest_wider(team) %>%
    rename(
      team_name = name, team_location = location, team_full = displayName,
      team_abb = abbreviation, team_color = color, team_color_alt = alternateColor,
      team_score = score
    ) %>%
    hoist(logos, team_logo = list(1, "href")) %>%
    hoist(record, team_record = list(1, "displayValue")) %>%
    select(
      !any_of(
        c(
          "links", "logos", "possession", "broadcasts", "league", "linescores",
          "record", "status", "rank", "team_uid", "team_name", "team_abb",
          "team_full", "team_color", "team_color_alt", "team_logo", "nickname", "id", "uid"
        )
      )

    ) %>%
    relocate(week, .after = date)

  left_join(category_game_summary, gen_boxscore, by = "team_id") %>%
    left_join(game_header, by = "team_id") %>%
    select(
      contains("game"), contains("season"), date, week, home_away, winner,
      contains("team"), everything(), -team_location
    )  %>%
    mutate(
      winner = as.integer(winner),
      team_score = as.integer(team_score)
      )

  }

