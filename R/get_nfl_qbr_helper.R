#' Get ESPN QBR for NFL football
#'
#' @param season Either numeric or character
#' @param week Either NA to return season or week 1 to 4 for playoffs or 1 to 17 for regular
#' @param season_type Character - either "Regular" or "Playoffs"
#' @return Returns a tibble
#' @import tidyr dplyr purrr httr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue

get_nfl_qbr_helper <- function(raw_json, week, season, season_type) {

  in_nm <- c(
    "qbr_total",
    "pts_added",
    "qb_plays",
    "epa_total",
    "pass",
    "run",
    "exp_sack",
    "penalty",
    "qbr_raw",
    "sack"
  )


  if (is.na(week)) {
    final_df <- raw_json[["athletes"]] %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value) %>%
      tidyr::hoist(categories, vals = list(1, "totals")) %>%
      dplyr::select(-name, -categories) %>%
      tidyr::unnest_wider(athlete) %>%
      tidyr::hoist(
        headshot,
        headshot_href = "href"
      ) %>%
      dplyr::select(
        team_abb = teamShortName,
        player_id = id,
        name_short = shortName,
        vals,
        name_first = firstName,
        name_last = lastName,
        name_display = displayName,
        headshot_href,
        team = teamName,
        team_id = teamId,
        team_uid = teamUId
      ) %>%
      dplyr::mutate(vals = purrr::map(vals, ~ purrr::set_names(.x, in_nm))) %>%
      tidyr::unnest_wider(vals) %>%
      dplyr::mutate(dplyr::across(qbr_total:sack, as.double)) %>%
      dplyr::mutate(
        rank = rank(desc(qbr_total)),
        game_week = "Season Total",
        season = season,
        season_type = season_type
      )  %>%
      dplyr::select(
        season,
        season_type,
        game_week,
        team_abb,
        player_id,
        name_short,
        rank,
        qbr_total:sack,
        name_first,
        name_last,
        name_display,
        headshot_href,
        team
      )
  } else {
    final_df <- raw_json[["athletes"]] %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value) %>%
      tidyr::hoist(categories, vals = list(1, "totals")) %>%
      tidyr::hoist(
        game,
        game_id = "id",
        game_date = "date",
        game_score = "score",
        home_away = "homeAway",
        week_num = "weekNumber",
        week_text = "weekText",
        opp_id = list("teamOpponent", "id"),
        opp_abb = list("teamOpponent", "abbreviation"),
        opp_team = list("teamOpponent", "displayName"),
        opp_name = list("teamOpponent", "name")
      ) %>%
      dplyr::select(-name, -categories, -game) %>%
      tidyr::unnest_wider(athlete) %>%
      tidyr::hoist(
        headshot,
        headshot_href = "href"
      ) %>%
      dplyr::select(
        game_id,
        week_num,
        week_text,
        team_abb = teamShortName,
        player_id = id,
        name_short = shortName,
        vals,
        name_first = firstName,
        name_last = lastName,
        name_display = displayName,
        headshot_href,
        team = teamName,
        opp_id:opp_name,
      ) %>%
      dplyr::mutate(vals = purrr::map(vals, ~ purrr::set_names(.x, in_nm))) %>%
      tidyr::unnest_wider(vals) %>%
      dplyr::mutate(dplyr::across(qbr_total:sack, as.double)) %>%
      dplyr::mutate(
        rank = rank(desc(qbr_total)),
        game_week = as.integer(week),
        season = season,
        season_type = season_type
      ) %>%
      dplyr::select(
        season,
        season_type,
        game_id,
        game_week,
        week_text,
        team_abb,
        player_id,
        name_short,
        rank,
        qbr_total:sack,
        name_first,
        name_last,
        name_display,
        headshot_href,
        team,
        opp_id:opp_name,
        week_num
      )

  }

  final_df
}
