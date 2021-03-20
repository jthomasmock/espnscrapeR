#' Get ELO ratings from FiveThirtyEight
#'
#' @param season character or numeric, must be 2015 or greater
#' @param stat character, must be one of "games", "weekly_elo", "weekly_rating", "distance", "qb_adj_playoff", "qb_adj"
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @examples
#' # Get elo for specific season and stat
#' get_538_elo(season = 2020, stat = "weekly_elo")


get_538_elo <- function(season = 2020, stat = "weekly_elo") {

  season <- as.integer(season)

  stat_types <- c(
    "games",
    "weekly_elo",
    "weekly_rating",
    "distance",
    "qb_adj_playoff",
    "qb_adj"
  )

  if (!(stat %in% stat_types)) {
    message(paste0("Stat must be one of ", paste0(stat_types, collapse = ", ")))
  }

  if (season < 2015) {
    # stats only available from 2015 on
    message("Stats only available for years 2015 to current year")

  } else if (season == 2015) {

    url_2015 <- "https://projects.fivethirtyeight.com/2015-nfl-predictions/data.json"

    json_2015 <- fromJSON(url_2015, simplifyVector = FALSE)
  } else if (season >= 2015) {

    in_url <- glue::glue("https://projects.fivethirtyeight.com/{season}-nfl-predictions/data.json")

    raw_json <- fromJSON(in_url, simplifyVector = FALSE)
  }

  # 2015 has a different version
  if (season == 2015 && stat == "games") {
    json_2015$games %>%
      tibble(value = .) %>%
      unnest_wider(value)
  } else if (season == 2015 && stat == "weekly_elo") {
    json_2015$weekly_forecasts$forecasts %>%
      tibble(value = .) %>%
      unnest_wider(value) %>%
      unnest_longer(teams) %>%
      unnest_wider(teams)
  } else if(season %in% c(2016:2018) && stat %in% c("qb_adj", "weekly_rating", "distance", "qb_adj_playoff")){

    message(paste0(stat, " not available for ", season))

  } else if (season >= 2019 && stat == "qb_adj_playoff") {
    raw_json$playoff_qb_adjustments %>%
      tibble(data = .) %>%
      unnest_wider(data)
  } else if (season >= 2019 && stat == "weekly_rating") {
    rating_df <- raw_json$weekly_forecasts$forecasts %>%
      tibble(data = .) %>%
      unnest_wider(data) %>%
      unnest_wider(types) %>%
      select(-elo) %>%
      unnest_longer(rating) %>%
      unnest_wider(rating)

    rating_df

  } else if (season >= 2019 && stat == "weekly_elo") {
    elo_df <- raw_json$weekly_forecasts$forecasts %>%
      enframe() %>%
      unnest_wider(value) %>%
      unnest_wider(types) %>%
      select(-name, -rating) %>%
      unnest_longer(elo) %>%
      unnest_wider(elo)

    elo_df
  } else if (season >= 2019 && stat == "games") {
    games_df <- raw_json$games %>%
      tibble(data = .) %>%
      unnest_wider(data)

    games_df
  } else if (season >= 2019 && stat == "distance") {
    distance_df <- raw_json$distances %>%
      tibble(data = .) %>%
      unnest_wider(data) %>%
      unnest_longer(distances)
    distance_df
  } else if (season >= 2019 && stat == "qb_adj") {
    qb_adj <- raw_json$qbs %>%
      tibble(data = .) %>%
      unnest_wider(data)
    qb_adj
  }
}

#' Get historical ELO ratings from FiveThirtyEight
#'
#' @param team character, team abbreviated name, must be one of "ari", "atl", "bal", "buf", "car", "chi", "cin", "cle", "dal", "den", "det", "gb", "hou", "ind", "jax", "kc", "oak", "lac", "lar", "mia", "min", "ne", "no", "nyg", "nyj", "phi", "pit", "sf", "sea", "tb", "ten", "wsh"
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @examples
#'
#' # Get historic elo for specific team
#' get_538_elo_historical(team = "pit")

get_538_elo_historical <- function(team = "pit"){

  all_teams <- c(
    "ari", "atl", "bal", "buf", "car", "chi", "cin", "cle", "dal", "den", "det",
    "gb", "hou", "ind", "jax", "kc", "oak", "lac", "lar", "mia", "min", "ne",
    "no", "nyg", "nyj", "phi", "pit", "sf", "sea", "tb", "ten", "wsh"
  )

  if(!(team %in% all_teams)){
    message(
      glue::glue(
        "Team not found, please use one of {paste0(all_teams, collapse = ', ')}"
      )
    )
  }



  join_teams <- tibble(
    team = c(
      "ari","atl","bal","buf",
      "car","chi","cin","cle","dal","den","det","gb","hou",
      "ind","jax","kc","oak","lac","lar","mia","min",
      "ne","no","nyg","nyj","phi","pit","sf","sea","tb",
      "ten","wsh"
    ),
    espn_uid = c(
      "22","1","33","2","29","3",
      "4","5","6","7","8","9","34","11","30","12",
      "13","24","14","15","16","17","18","19","20","21",
      "23","25","26","27","10","28"
    ),
    espn_abb = c(
      "ARI","ATL","BAL","BUF",
      "CAR","CHI","CIN","CLE","DAL","DEN","DET","GB","HOU",
      "IND","JAX","KC","LV","LAC","LAR","MIA","MIN",
      "NE","NO","NYG","NYJ","PHI","PIT","SF","SEA","TB",
      "TEN","WSH"
    )
  )

  url_team <- dplyr::case_when(
    team == "lar" ~ "stl",
    team == "lac" ~ "sd",
    TRUE ~ team
  )

  url_in <- glue::glue("https://projects.fivethirtyeight.com/complete-history-of-the-nfl/data/{url_team}.json")

  elo_json <- fromJSON(url_in, simplifyVector = FALSE)

  match_weeks <- tibble(
    week = c(
      NA, "Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7",
      "Week 8", "Week 9", "Week 10", "Week 11", "Week 12", "Week 13", "Week 14",
      "Week 15", "Week 16", "Week 17", "Week 18",
      "Wild card", "Divisional", "Conference", "Super Bowl"),
    week_num = c(NA, 1:18, 18,19,20,21)
  )

  elo_json$value %>%
    tibble(data= .) %>%
    hoist(
      data,
      year = "x",
      elo = "y",
      opp = "t",
      pts_for = "p",
      pts_against = "o",
      week = "w",
      date = "d"
    ) %>%
    mutate(pts_for = suppressWarnings(unlist(pts_for) %>% as.double())) %>%
    mutate(team = team, .before = year, date = as.Date(date, "%m/%d/%Y")) %>%
    left_join(match_weeks, by = "week") %>%
    left_join(join_teams, by = "team") %>%
    select(team, espn_uid, espn_abb, everything())


}
