#' Scrape NFL stats from nfl.com at the team level.
#'
#' Please note that the column names are identical between offense/defense and you should reference the 'role' column for offensive or defensive stats.
#'
#' @param stats character - either "GAME_STATS", "SCORING", "TEAM_PASSING", "TEAM_RUSHING", "TEAM_RECEIVING", or "OFFENSIVE_LINE"
#' @param season character or numeric - greater than 1970
#' @param season_type character - either 'Regular' or 'Playoffs'
#' @param role character - "offense" or "defense"
#' @import purrr tidyr dplyr
#' @importFrom dplyr %>%
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @importFrom glue glue
#' @importFrom readr parse_number type_convert
#' @export
#' @return tibble
#'
#' @examples
#' scrape_team_stats_nfl(season = 2018, stats = "GAME_STATS", role = "offense")
#'
#' scrape_team_stats_nfl(season = "2014, stats = "TEAM_PASSING", role = "defense)
scrape_team_stats_nfl <- function(season = 2019, stats = "GAME_STATS", role = "offense", season_type = "Regular") {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  if (!season_type %in% c("Regular", "Playoffs")) {
    stop("Please choose season_type of 'Regular' or 'Playoffs'")
  }

  if (!role %in% c("offense", "defense")) {
    stop("Please choose role of 'offense' or 'defense'")
  }

  if (!stats %in% c("GAME_STATS", "SCORING", "TEAM_PASSING", "RUSHING", "TEAM_RECEIVING", "OFFENSIVE_LINE")) {
    stop("Please choose season_type of 'GAME_STATS', 'SCORING', 'TEAM_PASSING', 'RUSHING', 'TEAM_RECEIVING', 'OFFENSIVE_LINE'!")
  }

  if (!dplyr::between(as.numeric(season), 1970, current_year)) {
    stop(paste("Please choose season between 1970 and", current_year))
  }


  message(glue::glue("Scraping {stats} for {role} from {season} {season_type}!"))

  season_type <- dplyr::if_else(season_type == "Regular", "REG", "POST")

  url <- dplyr::if_else(
    role == "offense",
    glue::glue("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory={stats}&defensiveStatisticCategory=null&season={season}&seasonType={season_type}&tabSeq=2&qualified=false&Submit=Go"),
    glue::glue("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory={stats}&season={season}&seasonType={season_type}&tabSeq=2&qualified=false&Submit=Go")
  )

  raw_html <- xml2::read_html(url)

  cleaned_tibble <- rvest::html_table(raw_html, fill = TRUE)[[1]] %>%
    janitor::clean_names() %>%
    dplyr::as_tibble()

  clean_scoring <- function(input_df) {
    suppressMessages(
      input_df %>%
        select(1:5, 8:dplyr::last_col()) %>%
        purrr::set_names(
          nm = c(
            "rank", "team", "games", "pts_game", "pts_total", "td_rush",
            "td_rec", "td_punt", "td_kick", "td_int", "td_fumble",
            "td_fg", "td_extra_pt", "extra_points_made", "field_goal_made",
            "safety", "two_point_converted"
          )
        ) %>%
        dplyr::slice(-1) %>%
        readr::type_convert()
    )
  }

  clean_off_line <- function(input_df) {
    suppressMessages(
      input_df %>%
        purrr::set_names(
          nm = c(
            "rank", "team", "experience", "rush_att", "rush_yds", "rush_avg",
            "rush_td", "left_rush_first", "left_rush_neg", "left_rush_10_plus", "left_rush_power",
            "center_rush_first", "center_rush_neg", "center_rush_10_plus", "center_rush_power",
            "right_rush_first", "right_rush_neg", "right_rush_10_plus", "right_rush_power",
            "sacks", "qb_hits"
          )
        ) %>%
        dplyr::slice(-1) %>%
        dplyr::mutate(rush_yds = readr::parse_number(rush_yds)) %>%
        readr::type_convert()
    )
  }

  clean_passing <- function(input_df) {
    suppressMessages(input_df %>%
      purrr::set_names(
        nm = c(
          "rank", "team", "games", "pts_game", "pts_total", "pass_comp",
          "pass_att", "pass_comp_pct", "pass_att_g", "pass_yds", "pass_avg",
          "pass_yds_g", "pass_td", "pass_int", "pass_1st", "pass_1st_pct",
          "pass_long", "pass_20_plus", "pass_40_plus", "pass_sack", "pass_rating"
        )
      ) %>%
      dplyr::mutate(pass_yds = readr::parse_number(pass_yds)))
  }

  clean_receiving <- function(input_df) {
    suppressMessages(input_df %>%
      purrr::set_names(
        nm = c(
          "rank", "team", "games", "pts_game", "pts_total", "rec",
          "rec_yds", "rec_avg", "rec_yds_g", "rec_lng", "rec_td",
          "rec_20_plus", "rec_40_plus", "rec_first", "rec_first_pct", "rec_fumbles"
        )
      ) %>%
      dplyr::mutate(
        rec_yds = readr::parse_number(rec_yds),
        rec_first_pct = rec_first_pct / 100
      ))
  }

  clean_rushing <- function(input_df) {

    # clean_rush_names <- janitor::make_clean_names(
    #   as.character(cleaned_tibble[1,])
    # )

    clean_rush_names <- c(
      "rank", "team", "games", "pts_game", "pts_total", "rush_att",
      "rush_att_g", "rush_yds", "rush_avg", "rush_yds_g", "rush_td",
      "rush_long", "rush_1st", "rush_1st_pct", "rush_20_plus",
      "rush_40_plus", "rush_fumbles"
    )

    # renamed_tibble <- purrr::set_names(
    #   cleaned_tibble,
      # nm = c(
      #   "rank", "team", "games", "pts_game", "pts_total", "rush_att",
      #   "rush_att_g", "rush_yds", "rush_avg", "rush_yds_g", "rush_td",
      #   "rush_long", "rush_1st", "rush_1st_pct", "rush_20_plus",
      #   "rush_40_plus", "rush_fumbles"
      # )
    # )

    renamed_tibble <- purrr::set_names(input_df, nm = clean_rush_names)

    suppressMessages(
      dplyr::mutate(renamed_tibble, rush_yds = readr::parse_number(rush_yds)) %>%
        readr::type_convert()

      )
  }

  clean_game <- function(input_df) {
    suppressMessages(
      input_df %>%
        purrr::set_names(
          nm = c(
            "rank", "team", "games", "pts_game", "pts_total", "plays_scrimmage",
            "yds_game", "yds_play", "first_down_g", "third_conv", "third_att",
            "third_pct", "fourth_conv", "fourth_att", "fourth_pct", "penalty",
            "penalty_yds", "time_of_poss", "fumbles_total", "fumbles_lost",
            "turnover_ratio"
          )
        ) %>%
        dplyr::mutate(
          plays_scrimmage = readr::parse_number(plays_scrimmage),
          third_pct = third_pct / 100,
          penalty_yds = readr::parse_number(penalty_yds),
          top_min = as.double(substr(time_of_poss, 1, 2)) * 60,
          top_sec = as.double(substr(time_of_poss, 4, 5)) + top_min,
          time_of_poss = top_sec / 60
        ) %>%
        dplyr::select(-top_min, -top_sec)
    )
  }

  return_table <- if (stats == "SCORING") {
    clean_scoring(cleaned_tibble)
  } else if (stats == "TEAM_PASSING") {
    clean_passing(cleaned_tibble)
  } else if (stats == "RUSHING") {
    clean_rushing(cleaned_tibble)
  } else if (stats == "GAME_STATS") {
    clean_game(cleaned_tibble)
  } else if (stats == "TEAM_RECEIVING") {
    clean_receiving(cleaned_tibble)
  } else if (stats == "OFFENSIVE_LINE") {
    clean_off_line(cleaned_tibble)
  } else {
    stop("TABLE NOT FOUND")
  }

  return_table %>%
    dplyr::mutate(
      stat = stats,
      season = season,
      season_type = season_type,
      role = role
    )
}
