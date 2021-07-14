#' Scrape NFL stats from ESPN
#'
#' @param stats character - either receiving, passing, or rushing
#' @param season character or numeric - greater than 1990
#' @param season_type character - either Regular or Playoffs
#' @import purrr tidyr dplyr stringr
#' @importFrom dplyr %>%
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @importFrom glue glue
#' @importFrom readr parse_number
#' @return tibble
#' @export
#'
#' @examples
#' scrape_espn_stats(season = 2000, stats = "passing")
#'

scrape_espn_stats <- function(season = 2019, stats = "receiving", season_type = "Regular") {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  if (!season_type %in% c("Regular", "Playoffs")) {
    stop("Please choose season_type of 'Regular' or 'Playoffs'")
  }

  if (!stats %in% c("receiving", "rushing", "passing")) {
    stop("Please choose season_type of 'receiving', 'rushing', or 'passing'!")
  }

  if (!dplyr::between(as.numeric(season), 1990, current_year)) {
    stop(paste("Please choose season between 1990 and", current_year))
  }

  message(
    dplyr::if_else(
      season_type == "Regular",
      glue::glue("Scraping {stats} stats from {season} {season_type} season!"),
      glue::glue("Scraping {stats} stats from {season} {season_type}!")
    )
  )

  season_type <- dplyr::if_else(season_type == "Regular", "2", "3")

  url_in <- glue::glue("https://www.espn.com/nfl/stats/player/_/stat/{stats}/season/{season}/seasontype/{season_type}&limit=500")

  pass_n <- c(
    "season", "season_type", "pass_rank", "name", "team", "pos",
    "games_played", "pass_completed", "pass_attempts", "comp_percent",
    "pass_yards", "pass_avg", "pass_yards_game", "pass_long", "pass_td",
    "pass_int", "sack", "sack_yards", "qbr", "pass_rating"
  )

  rush_n <- c(
    "season", "season_type", "rush_rank", "name", "team", "pos",
    "games_played", "rush_att", "rush_yards", "rush_avg", "rush_long",
    "rush_20plus", "rush_td", "rush_yards_game", "fumble", "fumble_lost",
    "rush_first_down"
  )


  rec_n <- c(
    "season", "season_type", "rec_rank", "name", "team", "pos",
    "games_played", "receptions", "targets", "rec_yards", "rec_avg",
    "rec_td", "rec_long", "rec_20plus", "rec_yards_game", "fumble",
    "fumble_lost", "yards_after_catch", "rec_first_down"
  )


  fix_names <- dplyr::case_when(
    stats == "passing" ~ list(pass_n),
    stats == "rushing" ~ list(rush_n),
    stats == "receiving" ~ list(rec_n)
  )[[1]]

  url_in %>%
    xml2::read_html() %>%
    rvest::html_table(fill = TRUE) %>%
    dplyr::bind_cols() %>%
    janitor::clean_names() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      # get rid of the comma and convert to numeric
      yds = readr::parse_number(as.character(yds)),
      # if there is a player with multiple teams
      # We need to figure out how many teams and then
      slash_ct = stringr::str_count(name, "/"),
      chr_ct = slash_ct * (-4) - 3,
      team = stringr::str_sub(name, chr_ct),
      team = stringr::str_remove(team, "[::a-z::]"),
      team = dplyr::if_else(
        # Find players with Jr/Sr/II/III/IV etc and drop them
        stringr::str_sub(team, 1) %in% c("I", "V", ".") & !stringr::str_detect(team, "IND"),
        # TRUE
        stringr::str_sub(team, 2, stringr::str_length(team)),
        # FALSE
        team
      ),
      team = stringr::str_remove(team, "\\."),
      name = stringr::str_remove(name, team)
    ) %>%
    dplyr::arrange(desc(yds)) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      season = season,
      season_type = dplyr::if_else(season_type == 2, "Regular", "Playoffs")
    ) %>%
    dplyr::select(
      season, season_type, rank, name, team, dplyr::everything(),
      -rk, -slash_ct, -chr_ct
    ) %>%
    purrr::set_names(nm = fix_names)
}
