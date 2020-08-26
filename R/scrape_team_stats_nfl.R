#' Scrape NFL stats from nfl.com at the team level.
#'
#' Please note that the column names are identical between offense/defense and you should reference the 'role' column for offensive or defensive stats.
#'
#' @param stats character - either "passing", "rushing", "receiving", "scoring", "downs"
#' @param season character or numeric - greater than 1971, note that some data is missing for earlier seasons.
#' @param role character - "offense" or "defense"
#' @import purrr tidyr dplyr
#' @importFrom dplyr %>%
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @importFrom glue glue
#' @importFrom readr parse_number type_convert
#' @importFrom stringr str_remove
#' @export
#' @return tibble
#'
#' @examples
#'
#' # Get the NFL.com game-level stats for offense in 2018
#' scrape_team_stats_nfl(season = 2018, stats = "passing", role = "offense")
#'
#' # Get the NFL.com team-level passing stats for defense in 2014
#' scrape_team_stats_nfl(season = "2014", stats = "rushing", role = "defense")
scrape_team_stats_nfl <- function(season = 2019, stats = "passing", role = "offense") {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  if (!role %in% c("offense", "defense")) {
    stop("Please choose role of 'offense' or 'defense'")
  }

  if (!stats %in% c("passing", "rushing", "receiving", "scoring", "downs")) {
    stop("Please choose stats for 'passing', 'rushing', 'receiving', 'scoring', 'downs'!")
  }

  if (!dplyr::between(as.numeric(season), 1971, current_year)) {
    stop(paste("Please choose season between 1971 and", current_year))
  }

  message(glue::glue("Scraping {stats} for {role} from {season}!"))

  url <- dplyr::if_else(
    role == "offense",
    glue::glue("https://www.nfl.com/stats/team-stats/offense/{stats}/{season}/reg/all"),
    glue::glue("https://www.nfl.com/stats/team-stats/defense/{stats}/{season}/reg/all")
  )

  raw_html <- xml2::read_html(url)

  cleaned_tibble <- rvest::html_table(raw_html, fill = TRUE)[[1]] %>%
    janitor::clean_names() %>%
    dplyr::as_tibble()

  clean_scoring <- function(input_df) {
    suppressMessages(
      input_df %>%
        purrr::set_names(
          nm = c("team", "rush_td", "rec_td", "total_td", "two_pt_conv")
        ) %>%
        dplyr::mutate(team = word(team, 1), team = str_remove(team, "\\n"))
    )
  }

  pass_off_names <- c("team", "pass_att", "pass_comp", "pass_comp_pct", "yds_att", "pass_yds",
                 "pass_td", "int", "pass_rating", "first_downs",
                 "pass_first_pct", "pass_20plus", "pass_40plus",
                 "pass_long", "sacks", "sack_yds"
  )

  pass_def_names <- c("team", "pass_att", "pass_comp", "pass_comp_pct", "yds_att", "pass_yds",
                 "pass_td", "int", "pass_rating", "first_downs",
                 "pass_first_pct", "pass_20plus", "pass_40plus",
                 "pass_long", "sacks"
  )

  passing_names <- dplyr::if_else(role == "defense",
                                  list(pass_def_names),
                                  list(pass_off_names)
                                  )[[1]]

  clean_passing <- function(input_df) {

    suppressMessages(input_df %>%
      purrr::set_names(
        nm = passing_names
      ) %>%
      dplyr::mutate(
        pass_comp_pct = pass_comp_pct / 100,
        pass_first_pct = pass_first_pct / 100,
        team = word(team, 1), team = str_remove(team, "\\n")
      )
    )
  }

  clean_receiving <- function(input_df) {
    suppressMessages(input_df %>%
      purrr::set_names(
        nm = c("team", "rec", "rec_yds", "rec_ypr", "rec_td", "rec_20plus",
               "rec_40plus", "rec_lng", "rec_first", "rec_first_pct", "rec_fum")
      ) %>%
      dplyr::mutate(
        rec_first_pct = rec_first_pct / 100,
        team = word(team, 1), team = str_remove(team, "\\n")
      )
    )
  }

  clean_rushing <- function(input_df) {
    clean_rush_names <- c("team", "rush_att", "rush_yds", "rush_ypc", "rush_td",
                          "rush_20plus", "rush_40plus", "rush_lng", "rush_first",
                          "rush_first_pct", "rush_fum")

    renamed_tibble <- purrr::set_names(input_df, nm = clean_rush_names)

    suppressMessages(
      dplyr::mutate(renamed_tibble,
        rush_first_pct = rush_first_pct / 100,
        team = word(team, 1), team = str_remove(team, "\\n")
      )
    )
  }

  clean_downs <- function(input_df) {
    clean_downs_names <- c("team", "third_att", "third_conv", "fourth_att",
                           "fourth_conv", "rec_first", "rec_first_pct",
                           "rush_first", "rush_first_pct", "scrim_plays")

    suppressMessages(
      input_df %>%
        purrr::set_names(
          nm = clean_downs_names
        ) %>%
        dplyr::mutate(
          third_pct = third_conv / (third_att + third_conv),
          fourth_pct = fourth_conv / (fourth_att + fourth_conv),
          team = word(team, 1), team = str_remove(team, "\\n")
        )
    )
  }

  return_table <- if (stats == "scoring") {
    clean_scoring(cleaned_tibble)
  } else if (stats == "passing") {
    clean_passing(cleaned_tibble)
  } else if (stats == "rushing") {
    clean_rushing(cleaned_tibble)
  } else if (stats == "downs") {
    clean_downs(cleaned_tibble)
  } else if (stats == "receiving") {
    clean_receiving(cleaned_tibble)
  } else {
    stop("TABLE NOT FOUND")
  }

  return_table %>%
    dplyr::mutate(
      stat = toupper(stats),
      season = as.integer(season),
      role = toupper(role)
    ) %>%
    dplyr::select(season, role, stat, team, everything())
}
