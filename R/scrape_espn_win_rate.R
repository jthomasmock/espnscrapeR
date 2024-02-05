#' Scrape ESPN Pass/Run Block/Rush Win Rates ratings for a specific season from ESPN's site
#'
#' @return Returns a tibble
#' @export
#' @import dplyr stringr
#' @importFrom dplyr %>%
#' @importFrom readr parse_number
#' @importFrom tidyr separate pivot_longer
#' @importFrom xml2 read_html
#' @importFrom purrr map2_dfr
#' @importFrom rvest html_node html_text
#' @importFrom tibble enframe
#' @examples
#' # Get off and def pass/run win rates
#' scrape_espn_win_rate()

scrape_espn_win_rate <- function(season = 2023){

  if(!(as.numeric(season) %in% c(2019:2023))) stop("Data available for 2019-2023")

  pbwr_2022 <- "https://www.espn.com/nfl/story/_/id/34536376/2022-nfl-pass-rushing-run-stopping-blocking-leaderboard-win-rate-rankings-top-players-teams"
  pbwr_2021 <- "https://www.espn.com/nfl/story/_/id/32176833/2021-nfl-pass-rushing-run-stopping-blocking-leaderboard-win-rate-rankings"
  pbwr_2020 <- "https://www.espn.com/nfl/story/_/id/29939464/2020-nfl-pass-rushing-run-stopping-blocking-leaderboard-win-rate-rankings"
  pbwr_2019 <- "https://www.espn.com/nfl/story/_/id/27584726/nfl-pass-blocking-pass-rushing-rankings-2019-pbwr-prwr-leaderboard#prwrteam"
  pbwr_2018 <- "https://www.espn.com/nfl/story/_/id/25074144/nfl-pass-blocking-pass-rushing-stats-final-leaderboard-pass-block-win-rate-pass-rush-win-rate"
  stats_in <- c(
    "Pass Rush Win Rate",
    "Run Stop Win Rate",
    "Pass Block Win Rate",
    "Run Block Win Rate"
  )

  stat_2019 <- c(
    "Pass Rush Win Rate",
    "Pass Block Win Rate"
  )

  # 2023 specific code:

  if(season == 2023){
    url_2023 <- "https://www.espn.com/nfl/story/_/id/38356170/2023-nfl-pass-rush-run-stop-blocking-win-rate-rankings-top-players-teams"

raw_html <- read_html(url_2023)

tab_23 <- raw_html %>% 
  html_table() %>%
  .[[9]] %>% 
  pivot_longer(cols = -1, names_to = "stat", values_to = "win_rate") %>% 
  mutate(
    stat = case_when(
      stat == "PRWR" ~ "Pass Rush Win Rate",
      stat == "RSWR" ~ "Run Stop Win Rate",
      stat == "PBWR" ~ "Pass Block Win Rate",
      stat == "RBWR" ~ "Run Block Win Rate"
    )) %>% 
      mutate(
    # extract just the string that is before a '%'
    win_rate = str_extract(win_rate, "^[^%]+"),
    # convert to a number
    win_pct = as.numeric(win_rate)
    ) %>% 
  mutate(date_updated = NA, season = 2023) %>% 
  arrange(stat, desc(win_pct)) %>% 
  group_by(stat) %>% 
  mutate(stat_rank = row_number()) %>% 
  ungroup() %>% 
  select(stat, stat_rank, team = Team, win_pct, date_updated, season)

  return(tab_23)
  }


  raw_html <- read_html(
    case_when(
      season == 2019 ~ pbwr_2019,
      season == 2020 ~ pbwr_2020,
      season == 2021 ~ pbwr_2021,
      season == 2022 ~ pbwr_2022
      )
    )

  date_updated <- raw_html %>%
    html_node("#article-feed > article:nth-child(1) > div > div.article-body > div.article-meta > span > span") %>%
    html_text()

  raw_text <- raw_html %>%
    html_nodes("#article-feed > article:nth-child(1) > div > div.article-body > p") %>%
    html_text()

  tibble::enframe(raw_text) %>%
    filter(str_detect(value, "1. ")) %>%
    mutate(name = if_else(season == 2019, list(stat_2019), list(stats_in))[[1]]) %>%
    mutate(value = str_split(value, "\n")) %>%
    unnest_longer(value) %>%
    separate(value, into = c("rank", "team", "win_pct"), sep = "\\. |, ") %>%
    mutate(
      rank = as.integer(rank),
      win_pct = str_remove(win_pct, "%"),
      win_pct = as.double(win_pct),
      date_updated = date_updated,
      season = season
      ) %>%
    rename(stat = name, stat_rank = rank)

}
