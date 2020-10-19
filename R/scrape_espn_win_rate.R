#' Scrape ESPN Pass/Run Block/Rush Win Rates ratings for a specific season from ESPN's site
#'
#' @return Returns a tibble
#' @export
#' @import dplyr stringr
#' @importFrom dplyr %>%
#' @importFrom readr parse_number
#' @importFrom tidyr separate
#' @importFrom xml2 read_html
#' @importFrom purrr map2_dfr
#' @importFrom rvest html_node html_text
#' @importFrom tibble enframe
#' @examples
#' # Get off and def pass/run win rates
#' scrape_espn_win_rate()

scrape_espn_win_rate <- function(season = 2020){

  pbwr_url <- "https://www.espn.com/nfl/story/_/id/29939464/2020-nfl-pass-rushing-run-stopping-blocking-leaderboard-win-rate-rankings"
  pbwr_2019 <- "https://www.espn.com/nfl/story/_/id/27584726/nfl-pass-blocking-pass-rushing-rankings-2019-pbwr-prwr-leaderboard#prwrteam"
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

  raw_html <- read_html(if_else(season == 2019, pbwr_2019, pbwr_url))

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

scrape_espn_win_rate(season = 2020)

