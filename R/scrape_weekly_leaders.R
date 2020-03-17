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

scrape_weekly_leaders <- function(season = 2019, week = 1, season_type = "Regular", stats = "passing"){

  current_year <- as.double(substr(Sys.Date(), 1, 4))

  if(!season_type %in% c("Regular", "Playoffs"))
  {stop("Please choose season_type of 'Regular' or 'Playoffs'")}

  if(!stats %in% c("receiving", "rushing", "passing"))
  {stop("Please choose season_type of 'receiving', 'rushing', or 'passing'!")}

  if(!dplyr::between(as.numeric(season), 2002, current_year))
  {stop(paste("Please choose season between 2002 and", current_year))}

  if(season_type == "Regular" & !dplyr::between(as.numeric(week), 1, 17))
    {stop("Please choose a week between 1 and 17")}

  if(season_type == "Playoffs" & !dplyr::between(as.numeric(week), 1, 4))
    {stop("Please choose a week between 1 and 3")}

  message(
    dplyr::if_else(season_type == "Regular",
                   glue::glue("Scraping {stats} stats for week {week} from {season} {season_type} season!"),
                   glue::glue("Scraping {stats} stats for week {week} from {season} {season_type}!"))
  )

  season_type <- dplyr::if_else(season_type == "Regular", "2", "3")

  url <- glue::glue("http://www.espn.com/nfl/weekly/leaders/_/week/{week}/seasontype/{season_type}/type/{stats}")

  pass_n <- c("rank", "name", "team", "result", "pass_comp", "pass_att",
              "pass_yds", "pass_td", "pass_int", "sack", "pass_fumbles", "pass_rating")

  rush_n <- c("rank", "name", "team", "result", "rush_att", "rush_yds",
              "rush_avg", "rush_td", "rush_long", "rush_fumbles", "drop")


  rec_n <- c("rank", "name", "team", "result", "rec", "rec_yds", "rec_avg",
             "rec_td", "rec_long", "rec_fumbles", "drop")


  fix_names <- dplyr::case_when(
    stats == "passing" ~  list(pass_n),
    stats == "rushing" ~  list(rush_n),
    stats == "receiving" ~ list(rec_n)
  )[[1]]

  raw_df <- url %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//*[@id="my-players-table"]/div[1]/div[1]/table[2]') %>%
    rvest::html_table(fill = TRUE) %>%
    purrr::set_names(nm = fix_names) %>%
    slice(3:n()) %>%
    dplyr::as_tibble()

  clean_df <- raw_df %>%
    dplyr::mutate(position = stringr::str_extract(name, stringr::str_sub(name, -2)),
                  name = stringr::str_remove(name, position),
                  name = stringr::str_remove(name, ",")) %>%
    dplyr::mutate(rank = dplyr::row_number(),
                  season = season,
                  week = week,
                  season_type = dplyr::if_else(season_type == 2, "Regular", "Playoffs")) %>%
    dplyr::select(season, season_type, week, rank, name, team:dplyr::contains("fumbles"))

    suppressMessages(readr::type_convert(clean_df))
}





