#' Scrape ESPN FPI ratings for a specific season from ESPN's site
#'
#' @param season Either numeric or character
#' @param stat One of 'FPI', 'EFF' or 'PROJ'
#'
#' @return Returns a tibble
#' @export
#' @import dplyr stringr
#' @importFrom dplyr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_nodes html_text html_table html_attr
#' @importFrom glue glue
#' @examples
#' # Get team offensive and defensive efficiency from 2020 season
#' scrape_fpi(2020, stat = "EFF")

scrape_fpi <- function(season = 2020, stat = "FPI"){

  # season must be >= 2015
  current_year <- str_sub(Sys.Date(), 1, 4) %>% as.integer()

  if (between(season, 2015, current_year) == FALSE)
    stop(glue::glue("Season must be between 2015 and {current_year}"))

  if (!(stat %in% c("FPI", "EFF", "PROJ")) == TRUE)
    stop(glue::glue("Stat must be one of 'FPI', 'EFF' or 'PROJ'"))

  message(glue::glue("Scraping {stat} for {season}!"))

  url_fpi <- glue::glue("https://www.espn.com/nfl/fpi/_/season/{season}")
  url_proj <- glue::glue("https://www.espn.com/nfl/fpi/_/view/projections/season/{season}")
  url_eff <- glue::glue("https://www.espn.com/nfl/fpi/_/view/efficiencies/season/{season}")

  if (stat == "FPI"){
    fpi_html <- read_html(url_fpi)

    fpi_names <- c(
      "team", "w_l", "fpi", "rk", "trend", "off",
      "def", "st", "sos", "rem_sos", "avgwp"
    )

    trend_data <- fpi_html %>%
      html_nodes("td:nth-child(4) > div") %>%
      html_attr("class")

    table_fpi <- suppressWarnings(
      fpi_html %>%
        html_table() %>%
        cbind.data.frame() %>%
        set_names(nm = fpi_names) %>%
        slice(-1) %>%
        mutate(
          across(c(fpi, off:st), as.double),
          across(c(rk, trend, sos:avgwp), as.integer),
          season = season
        ) %>%
        mutate(
          trend = if_else(
            trend_data[rk] == "trend negative",
            as.integer(trend * -1),
            trend)
        ) %>%
        select(season, everything()) %>%
        tibble()
    )

    table_fpi

  } else if (stat == "PROJ"){
    proj_html <- read_html(url_proj)

    table_proj <- proj_html %>%
      html_table() %>%
      bind_cols()  %>%
      set_names(nm = names(.) %>%
                  tolower() %>%
                  str_replace_all("-| ", "_") %>%
                  str_replace("%", "_pct")) %>%
      mutate(season = season) %>%
      select(season, everything()) %>%
      tibble()

    table_proj
  } else if (stat == "EFF"){
    eff_html <- read_html(url_eff)

    eff_names <- c(
      "team", "w_l", "eff_ove", "rnk_ove", "eff_off", "rnk_off",
      "eff_def", "rnk_def", "eff_spe", "rnk_spe"
    )

    table_eff <-
      eff_html %>%
      html_table() %>%
      cbind.data.frame() %>%
      set_names(nm=eff_names) %>%
      slice(-1) %>%
      mutate(
        across(contains("rnk"), as.integer),
        across(contains("eff"), as.double),
        season = season
      ) %>%
      select(season, everything()) %>%
      tibble()

    table_eff
  }

}
