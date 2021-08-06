
<!-- README.md is generated from README.Rmd. Please edit that file -->

# espnscrapeR

<!-- badges: start -->
<!-- badges: end -->

The goal of espnscrapeR is to collect or scrape QBR, NFL standings, and
stats from ESPN.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jthomasmock/espnscrapeR")
```

## Example

``` r
library(espnscrapeR)
```

``` r
# Get NFL QBR for the 2019 regular season week 4
get_nfl_qbr("2019", season_type = "Regular", week = 4)
#> Scraping weekly QBR for week 4 of 2019!
#> # A tibble: 31 × 29
#>    season season_type game_id   game_week week_text team_abb player_id name_short
#>    <chr>  <chr>       <chr>         <int> <chr>     <chr>    <chr>     <chr>     
#>  1 2019   Regular     401128069         4 Week 4    TB       2969939   J. Winston
#>  2 2019   Regular     401127863         4 Week 4    PHI      2573079   C. Wentz  
#>  3 2019   Regular     401128109         4 Week 4    PIT      3116407   M. Rudolph
#>  4 2019   Regular     401128055         4 Week 4    LAC      5529      P. Rivers 
#>  5 2019   Regular     401128093         4 Week 4    CHI      12471     C. Daniel 
#>  6 2019   Regular     401128102         4 Week 4    DAL      2577417   D. Presco…
#>  7 2019   Regular     401128023         4 Week 4    DET      12483     M. Staffo…
#>  8 2019   Regular     401128097         4 Week 4    DEN      11252     J. Flacco 
#>  9 2019   Regular     401127863         4 Week 4    GB       8439      A. Rodgers
#> 10 2019   Regular     401128018         4 Week 4    CLE      3052587   B. Mayfie…
#> # … with 21 more rows, and 21 more variables: rank <dbl>, qbr_total <dbl>,
#> #   pts_added <dbl>, qb_plays <dbl>, epa_total <dbl>, pass <dbl>, run <dbl>,
#> #   exp_sack <dbl>, penalty <dbl>, qbr_raw <dbl>, sack <dbl>, name_first <chr>,
#> #   name_last <chr>, name_display <chr>, headshot_href <chr>, team <chr>,
#> #   opp_id <chr>, opp_abb <chr>, opp_team <chr>, opp_name <chr>, week_num <int>
```

``` r
# Get NFL standings for 2010
get_nfl_standings(2010)
#> Returning 2010
#> # A tibble: 32 × 28
#>    conf  season team_id team_location team_name team_abb team_full  team_logo   
#>    <chr>  <int> <chr>   <chr>         <chr>     <chr>    <chr>      <chr>       
#>  1 AFC     2010 17      New England   Patriots  NE       New Engla… https://a.e…
#>  2 AFC     2010 23      Pittsburgh    Steelers  PIT      Pittsburg… https://a.e…
#>  3 AFC     2010 33      Baltimore     Ravens    BAL      Baltimore… https://a.e…
#>  4 AFC     2010 20      New York      Jets      NYJ      New York … https://a.e…
#>  5 AFC     2010 11      Indianapolis  Colts     IND      Indianapo… https://a.e…
#>  6 AFC     2010 12      Kansas City   Chiefs    KC       Kansas Ci… https://a.e…
#>  7 AFC     2010 24      San Diego     Chargers  SD       San Diego… https://a.e…
#>  8 AFC     2010 13      Oakland       Raiders   OAK      Oakland R… https://a.e…
#>  9 AFC     2010 30      Jacksonville  Jaguars   JAX      Jacksonvi… https://a.e…
#> 10 AFC     2010 15      Miami         Dolphins  MIA      Miami Dol… https://a.e…
#> # … with 22 more rows, and 20 more variables: seed <int>, wins <dbl>,
#> #   losses <dbl>, win_pct <dbl>, g_behind <dbl>, ties <dbl>, pts_for <dbl>,
#> #   pts_against <dbl>, pts_diff <dbl>, streak <dbl>, div_ties <dbl>,
#> #   record <chr>, home_wins <dbl>, home_losses <dbl>, away_wins <dbl>,
#> #   away_losses <dbl>, conf_wins <dbl>, conf_losses <dbl>, div_wins <dbl>,
#> #   div_losses <dbl>
```

``` r
# Get NFL 
scrape_espn_stats(2019, stat = "rushing")
#> Scraping rushing stats from 2019 Regular season!
#> # A tibble: 335 × 17
#>    season season_type rush_rank name           team  pos   games_played rush_att
#>     <dbl> <chr>           <int> <chr>          <chr> <chr>        <int>    <int>
#>  1   2019 Regular             1 Derrick Henry  TEN   RB              15      303
#>  2   2019 Regular             2 Nick Chubb     CLE   RB              16      298
#>  3   2019 Regular             3 Christian McC… CAR   RB              16      287
#>  4   2019 Regular             4 Ezekiel Ellio… DAL   RB              16      301
#>  5   2019 Regular             5 Chris Carson   SEA   RB              15      278
#>  6   2019 Regular             6 Lamar Jackson  BAL   QB              15      176
#>  7   2019 Regular             7 Leonard Fourn… JAX   RB              15      265
#>  8   2019 Regular             8 Josh Jacobs    OAK   RB              13      242
#>  9   2019 Regular             9 Joe Mixon      CIN   RB              16      278
#> 10   2019 Regular            10 Dalvin Cook    MIN   RB              14      250
#> # … with 325 more rows, and 9 more variables: rush_yards <dbl>, rush_avg <dbl>,
#> #   rush_long <int>, rush_20plus <int>, rush_td <int>, rush_yards_game <dbl>,
#> #   fumble <int>, fumble_lost <int>, rush_first_down <int>
```

``` r
# Get college QBR for 2014 week 5
get_college_qbr(season = 2014, type = "weekly")
#> Scraping QBR for all weeks of 2014!
#> # A tibble: 1,610 × 35
#>    season  week week_text week_type player_id player_uid  player_guid name_first
#>     <int> <int> <chr>     <chr>     <chr>     <chr>       <chr>       <chr>     
#>  1   2014     1 Week 1    Regular   533208    s:20~l:23~… d5b378f113… Cole      
#>  2   2014     1 Week 1    Regular   515409    s:20~l:23~… 0467fbf0ba… Everett   
#>  3   2014     1 Week 1    Regular   551184    s:20~l:23~… c268152939… Justin    
#>  4   2014     1 Week 1    Regular   513329    s:20~l:23~… 3c75884248… Cody      
#>  5   2014     1 Week 1    Regular   511459    s:20~l:23~… 33be1f4ad8… Marcus    
#>  6   2014     1 Week 1    Regular   533270    s:20~l:23~… 30a818d641… Tommy     
#>  7   2014     1 Week 1    Regular   548240    s:20~l:23~… 26507c0b44… Tyler     
#>  8   2014     1 Week 1    Regular   511552    s:20~l:23~… ef65357cb9… Derrius   
#>  9   2014     1 Week 1    Regular   504866    s:20~l:23~… 61b92d9914… Brandon   
#> 10   2014     1 Week 1    Regular   482594    s:20~l:23~… b4348fe9fd… Taysom    
#> # … with 1,600 more rows, and 27 more variables: name_last <chr>,
#> #   name_display <chr>, name_short <chr>, team_name <chr>,
#> #   team_short_name <chr>, slug <chr>, team_id <chr>, team_uid <chr>,
#> #   age <int>, headshot_href <chr>, game_id <chr>, game_date <chr>,
#> #   player_home_away <chr>, score <chr>, opp_team_id <chr>,
#> #   opp_team_name <chr>, opp_team_short_name <chr>, qbr_total <dbl>,
#> #   pts_added <dbl>, qb_plays <dbl>, epa_total <dbl>, pass <dbl>, run <dbl>, …
```

``` r
# Get NFL teams with logos, colors, alternatives, etc
get_nfl_teams()
#> Getting NFL teams!
#> # A tibble: 32 × 8
#>    team_id team_name team_nickname team_abb team_full_name     team_color
#>    <chr>   <chr>     <chr>         <chr>    <chr>              <chr>     
#>  1 22      Cardinals Arizona       ARI      Arizona Cardinals  #A40227   
#>  2 1       Falcons   Atlanta       ATL      Atlanta Falcons    #000000   
#>  3 33      Ravens    Baltimore     BAL      Baltimore Ravens   #2B025B   
#>  4 2       Bills     Buffalo       BUF      Buffalo Bills      #04407F   
#>  5 29      Panthers  Carolina      CAR      Carolina Panthers  #2177B0   
#>  6 3       Bears     Chicago       CHI      Chicago Bears      #152644   
#>  7 4       Bengals   Cincinnati    CIN      Cincinnati Bengals #FF2700   
#>  8 5       Browns    Cleveland     CLE      Cleveland Browns   #4C230E   
#>  9 6       Cowboys   Dallas        DAL      Dallas Cowboys     #002E4D   
#> 10 7       Broncos   Denver        DEN      Denver Broncos     #002E4D   
#> # … with 22 more rows, and 2 more variables: team_alt_color <chr>, logo <chr>
```
