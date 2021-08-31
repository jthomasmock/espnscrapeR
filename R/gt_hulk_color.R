#' Apply 'hulk' palette to specific columns in a gt table. The hulk names comes from the idea of a divergin purple and green theme that is colorblind safe and visually appealing. It is a useful alternative to the red/green palette.
#'
#' @param gt_object An existing gt table object
#' @param ... columns to apply color to
#' @param trim trim the palette to give less intense maximal colors
#' @return Returns a gt table
#' @importFrom dplyr %>%
#' @importFrom scales col_numeric
#' @export
#' @import gt
#' @examples
#'  # basic use
#'  mtcars |>
#'    head() |>
#'    gt::gt() |>
#'    gt_hulk_color(mpg)
#'
#'  mtcars |>
#'    head() |>
#'    gt::gt() |>
#'    # trim gives small range of colors
#'    gt_hulk_color(mpg:disp, trim = TRUE)
#'
#'  # option to reverse the color palette
#'  mtcars |>
#'    head() |>
#'    gt::gt() |>
#'    # trim gives small range of colors
#'    gt_hulk_color(mpg:disp, rev = TRUE)


gt_hulk_color <- function(gt_object, ..., domain = NULL, trim = FALSE, rev = TRUE){

  pal_hex <- c(
    "#762a83", "#af8dc3", "#e7d4e8",
    "#f7f7f7", "#d9f0d3", "#7fbf7b", "#1b7837"
    )

  pal_hex <- if(isTRUE(trim)){
    pal_hex[2:6]
  }else{
    pal_hex
  }

  pal_hex <- if(isTRUE(rev)){
    rev(pal_hex)
  }else{
    pal_hex
  }

  hulk_pal <- function(x){
    scales::col_numeric(
      pal_hex,
      domain = domain
    )(x)
  }

  gt::data_color(
    gt_object,
    columns = ...,
    colors = hulk_pal
                 )

}

