#' helpers
#'
#' @param image_file String character representing the file name of the
#'  image to display in the card. The file should be located here: /nacchoProfile/inst/app/www
#' @param date Date class, for example, as.Date("2024/01/01")
#'
#' @description This function creates the story thumbnails that appear on the
#'  landing page of the stories_tab.
#'
#' @return returns an image and date
#'
#' @noRd
card_thumbnail <- function(
    image,
    date) {
  div(
    img(src = image, height = "200px", width = "300px", style = "border-radius:50px 0px 50px 0px;"),
    p(as.Date(date, format = "%m/%d/%Y"), style = "padding-top: .6em")
  )
}

