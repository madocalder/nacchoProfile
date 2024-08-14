#' Data Story Card Thumbnail
#'
#' @description This function creates the story thumbnails that appear on the
#'  landing page of the stories_tab.
#'
#' @param image String character representing the file name of the
#'  image to display in the card. The file should be located here: /nacchoProfile/inst/app/www
#' @param date Date class, for example, as.Date("2024/01/01")
#'
#'
#' @return returns an image and date
#' @export
card_thumbnail <- function(
    image,
    date) {
  div(
    img(src = image, height = "200px", width = "300px", style = "border-radius:50px 0px 50px 0px;"),
    p(as.Date(date, format = "%m/%d/%Y"), style = "padding-top: .6em")
  )
}





#' Show the N for a visualization
#'
#' @param variable A vector as a list of numeric values.
#'
#' @return numeric or character that represents total responses for a field.
#' @export
#'
#' @examples
#' n = c(10, 10, 10, 20)
#' show_n(n)
#'
show_n <- function(variable) {
  min <- min(variable)
  max <- max(variable)

  if (min != max) {
    paste(min, "to", max)
  } else {
    min
  }
}




#' inputs with debounce
#'
#' @param inputs One or more inputs to feed an observeEvent
#'
#' @return a reactive
#' @export
#'
take_inputs <- function(selected_inputs) {
  selected_inputs <- list(...)
  reactive({
    for(i in 1:length(selected_inputs)) {
      selected_inputs <- list(input[[i]])
    }
      print(selected_inputs)
  }) %>% debounce(debounce_time)
}



#' set colors
#'
#'@param input One or more inputs to feed an observeEvent
#'
#' @return filtering for colors
#' @export
#'
set_colors <- function(input = input) {
  if (input$filter == "urbanrural2022") {
    c(naccho.viz::naccho_blue,
      naccho.viz::naccho_green)
  } else if (input$filter %in% c("c0govcat", "c0popcat3")) {
    c(naccho.viz::naccho_blue,
      naccho.viz::naccho_green,
      naccho.viz::naccho_orange)
  } else if (input$filter == "censusregion") {
    c(naccho.viz::naccho_blue,
      naccho.viz::naccho_green,
      naccho.viz::naccho_orange,
      naccho.viz::naccho_pink)
  }
}
