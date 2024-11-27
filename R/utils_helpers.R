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
      # print(selected_inputs)
  }) %>% debounce(debounce_time)
}




#' Apply str_to_title to all but "All LHDs:
#'
#' @param text the string or strings to apply function to
#'
#' @return formatted strings
#' @export
#'
custom_str_to_title <- function(text) {
  if (text == "All LHDs") {
    return(text)  # Return the text as is
  } else {
    return(stringr::str_to_title(text))  # Apply str_to_title to other text
  }
}


#' Reusable Right Button for Collapsible Pane
#'
#' @description A predefined button for toggling the right pane in a `flexlayout` structure.
#' This button is styled with a Font Awesome "info" icon and can be used across multiple Shiny modules.
#'
#' @details The `right_button` is a pre-configured `flexlayout::offcanvas_button` component
#' that displays a Font Awesome "info" icon (`fa-circle-info`). It is styled with a font size of 2em
#' and a custom color (`#3C8A97`). This variable can be sourced and reused wherever a collapsible
#' right pane toggle is required in a Shiny app.
#'
#' @format An `offcanvas_button` object created using the `flexlayout` package.
#'
#' @usage
#' # To use the predefined right_button:
#' flexlayout(
#'   left = NULL,
#'   center,
#'   right = NULL,
#'   right_button = right_button,
#'   left_width = 22,
#'   center_width = 56,
#'   right_width = 22
#' )
#'
#' @examples
#' \dontrun{
#' # Example usage in a flexlayout structure
#' library(flexlayout)
#' ui <- fluidPage(
#'   flexlayout(
#'     center = div("Main content"),
#'     right_button = right_button
#'   )
#' )
#'
#' server <- function(input, output, session) {}
#' shinyApp(ui, server)
#' }
#'
#' @seealso [flexlayout::offcanvas_button]
#'
#' @export
right_button <- flexlayout::offcanvas_button(
  tags$i(
    class = "fa-solid fa-info", # Font Awesome info icon
    style = "font-size: 3em; color: #3C8A97;"
  )
)


#' Custom Rounding Function: Round .5 Up
#'
#' This function rounds numeric values to the specified number of decimal places.
#' Unlike the default `round` function in R, this function ensures that numbers
#' with a fractional part of exactly 0.5 are always rounded up.
#'
#' @param x A numeric vector to be rounded.
#' @param digits An integer specifying the number of decimal places to round to.
#'   Defaults to 0. Negative values are allowed, which round to powers of ten.
#'
#' @return A numeric vector with values rounded to the specified number of decimal places.
#'
#' @examples
#' # Default rounding behavior
#' x <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
#' round(x)        # Default R rounding: -2 -2  0  0  2  2
#' round_up(x)     # Custom rounding: -2 -1  0  1  2  3
#'
#' # Rounding with decimal places
#' y <- c(1.45, 1.55, 1.65)
#' round(y, 1)     # Default R rounding: 1.4 1.6 1.6
#' round_up(y, 1)  # Custom rounding: 1.5 1.6 1.7
#'
#' # Rounding with negative digits (to powers of ten)
#' z <- c(135, 145, 155)
#' round(z, -1)    # Default R rounding: 140 140 160
#' round_up(z, -1) # Custom rounding: 140 150 160
#'
#' @export

round_up <- function(x, digits = 0) {
  multiplier <- 10^digits
  adjusted <- x * multiplier
  rounded <- ifelse(adjusted %% 1 == 0.5, ceiling(adjusted), round(adjusted))
  rounded / multiplier
}
