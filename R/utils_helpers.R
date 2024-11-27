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
