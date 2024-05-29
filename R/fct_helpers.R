#' helpers
#'
#' @param image_file String character representing the file name of the
#'  image to display in the card. The file should be located here: /nacchoProfile/inst/app/www
#' @param story_header String character for the type of story, e.g., "Data Story".
#' @param date Date class, for example, as.Date("2024/01/01")
#' @param link String character, should not be changed from ns("story_link"). Required to be namespaced.
#' @param link ns to namespace
#' @param title String character for the title of story
#' @param description String character for the title of story
#' @param tags numeric, number of tags for the story between 1 and 3
#' @param tag_one String character, topic of the story
#' @param tag_two String character, topic of the story
#' @param tag_three String character, topic of the story
#'
#' @description This function creates the story thumbnails that appear on the
#'  landing page of the stories_tab.
#'
#' @return returns a thumbnail with the image of the card
#'
#' @noRd
card_function <- function(
    image_file,
    date,
    link,
    title = "This is the title of the story",
    description = "This is a description about this amazing story and you have to read it!",
    tags = 1,
    tag_one = "",
    tag_two = "",
    tag_three = "") {

  if (tags == 1) {
    div(
      align = "left",
      img(src = image_file, height = "200px", width = "300px", style = "border-radius:50px 0px 50px 0px;"),
      p(as.Date(date, format = "%m/%d/%Y"), style = "padding-top: .6em"),
      actionLink(link, title),
      p(description),
      div(actionButton(tag_one, tag_one, class = "my-button"),
          style = "padding-top: .5em"
      )
    )
  } else if (tags == 2) {
    div(
      align = "left",
      img(src = image_file, height = "200px", width = "300px", style = "border-radius:50px 0px 50px 0px;"),
      p(as.Date(date, format = "%m/%d/%Y"), style = "padding-top: .6em"),
      actionLink(link, title),
      div(actionButton(tag_one, tag_one, class = "my-button"),
          actionButton(tag_two, tag_two, class = "my-button"),
          style = "padding-top: .5em"
      )
    )

  } else {
    div(
      align = "left",
      img(src = image_file, height = "200px", width = "300px", style = "border-radius:50px 0px 50px 0px;"),
      p(as.Date(date, format = "%m/%d/%Y"), style = "padding-top: .6em"),
      actionLink(link, title),
      div(actionButton(tag_one, tag_one, class = "my-button"),
          actionButton(tag_two, tag_two, class = "my-button"),
          actionButton(tag_three, tag_three, class = "my-button"),
          style = "padding-top: .5em"
      )
    )
  }
}
