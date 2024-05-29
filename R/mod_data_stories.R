#' data_stories UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_data_stories_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'data_stories_tab'",
      flexlayout::flexlayout(
        left = NULL,
        center = div(
          openMenu(),
          div(
            class = "key-findings-head",
            uiOutput(ns("cards_output")),
            uiOutput(ns("full_page_content"))
          )
        ),
        right = div()
      )
    )
  )
}

#' data_stories Server Functions
#'
#' @noRd
mod_data_stories_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


############################## CARD SCRIPTS HERE ###############################

    # card list ---------------------------------------------------------------
    card_data <- list(
      "story_one" = list(
        content =
          div(
            p("this is the content") # add module here
          )
      )
    )

########################### END EDITING CARD SCRIPTS ###########################




    # value for card content --------------------------------------------------
    card_content <- reactiveVal(NULL)


    # open card content -------------------------------------------------------
    selected_card <- function(input_id, card) {
      card_content(card_data[[input_id]]$content)
      updateNavbarPage(inputId = "tabs",
                       selected = "data_stories_tab")
    }


    # back button scroll to top -----------------------------------------------
    observeEvent(input$back_btn, {
      shinyjs::runjs("window.scrollTo(0, 0)")
    })


############################# ADD NEW CARDS HERE ###############################

    # events to "open" the card -----------------------------------------------
    observeEvent(input$story_link, selected_card("story_one"))

    # when you want to add a new card, you need to add the line of code above,
    # which causes the card to open when the link is selected



    # display the cards -------------------------------------------------------
    output$cards_output <- renderUI({

        # if no card is selected, the "cards" will display
        if (is.null(card_content())) {
          div(
            div(
              class = "row",
              div(
                class = "col-xl-9",
                h1("Data Stories")
              ),
              div(
                class = "col-xl-3",
                align = "left",
                textInput(ns("search"), "Search cards"),
              )
            ),

            # first row of cards
            div(
              class = "row",
              style = "padding-top: 5%;",
              div(
                class = "col-xl-4",
                card_function(
                  tags = 2,
                  link = ns("story_link"),
                  image_file = "www/small_town.jpeg",
                  date = "08/01/2024",
                  tag_one = "rural",
                  tag_two = "finance"
                )
              ),
              div(
                class = "col-xl-4",
                card_function(
                  tags = 2,
                  link = ns("story_link"),
                  image_file = "www/small_town.jpeg",
                  date = "08/01/2024",
                  tag_one = "informatics",
                  tag_two = "workforce"
                )
              ),
              div(
                class = "col-xl-4",
                card_function(
                  tags = 3,
                  link = ns("story_link"),
                  image_file = "www/small_town.jpeg",
                  date = "08/01/2024",
                  tag_one = "urban",
                  tag_two = "retention",
                  tag_three = "workforce"
                )
              )
            )
          )
        }
    })


    # display card content upon selection -------------------------------------
    output$full_page_content <- renderUI({

      if (!is.null(card_content())) {

        div(
          div(
            actionButton( # this sends you back to the top
              ns("back_to_stories"),
              "Back",
              icon = icon("arrow-left"),
              class = "top_button"
            ),
            id = "top_fixed_button"
          ),

          p(card_content()),
          div(
            actionButton( # this sends you back to the top
              "back_btn",
              "Top",
              icon = icon("arrow-up"),
              class = "side_button"
            ),
            id = "fixed_button"
          )
        )
      }
    })


    # reset card content ------------------------------------------------------
    observeEvent(input$back_to_stories, {
      card_content(NULL) # Reset the selected card content
    })

  })
}

## To be copied in the UI
# mod_data_stories_ui("data_stories_1")

## To be copied in the server
# mod_data_stories_server("data_stories_1")
