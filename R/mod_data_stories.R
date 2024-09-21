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
      div(
        openMenu(),
        div(
          class = "data-stories-head",
          div(
            class = "row",
            div(
              class = "col-sm-9",
              align = "left",
              h1("Data Stories"),
              hr(class = "header-hr")
            ),
            div(
              class = "col-sm-3 findings-align",
              div(class = "search-input",
                  textInput(ns("search"), NULL, placeholder = "Search tags"),
                  style = "margin-right: 5%;"
              )
            )
          )
        ),
        div(
          class = "data-stories-content",
          div(
            class = "row",
            uiOutput(ns("cards_output"))
          ),
          div(
            style = "padding-top: 10%",
            uiOutput(ns("full_page_content"))
          )
        )
      )
    )
  )
}

#' data_stories Server Functions
#'
#' @noRd
mod_data_stories_server <- function(id){
  moduleServer(id, function(input, output, session){
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

    cards_displayed <- reactive({
      list(
        list(
          thumbnail = card_thumbnail(
            image = "www/small_town.jpeg",
            date = "08/01/2024"
          ),
          title = actionLink(ns("story_link"), strong("This is the title of the first story", style = "font-size: 20px;")),
          description =
            "This is an amazing story, you have to check it out! It
            is about rural health.",
          tags = list(
            actionButton(ns("tag"), "rural health", class = "my-button"),
            actionButton(ns("tag"), "finance", class = "my-button"),
            actionButton(ns("tag"), "trends", class = "my-button")
          )
        ),
        list(
          thumbnail = card_thumbnail(
            image = "www/small_town.jpeg",
            date = "08/01/2024"
          ),
          title = actionLink(ns("story_link"), strong("This is the title of the second story", style = "font-size: 20px;")),
          description =
            "Wow, such interesting information in this story about
            urban health versus rural health in local agencies.",
          tags = list(
            actionButton(ns("tag"), "urban health", class = "my-button"),
            actionButton(ns("tag"), "rural health", class = "my-button")
          )
        ),
        list(
          thumbnail = card_thumbnail(
            image = "www/small_town.jpeg",
            date = "08/01/2024"
          ),
          title = actionLink(ns("story_link"), strong("This is the title of the third story", style = "font-size: 20px;")),
          description =
            "Oh boy, this is a really good story about some things and
            you should read it.",
          tags = list(
            actionButton(ns("tag"), "workforce", class = "my-button")
          )
        )
      )
    })

    # Extract unique tags for the dropdown menu
    observe({
      unique_tags <- unique(unlist(lapply(cards_displayed(), function(card) {
        sapply(card$tags, function(tag) tag$children[[1]])
      })))
      updateSelectInput(session, "search", choices = c("All", unique_tags))
    })

    filtered_cards <- reactive({
      if (input$search == "All" || input$search == "") {
        return(cards_displayed())
      } else {
        search_term <- tolower(input$search)
        card_list <- lapply(cards_displayed(), function(card) {
          if (any(sapply(card$tags, function(tag) grepl(search_term, tolower(tag$children[[1]]))))) {
            return(card)
          } else {
            return(NULL)
          }
        })
        card_list[!sapply(card_list, is.null)]
      }
    })

    # display the cards -------------------------------------------------------
    output$cards_output <- renderUI({
      card_list <- filtered_cards()

      # if no card is selected, the "cards" will display
      if (is.null(card_content())) {
        if (length(card_list) == 0) {
          return(h3("No stories with this tag"))
        }

        lapply(card_list, function(card) {
          div(class = "col-xl-4 col-lg-6 col-md-6 col-sm-12",
              div(
                card$thumbnail,
                card$title,
                div(
                  p(card$description),
                  class = "scroll-desc"
                ),
                do.call(tagList, card$tags)
              )
          )
        })
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
              ns("back_btn"),
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
