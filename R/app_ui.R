#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'

theme <- bslib::bs_theme(
  version = 5L,
  bootswatch = "lux",
  base_font = bslib::font_google("Heebo", local = FALSE, wght = "300..800"),
  bg = "#fff",
  fg = "#212F5B",
  primary = "#008A96",
  secondary = "#212F5B",
  success = "#80A14E",
  info = "#80648E",
  warning = "#AB6143"
)


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    bslib::page_sidebar(
      theme = theme,
      window_title = "NACCHO Profile Study",
      sidebar = bslib::sidebar(
        title = div(
          img(
            src = "www/logo.png",
            height = 120,
            style = "position: absolute; left: 19%; top: 0;")
        ),

        tabsetPanel(
          id = "tabs",

          tabPanel(
            "Key Findings",
            value = "key_findings_tab"
          ),

          tabPanel(
            "Data Stories",
            value = "data_stories_tab"
          ),

          tabPanel(
            "Workforce",
            value = "workforce_tab"
          ),

          tabPanel(
            "Finance",
            value = "finance_tab"
          ),

          tabPanel(
            "Services",
            value = "services_tab"
          ),

          tabPanel(
            "Policy",
            value = "policy_tab"
          ),

          tabPanel(
            "Topics",
            value = "topics_tab"
          ),

          tabPanel(
            actionButton("playground_button", "Data Playground", class = "pill-button"),
            value = "playground_tab"
          )
        )
      ),

      div(
        class = "row",
        div(
          class = "col-sm-9",
          h4("Column 1"),
          p("Content for column 1")
        ),
        div(
          class = "col-sm-3",
          div(
            class = "row color-block",
            div(
              class = "col-sm-4",
              br(),
              actionLink(
                inputId = "about",
                p("About", style = "font-size: 16px; font-weight: 700;"),
                icon = icon("circle-info"),
                class = "link-no-underline"
              )
            ),
            div(
              class = "col-sm-4",
              br(),
              actionLink(
                inputId = "context",
                p("Context", style = "font-size: 16px; font-weight: 700;"),
                icon = tags$i(class = "fa-solid fa-list"),
                class = "link-no-underline"
              )
            ),
            div(
              class = "col-sm-4",
              br(),
              actionLink(
                inputId = "more",
                p("More", style = "font-size: 16px; font-weight: 700;"),
                icon = tags$i(class = "fa-solid fa-key"),
                class = "link-no-underline"
              )
            )
          ),
          br(),
          div(
            style = "padding-left: 20%;",
            shinyWidgets::materialSwitch(
              inputId = "color_palette",
              p(
                HTML("<b>Enable Color Patters?</b> <br>
                   This allows you to distinguish columns using a pattern fill."
                ), style = "margin-bottom: -20px;"
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "nacchoProfile"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
