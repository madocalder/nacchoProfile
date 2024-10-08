#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
#'
#'
app_ui <- function(request) {

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

  tagList(

    flexlayout::defineGlobal(width = 1300),

    # Leave this function for adding external resources
    golem_add_external_resources(),
    htmltools::htmlDependency(
      name = "fontawesome", version = "99.0",
      src = "inst/app/www/fontawesome",
      stylesheet = "css/all.min.css"
    ),

    tags$head(
      tags$script(src = "www/script.js") # Correct path if the file is in inst/app/www/
    ),

    waiter::useWaiter(),

    waiter::waiterPreloader(
      html = div(
        div(
          img(src = "www/naccho.gif", width = "70%")
        )
      )
    ),




    # Your application UI logic
    bslib::page_sidebar(
      options = list(
        sidebar_collapse_breakpoint = "md"  # Set collapse breakpoint (e.g., "sm", "md", "lg")
      ),
      theme = theme,
      sidebar = bslib::sidebar(


        tabsetPanel(
          id = "tabs",

          tabPanel(
            div(
              img(
                src = "www/logo.png",
                alt = "NACCHO logo, Home page",
                height = "120vmin",
                style = "position: absolute; left: 47px; top: 0; ")
            ),
            value = "home_tab",
            class = "no-underline"
          ),

          tabPanel(
            "Overview",
            value = "overview_tab"
          ),

          tabPanel(
            "Agency & Leadership",
            value = "landscape_tab"
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
            "Other Topics",
            value = "topics_tab"
          ),

          tabPanel(
            "Data Stories",
            value = "data_stories_tab"
          ),

          tabPanel(
            actionButton("playground_button", "Key Findings", class = "pill-button"),
            value = "key_findings_tab"
          )
        )
      ),

      mod_overview_ui("overview_1"),
      mod_home_ui("home_1"),
      mod_landscape_ui("landscape_1"),
      mod_key_findings_ui("key_findings_1"),
      mod_data_stories_ui("data_stories_1"),
      mod_workforce_ui("workforce_1"),
      mod_finance_ui("finance_1"),
      mod_services_ui("services_1"),
      mod_topic_areas_ui("topic_areas_1")


    ),

    div(
    class = "footer",
      align = "center",
      p(
        tags$a(
          href = "https://naccho.org/", "NACCHO Home", target="_blank"),
        HTML("&nbsp", " | ", "&nbsp"),
        tags$a(
          href = "https://www.naccho.org/resources/lhd-research", "NACCHO Research", target="_blank"),
        HTML("&nbsp", " | ", "&nbsp"),
        tags$a(
          href = "https://www.naccho.org/uploads/downloadable-resources/NACCHO_2022_Profile_Report.pdf", "2022 Profile Report", target="_blank"),
        HTML("&nbsp", " | ", "&nbsp"),
        tags$a(
          href = "mailto:info@naccho.org", "Contact Us"),
        HTML("&nbsp", " | ", "&nbsp"),
        actionLink(
          inputId = "citation",
          "Citation"),
        HTML("&nbsp", " | ", "&nbsp"),
          actionButton("linkedin",
                       label="",
                       class = "linkedin-button",
                       icon = icon("linkedin"),
                       onclick ="window.open('https://www.linkedin.com/showcase/naccho-research', '_blank')"),
        style = "font-size: 2vmin; font-weight: 700; color: white !important;")
      ),

    # modal for key findings download
    bsutils::modal(
      id = "modal",
      bsutils::modalHeader(
        bsutils::modalTitle(
          p(strong("Suggested Citation"))
        )
      ),
      bsutils::modalBody(
        p("2022 National Profile of Local Health Departments. National
        Association of County and City Health Officials. Washington, DC.
        (2024). Accessed from www.naccho.org/profile on [date]."
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
      app_title = "NACCHO Profile Study"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}
