#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'home_tab'",
      div(
        class = "home-content",
        openMenu(),
        p("welcome to the", style = "font-size: 30px; font-weight: 400 !important;"),
        h1(HTML("NACCHO Profile <br> Study Dashboard"), style = "font-size: 60px; text-transform: none !important;"),
        br(),
        br(),
        br(),
        br(),
        bslib::accordion(
          id = "cord",
          open = FALSE,
          multiple = TRUE,
          style = "padding-top: 0px;",

          bslib::accordion_panel(
            title = h2("About the Survey"),
            value = "cord_about",
            "NACCHO conducts the National Profile of Local Health
                Departments (commonly referred to as the 'Profile Study') every
                three years to describe local health department infrastructure
                and services at the national level. This study is the largest
                and most reliable data source on local health departments in the
                United States and presents a complete and accurate overview of
                local health department funding, staffing, activities, and
                partnerships. The Profile Study data show both point-in-time
                information, as well as longitudinal change since the 2008
                Profile Study."
          ),

          bslib::accordion_panel(
            title = h2("Methods"),
            value = "cord_methods",
            "For the 2022 Profile Study, a total of 2,512 local health
                departments were included in the study population. Rhode Island
                was excluded from the study because the state health agency
                operates on behalf of local public health and has no sub-state
                units. All local health departments in the study population
                received the Core questionnaire. A random sample of local
                health departments also received one of the two sets of
                supplemental questions. NACCHO administered the survey from June
                through September 2022. Overall, 942 local health departments
                responded, for a response rate of 38%. For a full description
                of the methods, read our comprehensive report at",
            tags$a(href="naccho.org/profile", "naccho.org/profile.")
          ),

          bslib::accordion_panel(
            title = h2("Access the Data"),
            value = "cord_access",
            "Deidentified data from the 2022 Profile Study will be made
                publicly available through ICSPR's data repository in summer
                2024. For access to prior years' Profile data and data use
                policies for Profile data, please visit",
            tags$a(href="icpsr.umich.edu/web/pages/ICPSR/index.html",
                   "ICPSR's website.")
          ),

          bslib::accordion_panel(
            title = h2("Funding and Development Acknowledgments"),
            value = "cord_ack",
            "The 2022 Profile Study was supported by grant
                #6NU38OT000306-03-06 awarded to the National Association of
                County and City Health Officials and funded by the Centers for
                Disease Control and Prevention, as well as grant #78802 awarded
                to the National Association of County and City Health Officials
                and funded by the Robert Wood Johnson Foundation. The contents
                of this publication are solely the responsibility of the authors
                and do not necessarily represent the official views of the
                Centers for Disease Control and Prevention, the U.S. Department
                of Health and Human Services, or the Robert Wood Johnson
                Foundation.",
            br(),
            br(),
            "This dashboard was developed using R Shiny. Lindsay Jorgenson served
            as the lead developer, with Madoc Marshall as the senior developer
            representing the Association of State and Territorial Health Officials (ASTHO).
            Additional contributions were provided by John Coene and Pierre Saoter
            from the Y Company, and Elizabeth Woods from ASTHO.
            For inquiries or to report an issue, please visit",
            tags$a(href="https://github.com/lindsayjorgenson/nacchoProfile",
                   "the nacchoProfile GitHub repository"),
            "."
          ),

          bslib::accordion_panel(
            title = h2("Contact and Questions"),
            value = "cord_contact",
            "For more information about the 2022 Profile Study, please
                reach out to our Research team at",
            tags$a(href="mailto: profile@naccho.org",
                   "profile@naccho.org.")
          )
        )
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
