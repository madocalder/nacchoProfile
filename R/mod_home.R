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
        p("welcome to the", style = "font-size: 4vh; font-weight: 400 !important; margin-bottom: 1vh;"),
        h1(HTML("NACCHO Profile <br> Study Dashboard"), style = "font-size: 7vh;
    text-transform: none !important;
    line-height: 7.5vh;
    margin-bottom: 3vh;"),
        # br(),
        # br(),
        # br(),
        # br(),
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
                responded, for a response rate of 38%. ",
            tags$a(href="https://www.naccho.org/uploads/downloadable-resources/NACCHO-2022-Profile-Report.pdf#page=11",
                   "For a full description of the methods, read our comprehensive report.",
                   style = "text-decoration: underline !important;")
          ),

          bslib::accordion_panel(
            title = h2("Access the Data"),
            value = "cord_access",
            "Deidentified data from the 2022 Profile Study will be made
                publicly available through ICSPR's data repository in late
                2024. For access to prior years' Profile data and data use
                policies for Profile data, please visit",
            tags$a(href="https://url.us.m.mimecastprotect.com/s/kfg8C4xLl4IJ943rfxh9T4mqza?domain=icpsr.umich.edu",
                   "ICPSR's website.",
                   style = "text-decoration: underline !important;")
          ),

          bslib::accordion_panel(
            title = h2("About the Dashboard"),
            value = "cord_about_dash",
           "To navigate this dashboard, start by selecting a page from the left sidebar. Once on the selected page, choose a relevant topic from the 'Topic' dropdown menu to display an interactive visualization. The ‘Filter’ dropdown menu defaults to “All LHDs,” but when available, you can refine results by filtering for LHD characteristics such as Rurality, LHD Size, Region, or Governance.",
           br(),

"Topics may include various views, such as bar/column charts, line graphs, or tables. You can switch between these views by selecting the corresponding icons located underneath the 'Topic' and 'Filter' dropdown menus. Key takeaways, notes, and definitions are displayed in the right sidebar. On smaller screens, this sidebar may be hidden and can be shown by clicking the 'i' icon at the top-right of the screen. For some topics, additional input options may appear in the right sidebar and will toggle visible automatically.",
br(),

"Visualizations are interactive, allowing you to adjust filters directly by selecting or deselecting items within the legend at the bottom of a view. You can also download visualizations or their data by clicking the three lines in the top-right corner of each visualization. Options include exporting as a PDF, image, CSV, or XLS file, while tables export as CSV files.",
br(),

"To maximize the visualization area, you can hide the left sidebar by clicking the small arrow at the top-left of the screen."

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

            "This dashboard was developed using R Shiny. Madoc Alder Marshall
            served as the lead developer on behalf of the Association of State
            and Territorial Health Officials (ASTHO), building on the initial
            development work led by Lindsay Jorgenson during her tenure at ASTHO.
            Additional contributions were provided by John Coene and
            Pierre Saoter from the Y Company, and Elizabeth Woods from ASTHO.
            To see more visit ",

            tags$a(href="https://github.com/madalder/nacchoProfile",
                   "the nacchoProfile GitHub repository",
                   style = "text-decoration: underline !important;"),
            "."
          ),

          bslib::accordion_panel(
            title = h2("Contact and Questions"),
            value = "cord_contact",
            "For more information about the 2022 Profile Study, please
                reach out to our Research team at",
            tags$a(href="mailto: profile@naccho.org",
                   "profile@naccho.org.",
                   style = "text-decoration: underline !important;")
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
