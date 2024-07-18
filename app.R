# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
nacchoProfile::run_app(options = list(port = 3000L)) # add parameters here (if any)


# remotes::install_github("jbkunst/highcharter@8ff41366c8c411b497b5378d27be48617360f81f")

