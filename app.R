# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
# library(rsvg)
nacchoProfile::run_app(options = list(port = 3000L)) # add parameters here (if any)


# pak::pak("madocalder/nacchoProfile@628b9ad0e8a7a7813384d4aca6b7766664318a88")
