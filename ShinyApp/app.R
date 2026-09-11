# =============================================================================
# BVD Alerts Dashboard — Launch Script
# =============================================================================
# Entry point for launching the Shiny app. Sources ui.R and server.R,
# then creates the shinyApp object.
#
# Launch with:
#   shiny::runApp(".")
# or from R console:
#   shiny::runApp("/path/to/ShinyApp")
# =============================================================================

# Source UI and server definitions
shiny_dir <- if (file.exists("ui.R")) "." else if (dir.exists("ShinyApp")) "ShinyApp" else "."
withr::with_dir(shiny_dir, {
  source("ui.R", local = FALSE)
  source("server.R", local = FALSE)
})

# Create and return the Shiny app object
shinyApp(ui = ui, server = server)
