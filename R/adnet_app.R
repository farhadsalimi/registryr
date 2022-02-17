#' Rund the ADNeT Shiny App
#'
#' @keywords internal
#'
#' @export
#'
adnet_app <- function(...) {
  ui <- shiny::fluidPage(
    shiny::selectInput(
      inputId = "report_name",
      label = "Which report you need?",
      choices = c("Validation", "Invoice")
    ),
    shiny::passwordInput("registry_token", "Your token for the Registry database on RedCap:"),
    shiny::passwordInput("holding_token", "Your token for the Holding database on RedCap:"),
    shiny::passwordInput("optout_token", "Your token for the Opt-out database on RedCap:"),
    shiny::downloadButton("report", "Generate report")
  )

  server <- function(input, output) {
    output$report <- shiny::downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = glue::glue("report_{input$report_name}_{Sys.Date()}.html"),
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        rmd_location <- system.file("rmd",
                                    glue::glue("{stringr::str_to_lower(input$report_name)}_shiny.Rmd"),
                                    package = "registryr")
        file.copy(rmd_location,
          tempReport,
          overwrite = TRUE
        )

        # Set up parameters to pass to Rmd document
        params <- list(
          registry_token = input$registry_token,
          holding_token = input$holding_token,
          optout_token = input$optout_token
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        shiny::withProgress(message = "Making report", value = 0, {
          rmarkdown::render(tempReport,
            output_file = file,
            params = params,
            envir = new.env(parent = globalenv())
          )
        })
      }
    )
  }

  shiny::shinyApp(ui, server)
}
