#' Create ReadMe.md files
#'
#' This function creates ADNET reports
#' file structure
#'
#'@param report Which report should be created (options are 'validation', 'invoice', 'internal')
#'@param registry_token API token for ADNET registry
#'@param holding_token API token for ADNeT Holding Database
#'@param optout API token for ADNeT Opt-out Database
#'@return
#'This function creates the desired readme files. It will not overwrite the file
#'however if it does not exists. It does not return anything.
#'@keywords ReadMe ReadMe.md
#'@export
#'
create_adnet_report <- function(report = "validation",
                          registry_token = getPass::getPass(msg = "API token for REDCap project: ADNeT Registry"),
                          holding_token = getPass::getPass(msg = "API token for REDCap project: ADNeT Holding Database"),
                          optout = getPass::getPass(msg = "API token for REDCap project: ADNeT Opt-out Database"),
                          site_name = "all") {

  if(!(report %in% c("validation", "invoice", "internal"))) stop("Unknown report, accepted options are: 'validation', 'invoice, or 'internal'")

  output_dir <- here::here("reports")
  output_file <- paste0(report, "_", Sys.Date(), ".html")

  if(report == "validation") {



    ## Get directory of report markdown template
    report_dir <- system.file("rmd/adnet/validation.rmd", package = "registryr")

    ## Render report into html
    suppressWarnings(render(
      input = report_dir,
      output_format = rmarkdown::html_document(toc = TRUE),
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(registry_token = registry_token,
                    holding_token = holding_token,
                    optout = optout)))

      ## Open report
      report_path <- path.expand(file.path(output_dir, output_file))
      browseURL(report_path)
  }

  if(report == "invoice") {



    ## Get directory of report markdown template
    report_dir <- system.file("rmd/adnet/operations_invoice.rmd", package = "registryr")

    ## Render report into html
    suppressWarnings(render(
      input = report_dir,
      output_format = rmarkdown::html_document(toc = TRUE),
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(registry_token = registry_token,
                    holding_token = holding_token,
                    optout = optout)))

    ## Open report
    report_path <- path.expand(file.path(output_dir, output_file))
    browseURL(report_path)
  }

  if(report == "internal") {

    output_file <- paste0(report,  "_", site_name, "_", Sys.Date(), ".docx")

    ## Get directory of report markdown template
    report_dir <- system.file("rmd/adnet/internal_report.rmd", package = "registryr")

    ## Render report into html
    suppressWarnings(render(
      input = report_dir,
      output_format = rmarkdown::word_document(toc = TRUE),
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(registry_token = registry_token,
                    holding_token = holding_token,
                    optout = optout,
                    site_name = site_name)))

    ## Open report
    report_path <- path.expand(file.path(output_dir, output_file))
  }


}
