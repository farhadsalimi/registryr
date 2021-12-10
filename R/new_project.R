# this was copied from https://github.com/ledbettc/CIDAtools/blob/master/R/NewProject.R and modified
proj_setup <- function(path, ...){
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dots <- list(...)
  attach(dots)
  on.exit(detach(dots))
  ProjectName <- paste0(path)

  ### Setup ReadMe Files ----
  readme <- c(paste0("# ", ProjectName, "  "),
              paste0("**PI:**", PI, "  "),
              paste0("**Analyst**:", analyst, "  "),
              "",
              "Details about the folders:",
              '',
              "File | Description",
              "---|----------------------------------------------------------",
              "adhoc_req | contains the files (scripts, rmds, etc) that are not related to the main aims of the project",
              "assets | contains the scope of work and other relevant documents",
              "functions | contains all R files with functions",
              "plots | contains all the plots produced",
              "produced_data | contains the processed data used for analysis",
              "raw_data | contain all raw data provided by data custodians",
              "reports | contains all rmarkdown generated reports",
              "rmds | contains all rmarkdown files for this project",
              "rscripts | contains the scripts in R, used when there is too much code to fit in an rmarkdown",
              "tables | contains all the tables produced",
              "run_all.R | this file controls all the rmd files via registryr::render_report function or a project specific function"
              )


  # write to readme file
  writeLines(paste0(readme, collapse = '\n'),
             con = file.path(path, "ReadMe.md"))

  # readme files
  create_readme(path = path)

  # run_all file
  quoted_expr <-quote(registryr::render_report("1_cleaning.Rmd"))
  code_representation <- deparse(quoted_expr, backtick = TRUE)
  writeLines(code_representation, file.path(path, "run_all.R"))

  ### Create Meta File ----
  # for project info
  if(meta){
    dir.create(paste0(path, '/.ProjData'))
    ProjData <- list(ProjectName = ProjectName, PI = PI, analyst = analyst)
    write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  }

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
  ### Setup Git ----

  # add to current gitignore if exists
  if(file.exists(file.path(path, '.gitignore'))){
    gitignore <- readLines(con = file.path(path, '.gitignore'))
  } else {
    gitignore <- NULL
  }
  # add R template gitignore
  # (source: https://github.com/github/gitignore/blob/master/R.gitignore)
  gitignore <- paste0(c(gitignore,
                        "# History files",
                        ".Rhistory",
                        ".Rapp.history",

                        "# Session Data files",
                        ".RData",

                        "# User-specific files",
                        ".Ruserdata",

                        "# Example code in package build process",
                        "*-Ex.R",

                        "# Output files from R CMD build",
                        "/*.tar.gz",

                        "# Output files from R CMD check",
                        "/*.Rcheck/",

                        "# RStudio files",
                        ".Rproj.user/",

                        "# produced vignettes",
                        "vignettes/*.html",
                        "vignettes/*.pdf",

                        paste0("# OAuth2 token, see https://github.com/",
                               "hadley/httr/releases/tag/v0.3"),
                        ".httr-oauth",

                        "# knitr and R markdown default cache directories",
                        "/*_cache/",
                        "/cache/",

                        "# Temporary files created by R markdown",
                        "*.utf8.md",
                        "*.knit.md"), collapse = '\n')

  # by file type
  if(nodata == 'By File Type'){
    gitignore <- paste0(c(gitignore,
                          "# R Data files",
                          "*.RData",
                          "*.rda",
                          "*.rdata",
                          "*.rda",
                          "# Text files",
                          "*.csv",
                          "*.txt",
                          "*.dat",
                          "# Excel",
                          "*.xls*",
                          "# SAS",
                          "*.sas7bdat",
                          "*.xport",
                          "# Access",
                          "*.mdb"), collapse = '\n')
  }

  # by Folder
  if(nodata == "By Location"){
    gitignore <- paste0(c(gitignore,
                          "raw_data/*",
                          "produced_data/*",
                          "!*/ReadMe.md"), collapse = '\n')

  }


  # git initialize
  if(git_init){
    if (!requireNamespace('git2r', quietly = T)) {
      warning('git2r is required for git initialization')
    } else{
      tryCatch({
        writeLines(gitignore, con = file.path(path, '.gitignore'))
        repo <- git2r::init(path)
        if(remote_origin != '') git2r::remote_add(repo, 'origin', remote_origin)
        if(initcommit) {
          git2r::add(repo, 'ReadMe.md')
          git2r::commit(repo, message = 'Initial Commit')
          if(remote_origin != '') {
            system(paste('cd', path, '&& git push -u origin master'))
          }
        }
      }, error = function(e){
        paste0('There was an error setting up the git repo',
               e)
      })
    }
  }



}
