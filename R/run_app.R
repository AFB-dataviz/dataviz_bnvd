#' Run the Shiny Application
#'
#' @export
#' @importFrom rmarkdown run
run_app <- function() {
  appPath <- system.file("app/dataviz_bnvd.Rmd",
                         package = "bnvd")
  
  run(appPath, render_args = list(encoding = "UTF-8"), shiny_args = list(launch.browser = TRUE))
}
