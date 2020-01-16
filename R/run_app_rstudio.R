#' Run the Shiny app as a background job
#' 
#' @export
run_app_rstudio <- function() {
  pathTemp <- tempfile(fileext = ".R")
  
  writeLines(text = "bnvd::run_app()", con = pathTemp)
  
  rstudioapi::jobRunScript(path = pathTemp, encoding = "UTF-8")
}
