# Module UI

#' @title   mod_lider_ui and mod_slider_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_slider
#'
#' @keywords internal
#' @export 
#' @import shiny
#' 
mod_slider_ui <- function(id, label = "", values, ...){
  ns <- NS(id)
  tagList(
    
    sliderInput(inputId = ns("slider"),
                label   = label,
                min = min(values), max = max(values),
                ...)
  )
}

# Module Server

#' @rdname mod_slider
#' @export
#' @keywords internal

mod_slider_server <- function(input, output, session){
  ns <- session$ns
  
  return(
    reactive({input$slider})
  )
  
}

## To be copied in the UI
# mod_slider_ui("slider_ui_1")

## To be copied in the server
# callModule(mod_slider_server, "slider_ui_1")

