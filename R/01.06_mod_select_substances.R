# Module UI
  
#' @title   mod_select_substances_ui and mod_select_substances_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_select_substances
#'
#' @keywords internal
#' @export 
#' @import dplyr
#' @import shiny
mod_select_substances_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("substanceSelector"))
  )
}
    
# Module Server
    
#' @rdname mod_select_substances
#' @export
#' @keywords internal
    
mod_select_substances_server <- function(input, output, session, data, departement, classe_substance){
  
  n_top <- reactive({
    filter(data, departement %in% departement(), classification %in% classe_substance()) %>% 
      pull(substance) %>% 
      n_distinct()
  })
  
  output$substanceSelector <- renderUI({
    ns <- session$ns
    
    if (n_top() == 0) {
      HTML("<p style=\"color:grey; font-size:24px\"><br>Pas de données de ventes de substances pesticides</p>")
    } else {
      if (n_top() == 1) {
        HTML("<p style=\"color:#39a9dc; font-size:24px\"><br>Une seule substance représente plus de 50% des ventes</p>")
      } else {
        sliderInput(inputId = ns("nb_subst"), 
                    label   = "Nombre de substances à représenter",
                    value   = 5, min = 1, max = n_top(), step = 1, ticks = FALSE)
      }
    }
  })
  
  reactive(input$nb_subst)
  
}
    
## To be copied in the UI
# mod_select_substances_ui("select_substances_ui_1")
    
## To be copied in the server
# callModule(mod_select_substances_server, "select_substances_ui_1")
 
