# Module UI
  
#' @title   mod_trend_plot_ui and mod_trend_plot_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_trend_plot
#'
#' @keywords internal
#' @import dplyr
#' @import shiny
#' @importFrom dygraphs dygraph dygraphOutput renderDygraph dyAxis dyLegend dyOptions
#' @importFrom shinydashboard box
#' @importFrom tidyr pivot_wider
#' @export 
mod_trend_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fillCol(mod_select_substances_ui(ns("nombre")),
            HTML("<div></div>"),
            dygraphOutput(ns("trend")),
            box(title = "\n",
                textOutput(ns("legend"))),
            width = "95%", height = "100%", 
            flex = c(.75, .05, 4, 1.5))
  )
}
    
# Module Server
    
#' @rdname mod_trend_plot
#' @export
#' @keywords internal
    
mod_trend_plot_server <- function(input, output, session, 
                                  data, departement, classe_substance,
                                  main = "", ylab = ""){
  ns <- session$ns
  
  x_formatter <- "function(value){return value.toFixed(0);}"
  
  y_formatter_axis <- "function(value) {
    return value.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \" \");
}"
  y_formatter_value <- "function(value) {
    return value.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \" \") + \" tonnes\";
}"
  
  y_formatter_axis_small <- "function(value) {
    return value.toFixed(1).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \" \");
}"
  y_formatter_value_small <- "function(value) {
    return value.toFixed(1).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \" \") + \" tonnes\";
}"
  
  nombre_substance <- callModule(mod_select_substances_server, "nombre",
             data = data, departement = departement, classe_substance)

  output$trend <- renderDygraph({
    
    data_plot <- filter(data, departement %in% departement(),
                        classification %in% classe_substance(),
                        rang <= nombre_substance()) %>% 
      arrange(rang)
    
    substances <- pull(data_plot, substance) %>% 
      unique()
    
    quantity_max <- pull(data_plot, quantite) %>% 
      max()
    
    data_plot <- data_plot %>% 
      select(annee, substance, quantite) %>% 
      pivot_wider(names_from = substance, 
                  values_from = quantite, 
                  values_fill = list(quantite = 0)) %>% 
      select_at(c("annee", substances)) %>% 
      arrange(annee)

    if (nrow(data_plot) > 0) {
      trend_plot <- dygraph(data = data_plot,
              main = main,
              ylab = ylab) %>% 
        dyAxis("x", drawGrid = FALSE, axisLabelFormat = x_formatter) %>% 
        dyOptions(stackedGraph = TRUE, connectSeparatedPoints = TRUE,
                  sigFig = 0) %>% 
        dyLegend(show = "always", labelsDiv = ns("legend"),
                 labelsSeparateLines = TRUE)
      
      if (quantity_max <= 5) {
        trend_plot %>% 
          dyAxis("y", drawGrid = FALSE, axisLabelWidth = 75, 
                 valueRange = c(0, 1.1 * max(rowSums(select(data_plot, -1)))),
                 axisLabelFormatter = y_formatter_axis_small, 
                 valueFormatter = y_formatter_value_small)
      } else {
        trend_plot %>% 
          dyAxis("y", drawGrid = FALSE, axisLabelWidth = 75, 
                 valueRange = c(0, 1.1 * max(rowSums(select(data_plot, -1)))),
                 axisLabelFormatter = y_formatter_axis, 
                 valueFormatter = y_formatter_value) 
          
      }
      
    } else {
      NULL
    }
  })
}
    
## To be copied in the UI
# mod_trend_plot_ui("trend_plot_ui_1")
    
## To be copied in the server
# callModule(mod_trend_plot_server, "trend_plot_ui_1")
 
