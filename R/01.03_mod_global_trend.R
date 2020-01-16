# Module UI
  
#' @title   mod_global_trend_ui and mod_global_trend_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_global_trend
#'
#' @keywords internal
#' @export 
#' @import dplyr
#' @import ggplot2
#' @importFrom plotly ggplotly renderPlotly plotlyOutput hide_legend style
#' @import leaflet
mod_global_trend_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("trend"), height = "100%")
  )
}
    
# Module Server
    
#' @rdname mod_global_trend
#' @export
#' @keywords internal
    
mod_global_trend_server <- function(input, output, session, data, tendances, departement, classe_substance, palette_custom){
  ns <- session$ns

  output$trend <- renderPlotly({
    
    coefs <- filter(tendances, departement %in% departement(),
                    classification %in% classe_substance()) %>% 
      mutate(Tendance = paste0(round(pente, 1), "% par an"))
    
      data_plot <- filter(data,
             departement %in% departement(),
             classification %in% classe_substance()) %>% 
        ungroup() %>% 
        mutate(`Moyenne mobile` = case_when(
          Qrm >= 10 ~ paste0(format(round(Qrm), big.mark = " "), " tonnes"),
          Qrm < 10 ~ paste0(format(round(Qrm, 1), big.mark = " "), " tonnes"),
          TRUE ~ ""))
      
      range_y <- pull(data_plot, Qrel) %>% 
        range(na.rm = TRUE)
      
      range_x <- pull(data_plot, annee) %>% 
        range(na.rm = TRUE) 
      
      if (nrow(data_plot) == 0) {
        ggplotly({
          ggplot(data = data.frame(x = 0, y = 0),
                 aes(x, y)) +
            geom_text(label = "Pas de données de ventes<br>de substances pesticides",
                      size = 8, colour = "grey") +
            theme_minimal() +
            theme(panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank())
        })
      } else {
        ggplotly({
          g <- ggplot() +
            stat_function(data = data_plot, 
                          aes(x = annee),
                          fun = function(x) {100}, 
                          linetype = "dashed", colour = "lightgrey",
                          xlim = range_x) +
            stat_function(data = coefs,
                          aes(linetype = Tendance),
                          fun = function(x) {coefs$intercept + coefs$pente * x},
                          colour = palette_custom[[classe_substance()]](coefs$pente), 
                          xlim = range_x,
                          size = 1.5) +
            geom_point(data = data_plot, 
                       aes(x = annee, y = Qrel, 
                           text = paste0('Moyenne ', annee - 1, '-', annee + 1, 
                                         "\n", `Moyenne mobile`)),
                       colour = "black",
                       size = 2) +
            labs(title = paste0("Tonnes de substances vendues<br> (", if_else(coefs$pente > 0, "+ ", ""), paste0(round(coefs$pente, 1), "% par an)")), 
                 x = "", y = "") +
            scale_linetype_manual(values = "dashed") +
            scale_x_continuous(
              breaks = seq(from = min(range_x),
                           to = max(range_x),
                           by = 2),
              limits = c(min(range_x), 
                         max(range_x) + .5)) +
            scale_y_continuous(limits = c(max(c(0, min(range_y) - .1 * diff(range_y))),
                                          max(range_y) + .1 * diff(range_y)),
                               labels = function(x) {
                                 if (any(na.omit(x) / 100 * unique(data_plot$Qmoy) < 10)) {
                                   format(x / 100 * unique(data_plot$Qmoy), 
                                          scientific = FALSE, digits = 1, nsmall = 1, big.mark = " ")
                                 } else {
                                   format(x / 100 * unique(data_plot$Qmoy), 
                                          scientific = FALSE, digits = 0, nsmall = 0, big.mark = " ")
                                 }
                                
                               },
                               breaks = pretty(range_y / 100 * unique(data_plot$Qmoy), n = 3) / unique(data_plot$Qmoy) * 100) +
            coord_cartesian(ylim = c(min(range_y) - .5 * diff(range_y),
                                     max(range_y) + .1 * diff(range_y))) +
            theme_minimal() +
            theme(
              axis.line.y = element_line(),
              axis.ticks.y = element_line(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
          
          if (departement() == "FRANCE" & 
              classe_substance() == "danger_environnement") {
            g <- g + 
              annotate(geom = "text",
                       label = paste0("Moyenne des quantités vendues entre 2008 et ", max(bnvd_ventes$annee)),
                       x = quantile(range_x, probs = .01), 
                       y = 98,
                       colour = "grey") +
              annotate(geom = "text",
                       label = "Moyenne des quantités vendues\nsur trois ans (+/- 1 an)",
                       x = 2011.25,
                       y = filter(data_plot, annee == 2011)$Qrel,
                       hjust = 0,
                       vjust = 1,
                       colour = "black") +
                annotate(geom = "text",
                         label = "Droite de tendance",
                         color = palette_custom[[classe_substance()]](coefs$pente),
                         x = 2014.25,
                         y = 103)
          }
          
          g
        },
        tooltip = c("colour", "linetype", "text")) %>% 
          hide_legend() %>% 
        style(textposition = "right") 
        
      }
  })
}
    
## To be copied in the UI
# mod_global_trend_ui("global_trend_ui_1")
    
## To be copied in the server
# callModule(mod_global_trend_server, "global_trend_ui_1")
 
