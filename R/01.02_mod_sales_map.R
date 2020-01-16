# Module UI
  
#' @title   mod_sales_map_ui and mod_sales_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_sales_map
#'
#' @keywords internal
#' 
#' @import shiny
#' @import leaflet
#' 
#' @export 
mod_sales_map_ui <- function(id){
  ns <- NS(id)
  
  tagList(
      fillRow(
        fillCol(
          leafletOutput(ns("guadeloupe")), 
          HTML("<div></div>"),
          leafletOutput(ns("martinique")), 
          HTML("<div></div>"),
          leafletOutput(ns("guyane")), 
          HTML("<div></div>"),
          leafletOutput(ns("reunion")), 
          HTML("<div></div>"),
          leafletOutput(ns("mayotte")), 
          width = "95%", height = "100%",
          flex = c(10, 0.5, 10, 0.5, 10, 0.5, 10, 0.5, 10)),
        leafletOutput(ns("metropole")),
        height = "100%", width = "100%", 
        flex = c(1, 3)
      )
  )
}
    
# Module Server
    
#' @rdname mod_sales_map
#' @export
#' @keywords internal
    
mod_sales_map_server <- function(input, output, session, 
                                 data, choix_departement, classe_substance,
                                 palette_map, palette_legend){
  ns <- session$ns
  
  dom <- c("GUADELOUPE", "MARTINIQUE", "GUYANE", "LA REUNION", "MAYOTTE")
  
  output$metropole <- renderLeaflet({

    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>% 
      addPolygons(data = filter(data,
                                classification %in% classe_substance()),
                  fillColor = ~palette_map[[classe_substance()]](pente),
                  layerId = ~departement,
                  weight = 1, fillOpacity = 1,
                  label = ~ label) %>%
      addPolygons(data = filter(data,
                                departement %in% choix_departement()),
                  fill = FALSE, fillOpacity = 0, fillColor = NULL,
                  weight = 5,
                  color = "#FFFF00", opacity = 1,
                  layerId = "selected") %>% 
    addScaleBar(position = "bottomright", 
                  options = scaleBarOptions(imperial = FALSE)) %>% 
      fitBounds(lng1 = -5.141276, lat1 = 42.332755,
                lng2 = 12, lat2 = 51.088991) %>% 
      addLegend(title = "Evolution annuelle<br>(% par an)",
                pal = palette_legend[[classe_substance()]],
                values = seq(from = -1 * max(abs(filter(data, classification %in% classe_substance())$pente), na.rm = TRUE),
                             to = max(abs(filter(data, classification %in% classe_substance())$pente), na.rm = TRUE), 
                             by = 1),
                labFormat = labelFormat(transform = function(x) sort(round(x), decreasing = TRUE)),
                opacity = 1) 
  })
  
   output$guadeloupe <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>% 
      addPolygons(data = filter(data,
                                departement %in% dom,
                                classification %in% classe_substance()),
                  fillColor = ~palette_map[[classe_substance()]](pente),
                  layerId = ~departement,
                  weight = 1, fillOpacity = 1,
                  label = ~ label) %>%
       addPolygons(data = filter(data,
                                 departement %in% choix_departement()),
                   fill = FALSE, fillOpacity = 0, fillColor = NULL,
                   weight = 5,
                   color = "#FFFF00", opacity = 1,
                   layerId = "selected") %>% 
       addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE)) %>% 
      fitBounds(lng1 = -61.80976, lat1 = 15.83201,
                lng2 = -61.00037, lat2 = 16.51447)
      
  })
  
  output$martinique <- renderLeaflet({

    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>% 
      addPolygons(data = filter(data,
                                departement %in% dom,
                                classification %in% classe_substance()),
                  fillColor = ~palette_map[[classe_substance()]](pente),
                  layerId = ~departement,
                  weight = 1, fillOpacity = 1,
                  label = ~ label) %>%
      addPolygons(data = filter(data,
                                departement %in% choix_departement()),
                  fill = FALSE, fillOpacity = 0, fillColor = NULL,
                  weight = 5,
                  color = "#FFFF00", opacity = 1,
                  layerId = "selected") %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE)) %>% 
      fitBounds(lng1 = -61.22908, lat1 = 14.38870,
                lng2 = -60.80958, lat2 = 14.87870)
  })
  
  
  output$guyane <- renderLeaflet({

    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>% 
      addPolygons(data = filter(data,
                                departement %in% dom,
                                classification %in% classe_substance()),
                  fillColor = ~palette_map[[classe_substance()]](pente),
                  layerId = ~departement,
                  weight = 1, fillOpacity = 1,
                  label = ~ label) %>%
      addPolygons(data = filter(data,
                                departement %in% choix_departement()),
                  fill = FALSE, fillOpacity = 0, fillColor = NULL,
                  weight = 5,
                  color = "#FFFF00", opacity = 1,
                  layerId = "selected") %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE)) %>% 
      fitBounds(lng1 = -54.602780, lat1 = 2.112222,
                lng2 = -51.634614, lat2 = 5.750711)
  })
  
  output$reunion <- renderLeaflet({

    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>% 
      addPolygons(data = filter(data,
                                departement %in% dom,
                                classification %in% classe_substance()),
                  fillColor = ~palette_map[[classe_substance()]](pente),
                  layerId = ~departement,
                  weight = 1, fillOpacity = 1,
                  label = ~ label) %>%
      addPolygons(data = filter(data,
                                departement %in% choix_departement()),
                  fill = FALSE, fillOpacity = 0, fillColor = NULL,
                  weight = 5,
                  color = "#FFFF00", opacity = 1,
                  layerId = "selected") %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE)) %>% 
      fitBounds(lng1 = 55.21643, lat1 = -21.38973,
                lng2 = 55.83669, lat2 = -20.87171)
  })
  
  output$mayotte <- renderLeaflet({

    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>% 
      addPolygons(data = filter(data,
                                departement %in% dom,
                                classification %in% classe_substance()),
                  fillColor = ~palette_map[[classe_substance()]](pente),
                  layerId = ~departement,
                  weight = 1, fillOpacity = 1,
                  label = ~ label) %>%
      addPolygons(data = filter(data,
                                departement %in% choix_departement()),
                  fill = FALSE, fillOpacity = 0, fillColor = NULL,
                  weight = 5,
                  color = "#FFFF00", opacity = 1,
                  layerId = "selected") %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE)) %>% 
      fitBounds(lng1 = 45.01833, lat1 = -13.02101,
                lng2 = 45.29999, lat2 = -12.63659)
  })
  
}
    
## To be copied in the UI
# mod_sales_map_ui("sales_map_ui_1")
    
## To be copied in the server
# callModule(mod_sales_map_server, "sales_map_ui_1")
 
