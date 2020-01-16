# Module UI
  
#' @title   mod_buy_stats_ui and mod_buy_stats_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_buy_stats
#'
#' @keywords internal
#' @export 
#' @import shiny
#' @import leaflet
#' @import ggplot2
#' @importFrom plotly renderPlotly plotlyOutput plot_ly layout add_pie
#' @importFrom stringr str_wrap
#' @importFrom forcats fct_reorder fct_relevel
mod_buy_stats_ui <- function(id){
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
      fillCol(
        leafletOutput(ns("metropole")),
              height = "100%", flex = c(10)),
      HTML("<div></div>"),
      fillCol(
        h6(textOutput(ns("Title_Dep"))),
        plotlyOutput(ns("barplot"), width = "100%"),
        HTML("<div></div>"),
        plotlyOutput(ns("pie"), width = "100%"),
        flex = c(1, 4, .1, 6)
      ),
      height = "100%", width = "100%",
      flex = c(1, 4, .1, 3)
    )
    
  )
}

# Module Server
    
#' @rdname mod_buy_stats
#' @export
#' @keywords internal
    
mod_buy_stats_server <- function(input, output, session, annee, classe){
  
  annee_defaut <- max(DPT_Layer_Pt$annee)
  
  ns <- session$ns
  
  limits <- list(
    metropole = list(
      lng1 = -5.14, lat1 = 41.36, 
      lng2 = 9.55,  lat2 = 51.09
    ),
    guadeloupe = list(
      lng1 = -61.83, lat1 = 15.74,
      lng2 = -61.00, lat2 = 16.50
      ),
    martinique = list(
      lng1 = -61.27, lat1 = 14.25,
      lng2 = -60.74, lat2 = 14.92
      ),
    guyane = list(
      lng1 = -54.34, lat1 = 1.27,
      lng2 = -51.89, lat2 = 5.82
      ),
    reunion = list(
      lng1 = 55.21, lat1 = -21.50,
      lng2 = 55.84, lat2 = -20.86
      ),
    mayotte = list(
      lng1 = 45.004, lat1 = -13.03,
      lng2 = 45.31,  lat2 = -12.627
      )
  )
  
  set_base_map <- function(map) {
    data_pt    <- DPT_Layer_Pt %>% 
      filter(annee == annee_defaut,
             classification == "danger_environnement")
    
    data_stats <- DPT_Layer_stats %>% 
      filter(annee == annee_defaut,
             classification == "danger_environnement")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g t", data_stats$DEP,
      data_stats$QTE_SUBS
    ) %>% 
      lapply(htmltools::HTML)
    
    map %>%
      addProviderTiles(provider = "CartoDB.Positron",
                       options = tileOptions(maxZoom = 12)) %>% 
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = FALSE)) %>%
      addCircles(data = data_pt,
                 lng = ~long, lat = ~lat, weight = 1,
                 radius = ~sqrt(QTE_SUBS*1000) * 30, 
                 group = "circles") %>%
      addPolygons(data = filter(DPT_Layer,
                                classification == "danger_environnement"),
                  weight = 1,
                  color = "purple",
                  opacity = 1,
                  fillColor = "transparent",
                  layerId = ~DEP,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  }
  
  update_map <- function(map) {
    data_pt    <- DPT_Layer_Pt %>% 
      filter(annee == annee(),
             classification == classe())
    
    data_stats <- DPT_Layer_stats %>% 
      filter(annee == annee(),
             classification == classe())
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g t", data_stats$DEP,
      data_stats$QTE_SUBS
    ) %>% 
      lapply(htmltools::HTML)
    
    map %>%
      clearGroup("circles") %>%
      addCircles(data = data_pt,
                 lng = ~long, lat = ~lat, weight = 1,
                 radius = ~sqrt(QTE_SUBS*1000) * 30,
                 group = "circles") %>%
      addPolygons(data = filter(DPT_Layer,
                                classification == classe()),
                  weight = 1,
                  color = "purple",
                  opacity = 1,
                  fillColor = "transparent",
                  layerId = ~DEP,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  }
  
  output$metropole <- renderLeaflet({
    leaflet() %>%
      set_base_map() %>% 
      fitBounds(lng1 = limits$metropole$lng1,
                lat1 = limits$metropole$lat1,
                lng2 = limits$metropole$lng2,
                lat2 = limits$metropole$lat2)
  })
  
  output$guadeloupe <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      set_base_map() %>% 
      fitBounds(lng1 = limits$guadeloupe$lng1,
                lat1 = limits$guadeloupe$lat1,
                lng2 = limits$guadeloupe$lng2,
                lat2 = limits$guadeloupe$lat2)
  })
  
  output$martinique <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      set_base_map() %>% 
      fitBounds(lng1 = limits$martinique$lng1,
                lat1 = limits$martinique$lat1,
                lng2 = limits$martinique$lng2,
                lat2 = limits$martinique$lat2)
  })
  
  output$guyane <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      set_base_map() %>% 
      fitBounds(lng1 = limits$guyane$lng1,
                lat1 = limits$guyane$lat1,
                lng2 = limits$guyane$lng2,
                lat2 = limits$guyane$lat2)
  })
  
  output$reunion <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      set_base_map() %>% 
      fitBounds(lng1 = limits$reunion$lng1,
                lat1 = limits$reunion$lat1,
                lng2 = limits$reunion$lng2,
                lat2 = limits$reunion$lat2)
  })
  
  output$mayotte <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      set_base_map() %>% 
      fitBounds(lng1 = limits$mayotte$lng1,
                lat1 = limits$mayotte$lat1,
                lng2 = limits$mayotte$lng2,
                lat2 = limits$mayotte$lat2)
  })
  
  observe({

    if (!is.null(annee()) & !is.null(classe())) {
      
      # output$Title_Map <- renderText({
      #   paste0("Quantité de substances achetées en ", annee())
      # })
      
      leafletProxy(ns("metropole")) %>%
        update_map()
      
      leafletProxy(ns("guadeloupe")) %>%
        update_map()
      
      leafletProxy(ns("martinique")) %>%
        update_map()
      
      leafletProxy(ns("guyane")) %>%
        update_map()
      
      leafletProxy(ns("reunion")) %>%
        update_map()
      
      leafletProxy(ns("mayotte")) %>%
        update_map()
    }



  })
  
  zone_clic <- reactiveValues(id = NULL)
  
  observeEvent(input$metropole_shape_click,{
    zone_clic$id <- input$metropole_shape_click$id
  })
  observeEvent(input$metropole_marker_click,{
    zone_clic$id <- input$metropole_marker_click$id
  })
  
  observeEvent(input$guadeloupe_shape_click,{
    zone_clic$id <- input$guadeloupe_shape_click$id
  })
  
  observeEvent(input$martinique_shape_click,{
    zone_clic$id <- input$martinique_shape_click$id
  })
  
  observeEvent(input$guyane_shape_click,{
    zone_clic$id <- input$guyane_shape_click$id
  })    
  
  observeEvent(input$reunion_shape_click,{
    zone_clic$id <- input$reunion_shape_click$id
  })   
  
  observeEvent(input$mayotte_shape_click,{
    zone_clic$id <- input$mayotte_shape_click$id
  })  
  
  
  output$Title_Dep <- renderText({if (is.null(zone_clic$id)) {"Explorer les données (Cliquer sur un département de la carte)"}
    else {paste0("Département : ", zone_clic$id)}
  })
  

  #Substances majoritaires
  output$barplot <- renderPlotly({
    req(annee, classe, zone_clic)
    
    data_subs  <- ACHAT_DPT_SUBSTANCE_STATS %>% 
      distinct() %>% # pourquoi y a-t-il des doublons?
      filter(annee == annee(),
             classification == classe())
    
    my_color <- rgb(red = 197, blue = 148, green = 197, maxColorValue = 255)
    
    if(is.null(zone_clic$id)) return(NULL)
    else{

      subs_maj <- filter(data_subs, 
                         DPT == zone_clic$id,
                         T1 == "Quantite par substance") %>%
        ungroup() %>%
        select(T2, QTE_SUBS) %>% 
        mutate(label = paste0(T2, " : ", QTE_SUBS, " t")) %>% 
        arrange(QTE_SUBS)
      
      plot_ly(data = subs_maj,
              x = ~QTE_SUBS,
              y = ~T2,
              type = "bar", 
              orientation = "h", 
              marker = list(color = my_color),
              hoverinfo = "text",
              hovertext = ~label) %>% 
        layout(title = list(text = "5 principales substances achetées",
                            x = 0),
               xaxis = list(title = "Quantité (en tonnes)"),
               yaxis = list(title = "",
                            ticktext = str_wrap(subs_maj$T2, width = 24),
                            tickvals = ~subs_maj$T2))

    }
  })    
  
  output$pie <- renderPlotly({
    req(annee, classe, zone_clic)
    
    data_subs  <- ACHAT_DPT_SUBSTANCE_STATS %>% 
      filter(annee == annee(),
             classification == classe())
    
    if(is.null(zone_clic$id)) return(NULL)
    else{
      
      fonction <- filter(data_subs,
                         DPT == zone_clic$id,
                         T1 == "Fonction") %>%
        ungroup %>%
        select(T2, QTE_SUBS) %>% 
        mutate (T2 = fct_relevel(T2, "Autres produits", after = Inf))
      
      plot_ly(fonction, labels = ~T2, values = ~QTE_SUBS,
              marker = list(colors=c(rgb(red = 197, blue = 148, green = 197, maxColorValue = 255),
                                     rgb(red = 148, blue = 197, green = 148, maxColorValue = 255),
                                     rgb(red = 148, blue = 148, green = 197, maxColorValue = 255),
                                     rgb(red = 197, blue = 197, green = 148, maxColorValue = 255)
                                     ))) %>% 
        add_pie(hole = 0.6) %>%
        layout(title = list(text = "Répartition des achats par fonction",
                            x = 0),  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(orientation = "h",
                             font = list(size = 10)))

    }
    
  })
}
    
## To be copied in the UI
# mod_buy_stats_ui("buy_stats_ui_1")
    
## To be copied in the server
# callModule(mod_buy_stats_server, "buy_stats_ui_1")
 
