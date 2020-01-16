# Module UI

#' @title   mod_buy_map_ui and mod_buy_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_buy_map
#'
#' @keywords internal
#' @export 
#' @import shiny
#' @import leaflet
#' @importFrom leaflet.extras addSearchOSM
#' @importFrom sf st_as_sfc st_crs st_intersects
#' @importFrom stringr str_replace_all
mod_buy_map_ui <- function(id){
  ns <- NS(id)
  
  Surf_Ref <- c("Surface Agricole Utile" = "SAU", "Surface Agricole Utile - Surface Toujours en herbe" = "SAU_STH")
  
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
        flex = c(10, 0.5, 10, 0.5, 10, 0.5, 10, 0.5, 10))
      ,
      fillCol(leafletOutput(ns("metropole")), flex = c(10)),
      fillCol(
        leafletOutput(ns("legend")),
        sidebarPanel(selectInput(ns("Surf_Ref"), 
                                 "Surface de référence", Surf_Ref),
                     HTML("<div></div><p>Le niveau administratif affiché est celui du département. Le détail au niveau des codes postaux est accessible à l'aide du zoom ou de la fonction 'recherche'.</p>"),
                     # id = "controls",
                     width = 12)
      ),
      height = "100%", width = "100%",
      flex = c(1, 6, 1.5)
    )
  )
  
}

# Module Server

#' @rdname mod_buy_map
#' @export
#' @keywords internal

mod_buy_map_server <- function(input, output, session, annee = reactive("2015"), classe = reactive("toutes_substances")){
  
  ns <- session$ns
  
  transparence <- .6
  
  data <- reactive({
    bnvd::DPT_Layer %>% 
      rename(ID = DEP) %>% 
      filter(classification == "toutes_substances")
    })
  
  titles <- paste0("Quantité de substances <br>achetées rapportée à <br>la surface agricole utile", 
                   c("<br>(en kg/ha)",
                     ",<br>hors surfaces toujours<br>en herbe (en kg/ha)")) %>% 
    as.list()
  names(titles) <- c("SAU", "SAU_STH")
  
  Title_map <- titles$SAU
  
  bins <- c(-999, -888, 0, 0.01, 1,2.5, 5, Inf)
  palette_custom <- c('#6B0000','#AD5313','#F2A72E','#FAD155','#FFFFFF','#8FDB96','#FF00C5')
  
  Var <- 'QTE_SUBS_SAU_2015'
  
  lyr_maped <- reactiveValues(lyr_maped = 'DPT')
  var_maped <- reactiveValues(var_maped = Var)    
  sub_maped <- reactiveValues(sub_maped = "danger_environnement")
  
  colorData <- reactive({
    data()[[Var]]
    })
  
  pal <- reactive({
    colorBin(palette = palette_custom,
             domain = colorData(),
             bins = bins,
             reverse = TRUE)
    })
  
  popup <- reactive(paste0("<strong>", data()$ID,"</strong><br/>",
                  data()[[Var]]," kg/ha") %>%
    str_replace_all(pattern = "-888 kg/ha", replacement = "SAU nulle") %>%
    str_replace_all(pattern = "-999 kg/ha", replacement = "Données non communiquées"))
  
  output$metropole <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron", 
                       options = tileOptions(maxZoom = 12)) %>% 
      # addTiles(urlTemplate = 'http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',
      #          options = tileOptions(maxZoom = 12)) %>%
      addSearchOSM() %>%
      addPolygons(data = data(),
                  fillColor = ~pal()(colorData()),
                  weight = 1,
                  color = "white",
                  opacity = 1,
                  fillOpacity = transparence,
                  popup = popup()) %>%
      addScaleBar(position = "bottomleft",
                  options = scaleBarOptions(imperial = FALSE)) %>%
      fitBounds(lng1 = -5.14,lat1 = 41.36, lng2 = 12, lat2 = 51.09)
  }) 
  
  output$legend <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     attributionControl = FALSE)) %>% 
      addLegend(data = data(), 
                colors = palette_custom,
                opacity = transparence, 
                title = Title_map,
                labels = c("Plus de 5","Entre 2,5 et 5","Entre 1 et 2,5","Moins de 1","0","SAU nulle","Données non communiquées")) 
  })
  
  output$guadeloupe <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                    attributionControl = FALSE)) %>%
      addTiles(urlTemplate ='http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',options = tileOptions(maxZoom = 12)) %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = FALSE)) %>%
      fitBounds(lng1 = -61.83,lat1 = 15.74, lng2 = -61.00, lat2 = 16.50) %>% 
      addPolygons(data = data(),
                  fillColor = ~pal()(colorData()),
                  weight = 1,
                  color = "white",
                  opacity = 1,
                  fillOpacity = transparence,
                  popup = popup())
  }) 
  
  output$martinique <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                    attributionControl = FALSE)) %>%
      addTiles(urlTemplate ='http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',options = tileOptions(maxZoom = 12)) %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = FALSE)) %>%
      fitBounds(lng1 = -61.27,lat1 = 14.25, lng2 = -60.74, lat2 = 14.92) %>% 
      addPolygons(data = data(),
                  fillColor = ~pal()(colorData()),
                  weight = 1,
                  color = "white",
                  opacity = 1,
                  fillOpacity = transparence,
                  popup = popup())
  }) 
  
  output$guyane <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                    attributionControl = FALSE)) %>%
      addTiles(urlTemplate='http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',options = tileOptions(maxZoom = 12)) %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = FALSE)) %>%
      fitBounds(lng1 = -54.34,lat1 = 1.27, lng2 = -51.89, lat2 = 5.82) %>% 
      addPolygons(data = data(),
                  fillColor = ~pal()(colorData()),
                  weight = 1,
                  color = "white",
                  opacity = 1,
                  fillOpacity = transparence,
                  popup = popup())
  }) 
  
  output$reunion <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                    attributionControl = FALSE)) %>%
      addTiles(urlTemplate ='http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',options = tileOptions(maxZoom = 12)) %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = FALSE)) %>%
      fitBounds(lng1 = 55.21,lat1 = -21.50, lng2 = 55.84, lat2 = -20.86) %>% 
      addPolygons(data = data(),
                  fillColor = ~pal()(colorData()),
                  weight = 1,
                  color = "white",
                  opacity = 1,
                  fillOpacity = transparence,
                  popup = popup())
  }) 
  
  output$mayotte <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                    attributionControl = FALSE)) %>%
      addTiles(urlTemplate ='http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',options = tileOptions(maxZoom = 12)) %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = FALSE)) %>%
      fitBounds(lng1 = 45.004,lat1 = -13.03, lng2 = 45.31, lat2 = -12.627) %>% 
      addPolygons(data = data(),
                  fillColor = ~pal()(colorData()),
                  weight = 1,
                  color = "white",
                  opacity = 1,
                  fillOpacity = transparence,
                  popup = popup()) 
  })       
  
  
  observe({
    req(input$metropole_zoom, 
        input$Surf_Ref, annee, classe)
    
    Var <- paste0('QTE_SUBS_',input$Surf_Ref,'_',annee())
    
    zoom <- input$metropole_zoom
    
    subToMap <- classe()
    
    
    if (zoom > 8) {
      bbox <- input$metropole_bounds
      bbox_rect <- paste0("POLYGON((",
                          bbox$west, " ", bbox$south, ",",
                          bbox$west, " ", bbox$north, ",",
                          bbox$east, " ", bbox$north, ",",
                          bbox$east, " ", bbox$south, ",",
                          bbox$west, " ", bbox$south, "))") %>% 
        st_as_sfc(crs = st_crs(DPT_Layer))
      
      cover <- bnvd::CP_Layer %>% 
        st_intersects(y = bbox_rect)
      
      data <- bnvd::CP_Layer[lengths(cover) > 0,] %>% 
                         filter(classification == classe())
      
      lyrToMaped <- 'CP'
      
    } else {
      cover <- bnvd::DPT_Layer 
      
      data <- bnvd::DPT_Layer[lengths(cover) > 0,] %>% 
        rename(ID = DEP) %>% 
          filter(classification == classe())
      
      lyrToMaped <- 'DPT'
      
    }
    
    if (var_maped$var_maped != Var | sub_maped$sub_maped != subToMap)  {
      
      colorData <- data[[Var]]
      
      pal <- colorBin(palette = palette_custom, domain = colorData, bins = bins, reverse = TRUE)
      
      popup <- paste0("<strong>", data$ID,"</strong><br/>", 
                      data[[Var]]," kg/ha") %>% 
        str_replace_all(pattern = "-888 kg/ha", replacement = "SAU nulle") %>% 
        str_replace_all(pattern = "-999 kg/ha", replacement = "Données non communiquées")
      
      leafletProxy(ns("metropole")) %>%
        clearShapes() %>%
        addPolygons(data = data,
                    fillColor = ~pal(colorData),
                    weight = 1,
                    color = "white",
                    opacity = 1,
                    fillOpacity = transparence,
                    popup = popup)
      
      leafletProxy(ns("guadeloupe")) %>%
        clearShapes() %>%
        addPolygons(data = data,
                    fillColor = ~pal(colorData),
                    weight = 1,
                    color = "white",
                    opacity = 1,
                    fillOpacity = transparence,
                    popup = popup)
      
      leafletProxy(ns("martinique")) %>%
        clearShapes() %>%
        addPolygons(data = data,
                    fillColor = ~pal(colorData),
                    weight = 1,
                    color = "white",
                    opacity = 1,
                    fillOpacity = transparence,
                    popup = popup) 
      
      leafletProxy(ns("guyane")) %>%
        clearShapes() %>%
        addPolygons(data = data,
                    fillColor = ~pal(colorData),
                    weight = 1,
                    color = "white",
                    opacity = 1,
                    fillOpacity = transparence,
                    popup = popup) 
      
      leafletProxy(ns("reunion")) %>%
        clearShapes() %>%
        addPolygons(data = data,
                    fillColor = ~pal(colorData),
                    weight = 1,
                    color = "white",
                    opacity = 1,
                    fillOpacity = transparence,
                    popup = popup)  
      
      leafletProxy(ns("mayotte")) %>%
        clearShapes() %>%
        addPolygons(data = data,
                    fillColor = ~pal(colorData),
                    weight = 1,
                    color = "white",
                    opacity = 1,
                    fillOpacity = transparence,
                    popup = popup) 
      
      var_maped$var_maped <- Var
      sub_maped$sub_maped <- subToMap
      
    }        
    
    
    if (lyr_maped$lyr_maped != lyrToMaped | 
        lyr_maped$lyr_maped == 'CP') {
      
      colorData <- data[[Var]]
      
    pal <- colorBin(palette = palette_custom, domain = colorData, bins = bins, reverse = TRUE)
    
    popup <- paste0("<strong>", data$ID,"</strong><br/>", 
                    data[[Var]]," kg/ha") %>% 
      str_replace_all(pattern = "-888 kg/ha", replacement = "SAU nulle") %>% 
      str_replace_all(pattern = "-999 kg/ha", replacement = "Données non communiquées")
    
    leafletProxy(ns("metropole")) %>%
      clearShapes() %>%
      addPolygons(data = data,
                  fillColor = ~pal(colorData),
                  weight = 1,
                  color = "white",
                  opacity = 1,
                  fillOpacity = transparence,
                  popup = popup)
    
    lyr_maped$lyr_maped <- lyrToMaped
    }
    
    
  })
  
  
  observe({
    req(input$Surf_Ref)
    
    Title_map <- titles[[input$Surf_Ref]]
    
    leafletProxy(ns("legend")) %>% 
      clearControls() %>% 
      addLegend(data = data(), colors = palette_custom, 
                opacity = transparence, title = Title_map,
                labels = c("Plus de 5","Entre 2,5 et 5","Entre 1 et 2,5","Moins de 1","0","SAU nulle","Données non communiquées"))
  })
  
}

## To be copied in the UI
# mod_buy_map_ui("buy_map_ui_1")

## To be copied in the server
# callModule(mod_buy_map_server, "buy_map_ui_1")

