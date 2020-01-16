# Module UI
  
#' @title   mod_table_substances_ui and mod_table_substances_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_table_substances
#'
#' @keywords internal
#' @export 
#' @import dplyr
#' @import shiny
#' @importFrom formattable formattable area normalize_bar renderFormattable formattableOutput formatter percent proportion csscolor icontext
mod_table_substances_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    div(style = 'overflow-y: scroll; overflow-x: scroll',
        formattableOutput(ns("data_top"),width = "100%"))
      
  )
}
    
# Module Server
    
#' @rdname mod_table_substances
#' @export
#' @keywords internal
    
mod_table_substances_server <- function(input, output, session, data, departement, classe_substance, period){
  ns <- session$ns
  
  # Function from https://stackoverflow.com/questions/48870990/r-formattable-apply-two-format-to-an-area
  colorbar <- function(color = "lightgray", fun = "comma", digits = 0) {
    fun <- match.fun(fun)
    formatter("span", x ~ fun(x, digits = digits),
              style = function(y) formattable::style(
                display = "inline-block",
                direction = "rtl",
                "border-radius" = "4px",
                "padding-right" = "2px",
                "background-color" = csscolor(color),
                width = percent(proportion(as.numeric(y), na.rm = TRUE))
              )
    )
  }
  
  output$data_top <- renderFormattable({
    df <- filter(data, departement %in% departement(),
                 classification %in% classe_substance()) %>% 
      ungroup() %>% 
      mutate(Substance = substance,
        # Substance = paste0("<a href='http://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:PAR:FRA:code:", code_parametre, ":::referentiel:2:html' target='_blank'>", substance, "</a>"),
             quantite_prop = quantite_rel / 100) %>% 
      select(substance,
             Substance,
             `Pourcentage des ventes` = quantite_prop,
             `Evolution des ventes` = trend,
             Fonction = type_substance) %>% 
      (function(df) {
        colnames(df)[colnames(df) == "quantite_prop"] <- "Pourcentage des ventes"
        colnames(df)[colnames(df) == "trend"] <- "Evolution des ventes"
        
        df
      }) %>% 
      left_join(mutate(bnvd::classes_substances,
                       classification = case_when(
                         classification == "danger_sante" ~ "T, T+, CMR",
                         classification == "danger_environnement" ~ "N min., orga.",
                         classification == "autres_substances" ~ "Autres substances"
                       )) %>% 
                  select(`Classe de substance` = classification,
                         substance),
                by = "substance") %>% 
      select(-substance)
    
    formattable(df,
                list(
                  area(col = 2) ~ colorbar(color = "pink", fun = "percent", digits = 1),
                  "Evolution des ventes" = formatter("span", 
                                      style = ~ formattable::style(
                                        color = case_when(`Evolution des ventes` == "diminution" ~ "green",
                                                          `Evolution des ventes` == "augmentation" ~ "red",
                                                          TRUE ~ "black")),
                                      ~ icontext(sapply(`Evolution des ventes`, function(x) {
                                        case_when(x == "diminution" ~ "arrow-down",
                                                  x == "augmentation" ~ "arrow-up",
                                                  TRUE ~ "")
                                      } 
                                      )))
                )) 
  })
}
    
## To be copied in the UI
# mod_table_substances_ui("table_substances_ui_1")
    
## To be copied in the server
# callModule(mod_table_substances_server, "table_substances_ui_1")
 
