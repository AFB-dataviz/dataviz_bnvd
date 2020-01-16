#' @import dplyr
prepare_bnvd_ventes <- function(data, liste) {
  left_join(data, liste, by = "substance") %>%
    select(departement, annee, 
           type_substance, 
           substance, classification,
           quantite) %>% 
    bind_rows(.,
              group_by(., annee, type_substance, 
                       substance, 
                       classification
                       ) %>% 
                summarise(quantite = sum(quantite)) %>% 
                mutate(departement = "FRANCE")) 
}
