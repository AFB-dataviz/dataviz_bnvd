#' @import dplyr
import_departement <- function() {
  cat("\nLes limites de département (shapefile) sont téléchargées depuis data.gouv.fr:\nhttps://www.data.gouv.fr/fr/datasets/r/eb36371a-761d-44a8-93ec-3d728bec17ce\n")

    sf::st_read(system.file("extdata",
                            "departements-20180101.shp",
                            package = "bnvd"),
                stringsAsFactors = FALSE, quiet = TRUE) %>%
        select(code_insee, nom) %>%
        mutate(nom = iconv(x = nom,
                           from = "UTF-8",
                           to = "ASCII//TRANSLIT") %>%
                   toupper()) %>%
        mutate(nom = if_else(condition = code_insee %in% c("69D", "69M"),
                             true = "RHONE",
                             false = nom),
               code_insee = if_else(condition = code_insee %in% c("69D", "69M"),
                                    true = "69",
                                    false = code_insee)) %>%
        group_by(code_insee, nom) %>%
        summarise() %>% 
      rename(departement = nom) %>% 
      rmapshaper::ms_simplify(keep = .01)
}
