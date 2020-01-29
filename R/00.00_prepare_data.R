#' Prepare the package data
#' 
#' @import dplyr
#' @import tidyr
#' @import shiny
#' 
#' @export
prepare_data <- function(path = NULL, dev = TRUE) {

  ## Date de l'export des données ----
  date_preparation <- Sys.Date()
  
  liste_substances <- 
    readr::read_delim(system.file("extdata",
                                  "substance_fonction.csv",
                                  package = "bnvd"),
                      col_types = list(substance = readr::col_character(),
                                       cas       = readr::col_character(),
                                       fonction  = readr::col_character()),
                      delim = ";") %>% 
    rename(type_substance = fonction) %>% 
    mutate(
      substance = case_when(
        cas == "112-53-8"   ~ "1-dodecanol",
        cas == "75747-77-2" ~ "prochloraze-manganese",
        cas == "76703-62-3" ~ "gamma-cyhalothrine",
        substance == "cerevisane" ~ "cerevisiane",
        TRUE                      ~ substance
      ),
      type_substance = case_when(
        type_substance  %in% c("#N/A", "NON DETERMINE") ~ 
          "Non déterminé",
        grepl(x = type_substance, 
              pattern = "Autres \\(Molluscicides, rodenticides, activateur végétal, etc$") ~
          "Autres (molluscicides, rodenticides, activateur végétal, etc)",
        type_substance == "Multifonction (Autres (Molluscicides, rodenticides, activateur végétal, etc." ~
          "Multifonction (Autres)",
        type_substance == "Multifonction (Insecticides (dont acaricides))" ~
          "Multifonction (Insecticides)",
        type_substance == "Multifonction (Nématicides" ~
          "Multifonction (Nématicides)",
        type_substance == "Nématicide" ~
          "Nématicides",
        TRUE ~ type_substance
      )
    ) %>% 
    select(substance, type_substance) %>% 
    distinct() %>% 
    bind_rows(
      tribble(
        ~substance,                         ~type_substance,
        "2-methyl-1-naphtylacetamide",      "Substances de croissance",
        "acide 2-methyl-1-naphtylacetique", "Substances de croissance",
        "acide nonanoique",                 "Herbicides",
        "amitrole",                         "Herbicides",
        "azocyclotin",                      "Insecticides (dont acaricides)",
        "benzoate de sodium",               "Fongicides",
        "bromoxynil",                       "Herbicides",
        "chlormephos",                      "Insecticides (dont acaricides)",
        "difethialone",                     "Autres (molluscicides, rodenticides, activateur végétal, etc)",
        "flurprimidol",                     "Substances de croissance",
        "n-phosphonomethyl glycine",        "Herbicides",
        "orthophenylphenol",                "Fongicides",
        "propanil",                         "Herbicides",
        "streptococcus faecium",            "Autres (molluscicides, rodenticides, activateur végétal, etc)", #"Bactéricides" ou alors Fongicides?
        "ethoxyquine",                      "Autres (molluscicides, rodenticides, activateur végétal, etc)", #"Anti-oxydant"
      )
    ) %>% 
    mutate(type_substance = enc2utf8(type_substance))
    

  ## Données de ventes de pesticides au département ----
  bnvd_ventes <- download_bnvd(
    base_url = "http://www.data.eaufrance.fr/opendata-files/bd45f801-45f7-4f8c-b128-a1af3ea2aa3e/BNVD_2019_VENTE_",
    period = 2008:2018
  ) %>% 
   prepare_bnvd_ventes(data = ., 
                       liste = liste_substances) %>% 
    filter(!grepl(x = type_substance,
                  pattern = "A EXCLURE")) %>% 
    mutate(classification = factor(classification, 
                                   levels = c("danger_sante",
                                              "danger_environnement", 
                                              "autres_substances")))
  
  classes_substances <- select(bnvd_ventes, substance, 
                               annee, classification) %>% 
    distinct() %>% 
    arrange(substance, annee) %>% 
    ## Ne conserve que la classification la plus récente
    group_by(substance) %>% 
    filter(annee == max(annee)) %>% 
    arrange(substance, classification) 
  
  bnvd_ventes <-  
    left_join(select(bnvd_ventes, -classification),
              select(classes_substances, -annee),
              by = "substance") %>% 
    bind_rows(.,
              mutate(., classification = "toutes_substances"))
  
  ## Substances représentant plus de 50% des ventes (somme sur la période)
  classement_substances <- identify_top_substances(bnvd_ventes, prop = 0.5, 
                                                  group = c("departement", "classification")) %>% 
    left_join(x = .,
              y = liste_substances, by = "substance")
    
  ## Calcul des tendances
    # Calcule les ventes totales
  bnvd_ventes_totales <- group_by(bnvd_ventes, departement, 
                                  annee, classification) %>% 
    summarise(substance = "tous pesticides",
              quantite = sum(quantite)) %>% 
    group_by(departement, classification) %>% 
    # Calcule la moyenne mobile
    mutate(Qrm = zoo::rollmean(x = quantite,
                          k = 3,
                          fill = NA)) %>% 
    mutate(Qmoy = mean(Qrm, na.rm = TRUE)) %>% 
    mutate(Qrel = 100 * Qrm / Qmoy)
    
  
  tendances <- bnvd_ventes_totales %>% 
    # calcule des tendances
    calc_tendances(df = ., quantite = "Qrel",
                   group = c("departement", "classification")) 
  
  bnvd_ventes <- left_join(bnvd_ventes,
              classement_substances,
              by = c("departement", "type_substance",
                     "substance", "classification")) %>% 
    filter(!is.na(rang)) %>% 
    rename(quantite = quantite.x, quantite_totale = quantite.y)
  
  ## Données spatialisées
  departements <- import_departement() 
  
  tendances <- full_join(tendances, departements, by = "departement") %>% 
    ungroup() %>% 
    sf::st_as_sf() %>% 
    mutate(label = if_else(is.na(pente),
                           paste0(departement, "<br>", NA),
                           paste0(departement, "<br>",
                          round(pente, 1), "% par an"))) %>% 
    mutate(label = lapply(label, HTML))

  ## Données SDES
  ## limites de CP et départements
  load(system.file("extdata",
                   "achats_geo_indic.Rdata", package = "bnvd"))
  ## limites départements et centroides des départements
  load(system.file("extdata",
                   "achats_geo_stats.Rdata", package = "bnvd"))
  
  load(system.file("extdata",
                   "achats_subs_tot.Rdata", package = "bnvd"))
  load(system.file("extdata",
                   "achats_subst_stats.Rdata", package = "bnvd"))
  
  DPT_Layer_stats <- left_join(DPT_Layer,
                               ACHAT_DPT_SUBSTANCE_STATS %>% 
                                 filter(T1 == "Quantite totale"),
                               by = c("DEP" = "DPT")) %>% 
    mutate(classification = gsub(
      classification, 
      pattern = "Total", 
      replacement = "toutes_substances"
    ))
  
  DPT_Layer_Pt <- left_join(DPT_Layer_Pt, 
                            ACHAT_DPT_SUBSTANCE_STATS %>%
                              filter(T1 == "Quantite totale"),
                            by = c("DEP" = "DPT")) %>% 
    mutate(classification = gsub(
      classification, 
      pattern = "Total", 
      replacement = "toutes_substances"
    ))
  
  CP_Layer <- left_join(CP_Layer, 
                        INDIC_CP %>% 
                        # POURQUOI DES NA ???
                        filter(!is.na(classification)) %>% 
                          # AJOUTE LES CLASSIFICATIONS MANQUANTES
                          complete(CP, classification) %>% 
                          mutate_if(.predicate = is.numeric,
                                    .funs = function(x) {
                                      replace_na(x, -999)
                                      }),
                        by = c("ID" = "CP")) %>% 
    mutate(classification = gsub(
      classification, 
      pattern = "Total", 
      replacement = "toutes_substances"
    ))
  
  DPT_Layer <- left_join(DPT_Layer,
                         INDIC_DPT , by = c("DEP" = "DPT")) %>% 
    mutate(classification = gsub(
      classification, 
      pattern = "Total", 
      replacement = "toutes_substances"
    ))
  
  ACHAT_DPT_SUBSTANCE_STATS <- mutate(ACHAT_DPT_SUBSTANCE_STATS,
                                      classification = gsub(
                                        classification, 
                                        pattern = "Total", 
                                        replacement = "toutes_substances"
                                      ))
  
  ## Liste des départements
  liste_departements <- list(
    `France`                           = "FRANCE",
    `Auvergne-Rhône-Alpes` = c(
      `Ain (01)`                     = "AIN",
      `Allier (03)`                  = "ALLIER",
      `Ardèche (07)`                 = "ARDECHE",
      `Cantal (15)`                  = "CANTAL",
      `Drôme (26)`                   = "DROME", 
      `Isère (38)`                   = "ISERE",
      `Loire (42)`                   = "LOIRE",
      `Haute-Loire (43)`             = "HAUTE-LOIRE",
      `Puy-de-Dôme (63)`             = "PUY-DE-DOME",
      `Rhône (69)`                   = "RHONE",
      `Savoie (73)`                  = "SAVOIE",
      `Haute-Savoie (74)`            = "HAUTE-SAVOIE"
    ),
    `Bourgogne-Franche-Comté` = c(
      `Côte-d'Or (21)`               = "COTE-D'OR",
      `Doubs (25)`                   = "DOUBS",
      `Jura (39)`                    = "JURA",
      `Nièvre (58)`                  = "NIEVRE",
      `Haute-Saône (70)`             = "HAUTE-SAONE",
      `Saône-et-Loire (71)`          = "SAONE-ET-LOIRE",
      `Yonne (89)`                   = "YONNE",
      `Territoire de Belfort (90)`   = "TERRITOIRE-DE-BELFORT"
    ),
    `Bretagne` = c(
      `Côtes-d'Armor (22)`           = "COTES-D'ARMOR",
      `Finistère (29)`               = "FINISTERE",
      `Ille-et-Vilaine (35)`         = "ILLE-ET-VILAINE",
      `Morbihan (56)`                = "MORBIHAN"
    ),
    `Centre-Val de Loire` = c(
      `Cher (18)`                    = "CHER",
      `Eure-et-Loir (28)`            = "EURE-ET-LOIR",
      `Indre (36)`                   = "INDRE",
      `Indre-et-Loire (37)`          = "INDRE-ET-LOIRE",
      `Loir-et-Cher (41)`            = "LOIR-ET-CHER",
      `Loiret (45)`                  = "LOIRET"
    ),
    `Corse` = c(
      `Corse-du-Sud (2A)`            = "CORSE-DU-SUD",
      `Haute-Corse (2B)`             = "HAUTE-CORSE"
    ),
    `Grand Est` = c(
      `Ardennes (08)`                = "ARDENNES",
      `Aube (10)`                    = "AUBE",
      `Marne (51)`                   = "MARNE",
      `Haute-Marne (52)`             = "HAUTE-MARNE",
      `Meurthe-et-Moselle (54)`      = "MEURTHE-ET-MOSELLE",
      `Meuse (55)`                   = "MEUSE",
      `Moselle (57)`                 = "MOSELLE",
      `Bas-Rhin (67)`                = "BAS-RHIN",
      `Haut-Rhin (68)`               = "HAUT-RHIN",
      `Vosges (88)`                  = "VOSGES"
    ),
    `Guadeloupe` = c(
      `Guadeloupe (971)`             = "GUADELOUPE"
    ),
    `Guyane` = c(
      `Guyane (973)`                 = "GUYANE"
    ),
    `Hauts-de-France` = c(
      `Aisne (02)`                   = "AISNE",
      `Nord (59)`                    = "NORD",
      `Oise (60)`                    = "OISE",
      `Pas-de-Calais (62)`           = "PAS-DE-CALAIS",
      `Somme (80)`                   = "SOMME"
    ),
    `Ile-de-France` = c(
      `Paris (75)`                   = "PARIS",
      `Seine-et-Marne (77)`          = "SEINE-ET-MARNE",
      `Yvelines (78)`                = "YVELINES",
      `Essonne (91)`                 = "ESSONNE",
      `Hauts-de-Seine (92)`          = "HAUTS-DE-SEINE",
      `Seine-Saint-Denis (93)`       = "SEINE-SAINT-DENIS",
      `Val-de-Marne (94)`            = "VAL-DE-MARNE",
      `Val-d'Oise (95)`              = "VAL-D'OISE"
    ),
    `La Réunion` = c(
      `La Réunion (974)`             = "LA REUNION"
    ),
    `Martinique` = c(
      `Martinique (972)`             = "MARTINIQUE"
    ),
    `Mayotte` = c(
      `Mayotte (976)`                = "MAYOTTE"
    ),
    `Normandie` = c(
      `Calvados (14)`                = "CALVADOS",
      `Eure (27)`                    = "EURE",
      `Manche (50)`                  = "MANCHE",
      `Orne (61)`                    = "ORNE",
      `Seine-Maritime (76)`          = "SEINE-MARITIME"
    ),
    `Nouvelle-Aquitaine` = c(
      `Charente (16)`                = "CHARENTE",
      `Charente-Maritime (17)`       = "CHARENTE-MARITIME",
      `Corrèze (19)`                 = "CORREZE",
      `Creuse (23)`                  = "CREUSE",
      `Dordogne (24)`                = "DORDOGNE",
      `Gironde (33)`                 = "GIRONDE",
      `Landes (40)`                  = "LANDES",
      `Lot-et-Garonne (47)`          = "LOT-ET-GARONNE",
      `Pyrénées-Atlantiques (64)`    = "PYRENEES-ATLANTIQUES",
      `Deux-Sèvres (79)`             = "DEUX-SEVRES",
      `Vienne (86)`                  = "VIENNE",
      `Haute-Vienne (87)`            = "HAUTE-VIENNE"
    ),
    `Occitanie` = c(
      `Ariège (09)`                  = "ARIEGE",
      `Aude (11)`                    = "AUDE",
      `Aveyron (12)`                 = "AVEYRON",
      `Gard (30)`                    = "GARD",
      `Haute-Garonne (31)`           = "HAUTE-GARONNE",
      `Gers (32)`                    = "GERS",
      `Hérault (34)`                 = "HERAULT",
      `Lot (46)`                     = "LOT",
      `Lozère (48)`                  = "LOZERE",
      `Hautes-Pyrénées (65)`         = "HAUTES-PYRENEES",
      `Pyrénées-Orientales (66)`     = "PYRENEES-ORIENTALES",
      `Tarn (81)`                    = "TARN",
      `Tarn-et-Garonne (82)`         = "TARN-ET-GARONNE"
    ),
    `Pays de la Loire` = c(
      `Loire-Atlantique (44)`        = "LOIRE-ATLANTIQUE",
      `Maine-et-Loire (49)`          = "MAINE-ET-LOIRE",
      `Mayenne (53)`                 = "MAYENNE",
      `Sarthe (72)`                  = "SARTHE",
      `Vendée (85)`                  = "VENDEE"
    ),
    `Provence-Alpes-Côte d'Azur` = c(
      `Alpes-de-Haute-Provence (04)` = "ALPES-DE-HAUTE-PROVENCE",
      `Hautes-Alpes (05)`            = "HAUTES-ALPES",
      `Alpes-Maritimes (06)`         = "ALPES-MARITIMES",
      `Bouches-du-Rhône (13)`        = "BOUCHES-DU-RHONE",
      `Var (83)`                     = "VAR",
      `Vaucluse (84)`                = "VAUCLUSE"
    )
  ) %>% 
    (function(x) {
      names(x) <- enc2utf8(names(x))
      x
    })
  
  for (i in names(liste_departements)) {
    if (i != "France") {
      names(liste_departements[[i]]) <- enc2utf8(names(liste_departements[[i]]))
    }
  }
  
  liste_codes_departements <- list(
    `France`                           = "FRANCE",
    `Auvergne-Rhône-Alpes` = c(
      `Ain (01)`                     = "01",
      `Allier (03)`                  = "03",
      `Ardèche (07)`                 = "07",
      `Cantal (15)`                  = "15",
      `Drôme (26)`                   = "26", 
      `Isère (38)`                   = "38",
      `Loire (42)`                   = "42",
      `Haute-Loire (43)`             = "43",
      `Puy-de-Dôme (63)`             = "63",
      `Rhône (69)`                   = "69",
      `Savoie (73)`                  = "73",
      `Haute-Savoie (74)`            = "74"
    ),
    `Bourgogne-Franche-Comté` = c(
      `Côte-d'Or (21)`               = "21",
      `Doubs (25)`                   = "25",
      `Jura (39)`                    = "39",
      `Nièvre (58)`                  = "58",
      `Haute-Saône (70)`             = "70",
      `Saône-et-Loire (71)`          = "71",
      `Yonne (89)`                   = "89",
      `Territoire de Belfort (90)`   = "90"
    ),
    `Bretagne` = c(
      `Côtes-d'Armor (22)`           = "22",
      `Finistère (29)`               = "29",
      `Ille-et-Vilaine (35)`         = "35",
      `Morbihan (56)`                = "56"
    ),
    `Centre-Val de Loire` = c(
      `Cher (18)`                    = "18",
      `Eure-et-Loir (28)`            = "28",
      `Indre (36)`                   = "36",
      `Indre-et-Loire (37)`          = "37",
      `Loir-et-Cher (41)`            = "41",
      `Loiret (45)`                  = "45"
    ),
    `Corse` = c(
      `Corse-du-Sud (2A)`            = "2A",
      `Haute-Corse (2B)`             = "2B"
    ),
    `Grand Est` = c(
      `Ardennes (08)`                = "08",
      `Aube (10)`                    = "10",
      `Marne (51)`                   = "51",
      `Haute-Marne (52)`             = "52",
      `Meurthe-et-Moselle (54)`      = "54",
      `Meuse (55)`                   = "55",
      `Moselle (57)`                 = "57",
      `Bas-Rhin (67)`                = "67",
      `Haut-Rhin (68)`               = "68",
      `Vosges (88)`                  = "88"
    ),
    `Guadeloupe` = c(
      `Guadeloupe (971)`             = "971"
    ),
    `Guyane` = c(
      `Guyane (973)`                 = "973"
    ),
    `Hauts-de-France` = c(
      `Aisne (02)`                   = "02",
      `Nord (59)`                    = "59",
      `Oise (60)`                    = "60",
      `Pas-de-Calais (62)`           = "62",
      `Somme (80)`                   = "80"
    ),
    `Ile-de-France` = c(
      `Paris (75)`                   = "75",
      `Seine-et-Marne (77)`          = "77",
      `Yvelines (78)`                = "78",
      `Essonne (91)`                 = "91",
      `Hauts-de-Seine (92)`          = "92",
      `Seine-Saint-Denis (93)`       = "93",
      `Val-de-Marne (94)`            = "94",
      `Val-d'Oise (95)`              = "95"
    ),
    `La Réunion` = c(
      `La Réunion (974)`             = "974"
    ),
    `Martinique` = c(
      `Martinique (972)`             = "972"
    ),
    `Mayotte` = c(
      `Mayotte (976)`                = "976"
    ),
    `Normandie` = c(
      `Calvados (14)`                = "14",
      `Eure (27)`                    = "27",
      `Manche (50)`                  = "50",
      `Orne (61)`                    = "61",
      `Seine-Maritime (76)`          = "76"
    ),
    `Nouvelle-Aquitaine` = c(
      `Charente (16)`                = "16",
      `Charente-Maritime (17)`       = "17",
      `Corrèze (19)`                 = "19",
      `Creuse (23)`                  = "23",
      `Dordogne (24)`                = "24",
      `Gironde (33)`                 = "33",
      `Landes (40)`                  = "40",
      `Lot-et-Garonne (47)`          = "47",
      `Pyrénées-Atlantiques (64)`    = "64",
      `Deux-Sèvres (79)`             = "79",
      `Vienne (86)`                  = "86",
      `Haute-Vienne (87)`            = "87"
    ),
    `Occitanie` = c(
      `Ariège (09)`                  = "09",
      `Aude (11)`                    = "11",
      `Aveyron (12)`                 = "12",
      `Gard (30)`                    = "30",
      `Haute-Garonne (31)`           = "31",
      `Gers (32)`                    = "32",
      `Hérault (34)`                 = "34",
      `Lot (46)`                     = "46",
      `Lozère (48)`                  = "48",
      `Hautes-Pyrénées (65)`         = "65",
      `Pyrénées-Orientales (66)`     = "66",
      `Tarn (81)`                    = "81",
      `Tarn-et-Garonne (82)`         = "82"
    ),
    `Pays de la Loire` = c(
      `Loire-Atlantique (44)`        = "44",
      `Maine-et-Loire (49)`          = "49",
      `Mayenne (53)`                 = "53",
      `Sarthe (72)`                  = "72",
      `Vendée (85)`                  = "85"
    ),
    `Provence-Alpes-Côte d'Azur` = c(
      `Alpes-de-Haute-Provence (04)` = "04",
      `Hautes-Alpes (05)`            = "05",
      `Alpes-Maritimes (06)`         = "06",
      `Bouches-du-Rhône (13)`        = "13",
      `Var (83)`                     = "83",
      `Vaucluse (84)`                = "84"
    )
  ) %>% 
    (function(x) {
      names(x) <- enc2utf8(names(x))
      x
    })
  
  for (i in names(liste_departements)) {
    if (i != "France") {
      names(liste_departements[[i]]) <- enc2utf8(names(liste_departements[[i]]))
    }
  }
  
  ## Export des données

  if (dev) {
    usethis::use_data(date_preparation, classement_substances, 
                      classes_substances,
                      bnvd_ventes, bnvd_ventes_totales,
                      tendances, liste_departements,
                      CP_Layer, DPT_Layer,
                      DPT_Layer_stats, DPT_Layer_Pt, ACHAT_DPT_SUBSTANCE_STATS,
                      overwrite = TRUE)
  } else {
    save(list = c("date_preparation", "classement_substances",
                  "classes_substances",
                  "bnvd_ventes", "bnvd_ventes_totales",
                  "tendances", "liste_departements",
                  "CP_Layer", "DPT_Layer",
                  "DPT_Layer_stats", "DPT_Layer_Pt", "ACHAT_DPT_SUBSTANCE_STATS"
    ),
         file = path, compress = "bzip2")
  }
}
