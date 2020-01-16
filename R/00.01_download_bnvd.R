#' Import and prepare BNVD data
#'
#' @import dplyr
#' @export
download_bnvd <- function(base_url, period) {
  cat("\nLes données de vente de pesticides par département sont téléchargées depuis data.eaufrance.fr:\nhttp://www.data.eaufrance.fr/jdd/bd45f801-45f7-4f8c-b128-a1af3ea2aa3e\n\n")
  
  download_bnvd_file <- function(url, dest) {
    
    temp_file <- tempfile(tmpdir = dest, fileext = ".zip")
    
    download.file(url, destfile = temp_file, mode = "wb")
    
    unzip(temp_file, exdir = dest)

  }
  
  read_bnvd <- function(dest) {
    purrr::map(.x = list.files(path = dest,
                          pattern = "_VENTE_SUBSTANCE_",
                          full.names = TRUE),
               .f =  function(x) {
                 readr::read_delim(x, 
                            delim = ";", 
                            locale = readr::locale(decimal_mark = ","),
                            progress = FALSE )
                 }) %>% 
      bind_rows()
  }
  
  temp_dir = tempdir()
  
  paste0(base_url, period, ".zip")                                             %>% 
    purrr::walk(.x = ., .f = download_bnvd_file, dest = temp_dir)
  
  data <- read_bnvd(dest = temp_dir) %>%
    mutate(quantite_substance = as.numeric(quantite_substance)) %>% 
    group_by(departement, annee, substance, classification)  %>%
    summarise(quantite = sum(quantite_substance) / 1e3) %>%
    ungroup()                                     %>%
    mutate(departement = 
             case_when(departement == "TERRITOIRE DE BELFORT" ~
                         "TERRITOIRE-DE-BELFORT",
                       TRUE ~ departement),
           classification = case_when(
             classification %in% "T, T+, CMR" ~ "danger_sante",
             classification %in% c("N Organique", "N minéral") ~ "danger_environnement",
             classification %in% "Autre" ~ "autres_substances",
             TRUE ~ classification
           ))
  
  unlink(temp_dir, recursive = TRUE)
  
  return(data)
}
