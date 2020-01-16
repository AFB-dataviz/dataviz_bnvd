#' @import dplyr 
identify_top_substances <-  function(df, prop = 0.5, group = NULL) {

    calc_senslope <- function(dat, ...) {
        dat <- filter(dat, !is.na(Q_rm))
        
        if (nrow(dat) >= 3) {
             sens_slope <- arrange(dat, annee) %>% 
            pull(Q_rm) %>% 
            trend::sens.slope()
        
        tibble(pente   = sens_slope$estimate,
               p_value = sens_slope$p.value) %>% 
            mutate(intercept = median(pull(dat, Q_rm) - pente * pull(dat, annee)))
        } else {
            tibble(pente   = NA_real_,
                   p_value = NA_real_,
                   intercept = NA_real_)
        }
    }
    
    # Normalise les quantités de substances par rapport à la moyenne 
    df2 <- group_by_at(df, .vars = c(group, "substance")) %>% 
        mutate(Q_rm = zoo::rollmean(quantite, k = 3, fill = NA)) %>% 
        mutate(Q_rm = 100 * Q_rm / mean(Q_rm, na.rm = TRUE))
    
    # Classe les substances en fonction de leur proportion dans les quantités
    # vendues par département pour toutes les substances et l'ensemble des dates
    classement <- group_by_at(df, .vars = c(group, "substance")) %>%
        summarise(quantite = sum(quantite))                      %>%
        group_by_at(.vars = group) %>% 
        mutate(quantite_rel = 100 * quantite / sum(quantite)) %>%
        arrange(desc(quantite_rel), .by_group = TRUE) %>%
        mutate(quantite_cum = cumsum(quantite_rel))             %>%
        mutate(more_than_prop = quantite_cum >= prop * 100) %>%
        mutate(rang = seq(n())) %>% 
        mutate(top = if_else(rang <= min(which(more_than_prop)),
                             TRUE, FALSE)) %>% 
        select_at(.vars = c(group, "substance", "rang", "top",
                                "quantite", "quantite_rel")) %>% 
        ungroup()
        
    trend_substance <- left_join(df2, 
                                 select_at(classement,
                                           .vars = c(group, "substance", "top")), 
                                 by = c(group, "substance")) %>%
            filter(top) %>% 
            group_by_at(.vars = c(group, "substance")) %>% 
            group_modify(.f = calc_senslope) %>% 
            mutate(trend = case_when(
                round(pente) <= -5 ~ "diminution",
                round(pente) >= 5 ~ "augmentation",
                TRUE ~ "stable"
            )) %>% 
            select_at(.vars = c(group, "substance", "trend"))
        
        filter(classement, top) %>% 
            left_join(trend_substance, by = c(group, "substance")) %>% 
            select(-top)
    }

    
