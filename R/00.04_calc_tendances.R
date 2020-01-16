#' @import dplyr
calc_tendances <- function(df, quantite, group = NULL) {
  
  df <- rename(df, Q = !!quantite)

    calc_senslope <- function(df2, ...) {
      df2 <- filter(df2, !is.na(Q))
      
        if (nrow(df) >= 3) {
            sen_slope <- arrange(df2, annee) %>%
                pull(Q)           %>%
                trend::sens.slope()

            tibble(pente   = sen_slope$estimate,
                   p_value = sen_slope$p.value) %>% 
              mutate(intercept = median(df2$Q - pente * df2$annee))
        } else {
            tibble(pente   = NA_real_,
                   p_value = NA_real_,
                   intercept = NA_real_)
        }
    }

    group_by_at(df, .vars = c(group, "substance")) %>%
        group_modify(.f = calc_senslope) %>%
        ungroup() %>%
        filter(!is.na(pente))
}


