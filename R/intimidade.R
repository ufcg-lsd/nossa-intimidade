get_macs_perenes <- function(macs_macs_vistos) {
    library(dplyr)
    library(lubridate)
    
    dias_possiveis = macs_macs_vistos %>% 
        transmute(dia = paste(year(t), yday(t))) %>% 
        distinct() %>% 
        NROW()
    
    sempre_la = macs_macs_vistos %>% 
        filter(hour(t) %in% 1:6) %>% 
        group_by(mac, dia = day(t)) %>% 
        summarise(n = n()) %>% 
        filter(n > 1) %>% 
        summarise(dias = n() / dias_possiveis) %>% 
        filter(dias >= .2) # decidimos .2 olhando o histograma
    
    return(sempre_la)
}

conta_macs <- function(macs_vistos, intervalo = 5) {
    library(dplyr)
    library(lubridate)
    macs_vistos %>% 
        mutate(dia = floor_date(t, unit = "day")) %>% 
        group_by(periodo = floor_date(t, unit = "hour") + minutes(floor(minute(t)/intervalo)*intervalo)) %>%
        distinct(periodo, mac, .keep_all = TRUE) %>% 
        count() %>% 
        return()
}
