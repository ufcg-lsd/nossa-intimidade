get_macs_perenes <- function(macs_vistos) {
    library(dplyr)
    library(lubridate)
    
    dias_possiveis = macs_vistos %>% 
        transmute(dia = paste(year(t), yday(t))) %>% 
        distinct() %>% 
        NROW()
    
    sempre_la = macs_vistos %>% 
        filter(hour(t) %in% 1:6) %>% 
        group_by(mac, dia = day(t)) %>% 
        summarise(n = n()) %>% 
        filter(n > 1) %>% 
        summarise(dias = n() / dias_possiveis) %>% 
        filter(dias >= .2) # decidimos .2 olhando o histograma
    
    return(sempre_la)
}
