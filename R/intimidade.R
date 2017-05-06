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

filtra_pessoas_por_horario <- function(df, hora.inicio, hora.fim) {
    pessoas_no_horario <- df %>%
        filter(hour(t) >= hora.inicio, hour(t) < hora.fim)
    return(pessoas_no_horario)
}

agrupa_presencas_por_dia_da_semana <- function(df) {
    presenca_por_dia <- df %>%
        group_by(mac, dia_f = floor_date(t, unit = "day")) %>%
        filter(t == first(t)) %>% 
        ungroup() %>% 
        mutate(dia = wday(t)) %>%
        group_by(mac, dia) %>% 
        summarise(presenca = n()) %>%
        filter(presenca > 1)
    return(presenca_por_dia)
}

filtra_diferentoes <- function(df) {
    diferentoes <- df %>%
        group_by(mac) %>%
        summarise(soma.presencas = sum(presenca))
    corte <- quantile(diferentoes$soma.presencas, probs = c(.8))
    return(filter(diferentoes, soma.presencas >= corte))
}

filtra_atividade <- function(diferentoes) {
    atividade <- diferentoes %>%
        group_by(dia) %>%
        summarise(npessoas = n())
    return(atividade)
}