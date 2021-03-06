library(tidyverse)
library(jsonlite)
source("R/intimidade.R")

le_parte = function(arquivo_lido){
    lido = arquivo_lido %>% 
        as.tibble() %>% 
        transmute(t = ymd_hms(`timestamp`), 
                  mac = mac, 
                  signal = signal)
    perenes = get_macs_perenes(lido)
    
    lido %>% 
        filter(!(mac %in% perenes$mac)) %>% 
        return()
}

for (f in list.files("..", "db_split_*")) {
    arquivo = paste0("../", f)
    print(paste("Lendo", arquivo))
    x = stream_in(file(arquivo), pagesize = 50000, flatten = TRUE)
    em_csv = le_parte(x)
    write_csv(em_csv, paste0("../csv_", f, ".csv"))
}

vistos = tibble(arquivo = list.files("..", "csv*")) %>% 
    rowwise() %>%
    do(read_csv(paste0("../", .$arquivo), 
                col_types = cols(
                    t = col_datetime(format = ""),
                    mac = col_character(),
                    signal = col_integer()
                )))

write_csv(vistos, "db_lsd_sem_perenes.csv")
