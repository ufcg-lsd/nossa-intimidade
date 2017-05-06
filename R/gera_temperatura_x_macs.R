library("dplyr")

setwd("~/nossa-intimidade/")

temp_data <- read.csv("data/temp_data2.csv")
mac_data <- read.csv("data/daily_sum_macs.csv")

temperatura_completa <- select(temp_data, Data, Temp.Comp.Media)
temperatura_sem_na <- na.omit(temperatura_completa)
View(temperatura_sem_na)
merged_table <- merge(temperatura_sem_na, mac_data, by = "Data")
View(merged_table)
without_wknd <- filter(merged_table, num_macs > 36)
write.csv(without_wknd, file = "data/temperatura_macs.csv")
