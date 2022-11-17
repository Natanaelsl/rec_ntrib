
setwd('C:/Users/natanael.leite/Documents/Natanael')

library(readr)
library(dplyr)
library(tidyr)
library(tidyselect)

receitas <- read_csv2("detalhamento_transferencias_completo.csv",
                      locale = readr::locale(encoding = "latin1"),
                      col_names = TRUE,
                      col_select = c(UF, Ano, Mes, Transferencia, Total),
                      progress = show_progress()) %>% 
  filter(UF == "GO") %>% 
  mutate(Mes = case_when(Mes != 10 ~ paste0("0", Mes),
                         Mes == 10 ~ paste0(Mes))
         ) %>% 
  mutate(
    Mes = as.Date(paste0(Ano ,Mes , "01"), format = "%Y%m%d")
  ) %>%
  pivot_wider(names_from = "Transferencia", values_from = "Total") %>% 
  arrange(Mes) %>%
  dplyr::select(-c(Ano,
                   `LC 87/96 (Lei Kandir)/ICMS/LC 87/96 - Lei Kandir`,
                   `LC 173/2020 (PFEC)/LC 173/2020 - PFEC INCISO II`,
                   `Cessão Onerosa/PBAEA`,
                   `LC 173/2020 (PFEC)/LC 173/2020 - PFEC INCISO I`,
                   `Cessão Onerosa/PBAEB`, 
                   `AFM/AFE/AUX`,
                   `FEX`
                   )) %>%  
  mutate(
  # FUNDEB = select(., contains("FUNDEB")) %>% colSums(na.rm = TRUE)
  FUNDEB = rowSums(dplyr::across(contains("FUNDEB")), na.rm = TRUE),
  FUNDEF = rowSums(dplyr::across(contains("FUNDEF")), na.rm = TRUE),
  Royalties = rowSums(dplyr::across(contains("Royalties")), na.rm = TRUE)
  )



# patterns <- unique(substr(names(DT), 1, 3))  # store patterns in a vector
# new <- sapply(patterns, function(xx) rowSums(DT[,grep(xx, names(DT)), drop=FALSE]))  # loop through
