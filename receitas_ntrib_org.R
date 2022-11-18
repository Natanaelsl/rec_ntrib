
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
  mutate(Mes = case_when(Mes != 10 & Mes != 11 & Mes != 12 ~ paste0("0", Mes),
                         Mes == 10 | Mes == 11 | Mes == 12 ~ paste0(Mes))
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


# ANÁLISE PAR FPE
adf.test(receitas$FPE)
FPE <- ts(diff(receitas$FPE), start = c(1997,01), freq = 12)
ar <- ar(FPE, aic = TRUE, method = "mle") 
rect_ajustado <- FPE - ar$resid
predar <- predict(ar, n.ahead=12, se.fit = TRUE)
predar$pred
ts.plot(FPE, rect_ajustado, predar$pred, lty = 1:3, col = c("blue", "red", "green"))
m.lev <- as.matrix(receitas[, "FPE" ])
m.varf_lev_ft <- rbind(m.lev, matrix(NA, 12, 1))
m.ft_df <- do.call(cbind,lapply(predar$pred, 
                                function(x) x[]))
nr_lev <- nrow(receitas)
for(h in (nr_lev+1):(nr_lev+12)) {
  hf <- h - nr_lev
  m.varf_lev_ft[h,] <- m.varf_lev_ft[h-1,] + m.ft_df[hf]
}
w <- as.data.frame(m.varf_lev_ft)
w$Mes <- as.Date(seq(as.Date("1997-01-01"), by = "month", length.out = nrow(w)))
checkresiduals(ar)

## GRÁFICO FPE PROJEÇÃO
w <- w %>% 
  mutate(perid = case_when(Mes <= "2022-10-31" ~ paste0("realizado"),
                           Mes > "2022-10-31" ~ paste0("projetado"))
         )
ggplot(w, aes(x=Mes, y=FPE, color = perid)) +
  geom_line() +
  theme_bw()+
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "R$ ", suffix = " mi")) +
  scale_x_date(date_breaks ="1 year", date_labels = "%Y") +
  labs(x ="Ano", y = "Real vs Previsto", caption = NULL,
       title = "FPE")


# patterns <- unique(substr(names(DT), 1, 3))  # store patterns in a vector
# new <- sapply(patterns, function(xx) rowSums(DT[,grep(xx, names(DT)), drop=FALSE]))  # loop through
