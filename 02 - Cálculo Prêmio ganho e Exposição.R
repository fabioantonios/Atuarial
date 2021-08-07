base_emi <- as_tibble(data.frame(apolice=c("1","2","3"),
                                 ini_vig=as.Date(c("2020-05-01","2021-01-01-","2022-01-01"),format="%Y-%m-%d"),
                                 fim_vig=as.Date(c("2021-05-01","2022-01-01-","2023-01-01"),format="%Y-%m-%d")))

#data de apuração do resultado
data_faturamento <- as.Date("2021-12-31", forma="%Y-%m-%d")

#criação da base de exposição + prêmio ganho
base_emi_exp <- base_emi %>%
  #filtra as apólices que tem início de vigência  após a data de faturamento  
  filter(ini_vig<=data_faturamento) %>%  
  # cria vetor com os meses com prêmio ganho e empilha em linhas
  rowwise() %>% 
  mutate(meses_ganho = list(seq(from = as.Date(as.yearmon(ini_vig), frac = 0),
                                to = pmin(fim_vig, data_faturamento),
                                by = "1 month"))) %>% 
  unnest(increment_date) %>%
  ungroup() %>%
  
  
  base_emi_exp