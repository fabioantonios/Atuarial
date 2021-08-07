base_emi <- as_tibble(data.frame(apolice=c("1","2","3"),
                                 ini_vig=as.Date(c("2020-05-03","2021-01-01-","2022-01-01"),format="%Y-%m-%d"),
                                 fim_vig=as.Date(c("2021-05-02","2021-12-31-","2022-12-31"),format="%Y-%m-%d"),
                                 Premio=c(1200,1000,2000)))

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
  unnest(meses_ganho) %>%
  ungroup() %>%
  
  # define o primeiro e último periodo que o prêmio é ganho além de 
  # calcular a fração de prêmio ganho e exposição mensal
  mutate( inicio_pg = pmax(as.Date(as.yearmon(meses_ganho), frac = 0), ini_vig),
          fim_pg   = pmin(as.Date(as.yearmon(meses_ganho), frac = 1), pmin(fim_vig, data_faturamento)),
          pg_periodo   = round(as.numeric(fim_pg - inicio_pg + 1) / as.numeric(fim_vig - ini_vig + 1), 3),
          exposicao   = round(as.numeric(fim_pg - inicio_pg + 1) / 
                                as.numeric(as.Date(as.yearmon(meses_ganho), frac = 1) - as.Date(as.yearmon(meses_ganho), frac = 0) + 1), 3)) %>%
  identity() %>%
  #adiciona Anomes e prêmio ganho
  mutate(anomes = as.numeric(format(meses_ganho, "%Y"))*100+as.numeric(format(meses_ganho, "%m")),
         PG=pg_periodo*Premio) %>%
  #organiza variáveis
  select(apolice,anomes, ini_vig, fim_vig, PG, exposicao)
base_emi_exp
  
  #cria base sumarizada
  base_result <- base_emi_exp %>% 
                  group_by(anomes) %>%
                  summarise(PG = sum(PG),exposicao= sum(exposicao))
  base_result
