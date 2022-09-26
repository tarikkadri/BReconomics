## Carregando pacotes
library(tidyverse)
library(sidrar)
library(ggplot2)

## Análise da Contribuição dos Grupos do IPCA para a Variação Mensal

## Importando dados
variation <- 
  "/t/7060/n1/all/v/63/p/all/c315/7169,7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202" %>%
  sidrar::get_sidra(api=.) %>%
  mutate(date = readr::parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  select(date, `Geral, grupo, subgrupo, item e subitem`, valor=Valor) %>%
  as_tibble()

weight <- 
  "/t/7060/n1/all/v/66/p/all/c315/7169,7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v66%204" %>%
  sidrar::get_sidra(api=.) %>%
  mutate(date = readr::parse_date(`Mês (Código)`, format = "%Y%m")) %>%
  select(date, `Geral, grupo, subgrupo, item e subitem`, valor=Valor) %>%
  as_tibble()

ipca <- tibble(
  date = weight$date,
  group = weight$`Geral, grupo, subgrupo, item e subitem`,
  weighted_var = (variation$valor*weight$valor)/100,
  variation = variation$valor
)

ipca_no_index <- ipca %>%
  filter(group !="Índice geral") %>%
  tail(n = 9 * 24) # 9 grupos e ultimos 24 meses de dados 

ipca_index <- ipca %>%
  filter(group =="Índice geral")

## Gráfico
ggplot(ipca_no_index, aes(x=date, y=weighted_var, fill=group)) +
  geom_bar(stat="identity", position="stack") +
  theme(legend.key.size = unit(0.2, "cm"))

#grafico barras na horizontal com os valores dos 9 grupos

#grafico barras na vertical da serie historica completa

#grafico linhas do ipca acumulado até o respectivo mês

## histórico ipca acumulado e metas BACEN

## inflação implícita, calcular nos vértices
