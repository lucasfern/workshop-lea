library(tidyverse)
library(palmerpenguins)
library(gapminder)

# ANÁLISE EXPLORATÓRIA E VISUALIZAÇÃO DE DADOS ---------------------------------

?penguins

# visualizando os dados
glimpse(penguins)

## pinguins por espécie
penguins |> 
  count(species) |> 
  # informa mapeamento das variáveis
  ggplot(mapping = aes(x = reorder(species, desc(n)), y = n)) +
  # adiciona geometria de coluna
  geom_col(fill = "cornflowerblue") +
  # adiciona rótulos
  labs(x = "Espécie", y = "Frequência",
       title = "Distibuição dos pinguins por espécie") +
  # muda o tema do gráfico
  theme_minimal()

## pinguins por ilha
penguins |> 
  count(island) |> 
  ggplot(aes(x = reorder(island, desc(n)), y = n)) +
  geom_col(fill = "cornflowerblue") +
  labs(x = "Ilha", y = "Frequência",
       title = "Distibuição dos pinguins por ilha") +
  theme_minimal()

## pinguins por ilha e espécie
penguins |> 
  count(island, species) |> 
  ggplot(aes(x = island, y = n, fill = species)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.5) +
  labs(x = "Ilha", y = "Frequência", fill = "Espécie",
       title = "Distribuição dos pinguins por ilha e espécie") +
  theme_minimal()
  
penguins |> 
  count(island, species) |> 
  ggplot(aes(x = island, y = n)) +
  geom_col() +
  facet_wrap(~ species) +
  labs(x = "Ilha", y = "Frequência",
       title = "Distribuição dos pinguins por ilha e espécie") +
  theme_bw()

## pinguins por comprimento do bico
penguins |> 
  ggplot(aes(x = bill_length_mm)) +
  geom_histogram() +
  labs(x = "Comprimento do bico (mm)", y = "Frequência",
       title = "Distribuição do comprimento do bico dos pinguins") +
  theme_minimal()

## pinguins por comprimento do bico e espécie
penguins |> 
  ggplot(aes(x = bill_length_mm, fill = species)) +
  geom_histogram() +
  labs(
    x = "Comprimento do bico (mm)", y = "Frequência",
    title = "Distribuição do comprimento do bico dos pinguins, por espécie"
  ) +
  theme_minimal()

penguins |> 
  ggplot(aes(x = bill_length_mm)) +
  geom_histogram() +
  facet_wrap(~ species, ncol = 1) +
  labs(
    x = "Comprimento do bico (mm)", y = "Frequência",
    title = "Distribuição do comprimento do bico dos pinguins, por espécie"
  ) +
  theme_bw()

penguins |> 
  ggplot(aes(x = bill_length_mm, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(
    x = "Comprimento do bico (mm)", y = "Frequência",
    title = "Distribuição do comprimento do bico dos pinguins, por espécie"
  ) +
  theme_minimal()

penguins |> 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_boxplot() +
  labs(
    x = "Espécie", y = "Comprimento do bico (mm)",
    title = "Distribuição do comprimento do bico dos pinguins, por espécie"
  ) +
  theme_minimal()


## comprimento vs. largura do bico por espécie
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  labs(x = "Comprimento do bico (mm)", y = "Altura do bico (mm)",
       title = "Comprimento vs. altura do bico dos pinguins") +
  theme_minimal()

## comprimento vs. largura do bico por espécie
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species,
             shape = species)) +
  geom_point() +
  labs(x = "Comprimento do bico (mm)", y = "Altura do bico (mm)",
       title = "Comprimento vs. altura do bico dos pinguins, por espécie") +
  theme_minimal()

penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  facet_wrap(~ species, scales = "free") +
  labs(x = "Comprimento do bico (mm)", y = "Altura do bico (mm)",
       title = "Comprimento vs. altura do bico dos pinguins, por espécie") +
  theme_bw()

?gapminder

glimpse(gapminder)

## evolução da população mundial
gapminder |> 
  group_by(year) |>
  summarise(n = sum(pop)) |> 
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1e9)
  ) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(x = "Ano", y = "População (em bilhões)",
       title = "População mundial entre 1952 e 2007") +
  theme_minimal()


# EXERCÍCIOS -------------------------------------------------------------------

# Usando técnicas de análise exploratória de dados, produza gráficos
# adequados para responder:

# 1. Como foi a evolução da expectativa de vida no Brasil ao
#    longo dos anos?

# 2. Como estava a distribuição do PIB per capita por continente
#    em 2007?

# 3. Usando dados de 2007, há relação entre PIB per capita 
#    e expectativa de vida no mundo? e visualizando por continente? 

# 4. Mostre os 10 países com maior população em 2007.

# 5. Em que ano a Argentina ultrapassou o Chile em PIB per capita?

# Dica: 
# As funções filter() e slice_max() servem para filtrar linhas
# específicas. Utilize o help() para saber mais como funcionam.


# REFERÊNCIAS GRATUITAS --------------------------------------------------------

# https://r4ds.hadley.nz/
# https://ggplot2-book.org/
