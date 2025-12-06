library(tidyverse)
library(palmerpenguins)
library(janitor)

?penguins

# visualizando os dados
glimpse(penguins)

# análise exploratória

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

##
glimpse(penguins_raw)

penguins_raw |>
  filter(year(date_egg) == 2007) |> 
  count(date_egg) |> 
  ggplot(aes(x = date_egg, y = n)) +
  geom_line()

penguins |> 
  count(year) |> 
  ggplot(aes(x = year, y = n)) +
  geom_line()

library(gapminder)

glimpse(gapminder)

gapminder |> 
  group_by(year) |>
  summarise(n = sum(pop)) |> 
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  geom_point()


