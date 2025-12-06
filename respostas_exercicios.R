library(tidyverse)
library(gapminder)

# EXERCÍCIOS -------------------------------------------------------------------

# Usando técnicas de análise exploratória de dados, produza gráficos
# adequados para responder:

# 1. Como foi a evolução da expectativa de vida no Brasil ao
#    longo dos anos?

gapminder |> 
  filter(country == "Brazil") |> 
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_line() +
  labs(x = "Ano", y = "Expectativa de vida ao nascer (em anos)",
       title = "Evolução da expectativa de vida no Brasil") +
  theme_minimal()

# 2. Como estava a distribuição do PIB per capita por continente
#    em 2007?

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  labs(x = "Continente", y = "PIB per capita", 
       title = "PIB per capita em 2007, por continente") +
  theme_minimal()

# 3. Usando dados de 2007, há relação entre PIB per capita 
#    e expectativa de vida no mundo? e visualizando por continente? 

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point()

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  facet_wrap(~ continent, scales = "free") +
  scale_x_continuous(labels = scales::label_number(scale = 1/1e3)) +
  labs(x = "PIB per capita (em milhares de doláres)",
       y = "Expectativa de vida ao nascer (em anos)",
       title = "PIB per capita vs. expectativa de vida, por continente") +
  theme_bw()


# 4. Mostre os 10 países com maior população em 2007.

gapminder |> 
  filter(year == 2007) |> 
  slice_max(pop, n = 10) |> 
  ggplot(aes(x = pop, y = reorder(country, pop))) +
  geom_col() +
  scale_x_continuous(
    labels = scales::label_number(scale = 1/1e6)
  ) +
  labs(x = "População (em milhões de pessoas)", y = "País", 
       title = "10 Países com maior população em 2007") +
  theme_minimal()

# 5. Em que ano a Argentina ultrapassou o Chile em PIB per capita?

# R: em 2002, o PIB per capita do Chile ultrapassou o da Argentina

gapminder |> 
  filter(country %in% c("Argentina", "Chile")) |> 
  ggplot(aes(x = year, y = gdpPercap, color = country)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1e3)
  ) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(x = "Ano", y = "PIB per capita (em milhares de dólares)", color = "País",
       title = "Evolução do PIB per capita de Argentina e Chile") +
  theme_minimal()
