# instalando pacotes necessários
install.packages("palmerpenguins")
install.packages("gapminder")

# carregando pacotes necessários
library(tidyverse)
library(palmerpenguins)
library(gapminder)

penguins

help(penguins)
?penguins

glimpse(penguins)
# distribuição por espécie
penguins |> 
  # contagem dos pinguins por espécie
  count(species) |> 
  # informa mapeamento das variáveis
  ggplot(
    mapping = 
      aes(x = reorder(species, desc(n)),
          y = n,
          label = n)
  ) +
  # adiciona geometria de coluna
  geom_col(fill = "cornflowerblue") +
  geom_text(vjust = 0) +
  # personaliza rótulos
  labs(x = "Espécie", y = "Frequência",
       title = "Distribuição dos pinguins por espécie") +
  theme_minimal()

# distribuição por ilha
penguins |> 
  # contagem dos pinguins por espécie
  count(island) |> 
  # informa mapeamento das variáveis
  ggplot(
    mapping = 
      aes(x = reorder(island, desc(n)),
          y = n,
          label = n)
  ) +
  # adiciona geometria de coluna
  geom_col(fill = "cornflowerblue") +
  geom_text(vjust = 0) +
  # personaliza rótulos
  labs(x = "Ilha", y = "Frequência",
       title = "Distribuição dos pinguins por ilha") +
  theme_minimal()

# distribuição dos pinguins por ilha e espécie

penguins |> 
  count(species, island) |> 
  ggplot(aes(x = island, y = n, fill = species)) +
  geom_col(position = "dodge") +
  labs(x = "Ilha", y = "Frequência", fill = "Espécie",
       title = "Distribuição por ilha e espécie") +
  theme_bw() +
  theme(plot.title = element_text(color = "red"))

# distribuição do comprimento do bico
penguins |> 
  ggplot(aes(x = bill_length_mm, fill = species)) +
  geom_histogram()

penguins |> 
  ggplot(aes(x = bill_length_mm, fill = species)) +
  geom_density(alpha = 0.5)

penguins |> 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_boxplot()

# comparação comprimento vs. altura do bico

penguins |> 
  ggplot(
    aes(x = bill_length_mm, 
        y = bill_depth_mm,
        color = species)
  ) +
  geom_point()

penguins |> 
  ggplot(
    aes(x = bill_length_mm, 
        y = bill_depth_mm,
        color = species)
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ species, scales = "free")

####

gapminder
?gapminder

gapminder |>
  group_by(year) |> 
  summarise(soma_pop = sum(pop)) |> 
  ggplot(aes(x = year, y = soma_pop)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1e9)
  ) +
  labs(y = "População (em bilhões de pessoas)")


