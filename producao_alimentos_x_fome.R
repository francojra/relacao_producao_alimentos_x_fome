
# Relação entre produção alimentar e fome --------------------------------------------------------------------------------------------------
# Autoria do script ------------------------------------------------------------------------------------------------------------------------
# Data: 05/11/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/ --------------------------------------------------------------------------------------------------

# Países incluídos nas análises ------------------------------------------------------------------------------------------------------------

### India, China, Estados Unidos, Brasil, Russia, México.

# Período ----------------------------------------------------------------------------------------------------------------------------------

### De 2001 a 2019

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)

# Carregar tabelas -------------------------------------------------------------------------------------------------------------------------

carne <- read.csv("meat-production-tonnes.csv")
view(carne)
names(carne)
trigo <- read.csv("wheat-production.csv")
view(trigo)
names(trigo)
arroz <- read.csv("rice-production.csv")
view(arroz)
names(arroz)
batata <- read.csv("potato-production.csv")
view(batata)
names(batata)
fome <- read.csv("prevalence-of-undernourishment.csv")
view(fome)
names(fome)

# Manipular dados 1 --------------------------------------------------------------------------------------------------------------------------

carne <- carne %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States"),
         between(Year, 2001, 2019)) %>%
  rename(prod_carne = Meat..total...00001765....Production...005510....tonnes) %>%
  view()
  
trigo <- trigo %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States"),
         between(Year, 2001, 2019)) %>%
  rename(prod_trigo = Wheat...00000015....Production...005510....tonnes) %>%
  view()

arroz <- arroz %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States"),
         between(Year, 2001, 2019)) %>%
  rename(prod_arroz = Rice...00000027....Production...005510....tonnes) %>%
  view()

batata <- batata %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States"),
         between(Year, 2001, 2019)) %>%
  rename(prod_batata = Potatoes...00000116....Production...005510....tonnes) %>%
  view()

fome <- fome %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States"),
         between(Year, 2001, 2019)) %>%
  rename(porc_subnut = Prevalence.of.undernourishment....of.population.) %>%
  view()

# Unir tabelas -----------------------------------------------------------------------------------------------------------------------------

carne_fome <- right_join(carne, fome, by = c("Entity", "Year"))
view(carne_fome)

trigo_fome <- right_join(trigo, fome, by = c("Entity", "Year"))
view(trigo_fome)

arroz_fome <- right_join(arroz, fome, by = c("Entity", "Year"))
view(arroz_fome)

batata_fome <- right_join(batata, fome, by = c("Entity", "Year"))
view(batata_fome)

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

g1 <- ggplot(carne_fome, aes(x = Year, y = porc_subnut,
                        color = Entity, size = prod_carne)) +
  geom_point() 
g1

g2 <- ggplot(trigo_fome, aes(x = prod_trigo, y = porc_subnut,
                        label = Entity)) +
  geom_label(size = 5)
g2

g3 <- ggplot(arroz_fome, aes(x = prod_arroz, y = porc_subnut,
                        label = Entity)) +
  geom_label(size = 5)
g3

g4 <- ggplot(batata_fome, aes(x = prod_batata, y = porc_subnut,
                        label = Entity)) +
  geom_label(size = 5)
g4



