
# Relação entre produção alimentar e fome --------------------------------------------------------------------------------------------------
# Autoria do script ------------------------------------------------------------------------------------------------------------------------
# Data: 05/11/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/ --------------------------------------------------------------------------------------------------

# Países incluídos nas análises ------------------------------------------------------------------------------------------------------------

### India, China, Estados Unidos, Brasil, Russia, México,
### Espanha, Argentina e Canada.

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

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

carne <- carne %>%
  select(-Code) %>%
  filter(Entity %in% c("India", "China", "Brazil", "United States", 
                       "Mexico", "Spain", "Russia", "Canada", "Argentina"),
         between(Year, 2001, 2019)) %>%
  rename(prod_carne = Meat..total...00001765....Production...005510....tonnes) %>%
  view()
  
trigo <- trigo %>%
  select(-Code) %>%
  filter(Entity %in% c("India", "China", "Brazil", "United States", 
                       "Mexico", "Spain", "Russia", "Canada", "Argentina"),
         between(Year, 2001, 2019)) %>%
  rename(prod_trigo = Wheat...00000015....Production...005510....tonnes) %>%
  view()

arroz <- arroz %>%
  select(-Code) %>%
  filter(Entity %in% c("India", "China", "Brazil", "United States", 
                       "Mexico", "Spain", "Russia", "Canada", "Argentina"),
         between(Year, 2001, 2019)) %>%
  rename(prod_arroz = Rice...00000027....Production...005510....tonnes) %>%
  view()

batata <- batata %>%
  select(-Code) %>%
  filter(Entity %in% c("India", "China", "Brazil", "United States", 
                       "Mexico", "Spain", "Russia", "Canada", "Argentina"),
         between(Year, 2001, 2019)) %>%
  rename(prod_batata = Potatoes...00000116....Production...005510....tonnes) %>%
  view()

fome <- fome %>%
  select(-Code) %>%
  filter(Entity %in% c("India", "China", "Brazil", "United States", 
                       "Mexico", "Spain", "Russia", "Canada", "Argentina"),
         between(Year, 2001, 2019)) %>%
  rename(porc_subnut = Prevalence.of.undernourishment....of.population.) %>%
  view()
