
# Relação entre produção alimentar e fome --------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 05/11/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/ --------------------------------------------------------------------------------------------------

# Países incluídos nas análises ------------------------------------------------------------------------------------------------------------

### China, Índia, Estados Unidos, Indonésia, Paquistão e Brasil.

# Período ----------------------------------------------------------------------------------------------------------------------------------

### De 2001 a 2019

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(gridExtra)
library(ggthemr)

# Carregar tabelas -------------------------------------------------------------------------------------------------------------------------

carne <- read.csv("meat-production-tonnes.csv")
view(carne)
names(carne)
milho <- read.csv("maize-production.csv")
view(milho)
names(milho)
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
  filter(Entity %in% c("China", "Brazil", "United States",
                       "Pakistan","Indonesia", "India"),
         between(Year, 2001, 2019)) %>%
  rename(prod_carne = Meat..total...00001765....Production...005510....tonnes) %>%
  view()
  
milho <- milho %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States", 
                       "Pakistan",  "Indonesia", "India"),
         between(Year, 2001, 2019)) %>%
  rename(prod_milho = Maize...00000056....Production...005510....tonnes) %>%
  view()

arroz <- arroz %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States", 
                       "Pakistan", "Indonesia", "India"),
         between(Year, 2001, 2019)) %>%
  rename(prod_arroz = Rice...00000027....Production...005510....tonnes) %>%
  view()

batata <- batata %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States", 
                       "Pakistan", "Indonesia", "India"),
         between(Year, 2001, 2019)) %>%
  rename(prod_batata = Potatoes...00000116....Production...005510....tonnes) %>%
  view()

fome <- fome %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Brazil", "United States", 
                       "Pakistan", "Indonesia", "India"),
         between(Year, 2001, 2019)) %>%
  rename(porc_subnut = Prevalence.of.undernourishment....of.population.) %>%
  view()

# Unir tabelas -----------------------------------------------------------------------------------------------------------------------------

carne_fome <- right_join(carne, fome, by = c("Entity", "Year"))
view(carne_fome)

milho_fome <- right_join(milho, fome, by = c("Entity", "Year"))
view(milho_fome)

arroz_fome <- right_join(arroz, fome, by = c("Entity", "Year"))
view(arroz_fome)

batata_fome <- right_join(batata, fome, by = c("Entity", "Year"))
view(batata_fome)

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a_gui()
c4a("Dark2", 6)

ggthemr('greyscale') # tema do gráfico

g1 <- ggplot(carne_fome, aes(x = Year, y = porc_subnut,
                        color = Entity, size = prod_carne)) +
  geom_point() +
  geom_line(size = 0.5) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                 "#E7298A", "#66A61E", "#E6AB02"),
                     labels = c("Brasil", "China", "Índia", 
                                "Indonésia", "Paquistão", "Estados Unidos")) +
  scale_size_continuous(labels = scales::comma, 
                        name = "Produção de carne\n em toneladas") +
  labs(x = "Tempo (anos)", y = "Subnutrição (%)", col = "Países") +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12, color = "black"),
        legend.background = element_blank(),
        legend.key = element_blank()) +
   guides(colour = guide_legend(override.aes = list(size = 2.3, stroke = 1.5)))
g1

g2 <- ggplot(milho_fome, aes(x = Year, y = porc_subnut,
                        color = Entity, size = prod_milho,
                        group = Entity)) +
  geom_point() +
  geom_line(size = 0.5) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                 "#E7298A", "#66A61E", "#E6AB02"),
                     labels = c("Brasil", "China", "Índia", 
                                "Indonésia", "Paquistão", "Estados Unidos"),
                     guide = FALSE) +
  scale_size_continuous(labels = scales::comma, 
                        name = "Produção de milho\n em toneladas") +
  labs(x = "Tempo (anos)", y = "Subnutrição (%)", col = "Países") +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12, color = "black"),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2.3, stroke = 1.5)))
g2

g3 <- ggplot(arroz_fome, aes(x = Year, y = porc_subnut,
                        color = Entity, size = prod_arroz)) +
  geom_point() +
  geom_line(size = 0.5) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                 "#E7298A", "#66A61E", "#E6AB02"),
                     labels = c("Brasil", "China", "Índia", 
                                "Indonésia", "Paquistão", "Estados Unidos"),
                     guide = FALSE) +
  scale_size_continuous(labels = scales::comma, 
                        name = "Produção de arroz\n em toneladas") +
  labs(x = "Tempo (anos)", y = "Subnutrição (%)", col = "Países") +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12, color = "black"),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2.3, stroke = 1.5)))
g3

g4 <- ggplot(batata_fome, aes(x = Year, y = porc_subnut,
                        color = Entity, size = prod_batata)) +
  geom_point() +
  geom_line(size = 0.5) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                 "#E7298A", "#66A61E", "#E6AB02"),
                     labels = c("Brasil", "China", "Índia", 
                                "Indonésia", "Paquistão", "Estados Unidos"),
                     guide = FALSE) +
  scale_size_continuous(labels = scales::comma, 
                        name = "Produção de batata\n em toneladas") +
  labs(x = "Tempo (anos)", y = "Subnutrição (%)", col = "Países") +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12, color = "black"),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2.3, stroke = 1.5)))
g4

grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2) # 12 - 18

