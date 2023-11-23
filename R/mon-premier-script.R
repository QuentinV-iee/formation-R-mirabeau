# chargement des packages ----
library(tidyverse)
library(readxl) # pour lire l'excel
library(officer) # pour ecrire le pptx

# import des données -----
donnees_brutes <- readxl::read_xlsx("C:/Users/quent/Documents/formation R/projet01/projet01/donnees/mario-kart.xlsx", skip = 1) %>%
  select(-image, -color)

donnees_brutes

# transformation des données ----

## 'noms_scores' contient des noms à présenter
noms_scores <- colnames(donnees_brutes)
noms_scores <- noms_scores[-1]
noms_scores

## 'donnees' contient les donnnées pivotées
donnees <- donnees_brutes %>%
  pivot_longer(
    cols = all_of(noms_scores),
    names_to = "Ability",
    values_to = "score"
  )
donnees

## premier graphique pour voir les données

ggplot(data = donnees, mapping = aes(y = Characters, x = Ability, fill = score)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(2, 6)) +
  coord_equal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

# un peu de structuration -----

## détermination d'un ordre pour présenter les personnages -----
# dans la variable 'char_ordered'
char_ordered <- donnees %>%
  group_by(Characters) %>%
  summarise(score = sum(score, na.rm = TRUE)) %>%
  arrange(desc(score)) %>%
  pull(Characters)

## utilisation de l'ordre pour restructurer données -----
donnees <- donnees %>%
  mutate(Characters = factor(Characters, levels = char_ordered))

# reporting des résultats -----

## relance du graphique et affinnage -----
gg <- ggplot(
  data = donnees,
  mapping = aes(y = Characters, x = Ability, fill = score)
) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(2, 6)) +
  coord_equal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

print(gg)

## sauvegarde du graph dans 'graphiques/gg.png' -----
ggsave(gg,
  filename = "graphiques/gg.png",
  width = 7, height = 12,
  units = "in",
  dpi = 150,
  device = "png"
)

## génération d'un PowerPoint dans 'rapports/premier-rapport.pptx' -----

read_pptx() %>%
  add_slide() %>%
  ph_with(gg, location = ph_location_fullsize()) %>%
  print(target = "./rapports/premier-rapport.pptx")
