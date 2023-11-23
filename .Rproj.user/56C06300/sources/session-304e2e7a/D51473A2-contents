
library(tidyverse)
library(readxl) # pour lire l'excel
library(officer) # pour ecrire le pptx
library(palmerpenguins)

personnes <- read_csv(
  file = "./donnees/personnes.csv",
  col_types = cols(
    ID = col_skip(),
    Age = col_double(),
    Sex = col_character(),
    IQ = col_skip(),
    Height = col_double(),
    Died = col_logical(),
    start_date = col_date(format = ""),
    Scoring = col_double(),
    Smoker = col_skip(),
    Grade_1 = col_double(),
    Grade_2 = col_double(),
    Grade_3 = col_skip(),
    Item_1 = col_double(),
    Item_2 = col_double(),
    Item_3 = col_skip(),
    Item_4 = col_skip(),
    Sentence = col_skip(),
    Language_1 = col_character(),
    Language_2 = col_character(),
    State = col_skip()
  ))
personnes

dat <- personnes %>%
  select(Age, Sex, Scoring, Died, start_date, Item_1)
dat

selection <- c("Age", "Sex", "Scoring", "Jack-Jack")
dat <- personnes %>%
  select(any_of(selection))
dat


#3.2 Sélection avec des prédicats
select(personnes, where(is.character))

is_num2 <- function(x){
  is.numeric(x) &&
    !isTRUE(all.equal(x, as.integer(x)))
}

# On peut créer une fonction prédicat, elle doit simplement retourner TRUE ou FALSE.
# Créons par exemple une fonction nommée is_num2 qui ne sélectionne que les colonnes de type
# numérique et qui ne peuvent pas être tranformées en données de type “entier” sans perte
# d’information.

select(personnes, where(is_num2))

select(personnes, -where(is_num2))

select(personnes, where(~ is.character(.x) && length(unique(.x)) < 10 ))

#3.3 Sélection avec le mini langage (*) {#mini-langage}
dat <- select(personnes, start_date:Item_1)
dat


# 3.4 Sélection avec des motifs
dat <- select(personnes, starts_with("Item_"))
dat

dat <- select(personnes, ends_with("_1"))
dat

dat <- select(personnes, contains("te"))
dat

dat <- select(personnes, matches("_2$"))
dat

dat <- select(personnes, num_range("Grade_", range = 1:5))
dat
#3.5 Masquage des données: Data masking et sélection de données: tidyselect (*)

# data.frame
starwars[starwars$homeworld == "Naboo" & starwars$species == "Human", ,]

# avec un tibble et dplyr
starwars %>% filter(homeworld == "Naboo", species == "Human")



#Chapitre 4 Les premiers verbes dplyr---
# Nous allons illustrer cette partie avec un jeu de données nommé penguins disponible dans le package ‘palmerpenguins’ (Horst, Hill, and Gorman 2022).

# Rappelez-vous la section sur la nomenclature des noms: les fonctions appliquant des
#transformations sont dénommées par des verbes, ce sont des actions, alors que les noms
#de variables stockant des données subissant ces transformations sont des substantifs
#(comme en français).

#Ce n’est pas une régle gravée dans le marbre, mais l’écosystème tidyverse respecte cette
# convention, et le premier argument des fonctions du tidyverse est généralement l’objet sur
# lequel effectuer la transformation.

#4.1 Trier les données

penguins %>%
  select(species, island, year, bill_length_mm) %>%
  arrange(island, year, bill_length_mm)


#Pour modifier l’ordre du tri en descendant, il faut englober la variable dans un appel
#à la fonction desc (pour descending order).

penguins %>%
  select(island, year, bill_length_mm) %>%
  arrange(desc(island), year, desc(bill_length_mm))

#4.2 Modifier l’ordre des colonnes
# relocate() est un verbe dplyr qui permet de modifier la position des colonnes. Il faut utilisez la
# fonction avec la même syntaxe que select() pour faciliter le déplacement de blocs de colonnes en une
# seule fois.
#
# Par défaut, les colonnes sont positionnées en début de tableau. Deux arguments sont offerts pour contrôler
# la position, .before et .after. On utilise en général .before = 1 ou .after = last_col(), ce sont des
# sélecteurs de colonnes comme dans la fonction select().
#
# Par exemple, mettre toutes les variables character en premieres positions :

penguins %>%
  relocate(where(is.factor), year)

penguins %>%
  relocate(year, body_mass_g, ends_with("mm"), .after = last_col())

# 4.3 Filtrage de lignes

# Le filtrage consiste en la sélection d’un sous-ensemble de lignes, le filtrage garde les lignes sélectionnées. La fonction filter() est la fonction la plus importante à maîtriser.

# D’autres fonctions de filtrage sont disponibles, elles sont en général spécialisées sur un
# thème précis :

#  - Échantillonnage avec sample_frac(), sample_n() et slice_sample(),
#  - Suppression des doublons avec distinct()
#  - Sélection des lignes basées sur leurs indices : slice, slice_head(), slice_tail(), slice_min(), slice_max()
#     Mais slice ne fonctionne pas avec des base de donnéees, et dépéend de la position de la ligne, qui peut
#     changer, donc on ne recommande pas l'usage

# 4.3.1 Utilisation de filter

penguins %>%
  select(island, bill_length_mm) %>%
  filter(
    bill_length_mm > 39,
    island %in% c("Dream", "Biscoe")
  )

# 4.3.2 Dédoublonnage
select(penguins, species, island) %>% distinct()

penguins_dedoubl <- distinct(select(penguins, species, island))
penguins_dedoubl

# 4.3.3 Echantillonnage aléatoire
select(penguins, species, island, starts_with("bill_")) %>%
  sample_n(size = 5)

select(penguins, species, island, starts_with("bill_")) %>%
  sample_frac(size = .05)

# 4.3.4 Sélection de lignes par index de ligne
select(penguins, species, island, starts_with("bill_")) %>%
  slice(5:10)

# 4.3.5 filter des lignes correspondantes à un extremum
select(penguins, species, island, bill_length_mm) %>%
  slice_max(bill_length_mm, n = 5)

select(penguins, species, island, bill_length_mm) %>%
  slice_min(bill_length_mm, n = 5)

dat <- data.frame(x = c(1.1, 1.1, 1.1, 2.3, 2.5))

slice_min(dat, x, n = 1, with_ties = TRUE)


slice_min(dat, x, n = 1, with_ties = FALSE)

# 4.3.6 filter avec filter() et row_number()

select(penguins, species, island, bill_length_mm) %>%
  arrange(desc(bill_length_mm)) %>%
  filter(row_number() < 6)

penguins %>% select(species, island, bill_length_mm) %>%
  arrange(desc(bill_length_mm)) %>%
  filter(row_number() < 6)

penguins %>% select(species, island, bill_length_mm) %>%
  arrange(desc(bill_length_mm)) %>%
  filter(row_number() < 6 & row_number()>=3)

#4.4 Ajouter ou modifier des colonnes

select(penguins, species, island, machin = bill_length_mm) %>%
  mutate(
    machin_demi = machin / 2,
    machin_quart = machin / 4
  )

#"machin = bill_length_mm" sert juste à renomer

#La fonction peut bien sûr utiliser des fonctions vectorielles pour réaliser les calculs. Par exemple, on peut utiliser cut() (voire aussi cut) pour discrétiser une variable quantitative:

  select(penguins, species, island, machin = bill_length_mm) %>%
  mutate(
    machin_cat = cut(machin,
                     breaks = c(-Inf, 40, 50, +Inf),
                     labels = c("toupeti", "peti", "papeti")
    )
  )

#4.4.1 Découpages spécifiques avec case_when
  select(penguins, species, sex, machin = bill_length_mm) %>%
    mutate(
      denom = case_when(
        sex %in% "female" & machin < 40 ~ "jeune femme", # si femelle et taille inf 40 :  jeune femme
        sex %in% "male" & machin < 50 ~ "jeune homme", # si male et age inf à 50: jeune homme
        sex %in% "male" ~ "homme", #sinon, si male et auttre cas: homme
        TRUE ~ "femme" # tout les autres cas :femme
      )
    )

#4.4.2 Ajout brut de colonnes

#Chapitre 5 Exercices sur les verbes usuels de dplyr

Exercice
orthodont <- readRDS( "data/orthodont.RDS" )
orthodont

