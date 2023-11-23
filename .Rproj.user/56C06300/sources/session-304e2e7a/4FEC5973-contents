orthodont <- readRDS( "donnees/orthodont.RDS" )
orthodont


orthodont <- separate(orthodont, col = "sub_sex", into = c("subject", "genre"))
orthodont


# 6.2 Pivoter des colonnes
On utilise pivot_longer() pour passer de p colonnes à 2 colonnes :
#
#   une colonne discrete contenant le nom de la colonne originale
#   une colonne pour les valeurs des colonnes originales

  orthodont %>%
  pivot_longer(
    cols = -all_of(c("subject", "genre")),
    names_to = "age", values_to = "measure"
  )

# On utilise pivot_wider() pour passer de 2 colonnes à p colonnes : les nouvelles colonnes sont nommées
# avec les modalités de la variable discrete originale et leurs contenus correspondents aux contenus de
# la colonne de valeurs orginale.

my_data <- read_csv(file = "donnees/climate.csv")
my_data

my_data %>%
  pivot_wider(
    names_from = "mesure",
    values_from = "value")

# 6.3 Gestion des valeurs manquantes
airquality
# 6.3.1 Suppression des valeurs manquantes

drop_na(airquality, Ozone)
drop_na(airquality, any_of(c("Ozone", "Temp")))
drop_na(airquality, matches("zone$"))

# 6.3.3 Complèter par un ensemble de combinaisons
dataset <- readr::read_csv(file = "donnees/echanges_biens.csv")
dataset

dat <- expand(dataset, LOCATION, TIME)
dat

full_dataset <- complete(dataset, LOCATION, TIME, fill = list(Value = 0))
full_dataset
# 6.3.2 Remplacement des valeurs manquantes

replace_na(airquality, list(Ozone = 0, Solar.R = -1))
