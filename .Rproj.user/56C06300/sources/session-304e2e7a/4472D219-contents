
producers <- data.frame(
  surname = c(
    "Spielberg", "Scorsese", "Hitchcock",
    "Tarantino", "Audiard", "Tony Scott"
  ),
  nationality = c("US", "US", "UK", "US", "FR", "US"),
  stringsAsFactors = FALSE
)
producers


# Create destination dataframe
movies <- data.frame(
  surname = c(
    "Spielberg",
    "Scorsese",
    "Hitchcock",
    "Hitchcock",
    "Spielberg",
    "Tarantino",
    "Audiard",
    "Tony Gatlif"
  ),
  title = c(
    "Super 8",
    "Taxi Driver",
    "Psycho",
    "North by Northwest",
    "Catch Me If You Can",
    "Reservoir Dogs",
    "De battre mon coeur s'est arrêté",
    "Exils"
  ),
  stringsAsFactors = FALSE
)
movies


left_join(producers, movies, by = "surname")


inner_join(producers, movies, by = "surname")


right_join(producers, movies, by = "surname")


full_join(producers, movies, by = "surname")


semi_join(producers, movies, by = "surname")


anti_join(producers, movies, by = "surname")


# Chapitre 9 Utilitaires offerts par dplyr
# 9.1 Les fonctions contextuelles

summarise(penguins, nb_lignes = n())
dat <- group_by(penguins, species)
dat

summarise(
  .data = dat,
  n = n(),
  avg = mean(bill_length_mm, na.rm = TRUE)
)



# 9.3 Les groupes et leurs persistances
# Les index de groupes sont conservés tels quels après les opérations sauf avec la fonction summarise(). Pour ne plus conditionner les traitements selon les groupes définis il faut utiliser le verbe ungroup().

# Il est souvent souhaitable de supprimer les niveaux de regroupement avec la fonction ungroup() dès le traitement par groupes réalisé.

# 9.3.1 Cas des agrégations

dat <- penguins %>%
  group_by(species, island, sex) %>%
  summarise(bill_length_mm = mean(bill_length_mm, na.rm = TRUE),
            body_mass_g = mean(body_mass_g, na.rm = TRUE)
  )
## `summarise()` has grouped output by 'species', 'island'. You can override using
## the `.groups` argument.

dat %>%
  arrange(desc(body_mass_g)) %>%
  filter(row_number() ==1)

penguins %>%
  group_by(species, island, sex) %>%
  summarise(bill_length_mm = mean(bill_length_mm, na.rm = TRUE),
            body_mass_g = mean(body_mass_g, na.rm = TRUE),
            .groups = "drop"
  ) %>%
  arrange(desc(body_mass_g)) %>%
  filter(row_number() ==1)

# 9.3.2 Cas des autres opérations

centre_reduit <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

penguins %>%
  group_by(species, sex) %>%
  mutate(n = n(),
         rank = percent_rank(bill_length_mm),
         z = centre_reduit(bill_length_mm)) %>%
  ungroup() %>%
  arrange(rank, z) %>%
  select(species, sex, n, rank, z)


penguins %>%
  group_by(species, island) %>%
  arrange(desc(body_mass_g)) %>%
  filter(row_number() < 4) %>%
  ungroup()

# 9.4 Les fonctions utilitaires de dplyr

penguins %>%
  select(species, island, bill_length_mm, bill_depth_mm) %>%
  filter(between(bill_length_mm, left = 39.1, right = 40),
         between(bill_depth_mm, left = 20, right = 22)
  )



dat <- data.frame(
  a = c("A", NA, "B", NA, NA),
  b = c("B", "C", "C", "D", NA),
  c = c(NA, NA, NA, NA, "E")
) %>% mutate(z = coalesce(a, b, c))

dat


dat_1 <- data.frame(
  a = 1:2,
  b = rnorm(2),
  group = "un"
)
dat_2 <- data.frame(
  a = 11:13,
  b = rnorm(3),
  group = "deux"
)
dat_3 <- data.frame(
  a = 21:27,
  b = rnorm(7),
  group = "trois"
)
dat <- bind_rows(dat_1, dat_2, dat_3)
dat


# 9.5 Les fonctions d’agrégations
