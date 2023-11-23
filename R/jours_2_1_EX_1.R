


datamart <- penguins %>%
  select(species, bill_length_mm, sex, year)
datamart

datamart <- penguins %>%
  select(species , ends_with("mm"))
datamart


datamart <- penguins %>%
  select(-species , -starts_with("bill"))
datamart

# 5.2 Trier
# 5.2.1 Opérations usuelles

penguins %>%
   arrange(island, bill_depth_mm)

penguins %>%
  arrange(island, desc(bill_depth_mm))

# 5.3 Filtrage
# 5.3.1 Filtrage simple

penguins %>%
  filter(
    body_mass_g  > 3700,
    !is.na(sex))


penguins %>%
  filter(
    body_mass_g  > 3700,
    is.na(sex))

# 5.3.2 Filtrage et ensembles
penguins %>%
  filter(
    species  %in% c("Adelie", "Gentoo"))

# 5.4 Création de variables
# 5.4.1 Catégorisation d’une variable continue

penguins <-penguins%>%
  mutate(
    class_poids   = cut(body_mass_g ,
                     breaks = c(-Inf, 3500 , 4700, +Inf),
                     labels = c("leger", "moyen", "lourd")
    )
  )


count(penguins, class_poids)

# 5.4.2 Transformation de texte

mutate(penguins,
    sex   =  stringr::str_sub(sex,0,1)
    )
penguins
