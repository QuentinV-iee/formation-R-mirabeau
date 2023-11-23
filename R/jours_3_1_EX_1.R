#Chapitre 12 Agréger en bataille - exercice
experiment <- readxl::read_xlsx("./donnees/experiment.xlsx")
by <- c("age_cat", "ville", "tribu")



experiment <- experiment |> relocate(age_cat, ville, tribu) |>
  group_by(across(all_of(by)))

print(experiment)

cts <- experiment |>
  tally() |> ungroup()
cts

stat_col <- c("age", "nb_poulets")


# !!!!!!!!!!!! .group ne s'applique qu'au transformation ultérieures
# à celle que l'on applique!!!!!
integer_1 <- experiment|>
  summarise(
    across(
      .cols =  all_of(stat_col),
      .fns = list(
        mean = mean
      )
    )
  )

integer_3 <- experiment|>
  summarise(
    across(
      .cols =  all_of(stat_col),
      .fns = list(
        mean = mean
      )
    ),
    .groups = "keep"
  )

integer_2 <- experiment|>
  summarise(
    across(
      .cols =  all_of(stat_col),
      .fns = list(
        mean = mean
      )
    ),
    .groups = "drop"
  )
