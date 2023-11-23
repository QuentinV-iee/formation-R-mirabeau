T1 <- read_csv( "donnees/T1.csv" )
T1


data_nuances <-   select(T1, "Code département", circonscription, ends_with("nuance"))
data_nuances


data_nuances <- data_nuances %>%
  pivot_longer(
    cols = -all_of(c("Code département", "circonscription")),
    names_to = "nuance", values_to = "measure"
  )
data_nuances

data_nuances <- separate(data_nuances, col = "nuance", into = c("id", "dummy"))
data_nuances

nuances <-drop_na(data_nuances)

nuances
nuances <-rename(nuances, nuance=measure)
nuances <-select(nuances, -dummy)
# 7.2.2 Scores des nuances politiques

data_scores <-   select(T1, "Code département", circonscription, ends_with("voix"))
data_scores


data_scores <- data_scores %>%
  pivot_longer(
    cols = -all_of(c("Code département", "circonscription")),
    names_to = "voix", values_to = "measure"
  )
data_scores

data_scores <- separate(data_scores, col = "voix", into = c("id", "dummy"))
data_scores

scores <-drop_na(data_scores)

scores
scores <-rename(scores, voix=measure)
scores <-select(scores, -dummy)



# 7.3 Tidy data

result <-merge(scores, nuances,
               all.x = FALSE, all.y = FALSE)
result
