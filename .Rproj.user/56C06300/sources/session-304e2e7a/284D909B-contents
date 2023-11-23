label_A129_name<- c("code_A129", "label_A129")

label_A129  <- read_delim( "donnees/Libelle_A129.txt",
                           col_select = 1:2,
                           col_names = label_A129_name,
                           col_types = cols(
                             code_A129=col_character(),
                             label_A129=col_character()
                           ))

label_A129


label_CPF4_name<- c("code_CPF4", "label_CPF4")

label_CPF4  <- read_delim( "donnees/Libelle_CPF4.txt",
                           col_select = 1:2,
                           col_names = label_CPF4_name,
                           col_types = cols(
                             code_CPF4=col_character(),
                             label_CPF4=col_character()
                           ))

label_CPF4

label_PAYS_name<- c("code_pays", "label_pays")

label_PAYS  <- read_delim( "donnees/Libelle_PAYS.txt",
                           col_select = 1:2,
                           col_names = label_PAYS_name,
                           col_types = cols(
                             code_pays=col_character(),
                             label_pays=col_character()
                           ))

label_PAYS


export_2019_name <-c( "flux","trimestre", "annee", "code_dep", "code_reg", "code_A129", "code_CPF4", "code_pays", "valeur_exp", "masse_exp")

export_2019  <- read_delim( "donnees/Region_2019_Export.txt",
                           col_names = export_2019_name,
                           col_types = cols(
                             flux=col_skip(),
                             code_dep=col_character(),
                             code_reg=col_character(),
                             code_A129 =col_character(),
                             code_CPF4=col_character(),
                             code_dep=col_character(),
                           ))


export_2019

import_2019_name <-c( "flux","trimestre", "annee", "code_dep", "code_reg", "code_A129", "code_CPF4", "code_pays", "valeur_imp", "masse_imp")

import_2019  <- read_delim( "donnees/Region_2019_Import.txt",
                            col_names = import_2019_name,
                            col_types = cols(
                              flux=col_skip(),
                              code_dep=col_character(),
                              code_reg=col_character(),
                              code_A129 =col_character(),
                              code_CPF4=col_character(),
                              code_dep=col_character(),
                            ))


import_2019
intersect(colnames(export_2019),colnames(import_2019))
common_colnames <- intersect(colnames(export_2019),colnames(import_2019))

full_table <- full_join(import_2019, export_2019, by = common_colnames)

colnames_na <- c("valeur_exp", "masse_exp", "valeur_imp", "masse_imp")

full_table <-  replace_na(full_table, list(valeur_exp=0, masse_exp=0, valeur_imp=0, masse_imp=0))

colnames_group <- c("code_pays", "code_A129", "code_CPF4")
full_table <- group_by(full_table, code_pays,code_A129,code_CPF4)

comext <- summarise(
  .data = full_table,
  n = n(),
  sum_valeur_imp = sum(valeur_imp, na.rm = TRUE),
  sum_valeur_exp = sum(valeur_exp, na.rm = TRUE),
  sum_masse_imp = sum(masse_imp, na.rm = TRUE),
  sum_masse_exp = sum(masse_exp, na.rm = TRUE),
)


#Vous porriez rencontrer un problème à cause de la jointure avec le référentiel des pays. Chercher les causes
#du problème et résolvez les (indice : cherchez les doublons).


#on cherche les doublons

label_PAYS |> count(code_pays) |>filter(n>1)

#Des doublons sur tyrois pays
label_PAYS <- label_PAYS |> filter(
  !(code_pays %in% "XK" & label_pays %in% "Yougoslavie"),
  !(code_pays %in% "XS" & label_pays %in% "Saba"),
  !(code_pays %in% "QU" & label_pays %in% "Pays NDA"))


comext <- comext |>
  left_join(label_PAYS, by = "code_pays")|>
  left_join(label_CPF4, by = "code_CPF4") |>
  left_join(label_A129, by = "code_A129")

comext
