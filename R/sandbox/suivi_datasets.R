# Chargement des packages nécessaires
library(tibble)
library(dplyr)

# Utilisation exemple
# suivi_datasets <- ajouter_dataset_interactif(suivi_datasets)
# afficher_datasets(suivi_datasets)
# suivi_datasets <- modifier_dataset(suivi_datasets)
# suivi_datasets <- supprimer_dataset(suivi_datasets)
# filtrer_datasets(suivi_datasets)

# Fichier de suivi
chemin_csv <- "suivi_datasets.csv"

# 🔹 Initialisation du tableau s'il n'existe pas
if (!file.exists(chemin_csv)) {
  suivi_datasets <- tibble(
    nom_fichier = character(),
    doi = character(),
    source = character(),
    nom_article = character(),
    date_traitement = as.Date(character()),
    type = character(),
    inclus_gbif = logical(),
    fusionne_jeu_final = logical(),
    notes = character()
  )
  write.csv(suivi_datasets, chemin_csv, row.names = FALSE)
} else {
  suivi_datasets <- read.csv(chemin_csv, stringsAsFactors = FALSE)
  suivi_datasets$date_traitement <- as.Date(suivi_datasets$date_traitement)
}

# 🔸 Fonction : Ajouter un dataset
ajouter_dataset_interactif <- function(df, chemin_fichier = chemin_csv) {
  cat("\n--- Ajout interactif d'un jeu de données ---\n\n")
  
  nom_fichier <- readline("Nom du fichier : ")
  doi <- readline("DOI (laisser vide si absent) : ")
  source <- readline("Source (ex : Dryad, article, etc.) : ")
  nom_article <- readline("Nom de l'article (laisser vide si pas d'article) : ")
  
  repeat {
    date_saisie <- readline("Date de traitement (AAAA-MM-JJ) : ")
    if (!is.na(as.Date(date_saisie, format = "%Y-%m-%d"))) break
    cat("⚠️  Format de date incorrect. Réessaye.\n")
  }
  date_traitement <- as.Date(date_saisie)
  
  type_choix <- menu(c("marin", "terrestre", "autre"), title = "Type d’environnement")
  type <- c("marin", "terrestre", "autre")[type_choix]
  
  inclus_gbif <- menu(c("Oui", "Non"), title = "Inclus dans GBIF ?") == 1
  fusionne_jeu_final <- menu(c("Oui", "Non"), title = "Fusionné dans le jeu final ?") == 1
  
  notes <- readline("Notes éventuelles : ")
  
  df <- add_row(
    df,
    nom_fichier = nom_fichier,
    doi = doi,
    source = source,
    nom_article = nom_article,
    date_traitement = date_traitement,
    type = type,
    inclus_gbif = inclus_gbif,
    fusionne_jeu_final = fusionne_jeu_final,
    notes = notes
  )
  
  write.csv(df, chemin_fichier, row.names = FALSE)
  cat("\n✅ Jeu de données ajouté et sauvegardé avec succès.\n")
  return(df)
}

modifier_dataset <- function(df, chemin_fichier = chemin_csv) {
  cat("\nListe des jeux de données :\n")
  print(df %>% mutate(index = row_number()) %>% select(index, nom_fichier, date_traitement, source))
  
  index <- as.integer(readline("\nNuméro de la ligne à modifier : "))
  
  if (!is.na(index) && index >= 1 && index <= nrow(df)) {
    ligne <- df[index, ]
    cat("\n--- Modification de la ligne ---\n")
    
    champs <- names(df)
    for (champ in champs) {
      valeur_actuelle <- ligne[[champ]]
      nouvelle_valeur <- readline(paste0("→ ", champ, " (", valeur_actuelle, ") : "))
      if (nouvelle_valeur != "") {
        if (champ == "date_traitement") {
          ligne[[champ]] <- as.Date(nouvelle_valeur)
        } else if (champ %in% c("inclus_gbif", "fusionne_jeu_final")) {
          ligne[[champ]] <- tolower(nouvelle_valeur) %in% c("true", "1", "oui", "yes")
        } else {
          ligne[[champ]] <- nouvelle_valeur
        }
      }
    }
    
    df[index, ] <- ligne
    write.csv(df, chemin_fichier, row.names = FALSE)
    cat("✅ Ligne modifiée avec succès.\n")
  } else {
    cat("❌ Numéro invalide.\n")
  }
  return(df)
}


# 🔸 Fonction : Supprimer un dataset
supprimer_dataset <- function(df, chemin_fichier = chemin_csv) {
  cat("\nListe des jeux de données :\n")
  print(df %>% mutate(index = row_number()) %>% select(index, nom_fichier, date_traitement, source))
  
  index <- as.integer(readline("\nEntrez le numéro de la ligne à supprimer : "))
  
  if (!is.na(index) && index >= 1 && index <= nrow(df)) {
    df <- df[-index, ]
    write.csv(df, chemin_fichier, row.names = FALSE)
    cat("✅ Ligne supprimée et fichier mis à jour :", chemin_fichier, "\n")
  } else {
    cat("❌ Numéro invalide.\n")
  }
  return(df)
}

# 🔸 Fonction : Afficher
afficher_datasets <- function(df) {
  print(df)
}

# 🔸 Fonction : Filtrer
filtrer_datasets <- function(df) {
  choix <- menu(c("Type", "Source", "Fusionné", "Inclus GBIF"), title = "Filtrer selon :")
  
  if (choix == 1) {
    val <- readline("Type (marin/terrestre/autre) : ")
    print(filter(df, type == val))
  } else if (choix == 2) {
    val <- readline("Source (Dryad, etc.) : ")
    print(filter(df, source == val))
  } else if (choix == 3) {
    print(filter(df, fusionne_jeu_final == TRUE))
  } else if (choix == 4) {
    print(filter(df, inclus_gbif == TRUE))
  }
}

