overall_data_esp_uniques <- filtered_data %>% 
  distinct(species, .keep_all = TRUE) %>% 
  arrange(species)

marine_data <- filtered_data %>% 
  filter(habitat == "Marine")

marine_data_esp_uniques <- filtered_data %>% 
  filter(habitat == "Marine") %>% 
  distinct(species, .keep_all = TRUE) %>% 
  arrange(species)

  
  
terrestrial_data <- filtered_data %>% 
  filter(habitat == "Terrestrial")

terrestrial_data_esp_uniques <- filtered_data %>% 
  filter(habitat == "Terrestrial") %>% 
  distinct(species, .keep_all = TRUE) %>% 
  arrange(species)


mixed_data <- filtered_data %>% 
  filter(habitat != "Marine" & habitat != "Terrestrial")

mixed_data_esp_uniques <- filtered_data %>% 
  filter(habitat != "Marine" & habitat != "Terrestrial") %>% 
  distinct(species, .keep_all = TRUE) %>% 
  arrange(species)



na_species_rows_marine <- marine_data %>% filter(is.na(species))
na_species_rows_terrestrial <- terrestrial_data %>% filter(is.na(species))
na_species_rows <- filtered_data %>% filter(is.na(species))
head(sort(table(marine_data$species), decreasing = TRUE))
quantile(table(marine_data$species), probs = seq(0.1, 0.9, by = 0.1))
quantile(table(terrestrial_data$species), probs = seq(0.1, 0.9, by = 0.1))

occurrences_especes <- filtered_data %>% 
  group_by(class) %>% 
  summarise(
    occurrences = n(),                          # Nombre total d'occurrences
    especes_uniques = n_distinct(species)        # Nombre d'espèces uniques
  ) %>% 
  arrange(desc(occurrences))

occurrences_especes_marine <- marine_data %>% 
  group_by(class) %>% 
  summarise(
    occurrences = n(),                          # Nombre total d'occurrences
    especes_uniques = n_distinct(species)        # Nombre d'espèces uniques
  ) %>% 
  arrange(desc(occurrences))

occurrences_especes_terrestrial <- terrestrial_data %>% 
  group_by(class) %>% 
  summarise(
    occurrences = n(),                          # Nombre total d'occurrences
    especes_uniques = n_distinct(species)        # Nombre d'espèces uniques
  ) %>% 
  arrange(desc(occurrences))

birds <- filtered_data %>% 
  filter(class == "Aves") %>% 
  distinct(species)
