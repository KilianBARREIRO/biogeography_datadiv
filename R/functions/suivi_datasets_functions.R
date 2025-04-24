detect_species_formats <- function(data, species_col = "species") {
  species_sym <- rlang::sym(species_col)
  
  data %>%
    mutate(
      original = !!species_sym,
      nb_words = str_count(!!species_sym, "\\S+"),
      format = case_when(
        str_detect(!!species_sym, "^\\S+\\s\\S+$") ~ "Deux mots",
        str_detect(!!species_sym, "^\\S+$") ~ "Un mot",
        str_detect(!!species_sym, "^\\S+\\s\\S+\\s\\S+$") ~ "Trois mots",
        TRUE ~ "Autre"
      ),
      has_number = str_detect(!!species_sym, "\\d"),
      has_punct = str_detect(!!species_sym, "[[:punct:]]"),
      starts_with_cap = str_detect(!!species_sym, "^[A-Z]"),
      second_word_short = str_detect(!!species_sym, "^\\S+\\s+\\S{1,2}(\\s|$)")
    ) %>%
    group_by(format, has_number, has_punct, starts_with_cap, second_word_short) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n))
}


########################################################################



detect_date_formats <- function(df, col_name) {
  date_vec <- df[[col_name]]
  date_vec <- date_vec[!is.na(date_vec)]  # enlève les NA
  
  formats_found <- sapply(date_vec, function(x) {
    if (!is.character(x)) x <- as.character(x)
    
    # Essaie plusieurs formats courants
    formats <- c(
      "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", 
      "%d-%m-%Y", "%Y/%m/%d", "%d.%m.%Y",
      "%b %d, %Y", "%d %b %Y", "%B %d, %Y", "%d %B %Y"
    )
    
    matched <- NA
    for (fmt in formats) {
      try_date <- suppressWarnings(as.Date(x, format = fmt))
      if (!is.na(try_date)) {
        matched <- fmt
        break
      }
    }
    matched
  })
  
  formats_summary <- as.data.frame(table(formats_found), stringsAsFactors = FALSE)
  formats_summary <- formats_summary %>% rename(`Date format` = formats_found, `Count` = Freq)
  
  return(formats_summary)
}

########################################################################

filter_species_data <- function(data, col_species = species) {
  col <- enquo(col_species)
  
  data %>%
    mutate(
      temp_species = !!col,
      # Nettoyage initial : retirer parenthèses et texte supplémentaire
      temp_species = str_replace(temp_species, "^([\\w-]+) \\([^\\)]+\\) ([\\w-]+)$", "\\1 \\2"),
      temp_species = str_replace(temp_species, "^((\\S+\\s+\\S+))\\s+.*$", "\\1"),
      
      # Comptage du nombre de mots
      nb_words = str_count(temp_species, "\\S+"),
      
      format = case_when(
        nb_words == 2 ~ "Deux mots",
        nb_words == 1 ~ "Un mot",
        nb_words == 3 & str_detect(temp_species, "\\s+[a-z]+$") ~ "Genus species subspecies",  # Ajout pour détecter les sous-espèces
        nb_words == 3 ~ "Trois mots",
        TRUE ~ "Autre"
      ),
      
      has_number = str_detect(temp_species, "\\d"),
      has_punct = str_detect(temp_species, "[[:punct:]]"),
      starts_with_cap = str_detect(temp_species, "^[A-Z]"),
      second_word_short = str_detect(temp_species, "^\\S+\\s+\\S{1,2}(\\s|$)")
    ) %>% 
    filter(
      format != "Deux mots" |
        has_number == TRUE |
        has_punct == TRUE |
        starts_with_cap == FALSE |
        second_word_short == TRUE
    )
}


###########################################################
detect_name_formats <- function(data, col_name) {
  col <- data[[col_name]]
  
  tibble(name = col) %>%
    filter(!is.na(name)) %>%
    mutate(
      has_parentheses = str_detect(name, "\\(.*\\)"),
      has_author_year = str_detect(name, "\\(.*\\d{4}\\)"),
      has_more_than_two_words = str_count(name, "\\S+") > 2,
      is_binomial = str_detect(name, "^[A-Z][a-z]+ [a-z\\-]+$"),
      has_punctuation = str_detect(name, "[[:punct:]]"),
      format_type = case_when(
        is_binomial ~ "Genus species",
        has_author_year ~ "Genus species (Author, year)",
        has_parentheses ~ "Genus species (something)",
        has_more_than_two_words ~ "> 2 words",
        TRUE ~ "Autre"
      )
    ) %>%
    count(format_type, sort = TRUE)
}












#############################################################################

# date_format_example <- expeditionsMerged %>% 
#   mutate(eventDate_clean = str_trim(eventDate),
#          format_type = str_replace_all(eventDate_clean, "\\d", "D"),
#          format_type = str_replace_all(format_type, "[A-Za-z]", "A")) %>%
#   group_by(format_type) %>%
#   summarise(n = n(),
#             examples = paste(unique(head(eventDate_clean, 3)), collapse = " | ")) %>%
#   arrange(desc(n))

# checker format colonne caractères
# scientificName_formats <- Ramage2017TerrestrialArthropods %>%
#   mutate(
#     original = scientificName,
#     nb_words = str_count(scientificName, "\\S+"),
#     format = case_when(
#       str_detect(scientificName, "^\\S+\\s\\S+$") ~ "Deux mots",
#       str_detect(scientificName, "^\\S+$") ~ "Un mot",
#       str_detect(scientificName, "^\\S+\\s\\S+\\s\\S+$") ~ "Trois mots",
#       TRUE ~ "Autre"
#     ),
#     has_number = str_detect(scientificName, "\\d"),
#     has_punct = str_detect(scientificName, "[[:punct:]]"),
#     starts_with_cap = str_detect(scientificName, "^[A-Z]"),
#     second_word_short = str_detect(scientificName, "^\\S+\\s+\\S{1,2}(\\s|$)")
#   ) %>%
#   group_by(format, has_number, has_punct, starts_with_cap, second_word_short) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   arrange(desc(n))