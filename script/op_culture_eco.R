library(tidyverse)



# 2. Création de l'indice de culture écologique à 4 catégories
df <- df %>%
  mutate(culture_eco = case_when(
    ideal_transport == 1 & repertoire_eco >= 0.67 ~ 3,  # Culture éco forte
    ideal_transport == 1 & repertoire_eco < 0.67 ~ 2,   # Potentiel contraint
    ideal_transport == 0 & repertoire_eco >= 0.67 ~ 1,  # Répertoire non mobilisé
    ideal_transport == 0 & repertoire_eco < 0.67 ~ 0    # Culture non-éco
  ))

# 3. Étiquettes pour faciliter l'interprétation
df <- df %>%
  mutate(culture_eco_label = factor(culture_eco,
                                    levels = 0:3,
                                    labels = c("Culture non-éco",
                                             "Répertoire non mobilisé",
                                             "Potentiel contraint",
                                             "Culture éco forte"),
                                    ordered = TRUE))

# 4. Vérification de la distribution
table(df$culture_eco)
prop.table(table(df$culture_eco)) * 100

# 5. Statistiques descriptives par catégorie
df %>%
  group_by(culture_eco_label) %>%
  summarise(
    n = n(),
    transit_moyen = mean(transit_score, na.rm = TRUE),
    walk_moyen = mean(walk_score, na.rm = TRUE),
    bike_moyen = mean(bike_score, na.rm = TRUE),
    repertoire_moyen = mean(repertoire_eco, na.rm = TRUE)
  )