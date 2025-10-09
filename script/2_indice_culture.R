library(dplyr)

# Charger les données
data_sondage <- readRDS("_SharedFolder_culture_carbone/data/data_clean.rds")
data_scores <- readRDS("_SharedFolder_culture_carbone/data/data_scores.rds")

# ====================================================================
# 1. PRÉPARATION : Calculer le répertoire écologique par arrondissement
# ====================================================================

# Agrégation des scores par arrondissement avec pondération par population
repertoire_arrondissement <- data_scores %>%
  group_by(arrondissement) %>%
  summarise(
    # Scores pondérés par population
    walk_score = sum(walk_score * population) / sum(population),
    transit_score = sum(transit_score * population) / sum(population),
    bike_score = sum(bike_score * population) / sum(population),
    population_totale = sum(population),
    .groups = 'drop'
  ) %>%
  # Normaliser les scores (0-1)
  mutate(
    transit_norm = transit_score / 100,
    walk_norm = walk_score / 100,
    bike_norm = bike_score / 100
  ) %>%
  # Calcul du répertoire écologique combiné
  # Pondération: transit (0.4), walk (0.3), bike (0.3)
  mutate(
    repertoire_eco = 0.4 * transit_norm +
                     0.3 * walk_norm +
                     0.3 * bike_norm
  ) %>%
  select(arrondissement, repertoire_eco, walk_score, transit_score, bike_score)

# Harmoniser les noms d'arrondissement pour le merge
repertoire_arrondissement <- repertoire_arrondissement %>%
  mutate(
    arrondissement = case_when(
      arrondissement == "Ste-Foy-Sillery-Cap-Rouge" ~ "Ste-Foy-Sillery-Cap-Rouge",
      arrondissement == "Haute-St-Charles" ~ "Haute-St-Charles",
      TRUE ~ arrondissement
    )
  )

# ====================================================================
# 2. FUSION : Joindre répertoire au sondage via arrondissement
# ====================================================================

data_culture <- data_sondage %>%
  left_join(repertoire_arrondissement,
            by = c("ses_arrondissement" = "arrondissement"))

# ====================================================================
# 3. TYPOLOGIE CULTURELLE : 12 profils (Idéal × Répertoire × Comportement)
# ====================================================================

data_culture <- data_culture %>%
  mutate(
    # Catégoriser le répertoire en 3 niveaux basés sur les scores walk/bike/transit
    # < 50 = car-dependent, 50-69 = somewhat, 70+ = very
    # Le répertoire_eco est déjà sur échelle 0-1, donc on convertit les seuils
    repertoire_cat = case_when(
      repertoire_eco >= 0.70 ~ "Élevé",      # ≥ 70/100
      repertoire_eco >= 0.50 ~ "Modéré",     # 50-69/100
      repertoire_eco < 0.50 ~ "Faible",      # < 50/100 (car-dependent)
      TRUE ~ NA_character_
    ),

    # Convertir en facteur ordonné
    repertoire_cat = factor(repertoire_cat,
                           levels = c("Faible", "Modéré", "Élevé"),
                           ordered = TRUE)
  ) %>%
  mutate(
    # Créer la typologie à 12 profils (2 idéaux × 3 répertoires × 2 comportements)
    profil_culture = case_when(
      # ===== PROFILS IDÉAL ÉCOLOGIQUE (ideal_eco_bin == 1) =====

      # Comportement écologique
      ideal_eco_bin == 1 & repertoire_cat == "Élevé" & comp_transport_eco == 1 ~
        "Éco normalisé",

      ideal_eco_bin == 1 & repertoire_cat == "Modéré" & comp_transport_eco == 1 ~
        "Éco limité",

      ideal_eco_bin == 1 & repertoire_cat == "Faible" & comp_transport_eco == 1 ~
        "Éco résistant",

      # Comportement carbone
      ideal_eco_bin == 1 & repertoire_cat == "Élevé" & comp_transport_eco == 0 ~
        "Éco latent",

      ideal_eco_bin == 1 & repertoire_cat == "Modéré" & comp_transport_eco == 0 ~
        "Éco empêché",

      ideal_eco_bin == 1 & repertoire_cat == "Faible" & comp_transport_eco == 0 ~
        "Éco bloqué",

      # ===== PROFILS IDÉAL CARBONE (ideal_eco_bin == 0) =====

      # Comportement carbone
      ideal_eco_bin == 0 & repertoire_cat == "Élevé" & comp_transport_eco == 0 ~
        "Carbone résistant",

      ideal_eco_bin == 0 & repertoire_cat == "Modéré" & comp_transport_eco == 0 ~
        "Carbone normalisé modéré",

      ideal_eco_bin == 0 & repertoire_cat == "Faible" & comp_transport_eco == 0 ~
        "Carbone normalisé",

      # Comportement écologique
      ideal_eco_bin == 0 & repertoire_cat == "Élevé" & comp_transport_eco == 1 ~
        "Carbone adapté",

      ideal_eco_bin == 0 & repertoire_cat == "Modéré" & comp_transport_eco == 1 ~
        "Carbone atypique modéré",

      ideal_eco_bin == 0 & repertoire_cat == "Faible" & comp_transport_eco == 1 ~
        "Carbone atypique",

      # Cas manquants (NA dans ideal_eco_bin)
      TRUE ~ NA_character_
    )
  ) %>%
  # Convertir en facteur pour faciliter les analyses
  mutate(
    profil_culture = factor(profil_culture,
                           levels = c("Éco normalisé", "Éco limité", "Éco résistant",
                                     "Éco latent", "Éco empêché", "Éco bloqué",
                                     "Carbone résistant", "Carbone normalisé modéré", "Carbone normalisé",
                                     "Carbone adapté", "Carbone atypique modéré", "Carbone atypique"),
                           ordered = FALSE)
  )

# ====================================================================
# 4. SCORE DE SENSE-MAKING (continu)
# ====================================================================

data_culture <- data_culture %>%
  mutate(
    # Congruence idéal-comportement (1 = parfaite, 0 = totale dissonance)
    congruence = 1 - abs(ideal_eco_bin - comp_transport_eco),

    # Facilitation du répertoire (asymétrique selon la direction)
    # Plus le répertoire est favorable à l'alignement, plus le sense-making est élevé
    # Utilisation d'une échelle graduée basée sur les 3 catégories
    facilitation = case_when(
      # ===== ALIGNEMENT ÉCOLOGIQUE (I=1, C=1) =====
      # Plus le répertoire est élevé, plus c'est facilité
      ideal_eco_bin == 1 & comp_transport_eco == 1 & repertoire_cat == "Élevé" ~ 1.0,
      ideal_eco_bin == 1 & comp_transport_eco == 1 & repertoire_cat == "Modéré" ~ 0.75,
      ideal_eco_bin == 1 & comp_transport_eco == 1 & repertoire_cat == "Faible" ~ 0.5,

      # ===== ALIGNEMENT CARBONE (I=0, C=0) =====
      # Plus le répertoire est faible, plus c'est facilité (justification)
      ideal_eco_bin == 0 & comp_transport_eco == 0 & repertoire_cat == "Faible" ~ 1.0,
      ideal_eco_bin == 0 & comp_transport_eco == 0 & repertoire_cat == "Modéré" ~ 0.75,
      ideal_eco_bin == 0 & comp_transport_eco == 0 & repertoire_cat == "Élevé" ~ 0.5,

      # ===== DISSONANCE (pas d'alignement) =====
      # Aucune facilitation, dissonance cognitive totale
      TRUE ~ 0.0
    ),

    # Score final de sense-making (produit de congruence × facilitation)
    sense_making = congruence * facilitation
  ) %>%
  # Catégorisation ordinale du sense-making
  mutate(
    sense_making_cat = cut(sense_making,
                          breaks = c(-Inf, 0.25, 0.5, 0.75, Inf),
                          labels = c("Très faible", "Faible", "Modéré", "Fort"),
                          ordered_result = TRUE)
  )

# ====================================================================
# 5. STATISTIQUES DESCRIPTIVES
# ====================================================================

cat("\n========================================\n")
cat("DISTRIBUTION DES PROFILS CULTURELS\n")
cat("========================================\n\n")

# Table de fréquence des profils
table_profils <- table(data_culture$profil_culture, useNA = "ifany")
prop_profils <- prop.table(table_profils) * 100

cat("Effectifs:\n")
print(table_profils)
cat("\n\nPourcentages:\n")
print(round(prop_profils, 1))

# Statistiques croisées
cat("\n\n========================================\n")
cat("RÉPARTITION PAR ARRONDISSEMENT\n")
cat("========================================\n\n")

table_croisee <- table(data_culture$ses_arrondissement,
                       data_culture$profil_culture)
print(table_croisee)

# Sense-making moyen par profil
cat("\n\n========================================\n")
cat("SENSE-MAKING MOYEN PAR PROFIL\n")
cat("========================================\n\n")

sense_by_profil <- data_culture %>%
  filter(!is.na(profil_culture)) %>%
  group_by(profil_culture) %>%
  summarise(
    n = n(),
    sense_making_moyen = mean(sense_making, na.rm = TRUE),
    sense_making_sd = sd(sense_making, na.rm = TRUE),
    repertoire_eco_moyen = mean(repertoire_eco, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(sense_making_moyen))

print(sense_by_profil, n = Inf)

# ====================================================================
# 6. SAUVEGARDE
# ====================================================================

saveRDS(data_culture, "_SharedFolder_culture_carbone/data/data_culture.rds")
cat("\n\nDonnées sauvegardées: data_culture.rds\n")
