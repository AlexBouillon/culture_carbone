
library(readxl)
library(dplyr)

# Charger les données de scores par quartier
data_scores <- read_excel("_SharedFolder_culture_carbone/data/walk_scores.xlsx")

# Renommer les colonnes pour faciliter la manipulation
data_scores <- data_scores %>%
  rename(
    quartier = Name,
    arrondissement = Arrondissement,
    walk_score = `Walk Score`,
    transit_score = `Transit Score`,
    bike_score = `Bike Score`,
    population = Population
  )

# ====================================================================
# 1. TABLEAU DÉTAILLÉ : Scores par quartier
# ====================================================================

scores_quartiers <- data_scores %>%
  mutate(
    # Normaliser les scores (0-1)
    transit_score_norm = transit_score / 100,
    walk_score_norm = walk_score / 100,
    bike_score_norm = bike_score / 100
  ) %>%
  mutate(
    # Calcul du répertoire écologique par quartier
    # Pondération: transit (0.4), walk (0.3), bike (0.3)
    repertoire_eco = 0.4 * transit_score_norm +
                     0.3 * walk_score_norm +
                     0.3 * bike_score_norm
  ) %>%
  select(
    quartier,
    arrondissement,
    walk_score,
    transit_score,
    bike_score,
    repertoire_eco,
    population
  ) %>%
  arrange(arrondissement, desc(repertoire_eco))

# ====================================================================
# 2. TABLEAU AGRÉGÉ : Scores par arrondissement
# ====================================================================

# Agrégation des scores par arrondissement avec pondération par population
scores_arrondissement <- data_scores %>%
  group_by(arrondissement) %>%
  summarise(
    # Scores pondérés par population
    walk_score_pondered = sum(walk_score * population) / sum(population),
    transit_score_pondered = sum(transit_score * population) / sum(population),
    bike_score_pondered = sum(bike_score * population) / sum(population),
    # Population totale de l'arrondissement
    population_totale = sum(population),
    # Nombre de quartiers
    nb_quartiers = n(),
    .groups = 'drop'
  ) %>%
  # Normaliser les scores (0-1)
  mutate(
    transit_score = transit_score_pondered / 100,
    walk_score = walk_score_pondered / 100,
    bike_score = bike_score_pondered / 100
  ) %>%
  # Calcul du répertoire écologique combiné
  # Pondération: transit (0.4), walk (0.3), bike (0.3)
  mutate(
    repertoire_eco = 0.4 * transit_score +
                     0.3 * walk_score +
                     0.3 * bike_score
  )

# ====================================================================
# 3. AFFICHAGE DES RÉSULTATS
# ====================================================================

cat("\n========================================\n")
cat("TABLEAU 1: Scores par quartier\n")
cat("========================================\n\n")
print(scores_quartiers, n = Inf)

cat("\n\n========================================\n")
cat("TABLEAU 2: Scores par arrondissement (pondérés par population)\n")
cat("========================================\n\n")
print(scores_arrondissement)

# ====================================================================
# 4. SAUVEGARDE DES DONNÉES
# ====================================================================

saveRDS(data_scores, "_SharedFolder_culture_carbone/data/data_scores.rds")
cat("\n\nDonnées sauvegardées: data_scores.rds\n")


