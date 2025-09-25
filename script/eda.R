# Analyse exploratoire des données - Culture Carbone
# Script d'analyse exploratoire pour data_clean.rds avec pondération
# FOCUS: Variable dépendante 'ideal' (mode de transport idéal)

# Vérifier et charger les packages nécessaires
if (!require(ggplot2, quietly = TRUE)) install.packages("ggplot2")
if (!require(dplyr, quietly = TRUE)) install.packages("dplyr")
if (!require(gridExtra, quietly = TRUE)) install.packages("gridExtra")
if (!require(corrplot, quietly = TRUE)) install.packages("corrplot")
if (!require(weights, quietly = TRUE)) install.packages("weights")
library(ggthemes)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(weights) # Pour les analyses pondérées

# Vérifier la version de ggplot2 et charger GGally si compatible
ggplot_version <- packageVersion("ggplot2")
cat("Version de ggplot2 installée:", as.character(ggplot_version), "\n")

if (ggplot_version >= "4.0.0") {
  if (!require(GGally, quietly = TRUE)) install.packages("GGally")
  library(GGally)
  use_ggally <- TRUE
} else {
  cat("ATTENTION: GGally nécessite ggplot2 >= 4.0.0. Analyse multivariée limitée.\n")
  cat("Pour résoudre: update.packages('ggplot2') ou install.packages('ggplot2', type='source')\n")
  use_ggally <- FALSE
}

# Charger les données
data <- readRDS("_SharedFolder_culture_carbone/data/data_clean.rds")

# Vérifier les poids de pondération
summary(data$POND)

# === VARIABLE DÉPENDANTE PRINCIPALE ===

# 0. Distribution de la variable dépendante : transport idéal (pondérée)
p_ideal_main <- ggplot(data, aes(x = ideal, weight = POND)) +
  geom_bar(fill = "darkgreen", alpha = 0.8) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "VARIABLE DÉPENDANTE: Distribution du mode de transport idéal (pondérée)",
       x = "Mode de transport idéal",
       y = "Fréquence pondérée") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("_SharedFolder_culture_carbone/graph/MAIN_ideal_distribution.png", p_ideal_main, width = 10, height = 7)

# === GRAPHIQUES DE DISTRIBUTION UNIVARIÉS DES PRÉDICTEURS ===

# 1. Distribution par arrondissement (pondérée)
p1 <- ggplot(data, aes(x = ses_arrondissement, weight = POND)) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par arrondissement (pondérée)",
       x = "Arrondissement",
       y = "Fréquence pondérée") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
        )

ggsave("_SharedFolder_culture_carbone/graph/dist_arrondissement.png", p1, width = 10, height = 6)

# 2. Distribution par âge (pondérée)
p2 <- ggplot(data, aes(x = ses_age, weight = POND)) +
  geom_bar(fill = "forestgreen", alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par groupe d'âge (pondérée)",
       x = "Groupe d'âge",
       y = "Fréquence pondérée") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
              )

ggsave("_SharedFolder_culture_carbone/graph/dist_age.png", p2, width = 8, height = 6)

# 3. Distribution du genre (pondérée)
p3 <- ggplot(data, aes(x = factor(ses_femme, labels = c("Homme", "Femme")), weight = POND)) +
  geom_bar(fill = "coral", alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par genre (pondérée)",
       x = "Genre",
       y = "Fréquence pondérée") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
              )

ggsave("_SharedFolder_culture_carbone/graph/dist_genre.png", p3, width = 6, height = 6)

# 4. Distribution de l'éducation (pondérée)
p4 <- ggplot(data, aes(x = ses_educ, weight = POND)) +
  geom_bar(fill = "purple", alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par niveau d'éducation (pondérée)",
       x = "Niveau d'éducation",
       y = "Fréquence pondérée") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
              )

ggsave("_SharedFolder_culture_carbone/graph/dist_education.png", p4, width = 10, height = 6)

# 5. Distribution des revenus (pondérée)
p5 <- ggplot(data, aes(x = ses_revenu_grouped, weight = POND)) +
  geom_bar(fill = "orange", alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par groupe de revenus (pondérée)",
       x = "Groupe de revenus",
       y = "Fréquence pondérée") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
              )

ggsave("_SharedFolder_culture_carbone/graph/dist_revenus.png", p5, width = 8, height = 6)

# 6. Distribution du transport principal (pondérée)
p6 <- ggplot(data, aes(x = main_transport, weight = POND)) +
  geom_bar(fill = "darkblue", alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution du mode de transport principal (pondérée)",
       x = "Mode de transport principal",
       y = "Fréquence pondérée") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
              )

ggsave("_SharedFolder_culture_carbone/graph/dist_transport_principal.png", p6, width = 10, height = 6)


# 8. Distributions des variables d'attitude (scores numériques, pondérées)
attitude_vars <- c("ouverture_tc", "ouverture_actif", "ouverture_combine",
                  "mobilite_active", "dev_durable", "dev_economique",
                  "densification", "sante_transport_actif")

# Histogrammes des variables d'attitude (pondérés)
for (var in attitude_vars) {
  p_hist <- ggplot(data, aes_string(x = var, weight = "POND")) +
    geom_histogram(bins = 20, fill = "lightblue", alpha = 0.7, color = "black") +
      theme_fivethirtyeight(base_size = 14) +
    labs(title = paste("Distribution de", var, "(pondérée)"),
         x = var,
         y = "Fréquence pondérée")

  ggsave(paste0("_SharedFolder_culture_carbone/graph/hist_", var, ".png"),
         p_hist, width = 8, height = 6)
}

# 9. Distribution des opinions sur les projets de transport (pondérées)
opinion_vars <- c("op_rtc_num", "op_velo_num", "op_tram_num")

for (var in opinion_vars) {
  p_op <- ggplot(data[!is.na(data[[var]]), ], aes_string(x = var, weight = "POND")) +
    geom_histogram(bins = 15, fill = "salmon", alpha = 0.7, color = "black") +
      theme_fivethirtyeight(base_size = 14) +
    labs(title = paste("Distribution des opinions -", var, "(pondérée)"),
         x = paste("Score d'opinion", var),
         y = "Fréquence pondérée")

  ggsave(paste0("_SharedFolder_culture_carbone/graph/hist_", var, ".png"),
         p_op, width = 8, height = 6)
}

# 10. Distribution de la pondération (non pondérée - pour diagnostic)
p_pond <- ggplot(data, aes(x = POND)) +
  geom_histogram(bins = 30, fill = "gold", alpha = 0.7, color = "black") +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution des poids de pondération",
       x = "Poids de pondération",
       y = "Fréquence (non pondérée)")

ggsave("_SharedFolder_culture_carbone/graph/hist_ponderation.png", p_pond, width = 8, height = 6)

# === GRAPHIQUES BI-VARIÉS AVEC VARIABLE DÉPENDANTE 'IDEAL' ===

# PRÉDICTEURS SOCIODÉMOGRAPHIQUES vs IDEAL

# 1. Transport idéal vs Arrondissement (pondéré)
p_ideal_arr <- ggplot(data, aes(x = ses_arrondissement, fill = ideal, weight = POND)) +
  geom_bar(position = "fill") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Transport IDÉAL par arrondissement (pondéré)",
       x = "Arrondissement",
       y = "Proportion pondérée",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_arrondissement.png", p_ideal_arr, width = 12, height = 8)

# 2. Transport idéal vs Âge (pondéré)
p_ideal_age <- ggplot(data, aes(x = ses_age, fill = ideal, weight = POND)) +
  geom_bar(position = "fill") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Transport IDÉAL par groupe d'âge (pondéré)",
       x = "Groupe d'âge",
       y = "Proportion pondérée",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_age.png", p_ideal_age, width = 10, height = 6)

# 3. Transport idéal vs Genre (pondéré)
p_ideal_genre <- ggplot(data, aes(x = factor(ses_femme, labels = c("Homme", "Femme")), fill = ideal, weight = POND)) +
  geom_bar(position = "dodge") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Transport IDÉAL par genre (pondéré)",
       x = "Genre",
       y = "Fréquence pondérée",
       fill = "Transport idéal") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_genre.png", p_ideal_genre, width = 10, height = 6)

# 4. Transport idéal vs Revenus (pondéré)
p_ideal_rev <- ggplot(data, aes(x = ses_revenu_grouped, fill = ideal, weight = POND)) +
  geom_bar(position = "fill") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Transport IDÉAL par niveau de revenus (pondéré)",
       x = "Revenus",
       y = "Proportion pondérée",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_revenus.png", p_ideal_rev, width = 10, height = 6)

# 5. Transport idéal vs Éducation (pondéré)
p_ideal_educ <- ggplot(data, aes(x = ses_educ, fill = ideal, weight = POND)) +
  geom_bar(position = "fill") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Transport IDÉAL par niveau d'éducation (pondéré)",
       x = "Éducation",
       y = "Proportion pondérée",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_education.png", p_ideal_educ, width = 12, height = 6)

# 6. Transport idéal vs Propriété (pondéré)
p_ideal_prop <- ggplot(data, aes(x = factor(ses_proprio, labels = c("Locataire", "Propriétaire")),
                                fill = ideal, weight = POND)) +
  geom_bar(position = "fill") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Transport IDÉAL par statut de propriété (pondéré)",
       x = "Statut de propriété",
       y = "Proportion pondérée",
       fill = "Transport idéal") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_propriete.png", p_ideal_prop, width = 8, height = 6)

# ATTITUDES vs IDEAL (boxplots)

# 7. Ouverture aux transports en commun vs Transport idéal
p_ideal_tc <- ggplot(data, aes(x = ideal, y = ouverture_tc, fill = ideal)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Ouverture aux transports en commun par transport IDÉAL",
       x = "Transport idéal",
       y = "Score d'ouverture TC",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_ouverture_tc.png", p_ideal_tc, width = 10, height = 6)

# 8. Mobilité active vs Transport idéal
p_ideal_mob <- ggplot(data, aes(x = ideal, y = mobilite_active, fill = ideal)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Attitude envers la mobilité active par transport IDÉAL",
       x = "Transport idéal",
       y = "Score mobilité active",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_mobilite_active.png", p_ideal_mob, width = 10, height = 6)

# 9. Développement durable vs Transport idéal
p_ideal_dur <- ggplot(data, aes(x = ideal, y = dev_durable, fill = ideal)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Attitude envers le développement durable par transport IDÉAL",
       x = "Transport idéal",
       y = "Score développement durable",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_dev_durable.png", p_ideal_dur, width = 10, height = 6)

# TRANSPORT ACTUEL vs IDEAL

# 10. Transport idéal vs Transport principal (matrice de contingence)
p_ideal_actuel <- ggplot(data, aes(x = main_transport_3, fill = ideal, weight = POND)) +
  geom_bar(position = "fill") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Transport IDÉAL selon le transport principal actuel (pondéré)",
       x = "Transport principal actuel",
       y = "Proportion pondérée",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_transport_actuel.png", p_ideal_actuel, width = 10, height = 6)

# === GRAPHIQUES BI-VARIÉS TRADITIONNELS (PRÉDICTEURS) ===

# 1. Transport principal vs Arrondissement (pondéré)
p_biv1 <- ggplot(data, aes(x = ses_arrondissement, fill = main_transport_3, weight = POND)) +
  geom_bar(position = "dodge") +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Mode de transport principal par arrondissement (pondéré)",
       x = "Arrondissement",
       y = "Fréquence pondérée",
       fill = "Transport principal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/biv_transport_arrondissement.png", p_biv1, width = 12, height = 8)

# 2. Transport principal vs Âge (pondéré)
p_biv2 <- ggplot(data, aes(x = ses_age, fill = main_transport_3, weight = POND)) +
  geom_bar(position = "fill") +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Proportion des modes de transport par groupe d'âge (pondérée)",
       x = "Groupe d'âge",
       y = "Proportion pondérée",
       fill = "Transport principal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/biv_transport_age.png", p_biv2, width = 10, height = 6)

# 3. Transport principal vs Revenus (pondéré)
p_biv3 <- ggplot(data, aes(x = ses_revenu_grouped, fill = main_transport_3, weight = POND)) +
  geom_bar(position = "fill") +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Proportion des modes de transport par niveau de revenus (pondérée)",
       x = "Revenus",
       y = "Proportion pondérée",
       fill = "Transport principal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/biv_transport_revenus.png", p_biv3, width = 10, height = 6)

# 4. Genre vs Transport principal (pondéré)
p_biv4 <- ggplot(data, aes(x = factor(ses_femme, labels = c("Homme", "Femme")), fill = main_transport_3, weight = POND)) +
  geom_bar(position = "dodge") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Mode de transport principal par genre (pondéré)",
       x = "Genre",
       y = "Fréquence pondérée",
       fill = "Transport principal")

ggsave("_SharedFolder_culture_carbone/graph/biv_transport_genre.png", p_biv4, width = 8, height = 6)

# 5. Éducation vs Transport principal (pondéré)
p_biv5 <- ggplot(data, aes(x = ses_educ, fill = main_transport_3, weight = POND)) +
  geom_bar(position = "fill") +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Proportion des modes de transport par niveau d'éducation (pondérée)",
       x = "Éducation",
       y = "Proportion pondérée",
       fill = "Transport principal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/biv_transport_education.png", p_biv5, width = 12, height = 6)

# 6. Ouverture aux transports en commun vs Transport principal (boxplot)
# NOTE: La pondération dans les boxplots ggplot2 est limitée - pour une analyse rigoureuse,
# il faudrait calculer les statistiques pondérées manuellement
p_biv6 <- ggplot(data, aes(x = main_transport_3, y = ouverture_tc, fill = main_transport_3, weight = POND)) +
  geom_boxplot(alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Ouverture aux transports en commun par mode de transport principal",
       x = "Transport principal",
       y = "Score d'ouverture TC",
       fill = "Transport principal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/biv_ouverture_tc_transport.png", p_biv6, width = 10, height = 6)

# 7. Mobilité active vs Transport principal (boxplot)
p_biv7 <- ggplot(data, aes(x = main_transport_3, y = mobilite_active, fill = main_transport_3, weight = POND)) +
  geom_boxplot(alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Attitude envers la mobilité active par mode de transport principal",
       x = "Transport principal",
       y = "Score mobilité active",
       fill = "Transport principal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/biv_mobilite_active_transport.png", p_biv7, width = 10, height = 6)

# 8. Développement durable vs Transport principal
p_biv8 <- ggplot(data, aes(x = main_transport_3, y = dev_durable, fill = main_transport_3, weight = POND)) +
  geom_boxplot(alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Attitude envers le développement durable par mode de transport",
       x = "Transport principal",
       y = "Score développement durable",
       fill = "Transport principal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/biv_dev_durable_transport.png", p_biv8, width = 10, height = 6)

# 9. Propriétaire vs Transport principal (pondéré)
p_biv9 <- ggplot(data, aes(x = factor(ses_proprio, labels = c("Locataire", "Propriétaire")),
                          fill = main_transport_3, weight = POND)) +
  geom_bar(position = "fill") +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Proportion des modes de transport par statut de propriété (pondérée)",
       x = "Statut de propriété",
       y = "Proportion pondérée",
       fill = "Transport principal")

ggsave("_SharedFolder_culture_carbone/graph/biv_transport_propriete.png", p_biv9, width = 8, height = 6)


# === MATRICE DE CORRÉLATION (PONDÉRÉE) ===

# Sélectionner les variables numériques pour la matrice de corrélation
numeric_vars <- data %>%
  select(ouverture_tc, ouverture_actif, ouverture_combine, mobilite_active,
         dev_durable, dev_economique, densification, sante_transport_actif,
         op_rtc_num, op_velo_num, op_tram_num, POND) %>%
  select_if(is.numeric)

# Calculer la matrice de corrélation pondérée (en excluant les NA)
# Note: La corrélation pondérée nécessite des données complètes
data_complete <- data[complete.cases(data[, names(numeric_vars)[names(numeric_vars) != "POND"]]), ]

# Pour la corrélation pondérée, on utilisera la fonction wtd.cor du package weights
if(require(weights, quietly = TRUE)) {
  cor_matrix <- wtd.cor(data_complete[, names(numeric_vars)[names(numeric_vars) != "POND"]],
                        weight = data_complete$POND)$correlation
} else {
  # Fallback vers corrélation standard si weights n'est pas disponible
  cor_matrix <- cor(numeric_vars[, names(numeric_vars) != "POND"], use = "pairwise.complete.obs")
  warning("Package 'weights' non disponible. Utilisation de la corrélation non-pondérée.")
}

# Graphique de corrélation
png("_SharedFolder_culture_carbone/graph/matrice_correlation.png", width = 12, height = 10, units = "in", res = 300)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.cex = 0.8, tl.col = "black", tl.srt = 45,
         title = "Matrice de corrélation pondérée des variables d'attitude et d'opinion")
dev.off()

# === ANALYSE MULTIVARIÉE AVEC VARIABLE DÉPENDANTE 'IDEAL' ===

# Graphique pairs plot pour les principales variables d'attitude avec IDEAL
attitude_subset_ideal <- data %>%
  select(ouverture_tc, mobilite_active, dev_durable, dev_economique, ideal) %>%
  filter(!is.na(ouverture_tc), !is.na(mobilite_active), !is.na(dev_durable), !is.na(dev_economique), !is.na(ideal))

if(use_ggally) {
  p_pairs_ideal <- ggpairs(attitude_subset_ideal,
                           aes(color = ideal, alpha = 0.6),
                           title = "Relations entre variables d'attitude par transport IDÉAL")

  ggsave("_SharedFolder_culture_carbone/graph/IDEAL_pairs_plot_attitudes.png", p_pairs_ideal, width = 14, height = 12)
}

# Graphique pairs plot traditionnel pour comparaison
attitude_subset <- data %>%
  select(ouverture_tc, mobilite_active, dev_durable, dev_economique, main_transport_3) %>%
  filter(!is.na(ouverture_tc), !is.na(mobilite_active), !is.na(dev_durable), !is.na(dev_economique))

if(use_ggally) {
  p_pairs <- ggpairs(attitude_subset,
                     aes(color = main_transport_3, alpha = 0.6),
                     title = "Relations entre variables d'attitude par transport ACTUEL")

  ggsave("_SharedFolder_culture_carbone/graph/pairs_plot_attitudes.png", p_pairs, width = 12, height = 10)
}

# === GRAPHIQUES SPÉCIALISÉS AVEC FOCUS SUR 'IDEAL' ===

# Graphiques sur les opinions par transport idéal
# 11. Opinion sur RTC par transport idéal
p_ideal_rtc <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = ideal, y = op_rtc_num, fill = ideal)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion sur le RTC par transport IDÉAL",
       x = "Transport idéal",
       y = "Score opinion RTC",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_opinion_rtc.png", p_ideal_rtc, width = 10, height = 6)

# 12. Opinion sur vélo par transport idéal
p_ideal_velo <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = ideal, y = op_velo_num, fill = ideal)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion sur les infrastructures vélo par transport IDÉAL",
       x = "Transport idéal",
       y = "Score opinion vélo",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_opinion_velo.png", p_ideal_velo, width = 10, height = 6)

# 13. Transport idéal vs Fréquence d'utilisation du bus
p_ideal_freq_bus <- ggplot(data, aes(x = freq_bus3, fill = ideal, weight = POND)) +
  geom_bar(position = "fill") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Transport IDÉAL par fréquence d'utilisation du bus (pondéré)",
       x = "Fréquence d'utilisation du bus",
       y = "Proportion pondérée",
       fill = "Transport idéal") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("_SharedFolder_culture_carbone/graph/IDEAL_vs_freq_bus.png", p_ideal_freq_bus, width = 10, height = 6)

# === GRAPHIQUES SPÉCIALISÉS TRADITIONNELS ===

# 14. Fréquence d'utilisation du bus vs autres variables
# Note: Pour les boxplots, la pondération est plus complexe à implémenter avec ggplot2
# On peut utiliser stat_boxplot avec weight, mais l'effet est limité
p_biv11 <- ggplot(data, aes(x = freq_bus3, y = ouverture_tc, fill = freq_bus3, weight = POND)) +
  geom_boxplot(alpha = 0.7) +
    theme_fivethirtyeight(base_size = 14) +
  labs(title = "Ouverture aux transports en commun par fréquence d'utilisation du bus",
       x = "Fréquence d'utilisation du bus",
       y = "Score d'ouverture TC",
       fill = "Fréquence bus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/biv_freq_bus_ouverture_tc.png", p_biv11, width = 10, height = 6)

# 12. Connaissances des projets vs opinions
knowledge_opinion <- data %>%
  filter(!is.na(op_rtc_num), know_rtc == 1) %>%
  ggplot(aes(x = factor(know_rtc, labels = c("Ne connaît pas", "Connaît")),
             y = op_rtc_num)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Opinion sur le RTC selon la connaissance du projet",
       x = "Connaissance du projet RTC",
       y = "Score d'opinion RTC")

# Comparaison pour tous les projets
p_knowledge <- grid.arrange(
  knowledge_opinion,
  data %>%
    filter(!is.na(op_velo_num)) %>%
    ggplot(aes(x = factor(know_velo, labels = c("Ne connaît pas", "Connaît")),
               y = op_velo_num)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
      theme_fivethirtyeight(base_size = 14) +
    labs(title = "Opinion sur le vélo selon la connaissance",
         x = "Connaissance projet vélo",
         y = "Score opinion vélo"),
  data %>%
    filter(!is.na(op_tram_num)) %>%
    ggplot(aes(x = factor(know_tram, labels = c("Ne connaît pas", "Connaît")),
               y = op_tram_num)) +
    geom_boxplot(fill = "lightyellow", alpha = 0.7) +
      theme_fivethirtyeight(base_size = 14) +
    labs(title = "Opinion sur le tramway selon la connaissance",
         x = "Connaissance projet tramway",
         y = "Score opinion tramway"),
  ncol = 1
)

ggsave("_SharedFolder_culture_carbone/graph/biv_connaissance_opinion.png", p_knowledge, width = 10, height = 12)
