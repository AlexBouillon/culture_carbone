# Analyse exploratoire des données - Culture Carbone
# Script d'analyse exploratoire pour data_clean.rds avec pondération
# FOCUS: Variables dépendantes 'op_tram_num', 'op_rtc_num', 'op_velo_num' (opinions sur les projets, échelle 1-4)
#        Variables explicatives: 'ideal_eco_bin', 'comp_transport_eco' + variables sociodémographiques

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

# === VARIABLES DÉPENDANTES PRINCIPALES (OPINIONS) ===

# 1. Distribution op_tram_num (pondérée)
p_dep_tram <- data %>%
  filter(!is.na(op_tram_num)) %>%
  ggplot(aes(x = factor(op_tram_num), weight = POND)) +
  geom_bar(fill = "#E69F00", alpha = 0.8) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "VAR DÉPENDANTE: Opinion sur le tramway (pondérée)",
       subtitle = "1=Très mauvaise, 2=Plutôt mauvaise, 3=Plutôt bonne, 4=Très bonne",
       x = "Opinion tramway",
       y = "Fréquence pondérée") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

ggsave("_SharedFolder_culture_carbone/graph/DEP_op_tram_num.png", p_dep_tram, width = 10, height = 6)

# 2. Distribution op_rtc_num (pondérée)
p_dep_rtc <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = factor(op_rtc_num), weight = POND)) +
  geom_bar(fill = "#56B4E9", alpha = 0.8) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "VAR DÉPENDANTE: Opinion sur le RTC (pondérée)",
       subtitle = "1=Très mauvaise, 2=Plutôt mauvaise, 3=Plutôt bonne, 4=Très bonne",
       x = "Opinion RTC",
       y = "Fréquence pondérée") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

ggsave("_SharedFolder_culture_carbone/graph/DEP_op_rtc_num.png", p_dep_rtc, width = 10, height = 6)

# 3. Distribution op_velo_num (pondérée)
p_dep_velo <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = factor(op_velo_num), weight = POND)) +
  geom_bar(fill = "#009E73", alpha = 0.8) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "VAR DÉPENDANTE: Opinion sur les infrastructures vélo (pondérée)",
       subtitle = "1=Très mauvaise, 2=Plutôt mauvaise, 3=Plutôt bonne, 4=Très bonne",
       x = "Opinion vélo",
       y = "Fréquence pondérée") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

ggsave("_SharedFolder_culture_carbone/graph/DEP_op_velo_num.png", p_dep_velo, width = 10, height = 6)

# === VARIABLES EXPLICATIVES PRINCIPALES ===

# 4. Distribution ideal_eco_bin (pondérée)
p_expl_ideal <- data %>%
  filter(!is.na(ideal_eco_bin)) %>%
  ggplot(aes(x = factor(ideal_eco_bin, labels = c("Auto", "Écologique")), weight = POND)) +
  geom_bar(fill = "darkgreen", alpha = 0.8) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "VAR EXPLICATIVE: Transport idéal écologique (pondérée)",
       x = "Transport idéal",
       y = "Fréquence pondérée") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

ggsave("_SharedFolder_culture_carbone/graph/EXPL_ideal_eco_bin.png", p_expl_ideal, width = 8, height = 6)

# 5. Distribution comp_transport_eco (pondérée)
p_expl_comp <- data %>%
  filter(!is.na(comp_transport_eco)) %>%
  ggplot(aes(x = factor(comp_transport_eco, labels = c("Auto", "Écologique")), weight = POND)) +
  geom_bar(fill = "darkblue", alpha = 0.8) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "VAR EXPLICATIVE: Comportement transport écologique (pondérée)",
       x = "Comportement transport",
       y = "Fréquence pondérée") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

ggsave("_SharedFolder_culture_carbone/graph/EXPL_comp_transport_eco.png", p_expl_comp, width = 8, height = 6)

# === DISTRIBUTIONS SOCIODÉMOGRAPHIQUES ===

# 6. Arrondissement
p_ses_arr <- ggplot(data, aes(x = ses_arrondissement, weight = POND)) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par arrondissement (pondérée)",
       x = "Arrondissement",
       y = "Fréquence pondérée") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/SES_arrondissement.png", p_ses_arr, width = 10, height = 6)

# 7. Âge
p_ses_age <- ggplot(data, aes(x = ses_age, weight = POND)) +
  geom_bar(fill = "forestgreen", alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par groupe d'âge (pondérée)",
       x = "Groupe d'âge",
       y = "Fréquence pondérée") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/SES_age.png", p_ses_age, width = 8, height = 6)

# 8. Genre
p_ses_genre <- ggplot(data, aes(x = factor(ses_femme, labels = c("Homme", "Femme")), weight = POND)) +
  geom_bar(fill = "coral", alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par genre (pondérée)",
       x = "Genre",
       y = "Fréquence pondérée")

ggsave("_SharedFolder_culture_carbone/graph/SES_genre.png", p_ses_genre, width = 6, height = 6)

# 9. Éducation
p_ses_educ <- ggplot(data, aes(x = ses_educ, weight = POND)) +
  geom_bar(fill = "purple", alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par niveau d'éducation (pondérée)",
       x = "Niveau d'éducation",
       y = "Fréquence pondérée") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/SES_education.png", p_ses_educ, width = 10, height = 6)

# 10. Revenus
p_ses_rev <- ggplot(data, aes(x = ses_revenu_grouped, weight = POND)) +
  geom_bar(fill = "orange", alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par groupe de revenus (pondérée)",
       x = "Groupe de revenus",
       y = "Fréquence pondérée") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/SES_revenus.png", p_ses_rev, width = 8, height = 6)

# 11. Occupation
p_ses_occup <- ggplot(data, aes(x = ses_occup, weight = POND)) +
  geom_bar(fill = "darkred", alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par occupation (pondérée)",
       x = "Occupation",
       y = "Fréquence pondérée") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_culture_carbone/graph/SES_occupation.png", p_ses_occup, width = 8, height = 6)

# 12. Propriété
p_ses_prop <- ggplot(data, aes(x = factor(ses_proprio, labels = c("Locataire", "Propriétaire")), weight = POND)) +
  geom_bar(fill = "tan", alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Distribution par statut de propriété (pondérée)",
       x = "Statut de propriété",
       y = "Fréquence pondérée")

ggsave("_SharedFolder_culture_carbone/graph/SES_propriete.png", p_ses_prop, width = 6, height = 6)

# === ANALYSES BIVARIÉES: OPINIONS vs VARIABLES EXPLICATIVES PRINCIPALES ===

# Opinion tramway vs ideal_eco_bin (boxplot)
p_tram_ideal <- data %>%
  filter(!is.na(op_tram_num), !is.na(ideal_eco_bin)) %>%
  ggplot(aes(x = factor(ideal_eco_bin, labels = c("Auto", "Écologique")),
             y = op_tram_num, fill = factor(ideal_eco_bin, labels = c("Auto", "Écologique")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion sur le tramway par transport idéal",
       x = "Transport idéal",
       y = "Score opinion tramway (1-4)",
       fill = "Transport idéal") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_ideal_eco.png", p_tram_ideal, width = 10, height = 6)

# Opinion tramway vs comp_transport_eco (boxplot)
p_tram_comp <- data %>%
  filter(!is.na(op_tram_num), !is.na(comp_transport_eco)) %>%
  ggplot(aes(x = factor(comp_transport_eco, labels = c("Auto", "Écologique")),
             y = op_tram_num, fill = factor(comp_transport_eco, labels = c("Auto", "Écologique")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion sur le tramway par comportement transport",
       x = "Comportement transport",
       y = "Score opinion tramway (1-4)",
       fill = "Comportement") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_comp_transport.png", p_tram_comp, width = 10, height = 6)

# Opinion RTC vs ideal_eco_bin (boxplot)
p_rtc_ideal <- data %>%
  filter(!is.na(op_rtc_num), !is.na(ideal_eco_bin)) %>%
  ggplot(aes(x = factor(ideal_eco_bin, labels = c("Auto", "Écologique")),
             y = op_rtc_num, fill = factor(ideal_eco_bin, labels = c("Auto", "Écologique")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion sur le RTC par transport idéal",
       x = "Transport idéal",
       y = "Score opinion RTC (1-4)",
       fill = "Transport idéal") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_ideal_eco.png", p_rtc_ideal, width = 10, height = 6)

# Opinion RTC vs comp_transport_eco (boxplot)
p_rtc_comp <- data %>%
  filter(!is.na(op_rtc_num), !is.na(comp_transport_eco)) %>%
  ggplot(aes(x = factor(comp_transport_eco, labels = c("Auto", "Écologique")),
             y = op_rtc_num, fill = factor(comp_transport_eco, labels = c("Auto", "Écologique")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion sur le RTC par comportement transport",
       x = "Comportement transport",
       y = "Score opinion RTC (1-4)",
       fill = "Comportement") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_comp_transport.png", p_rtc_comp, width = 10, height = 6)

# Opinion vélo vs ideal_eco_bin (boxplot)
p_velo_ideal <- data %>%
  filter(!is.na(op_velo_num), !is.na(ideal_eco_bin)) %>%
  ggplot(aes(x = factor(ideal_eco_bin, labels = c("Auto", "Écologique")),
             y = op_velo_num, fill = factor(ideal_eco_bin, labels = c("Auto", "Écologique")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion sur les infrastructures vélo par transport idéal",
       x = "Transport idéal",
       y = "Score opinion vélo (1-4)",
       fill = "Transport idéal") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_ideal_eco.png", p_velo_ideal, width = 10, height = 6)

# Opinion vélo vs comp_transport_eco (boxplot)
p_velo_comp <- data %>%
  filter(!is.na(op_velo_num), !is.na(comp_transport_eco)) %>%
  ggplot(aes(x = factor(comp_transport_eco, labels = c("Auto", "Écologique")),
             y = op_velo_num, fill = factor(comp_transport_eco, labels = c("Auto", "Écologique")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion sur les infrastructures vélo par comportement transport",
       x = "Comportement transport",
       y = "Score opinion vélo (1-4)",
       fill = "Comportement") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_comp_transport.png", p_velo_comp, width = 10, height = 6)

# === ANALYSES BIVARIÉES: OPINIONS vs SOCIODÉMOGRAPHIQUES ===

# Opinion tramway vs variables sociodémographiques
p_tram_age <- data %>%
  filter(!is.na(op_tram_num)) %>%
  ggplot(aes(x = ses_age, y = op_tram_num, fill = ses_age)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion tramway par âge",
       x = "Âge", y = "Score opinion (1-4)", fill = "Âge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_age.png", p_tram_age, width = 10, height = 6)

p_tram_genre <- data %>%
  filter(!is.na(op_tram_num)) %>%
  ggplot(aes(x = factor(ses_femme, labels = c("Homme", "Femme")),
             y = op_tram_num, fill = factor(ses_femme, labels = c("Homme", "Femme")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion tramway par genre",
       x = "Genre", y = "Score opinion (1-4)", fill = "Genre")

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_genre.png", p_tram_genre, width = 8, height = 6)

p_tram_educ <- data %>%
  filter(!is.na(op_tram_num)) %>%
  ggplot(aes(x = ses_educ, y = op_tram_num, fill = ses_educ)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion tramway par éducation",
       x = "Éducation", y = "Score opinion (1-4)", fill = "Éducation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_education.png", p_tram_educ, width = 10, height = 6)

p_tram_rev <- data %>%
  filter(!is.na(op_tram_num)) %>%
  ggplot(aes(x = ses_revenu_grouped, y = op_tram_num, fill = ses_revenu_grouped)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion tramway par revenus",
       x = "Revenus", y = "Score opinion (1-4)", fill = "Revenus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_revenus.png", p_tram_rev, width = 10, height = 6)

p_tram_arr <- data %>%
  filter(!is.na(op_tram_num)) %>%
  ggplot(aes(x = ses_arrondissement, y = op_tram_num, fill = ses_arrondissement)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion tramway par arrondissement",
       x = "Arrondissement", y = "Score opinion (1-4)", fill = "Arrondissement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_arrondissement.png", p_tram_arr, width = 12, height = 6)

p_tram_occup <- data %>%
  filter(!is.na(op_tram_num)) %>%
  ggplot(aes(x = ses_occup, y = op_tram_num, fill = ses_occup)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion tramway par occupation",
       x = "Occupation", y = "Score opinion (1-4)", fill = "Occupation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_occupation.png", p_tram_occup, width = 10, height = 6)

p_tram_prop <- data %>%
  filter(!is.na(op_tram_num)) %>%
  ggplot(aes(x = factor(ses_proprio, labels = c("Locataire", "Propriétaire")),
             y = op_tram_num, fill = factor(ses_proprio, labels = c("Locataire", "Propriétaire")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion tramway par propriété",
       x = "Statut propriété", y = "Score opinion (1-4)", fill = "Statut")

ggsave("_SharedFolder_culture_carbone/graph/TRAM_vs_propriete.png", p_tram_prop, width = 8, height = 6)

# Opinion RTC vs variables sociodémographiques
p_rtc_age <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = ses_age, y = op_rtc_num, fill = ses_age)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion RTC par âge",
       x = "Âge", y = "Score opinion (1-4)", fill = "Âge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_age.png", p_rtc_age, width = 10, height = 6)

p_rtc_genre <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = factor(ses_femme, labels = c("Homme", "Femme")),
             y = op_rtc_num, fill = factor(ses_femme, labels = c("Homme", "Femme")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion RTC par genre",
       x = "Genre", y = "Score opinion (1-4)", fill = "Genre")

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_genre.png", p_rtc_genre, width = 8, height = 6)

p_rtc_educ <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = ses_educ, y = op_rtc_num, fill = ses_educ)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion RTC par éducation",
       x = "Éducation", y = "Score opinion (1-4)", fill = "Éducation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_education.png", p_rtc_educ, width = 10, height = 6)

p_rtc_rev <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = ses_revenu_grouped, y = op_rtc_num, fill = ses_revenu_grouped)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion RTC par revenus",
       x = "Revenus", y = "Score opinion (1-4)", fill = "Revenus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_revenus.png", p_rtc_rev, width = 10, height = 6)

p_rtc_arr <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = ses_arrondissement, y = op_rtc_num, fill = ses_arrondissement)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion RTC par arrondissement",
       x = "Arrondissement", y = "Score opinion (1-4)", fill = "Arrondissement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_arrondissement.png", p_rtc_arr, width = 12, height = 6)

p_rtc_occup <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = ses_occup, y = op_rtc_num, fill = ses_occup)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion RTC par occupation",
       x = "Occupation", y = "Score opinion (1-4)", fill = "Occupation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_occupation.png", p_rtc_occup, width = 10, height = 6)

p_rtc_prop <- data %>%
  filter(!is.na(op_rtc_num)) %>%
  ggplot(aes(x = factor(ses_proprio, labels = c("Locataire", "Propriétaire")),
             y = op_rtc_num, fill = factor(ses_proprio, labels = c("Locataire", "Propriétaire")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion RTC par propriété",
       x = "Statut propriété", y = "Score opinion (1-4)", fill = "Statut")

ggsave("_SharedFolder_culture_carbone/graph/RTC_vs_propriete.png", p_rtc_prop, width = 8, height = 6)

# Opinion vélo vs variables sociodémographiques
p_velo_age <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = ses_age, y = op_velo_num, fill = ses_age)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion vélo par âge",
       x = "Âge", y = "Score opinion (1-4)", fill = "Âge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_age.png", p_velo_age, width = 10, height = 6)

p_velo_genre <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = factor(ses_femme, labels = c("Homme", "Femme")),
             y = op_velo_num, fill = factor(ses_femme, labels = c("Homme", "Femme")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion vélo par genre",
       x = "Genre", y = "Score opinion (1-4)", fill = "Genre")

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_genre.png", p_velo_genre, width = 8, height = 6)

p_velo_educ <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = ses_educ, y = op_velo_num, fill = ses_educ)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion vélo par éducation",
       x = "Éducation", y = "Score opinion (1-4)", fill = "Éducation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_education.png", p_velo_educ, width = 10, height = 6)

p_velo_rev <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = ses_revenu_grouped, y = op_velo_num, fill = ses_revenu_grouped)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion vélo par revenus",
       x = "Revenus", y = "Score opinion (1-4)", fill = "Revenus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_revenus.png", p_velo_rev, width = 10, height = 6)

p_velo_arr <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = ses_arrondissement, y = op_velo_num, fill = ses_arrondissement)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion vélo par arrondissement",
       x = "Arrondissement", y = "Score opinion (1-4)", fill = "Arrondissement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_arrondissement.png", p_velo_arr, width = 12, height = 6)

p_velo_occup <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = ses_occup, y = op_velo_num, fill = ses_occup)) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion vélo par occupation",
       x = "Occupation", y = "Score opinion (1-4)", fill = "Occupation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_occupation.png", p_velo_occup, width = 10, height = 6)

p_velo_prop <- data %>%
  filter(!is.na(op_velo_num)) %>%
  ggplot(aes(x = factor(ses_proprio, labels = c("Locataire", "Propriétaire")),
             y = op_velo_num, fill = factor(ses_proprio, labels = c("Locataire", "Propriétaire")))) +
  geom_boxplot(alpha = 0.7) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Opinion vélo par propriété",
       x = "Statut propriété", y = "Score opinion (1-4)", fill = "Statut")

ggsave("_SharedFolder_culture_carbone/graph/VELO_vs_propriete.png", p_velo_prop, width = 8, height = 6)

# === MATRICE DE CORRÉLATION ENTRE LES 3 OPINIONS ===

# Corrélation entre opinions (version numérique)
opinions_numeric <- data %>%
  select(op_tram_num, op_rtc_num, op_velo_num, POND) %>%
  filter(complete.cases(.))

if(nrow(opinions_numeric) > 0 && require(weights, quietly = TRUE)) {
  cor_matrix_op <- wtd.cor(opinions_numeric[, c("op_tram_num", "op_rtc_num", "op_velo_num")],
                           weight = opinions_numeric$POND)$correlation

  png("_SharedFolder_culture_carbone/graph/correlation_opinions.png",
      width = 8, height = 8, units = "in", res = 300)
  corrplot(cor_matrix_op, method = "color", type = "upper",
           tl.cex = 1.2, tl.col = "black", tl.srt = 45,
           title = "Corrélation entre les opinions (pondérée)",
           addCoef.col = "black", number.cex = 1.5,
           mar = c(0,0,2,0))
  dev.off()
}

# === TABLE CROISÉE: VARIABLES EXPLICATIVES ===

# Tableau croisé ideal_eco_bin vs comp_transport_eco
p_expl_cross <- data %>%
  filter(!is.na(ideal_eco_bin), !is.na(comp_transport_eco)) %>%
  ggplot(aes(x = factor(ideal_eco_bin, labels = c("Auto", "Écologique")),
             fill = factor(comp_transport_eco, labels = c("Auto", "Écologique")),
             weight = POND)) +
  geom_bar(position = "fill") +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Cohérence entre transport idéal et comportement réel (pondéré)",
       x = "Transport idéal",
       y = "Proportion",
       fill = "Comportement réel") +
  theme(plot.title = element_text(face = "bold"))

ggsave("_SharedFolder_culture_carbone/graph/EXPL_ideal_vs_comportement.png", p_expl_cross, width = 10, height = 6)

# === STATISTIQUES DESCRIPTIVES DES OPINIONS ===

cat("\n=== Statistiques descriptives des opinions (pondérées) ===\n")

# Fonction pour calculer stats pondérées
weighted_stats <- function(var, weight) {
  data_temp <- data.frame(var = var, weight = weight) %>%
    filter(!is.na(var))

  if(nrow(data_temp) == 0) return(NULL)

  list(
    mean = weighted.mean(data_temp$var, data_temp$weight),
    median = matrixStats::weightedMedian(data_temp$var, data_temp$weight),
    sd = sqrt(wtd.var(data_temp$var, data_temp$weight)),
    n = nrow(data_temp)
  )
}

cat("\nOpinion tramway:\n")
print(weighted_stats(data$op_tram_num, data$POND))

cat("\nOpinion RTC:\n")
print(weighted_stats(data$op_rtc_num, data$POND))

cat("\nOpinion vélo:\n")
print(weighted_stats(data$op_velo_num, data$POND))

cat("\n=== Analyse exploratoire terminée ===\n")
cat("Tous les graphiques ont été sauvegardés dans _SharedFolder_culture_carbone/graph/\n")
