library(dplyr)
library(ggplot2)
library(MASS)  # Pour polr (régression ordinale)
library(broom)
library(marginaleffects)  # Pour probabilités prédites
library(nnet)  # Pour régression multinomiale
library(tidyr)  # Pour pivot_wider

# ====================================================================
# 1. CHARGEMENT DES DONNÉES
# ====================================================================

data_culture <- readRDS("_SharedFolder_culture_carbone/data/data_culture.rds")

cat("\n========================================\n")
cat("ANALYSE DE RÉGRESSION\n")
cat("VD: Position sur le tramway (op_tram_num)\n")
cat("========================================\n\n")

# ====================================================================
# 2. PRÉPARATION DES DONNÉES
# ====================================================================

# La variable op_tram_num est ordinale avec 4 niveaux: 0, 0.33, 0.67, 1
# On va créer une version factorielle pour la régression ordinale
data_reg <- data_culture %>%
  filter(!is.na(op_tram_num)) %>%
  mutate(
    # Variable dépendante ordinale
    op_tram_ord = factor(op_tram_num,
                        levels = c(0, 0.33, 0.67, 1),
                        labels = c("Opposé", "Plutôt opposé",
                                  "Plutôt favorable", "Favorable"),
                        ordered = TRUE),

    # Variable binaire pour régression logistique (opposé vs favorable)
    op_tram_bin = ifelse(op_tram_num >= 0.67, 1, 0),

    # Variables indépendantes - SES
    # Éducation (référence: Secondaire)
    ses_educ_fct = relevel(factor(ses_educ), ref = "Secondaire"),

    # Revenu (référence: <40k - le plus faible)
    ses_revenu_fct = relevel(factor(ses_revenu_grouped), ref = "<40k"),

    # Âge (continue, centrée pour interprétation)
    ses_age_c = as.numeric(scale(as.numeric(ses_age), center = TRUE, scale = FALSE)),

    # Sexe (0 = homme, 1 = femme)
    ses_femme_fct = factor(ses_femme),

    # Statut propriétaire (0 = locataire, 1 = propriétaire)
    ses_proprio_fct = factor(ses_proprio),

    # Arrondissement (référence: Haute-St-Charles - le plus car-dependent)
    ses_arrondissement_fct = relevel(factor(ses_arrondissement),
                                    ref = "Haute-St-Charles")
  )

# Statistiques descriptives
cat("Distribution de la VD:\n")
print(table(data_reg$op_tram_ord))
cat("\n")

# ====================================================================
# 3. MODÈLE 1: Régression logistique ordinale (proportional odds)
# ====================================================================

cat("\n========================================\n")
cat("MODÈLE 1: RÉGRESSION ORDINALE\n")
cat("========================================\n\n")

# Modèle complet avec tous les contrôles SES
model_ord <- polr(op_tram_ord ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                ses_femme_fct + ses_proprio_fct +
                                ses_arrondissement_fct,
                  data = data_reg,
                  weights = POND,
                  Hess = TRUE)

# Résumé du modèle
summary(model_ord)

# Test de significativité globale
cat("\n--- Tests de significativité (Wald) ---\n")
coef_table <- coef(summary(model_ord))
p_values_all <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2
results <- cbind(coef_table, "p-value" = p_values_all)
print(results)

# Garder seulement les p-values des coefficients (pas les intercepts)
n_coef <- nrow(coef_table) - 3  # 3 intercepts dans le modèle ordinal
p_values <- p_values_all[1:n_coef]

# ====================================================================
# 4. ODDS RATIOS ET INTERVALLES DE CONFIANCE
# ====================================================================

cat("\n========================================\n")
cat("ODDS RATIOS (Régression ordinale)\n")
cat("========================================\n\n")

# Calculer les odds ratios
or_ord <- exp(coef(model_ord))
ci_ord_full <- confint(model_ord)

# Extraire seulement les CI des coefficients (en excluant les intercepts)
# Les intercepts sont à la fin dans confint()
coef_names <- names(or_ord)
ci_ord <- ci_ord_full[coef_names, ]
ci_ord <- exp(ci_ord)

# Créer un tableau des OR avec IC à 95%
or_table <- data.frame(
  Variable = coef_names,
  OR = as.numeric(or_ord),
  CI_lower = ci_ord[, 1],
  CI_upper = ci_ord[, 2],
  p_value = p_values,
  row.names = NULL
)

print(or_table)

# ====================================================================
# 5. MODÈLE 2: Régression logistique binaire (opposé vs favorable)
# ====================================================================

cat("\n========================================\n")
cat("MODÈLE 2: RÉGRESSION LOGISTIQUE BINAIRE\n")
cat("VD: Favorable (≥0.67) vs Opposé (<0.67)\n")
cat("========================================\n\n")

model_bin <- glm(op_tram_bin ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                               ses_femme_fct + ses_proprio_fct +
                               ses_arrondissement_fct,
                data = data_reg,
                weights = POND,
                family = binomial(link = "logit"))

summary(model_bin)

# Odds ratios pour modèle binaire
cat("\n--- Odds Ratios (Régression binaire) ---\n")
or_bin <- exp(coef(model_bin))
ci_bin <- exp(confint(model_bin))

or_table_bin <- data.frame(
  Variable = names(or_bin),
  OR = or_bin,
  CI_lower = ci_bin[, 1],
  CI_upper = ci_bin[, 2]
)

print(or_table_bin)

# ====================================================================
# 6. PROBABILITÉS PRÉDITES
# ====================================================================

cat("\n========================================\n")
cat("PROBABILITÉS PRÉDITES\n")
cat("========================================\n\n")

# 6.1 Probabilités par niveau d'éducation (autres variables à leur valeur modale/moyenne)
cat("--- Par niveau d'éducation ---\n")

pred_educ <- predictions(
  model_ord,
  newdata = datagrid(
    ses_educ_fct = unique(data_reg$ses_educ_fct),
    ses_revenu_fct = "40-79k",  # Revenu modal
    ses_age_c = 0,  # Âge moyen (centré)
    ses_femme_fct = "0",  # Homme (référence)
    ses_proprio_fct = "1",  # Propriétaire (modal)
    ses_arrondissement_fct = "Cité-Limoilou"  # Arrondissement modal
  ),
  type = "probs"
)

print(as.data.frame(pred_educ))

# 6.2 Probabilités par arrondissement (autres variables à leur valeur modale/moyenne)
cat("\n--- Par arrondissement ---\n")

pred_arrond <- predictions(
  model_ord,
  newdata = datagrid(
    ses_educ_fct = "Collegial/Certificat",  # Éducation modale
    ses_revenu_fct = "40-79k",  # Revenu modal
    ses_age_c = 0,  # Âge moyen (centré)
    ses_femme_fct = "0",  # Homme (référence)
    ses_proprio_fct = "1",  # Propriétaire (modal)
    ses_arrondissement_fct = unique(data_reg$ses_arrondissement_fct)
  ),
  type = "probs"
)

print(as.data.frame(pred_arrond))

# ====================================================================
# 7. GRAPHIQUES
# ====================================================================

cat("\n========================================\n")
cat("CRÉATION DES GRAPHIQUES\n")
cat("========================================\n\n")

# 7.1 Forest plot des odds ratios
or_plot_data <- or_table %>%
  filter(!grepl("Intercept", Variable)) %>%
  mutate(
    # Catégorisation des variables
    Category = case_when(
      grepl("ses_educ_fct", Variable) ~ "Éducation",
      grepl("ses_revenu_fct", Variable) ~ "Revenu",
      grepl("ses_arrondissement_fct", Variable) ~ "Arrondissement",
      grepl("ses_age_c", Variable) ~ "Démographie",
      grepl("ses_femme_fct", Variable) ~ "Démographie",
      grepl("ses_proprio_fct", Variable) ~ "Statut résidentiel",
      TRUE ~ "Autre"
    ),
    # Ordre pour tri dans chaque catégorie
    Order = case_when(
      Variable == "ses_educ_fctCollegial/Certificat" ~ 1,
      Variable == "ses_educ_fctBacc" ~ 2,
      Variable == "ses_educ_fctGraduate" ~ 3,
      Variable == "ses_revenu_fct40-79k" ~ 4,
      Variable == "ses_revenu_fct80-119k" ~ 5,
      Variable == "ses_revenu_fct120-159k" ~ 6,
      Variable == "ses_revenu_fct160k+" ~ 7,
      Variable == "ses_age_c" ~ 8,
      Variable == "ses_femme_fct1" ~ 9,
      Variable == "ses_proprio_fct1" ~ 10,
      TRUE ~ 11
    ),
    # Noms de variables nettoyés
    Variable_clean = case_when(
      Variable == "ses_educ_fctCollegial/Certificat" ~ "Collégial/Certificat",
      Variable == "ses_educ_fctBacc" ~ "Baccalauréat",
      Variable == "ses_educ_fctGraduate" ~ "Études graduées",
      Variable == "ses_revenu_fct40-79k" ~ "40-79k$",
      Variable == "ses_revenu_fct80-119k" ~ "80-119k$",
      Variable == "ses_revenu_fct120-159k" ~ "120-159k$",
      Variable == "ses_revenu_fct160k+" ~ "160k$+",
      Variable == "ses_age_c" ~ "Âge (centré)",
      Variable == "ses_femme_fct1" ~ "Femme",
      Variable == "ses_proprio_fct1" ~ "Propriétaire",
      grepl("ses_arrondissement_fct", Variable) ~ gsub("ses_arrondissement_fct", "", Variable),
      TRUE ~ Variable
    ),
    # Ajouter le nom de catégorie pour l'affichage
    Variable_display = paste0(Variable_clean),
    Significant = ifelse(p_value < 0.05, "Significatif (p < 0.05)", "Non significatif"),
    # Texte pour afficher OR et IC
    OR_text = sprintf("%.2f [%.2f-%.2f]", OR, CI_lower, CI_upper)
  ) %>%
  arrange(desc(Category), Order)

# Créer une variable de position pour l'axe Y
or_plot_data <- or_plot_data %>%
  mutate(y_position = row_number())

plot_or <- ggplot(or_plot_data, aes(x = OR, y = y_position)) +
  # Ligne de référence à OR = 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.8) +
  # Barres d'erreur (IC 95%)
  geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper, color = Significant),
                width = 0.3, linewidth = 0.9) +
  # Points pour les OR
  geom_point(aes(color = Significant), size = 4) +
  # Ajouter les valeurs numériques à droite
  geom_text(aes(label = OR_text, x = max(CI_upper) * 1.5),
            size = 3, hjust = 0, family = "mono") +
  # Échelle logarithmique
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 4),
                labels = c("0.5", "0.75", "1.0", "1.5", "2.0", "3.0", "4.0")) +
  scale_y_continuous(breaks = or_plot_data$y_position,
                    labels = or_plot_data$Variable_display) +
  scale_color_manual(values = c("Significatif (p < 0.05)" = "#1976D2",
                                "Non significatif" = "grey60")) +
  # Ajouter des séparateurs entre catégories
  geom_hline(yintercept = c(3.5, 7.5, 9.5, 10.5),
             linetype = "dotted", color = "grey70", linewidth = 0.5) +
  labs(title = "Odds Ratios: Effets sur la position envers le tramway",
       subtitle = "Modèle de régression ordinale (échelle logarithmique)\nRéférences: Secondaire, <40k$, Haute-St-Charles, Homme, Locataire",
       x = "Odds Ratio (IC à 95%)",
       y = "",
       color = "") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 15)),
    axis.text.y = element_text(size = 10, hjust = 1),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
    plot.margin = margin(10, 80, 10, 10)
  )

ggsave("_SharedFolder_culture_carbone/graph/output/odds_ratios_tramway.png",
       plot_or, width = 10, height = 6, dpi = 300)

cat("Graphique sauvegardé: odds_ratios_tramway.png\n")

# 7.2 Probabilités prédites par éducation
pred_educ_df <- as.data.frame(pred_educ) %>%
  filter(!is.na(ses_educ_fct)) %>%  # Enlever les NA
  mutate(
    Education = gsub("ses_educ_fct", "", ses_educ_fct),
    # Renommer Graduate
    Education = gsub("Graduate", "Études graduées", Education),
    # Ordonner les niveaux d'éducation
    Education = factor(Education,
                      levels = c("Secondaire", "Collegial/Certificat",
                                "Bacc", "Études graduées")),
    # Correspondance avec les niveaux du modèle (sans accents)
    Outcome = factor(group,
                    levels = c("Opposé", "Plutôt opposé",
                              "Plutôt favorable", "Favorable"),
                    ordered = TRUE)
  )

plot_pred_educ <- ggplot(pred_educ_df,
                        aes(x = Education, y = estimate, fill = Outcome)) +
  geom_col(position = "stack") +
  geom_text(aes(label = sprintf("%.0f%%", estimate * 100)),
           position = position_stack(vjust = 0.5),
           size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(title = "Probabilités prédites de position sur le tramway par niveau d'éducation",
       subtitle = "Arrondissement de référence: Cité-Limoilou",
       x = "Niveau d'éducation",
       y = "Probabilité prédite",
       fill = "Position") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave("_SharedFolder_culture_carbone/graph/output/prob_pred_education.png",
       plot_pred_educ, width = 10, height = 6, dpi = 300)

cat("Graphique sauvegardé: prob_pred_education.png\n")

# 7.3 Probabilités prédites par arrondissement
pred_arrond_df <- as.data.frame(pred_arrond) %>%
  mutate(
    Arrondissement = ses_arrondissement_fct,
    # Correspondance avec les niveaux du modèle (avec accents maintenant)
    Outcome = factor(group,
                    levels = c("Opposé", "Plutôt opposé",
                              "Plutôt favorable", "Favorable"),
                    ordered = TRUE)
  )

plot_pred_arrond <- ggplot(pred_arrond_df,
                          aes(x = Arrondissement, y = estimate, fill = Outcome)) +
  geom_col(position = "stack") +
  geom_text(aes(label = sprintf("%.0f%%", estimate * 100)),
           position = position_stack(vjust = 0.5),
           size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(title = "Probabilités prédites de position sur le tramway par arrondissement",
       subtitle = "Niveau d'éducation de référence: Collegial/Certificat",
       x = "Arrondissement",
       y = "Probabilité prédite",
       fill = "Position") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave("_SharedFolder_culture_carbone/graph/output/prob_pred_arrondissement.png",
       plot_pred_arrond, width = 10, height = 6, dpi = 300)

cat("Graphique sauvegardé: prob_pred_arrondissement.png\n")

# ====================================================================
# 8. MODÈLE 3: PROFILS CULTURELS (Régression multinomiale)
# ====================================================================

cat("\n========================================\n")
cat("MODÈLE 3: PROFILS CULTURELS\n")
cat("VD: profil_culture (8 catégories)\n")
cat("========================================\n\n")

# Préparation des données pour profils culturels
data_profils <- data_culture %>%
  filter(!is.na(profil_culture)) %>%
  mutate(
    # Variables indépendantes (mêmes que précédemment)
    ses_educ_fct = relevel(factor(ses_educ), ref = "Secondaire"),
    ses_revenu_fct = relevel(factor(ses_revenu_grouped), ref = "<40k"),
    ses_age_c = as.numeric(scale(as.numeric(ses_age), center = TRUE, scale = FALSE)),
    ses_femme_fct = factor(ses_femme),
    ses_proprio_fct = factor(ses_proprio),
    ses_arrondissement_fct = relevel(factor(ses_arrondissement), ref = "Haute-St-Charles"),

    # Catégorie de référence pour profil: "Carbone normalisé" (le plus fréquent côté carbone)
    profil_culture = relevel(profil_culture, ref = "Carbone normalisé")
  )

# Distribution des profils
cat("Distribution des profils culturels:\n")
print(table(data_profils$profil_culture))
cat("\n")

# 8.1 Modèle sans interaction
cat("\n--- Modèle principal (effets additifs) ---\n")
model_profils <- multinom(profil_culture ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                           ses_femme_fct + ses_proprio_fct +
                                           ses_arrondissement_fct,
                          data = data_profils,
                          weights = POND,
                          trace = FALSE)

# Résumé du modèle
summary(model_profils)

# Tests de significativité (z-tests)
z_stats <- summary(model_profils)$coefficients / summary(model_profils)$standard.errors
p_values_multinom <- 2 * (1 - pnorm(abs(z_stats)))

cat("\n--- p-values (tests z) ---\n")
print(round(p_values_multinom, 4))

# 8.2 Modèle avec interaction éducation × arrondissement
cat("\n\n--- Modèle avec interaction éducation × arrondissement ---\n")
model_profils_inter <- multinom(profil_culture ~ ses_educ_fct * ses_arrondissement_fct +
                                                 ses_revenu_fct + ses_age_c +
                                                 ses_femme_fct + ses_proprio_fct,
                                data = data_profils,
                                weights = POND,
                                trace = FALSE,
                                maxit = 500)

# Test du rapport de vraisemblance pour l'interaction
lr_test <- anova(model_profils, model_profils_inter, test = "Chisq")
cat("\n--- Test du rapport de vraisemblance (interaction) ---\n")
print(lr_test)

# Critères d'ajustement
cat("\n--- Critères d'ajustement ---\n")
cat("Modèle sans interaction:\n")
cat("  AIC:", AIC(model_profils), "\n")
cat("  BIC:", BIC(model_profils), "\n")
cat("\nModèle avec interaction:\n")
cat("  AIC:", AIC(model_profils_inter), "\n")
cat("  BIC:", BIC(model_profils_inter), "\n")

# 8.3 Probabilités prédites par éducation
cat("\n\n--- Probabilités prédites par niveau d'éducation ---\n")

pred_profils_educ <- predictions(
  model_profils,
  newdata = datagrid(
    ses_educ_fct = unique(data_profils$ses_educ_fct),
    ses_revenu_fct = "40-79k",
    ses_age_c = 0,
    ses_femme_fct = "0",
    ses_proprio_fct = "1",
    ses_arrondissement_fct = "Cité-Limoilou"
  ),
  type = "probs"
)

print(as.data.frame(pred_profils_educ) %>% dplyr::select(group, estimate, ses_educ_fct))

# 8.4 Probabilités prédites par arrondissement
cat("\n--- Probabilités prédites par arrondissement ---\n")

pred_profils_arrond <- predictions(
  model_profils,
  newdata = datagrid(
    ses_educ_fct = "Collegial/Certificat",
    ses_revenu_fct = "40-79k",
    ses_age_c = 0,
    ses_femme_fct = "0",
    ses_proprio_fct = "1",
    ses_arrondissement_fct = unique(data_profils$ses_arrondissement_fct)
  ),
  type = "probs"
)

print(as.data.frame(pred_profils_arrond) %>% dplyr::select(group, estimate, ses_arrondissement_fct))

# ====================================================================
# 9. GRAPHIQUES PROFILS CULTURELS
# ====================================================================

cat("\n========================================\n")
cat("CRÉATION DES GRAPHIQUES - PROFILS\n")
cat("========================================\n\n")

# 9.1 Graphique: Probabilités prédites par éducation
pred_profils_educ_df <- as.data.frame(pred_profils_educ) %>%
  filter(!is.na(ses_educ_fct)) %>%
  mutate(
    Education = case_when(
      ses_educ_fct == "Secondaire" ~ "Secondaire",
      ses_educ_fct == "Collegial/Certificat" ~ "Collégial/Certificat",
      ses_educ_fct == "Bacc" ~ "Baccalauréat",
      ses_educ_fct == "Graduate" ~ "Études graduées",
      TRUE ~ as.character(ses_educ_fct)
    ),
    Education = factor(Education,
                      levels = c("Secondaire", "Collégial/Certificat",
                                "Baccalauréat", "Études graduées")),
    Profil = factor(group,
                   levels = c("Carbone normalisé", "Carbone résistant", "Carbone atypique",
                             "Éco contraint", "Éco latent", "Éco résistant",
                             "Éco limité", "Éco normalisé")),
    Type = ifelse(grepl("^Éco", group), "Écologique", "Carbone")
  )

plot_profils_educ <- ggplot(pred_profils_educ_df,
                            aes(x = Education, y = estimate, fill = Profil)) +
  geom_col(position = "stack") +
  geom_text(aes(label = ifelse(estimate > 0.05, sprintf("%.0f%%", estimate * 100), "")),
           position = position_stack(vjust = 0.5),
           size = 2.5, color = "white") +
  scale_fill_manual(values = c(
    "Carbone normalisé" = "#8B4513",
    "Carbone résistant" = "#A0522D",
    "Carbone atypique" = "#CD853F",
    "Éco contraint" = "#B8D4B8",
    "Éco latent" = "#90C890",
    "Éco résistant" = "#4CAF50",
    "Éco limité" = "#66BB6A",
    "Éco normalisé" = "#2E7D32"
  )) +
  labs(title = "Probabilités prédites des profils culturels par niveau d'éducation",
       subtitle = "Référence: Carbone normalisé | Arrondissement: Cité-Limoilou",
       x = "Niveau d'éducation",
       y = "Probabilité prédite",
       fill = "Profil culturel") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 9)
  )

ggsave("_SharedFolder_culture_carbone/graph/output/profils_culture_education.png",
       plot_profils_educ, width = 12, height = 7, dpi = 300)

cat("Graphique sauvegardé: profils_culture_education.png\n")

# 9.2 Graphique: Probabilités prédites par arrondissement
pred_profils_arrond_df <- as.data.frame(pred_profils_arrond) %>%
  mutate(
    Arrondissement = ses_arrondissement_fct,
    Profil = factor(group,
                   levels = c("Carbone normalisé", "Carbone résistant", "Carbone atypique",
                             "Éco contraint", "Éco latent", "Éco résistant",
                             "Éco limité", "Éco normalisé")),
    Type = ifelse(grepl("^Éco", group), "Écologique", "Carbone")
  )

plot_profils_arrond <- ggplot(pred_profils_arrond_df,
                              aes(x = Arrondissement, y = estimate, fill = Profil)) +
  geom_col(position = "stack") +
  geom_text(aes(label = ifelse(estimate > 0.05, sprintf("%.0f%%", estimate * 100), "")),
           position = position_stack(vjust = 0.5),
           size = 2.5, color = "white") +
  scale_fill_manual(values = c(
    "Carbone normalisé" = "#8B4513",
    "Carbone résistant" = "#A0522D",
    "Carbone atypique" = "#CD853F",
    "Éco contraint" = "#B8D4B8",
    "Éco latent" = "#90C890",
    "Éco résistant" = "#4CAF50",
    "Éco limité" = "#66BB6A",
    "Éco normalisé" = "#2E7D32"
  )) +
  labs(title = "Probabilités prédites des profils culturels par arrondissement",
       subtitle = "Référence: Carbone normalisé | Éducation: Collégial/Certificat",
       x = "Arrondissement",
       y = "Probabilité prédite",
       fill = "Profil culturel") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 9)
  )

ggsave("_SharedFolder_culture_carbone/graph/output/profils_culture_arrondissement.png",
       plot_profils_arrond, width = 12, height = 7, dpi = 300)

cat("Graphique sauvegardé: profils_culture_arrondissement.png\n")

cat("\n========================================\n")
cat("ANALYSE TERMINÉE\n")
cat("========================================\n")
