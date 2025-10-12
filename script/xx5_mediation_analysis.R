library(dplyr)
library(nnet)
library(marginaleffects)
library(ggplot2)
library(mediation)

# Charger les données
data_culture <- readRDS("_SharedFolder_culture_carbone/data/data_culture.rds")

cat("\n========================================\n")
cat("ANALYSE DE MÉDIATION - RÉPERTOIRE ÉCOLOGIQUE\n")
cat("========================================\n\n")

# ====================================================================
# 1. NOUVEAUX PROFILS CULTURELS SIMPLIFIÉS (sans repertoire_eco)
# ====================================================================

cat("ÉTAPE 1: Création des profils culturels simplifiés\n")
cat("----------------------------------------------------\n\n")

data_culture <- data_culture %>%
  mutate(
    # 4 profils basés UNIQUEMENT sur idéal × comportement
    profil_simple = case_when(
      ideal_eco_bin == 1 & comp_transport_eco == 1 ~ "Éco cohérent",
      ideal_eco_bin == 0 & comp_transport_eco == 0 ~ "Carbone cohérent",
      ideal_eco_bin == 1 & comp_transport_eco == 0 ~ "Aspiration contrainte",
      ideal_eco_bin == 0 & comp_transport_eco == 1 ~ "Pratique non assumée",
      TRUE ~ NA_character_
    ),
    # Convertir en facteur
    profil_simple = factor(profil_simple,
                          levels = c("Éco cohérent", "Aspiration contrainte",
                                   "Pratique non assumée", "Carbone cohérent"),
                          ordered = FALSE),

    # Variable de cohérence binaire (alignement idéal-comportement)
    coherence = case_when(
      ideal_eco_bin == comp_transport_eco ~ 1,  # Alignés
      ideal_eco_bin != comp_transport_eco ~ 0,  # Désalignés
      TRUE ~ NA_real_
    ),

    # Type de cohérence (pour analyses descriptives)
    type_coherence = case_when(
      profil_simple == "Éco cohérent" ~ "Cohérence écologique",
      profil_simple == "Carbone cohérent" ~ "Cohérence carbone",
      profil_simple == "Aspiration contrainte" ~ "Dissonance (aspiration éco)",
      profil_simple == "Pratique non assumée" ~ "Dissonance (pratique éco)"
    )
  )

# Distribution des nouveaux profils
cat("Distribution des profils culturels simplifiés:\n\n")
table_profils_simple <- table(data_culture$profil_simple, useNA = "ifany")
print(table_profils_simple)
cat("\n")
print(round(prop.table(table_profils_simple) * 100, 1))

cat("\n\nDistribution de la cohérence:\n")
table_coherence <- table(data_culture$coherence, useNA = "ifany")
names(table_coherence) <- c("Dissonance", "Cohérence", "NA")
print(table_coherence)
cat("\n")
print(round(prop.table(table_coherence[1:2]) * 100, 1))

# ====================================================================
# 2. ANALYSES DESCRIPTIVES: RÉPERTOIRE PAR PROFIL
# ====================================================================

cat("\n\n========================================\n")
cat("ÉTAPE 2: Répertoire écologique par profil\n")
cat("========================================\n\n")

repertoire_by_profil <- data_culture %>%
  filter(!is.na(profil_simple)) %>%
  group_by(profil_simple) %>%
  summarise(
    n = n(),
    repertoire_moyen = mean(repertoire_eco, na.rm = TRUE),
    repertoire_sd = sd(repertoire_eco, na.rm = TRUE),
    repertoire_min = min(repertoire_eco, na.rm = TRUE),
    repertoire_max = max(repertoire_eco, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(repertoire_moyen))

print(repertoire_by_profil)

cat("\n\nInterprétation:\n")
cat("- Si 'Éco cohérent' a un répertoire plus élevé que 'Aspiration contrainte',\n")
cat("  cela suggère que le répertoire FACILITE la cohérence écologique.\n")
cat("- Si 'Carbone cohérent' a un répertoire plus faible que 'Pratique non assumée',\n")
cat("  cela suggère que le faible répertoire FACILITE/JUSTIFIE la cohérence carbone.\n\n")

# ====================================================================
# 3. TEST DIRECT: RÉPERTOIRE PRÉDIT-IL LA COHÉRENCE?
# ====================================================================

cat("\n========================================\n")
cat("ÉTAPE 3: Répertoire prédit-il la cohérence?\n")
cat("========================================\n\n")

# Préparer les données (exclure NA)
data_mediation <- data_culture %>%
  filter(!is.na(coherence) & !is.na(repertoire_eco) & !is.na(ses_arrondissement)) %>%
  mutate(
    ses_arrondissement_fct = factor(ses_arrondissement,
                                   levels = c("Haute-St-Charles", "Beauport", "Charlesbourg",
                                            "Cité-Limoilou", "Les Rivières", "Ste-Foy-Sillery-Cap-Rouge"))
  )

cat("N pour analyses de médiation:", nrow(data_mediation), "\n\n")

# Modèle simple: répertoire → cohérence
cat("--- Modèle simple: repertoire_eco → coherence ---\n\n")
model_direct <- glm(coherence ~ repertoire_eco,
                   data = data_mediation,
                   family = binomial(link = "logit"),
                   weights = POND)

summary(model_direct)

# Calculer OR
or_direct <- exp(coef(model_direct)[2])
ci_direct <- exp(confint(model_direct)[2,])

cat("\n\nOdds Ratio du répertoire écologique:\n")
cat(sprintf("OR = %.3f [%.3f - %.3f]\n", or_direct, ci_direct[1], ci_direct[2]))

if (summary(model_direct)$coefficients[2,4] < 0.05) {
  if (or_direct > 1) {
    cat("\n✓ Le répertoire écologique AUGMENTE significativement la cohérence.\n")
  } else {
    cat("\n✓ Le répertoire écologique DIMINUE significativement la cohérence.\n")
  }
} else {
  cat("\n✗ Le répertoire écologique n'a PAS d'effet significatif sur la cohérence.\n")
}

# ====================================================================
# 4. ANALYSE DE MÉDIATION FORMELLE
# ====================================================================

cat("\n\n========================================\n")
cat("ÉTAPE 4: Analyse de médiation formelle\n")
cat("========================================\n\n")

cat("Modèle testé:\n")
cat("  Arrondissement → Repertoire_eco → Cohérence\n")
cat("            ↓                           ↑\n")
cat("            └─────────────────────────┘\n")
cat("                  (effet direct)\n\n")

# Dichotomiser arrondissement pour simplifier
data_mediation <- data_mediation %>%
  mutate(
    arrond_centre = ifelse(ses_arrondissement %in% c("Cité-Limoilou", "Ste-Foy-Sillery-Cap-Rouge"), 1, 0)
  )

cat("Dichotomisation géographique:\n")
cat("- Centre (1): Cité-Limoilou, Ste-Foy-Sillery-Cap-Rouge\n")
cat("- Périphérie (0): Autres arrondissements\n\n")

# Modèle de médiation selon Baron & Kenny (1986)
cat("--- Étape 1: Effet total (c) ---\n")
cat("Arrondissement → Cohérence (sans médiateur)\n\n")

model_total <- glm(coherence ~ arrond_centre,
                  data = data_mediation,
                  family = binomial(link = "logit"),
                  weights = POND)

summary(model_total)

or_total <- exp(coef(model_total)[2])
ci_total <- exp(confint(model_total)[2,])
cat(sprintf("\nEffet total (OR): %.3f [%.3f - %.3f]\n", or_total, ci_total[1], ci_total[2]))

cat("\n\n--- Étape 2: Effet sur le médiateur (a) ---\n")
cat("Arrondissement → Repertoire_eco\n\n")

model_mediator <- lm(repertoire_eco ~ arrond_centre,
                    data = data_mediation,
                    weights = POND)

summary(model_mediator)

coef_a <- coef(model_mediator)[2]
cat(sprintf("\nCoefficient a: %.4f\n", coef_a))
cat("Interprétation: Les arrondissements centraux ont un répertoire écologique")
cat(sprintf(" %.4f points plus élevé que la périphérie.\n", coef_a))

cat("\n\n--- Étape 3: Effet du médiateur contrôlant X (b) ---\n")
cat("Arrondissement + Repertoire_eco → Cohérence\n\n")

model_mediated <- glm(coherence ~ arrond_centre + repertoire_eco,
                     data = data_mediation,
                     family = binomial(link = "logit"),
                     weights = POND)

summary(model_mediated)

or_direct_controlled <- exp(coef(model_mediated)[2])  # c'
or_mediator_controlled <- exp(coef(model_mediated)[3])  # b
ci_direct_controlled <- exp(confint(model_mediated)[2,])
ci_mediator_controlled <- exp(confint(model_mediated)[3,])

cat("\n\nEffets dans le modèle complet:\n")
cat(sprintf("Effet direct (c', arrondissement): OR = %.3f [%.3f - %.3f]\n",
            or_direct_controlled, ci_direct_controlled[1], ci_direct_controlled[2]))
cat(sprintf("Effet médiateur (b, répertoire): OR = %.3f [%.3f - %.3f]\n",
            or_mediator_controlled, ci_mediator_controlled[1], ci_mediator_controlled[2]))

# Test de médiation avec package mediation
cat("\n\n--- Test de médiation avec bootstrap ---\n")
cat("(cela peut prendre quelques secondes...)\n\n")

# Modèles pour package mediation
med_fit <- lm(repertoire_eco ~ arrond_centre, data = data_mediation, weights = POND)
out_fit <- glm(coherence ~ arrond_centre + repertoire_eco,
               data = data_mediation,
               family = binomial(link = "logit"),
               weights = POND)

# Analyse de médiation avec bootstrap (1000 simulations)
set.seed(2025)
med_results <- mediate(med_fit, out_fit,
                      treat = "arrond_centre",
                      mediator = "repertoire_eco",
                      boot = TRUE,
                      sims = 1000)

summary(med_results)

# ====================================================================
# 5. RÉGRESSIONS MULTINOMIALES AVEC PROFILS SIMPLIFIÉS
# ====================================================================

cat("\n\n========================================\n")
cat("ÉTAPE 5: Régressions avec profils simplifiés\n")
cat("========================================\n\n")

# Préparer données pour régression
data_reg_simple <- data_culture %>%
  filter(!is.na(profil_simple) & !is.na(ses_educ) & !is.na(ses_arrondissement)) %>%
  mutate(
    ses_educ_fct = factor(ses_educ,
                         levels = c("Secondaire", "Collegial/Certificat", "Bacc", "Graduate")),
    ses_arrondissement_fct = factor(ses_arrondissement,
                                   levels = c("Haute-St-Charles", "Beauport", "Charlesbourg",
                                            "Cité-Limoilou", "Les Rivières", "Ste-Foy-Sillery-Cap-Rouge")),
    ses_revenu_fct = factor(ses_revenu_grouped,
                           levels = c("<40k", "40-79k", "80-119k", "120-159k", "160k+")),
    ses_age_c = as.numeric(scale(as.numeric(ses_age), center = TRUE, scale = FALSE)),
    ses_femme_fct = factor(ses_femme),
    ses_proprio_fct = factor(ses_proprio),
    # Mettre "Carbone cohérent" comme référence (profil modal)
    profil_simple = relevel(profil_simple, ref = "Carbone cohérent")
  )

cat("Distribution des profils pour régression:\n")
print(table(data_reg_simple$profil_simple))
cat("\n")

# Modèle principal (effets additifs)
cat("--- Modèle multinomial: Profils simplifiés (4 catégories) ---\n\n")

model_simple <- multinom(profil_simple ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                        ses_femme_fct + ses_proprio_fct + ses_arrondissement_fct,
                        data = data_reg_simple,
                        weights = POND,
                        trace = FALSE)

summary(model_simple)

# Tests de significativité
cat("\n\n--- Tests de significativité (z-tests) ---\n\n")
z_values <- summary(model_simple)$coefficients / summary(model_simple)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))

cat("P-values pour chaque prédicteur:\n")
print(round(p_values, 4))

# Modèle avec interaction éducation × arrondissement
cat("\n\n--- Modèle avec interaction éducation × arrondissement ---\n\n")

model_simple_inter <- multinom(profil_simple ~ ses_educ_fct * ses_arrondissement_fct +
                                              ses_revenu_fct + ses_age_c +
                                              ses_femme_fct + ses_proprio_fct,
                              data = data_reg_simple,
                              weights = POND,
                              trace = FALSE,
                              maxit = 500)

# Test du rapport de vraisemblance
cat("\n--- Test du rapport de vraisemblance (interaction) ---\n")
anova_result <- anova(model_simple, model_simple_inter)
print(anova_result)

# Critères d'ajustement
cat("\n--- Critères d'ajustement ---\n")
cat("Modèle sans interaction:\n")
cat("  AIC:", AIC(model_simple), "\n")
cat("  BIC:", BIC(model_simple), "\n\n")

cat("Modèle avec interaction:\n")
cat("  AIC:", AIC(model_simple_inter), "\n")
cat("  BIC:", BIC(model_simple_inter), "\n\n")

if (anova_result$`Pr(Chi)`[2] < 0.05) {
  cat("✓ L'interaction est SIGNIFICATIVE (p < 0.05)\n")
} else {
  cat("✗ L'interaction n'est PAS significative (p >= 0.05)\n")
  cat("→ Le modèle additif est préférable (AIC/BIC plus faibles)\n")
}

# ====================================================================
# 6. PROBABILITÉS PRÉDITES ET VISUALISATIONS
# ====================================================================

cat("\n\n========================================\n")
cat("ÉTAPE 6: Probabilités prédites\n")
cat("========================================\n\n")

# Probabilités prédites par éducation
cat("--- Probabilités prédites par niveau d'éducation ---\n\n")

pred_simple_educ <- predictions(
  model_simple,
  newdata = datagrid(
    ses_educ_fct = c("Secondaire", "Collegial/Certificat", "Bacc", "Graduate"),
    ses_revenu_fct = "40-79k",
    ses_age_c = 0,
    ses_femme_fct = "0",
    ses_proprio_fct = "1",
    ses_arrondissement_fct = "Cité-Limoilou"
  ),
  type = "probs"
)

print(as.data.frame(pred_simple_educ) %>%
        dplyr::select(group, estimate, ses_educ_fct) %>%
        arrange(group, ses_educ_fct))

# Probabilités prédites par arrondissement
cat("\n\n--- Probabilités prédites par arrondissement ---\n\n")

pred_simple_arrond <- predictions(
  model_simple,
  newdata = datagrid(
    ses_educ_fct = "Collegial/Certificat",
    ses_revenu_fct = "40-79k",
    ses_age_c = 0,
    ses_femme_fct = "0",
    ses_proprio_fct = "1",
    ses_arrondissement_fct = unique(data_reg_simple$ses_arrondissement_fct)
  ),
  type = "probs"
)

print(as.data.frame(pred_simple_arrond) %>%
        dplyr::select(group, estimate, ses_arrondissement_fct) %>%
        arrange(group, ses_arrondissement_fct))

# ====================================================================
# 7. GRAPHIQUES COMPARATIFS
# ====================================================================

cat("\n\n========================================\n")
cat("ÉTAPE 7: Création des graphiques\n")
cat("========================================\n\n")

# Graphique 1: Distribution des profils simplifiés par éducation
plot_data_educ <- as.data.frame(pred_simple_educ) %>%
  mutate(
    Profil = factor(group, levels = c("Éco cohérent", "Aspiration contrainte",
                                       "Pratique non assumée", "Carbone cohérent"))
  )

p1 <- ggplot(plot_data_educ, aes(x = ses_educ_fct, y = estimate, fill = Profil)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Éco cohérent" = "#2E7D32",
                               "Aspiration contrainte" = "#FFA726",
                               "Pratique non assumée" = "#9C27B0",
                               "Carbone cohérent" = "#D32F2F")) +
  labs(title = "Distribution des profils culturels simplifiés par niveau d'éducation",
       subtitle = "Probabilités prédites (pondérées)",
       x = "Niveau d'éducation",
       y = "Probabilité prédite",
       fill = "Profil culturel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))

ggsave("profils_simples_education.png", p1, width = 10, height = 7, dpi = 300)
cat("Graphique sauvegardé: profils_simples_education.png\n")

# Graphique 2: Distribution des profils simplifiés par arrondissement
plot_data_arrond <- as.data.frame(pred_simple_arrond) %>%
  mutate(
    Profil = factor(group, levels = c("Éco cohérent", "Aspiration contrainte",
                                       "Pratique non assumée", "Carbone cohérent")),
    ses_arrondissement_fct = factor(ses_arrondissement_fct,
                                   levels = c("Haute-St-Charles", "Beauport", "Charlesbourg",
                                            "Les Rivières", "Cité-Limoilou",
                                            "Ste-Foy-Sillery-Cap-Rouge"))
  )

p2 <- ggplot(plot_data_arrond, aes(x = ses_arrondissement_fct, y = estimate, fill = Profil)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Éco cohérent" = "#2E7D32",
                               "Aspiration contrainte" = "#FFA726",
                               "Pratique non assumée" = "#9C27B0",
                               "Carbone cohérent" = "#D32F2F")) +
  labs(title = "Distribution des profils culturels simplifiés par arrondissement",
       subtitle = "Probabilités prédites (pondérées)",
       x = "Arrondissement",
       y = "Probabilité prédite",
       fill = "Profil culturel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))

ggsave("profils_simples_arrondissement.png", p2, width = 12, height = 7, dpi = 300)
cat("Graphique sauvegardé: profils_simples_arrondissement.png\n")

# Graphique 3: Répertoire écologique par profil (boxplot)
p3 <- data_culture %>%
  filter(!is.na(profil_simple)) %>%
  ggplot(aes(x = profil_simple, y = repertoire_eco, fill = profil_simple)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Éco cohérent" = "#2E7D32",
                               "Aspiration contrainte" = "#FFA726",
                               "Pratique non assumée" = "#9C27B0",
                               "Carbone cohérent" = "#D32F2F")) +
  labs(title = "Répertoire écologique par profil culturel",
       subtitle = "Le répertoire facilite-t-il la cohérence idéal-comportement?",
       x = "Profil culturel",
       y = "Répertoire écologique (0-1)",
       caption = "Note: Répertoire = moyenne pondérée de walk_score, bike_score, transit_score de l'arrondissement") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

ggsave("repertoire_par_profil.png", p3, width = 10, height = 7, dpi = 300)
cat("Graphique sauvegardé: repertoire_par_profil.png\n")

# ====================================================================
# 8. SAUVEGARDE DES DONNÉES ENRICHIES
# ====================================================================

# Sauvegarder les données avec nouveaux profils
saveRDS(data_culture, "_SharedFolder_culture_carbone/data/data_culture_v2.rds")
cat("\n\nDonnées enrichies sauvegardées: data_culture_v2.rds\n")
cat("(Contient les nouveaux profils simplifiés et la variable de cohérence)\n")

cat("\n\n========================================\n")
cat("ANALYSE TERMINÉE\n")
cat("========================================\n\n")

cat("RÉSUMÉ DES RÉSULTATS:\n")
cat("----------------------\n")
cat("1. Nouveaux profils créés (4 catégories sans circularité)\n")
cat("2. Test de l'effet du répertoire sur la cohérence effectué\n")
cat("3. Analyse de médiation formelle complétée\n")
cat("4. Régressions multinomiales avec profils simplifiés\n")
cat("5. Graphiques comparatifs générés\n\n")

cat("FICHIERS GÉNÉRÉS:\n")
cat("-----------------\n")
cat("- profils_simples_education.png\n")
cat("- profils_simples_arrondissement.png\n")
cat("- repertoire_par_profil.png\n")
cat("- data_culture_v2.rds (données enrichies)\n\n")
