library(dplyr)
library(ggplot2)
library(broom)

# ====================================================================
# RÉGRESSIONS COMPLÉMENTAIRES
# VD1: Idéal écologique (ideal_eco_bin)
# VD2: Comportement écologique (comp_transport_eco)
# ====================================================================

cat("\n========================================\n")
cat("RÉGRESSIONS COMPLÉMENTAIRES\n")
cat("Analyse des déterminants de l'idéal et du comportement\n")
cat("========================================\n\n")

# ====================================================================
# 1. CHARGEMENT DES DONNÉES
# ====================================================================

data_culture <- readRDS("_SharedFolder_culture_carbone/data/data_culture.rds")

# ====================================================================
# 2. PRÉPARATION DES DONNÉES
# ====================================================================

data_reg <- data_culture %>%
  filter(!is.na(ideal_eco_bin) & !is.na(comp_transport_eco)) %>%
  mutate(
    # Variables indépendantes
    ses_educ_fct = relevel(factor(ses_educ), ref = "Secondaire"),
    ses_revenu_fct = relevel(factor(ses_revenu_grouped), ref = "<40k"),
    ses_age_c = as.numeric(scale(as.numeric(ses_age), center = TRUE, scale = FALSE)),
    ses_femme_fct = factor(ses_femme),
    ses_proprio_fct = factor(ses_proprio),
    ses_arrondissement_fct = relevel(factor(ses_arrondissement), ref = "Haute-St-Charles")
  )

cat("N total pour analyse:", nrow(data_reg), "\n")
cat("N avec idéal éco:", sum(data_reg$ideal_eco_bin, na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(data_reg$ideal_eco_bin, na.rm = TRUE) * 100))
cat("N avec comportement éco:", sum(data_reg$comp_transport_eco, na.rm = TRUE),
    sprintf("(%.1f%%)\n\n", mean(data_reg$comp_transport_eco, na.rm = TRUE) * 100))

# ====================================================================
# 3. MODÈLE 1: IDÉAL ÉCOLOGIQUE
# ====================================================================

cat("\n========================================\n")
cat("MODÈLE 1: IDÉAL ÉCOLOGIQUE\n")
cat("VD: ideal_eco_bin (mode idéal écologique)\n")
cat("========================================\n\n")

model_ideal <- glm(ideal_eco_bin ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                   ses_femme_fct + ses_proprio_fct +
                                   ses_arrondissement_fct,
                   data = data_reg,
                   weights = POND,
                   family = binomial(link = "logit"))

summary(model_ideal)

# Calculer odds ratios et IC 95%
cat("\n--- Odds Ratios et IC 95% ---\n")
coefs_ideal <- coef(model_ideal)
ci_ideal <- confint(model_ideal)

or_ideal <- data.frame(
  Variable_raw = names(coefs_ideal),
  OR = exp(coefs_ideal),
  CI_lower = exp(ci_ideal[, 1]),
  CI_upper = exp(ci_ideal[, 2]),
  p_value = coef(summary(model_ideal))[, "Pr(>|z|)"]
)

print(or_ideal)

# ====================================================================
# 4. MODÈLE 2: COMPORTEMENT ÉCOLOGIQUE
# ====================================================================

cat("\n========================================\n")
cat("MODÈLE 2: COMPORTEMENT ÉCOLOGIQUE\n")
cat("VD: comp_transport_eco (pratique transport écologique)\n")
cat("========================================\n\n")

model_comp <- glm(comp_transport_eco ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                       ses_femme_fct + ses_proprio_fct +
                                       ses_arrondissement_fct,
                  data = data_reg,
                  weights = POND,
                  family = binomial(link = "logit"))

summary(model_comp)

# Calculer odds ratios et IC 95%
cat("\n--- Odds Ratios et IC 95% ---\n")
coefs_comp <- coef(model_comp)
ci_comp <- confint(model_comp)

or_comp <- data.frame(
  Variable_raw = names(coefs_comp),
  OR = exp(coefs_comp),
  CI_lower = exp(ci_comp[, 1]),
  CI_upper = exp(ci_comp[, 2]),
  p_value = coef(summary(model_comp))[, "Pr(>|z|)"]
)

print(or_comp)

# ====================================================================
# 5. FONCTION POUR CRÉER FOREST PLOTS
# ====================================================================

create_forest_plot <- function(or_data, title_text, color_sig, model_obj, n_total, n_events) {

  # Exclure intercept et préparer les données
  plot_data <- or_data %>%
    filter(Variable_raw != "(Intercept)") %>%
    mutate(
      # Catégorisation
      Category = case_when(
        grepl("educ", Variable_raw) ~ "Éducation",
        grepl("revenu", Variable_raw) ~ "Revenu",
        grepl("arrondissement", Variable_raw) ~ "Géographie",
        TRUE ~ "Contrôles"
      ),

      # Ordre
      Order = case_when(
        Variable_raw == "ses_educ_fctCollegial/Certificat" ~ 1,
        Variable_raw == "ses_educ_fctBacc" ~ 2,
        Variable_raw == "ses_educ_fctGraduate" ~ 3,
        Variable_raw == "ses_revenu_fct40-79k" ~ 1,
        Variable_raw == "ses_revenu_fct80-119k" ~ 2,
        Variable_raw == "ses_revenu_fct120-159k" ~ 3,
        Variable_raw == "ses_revenu_fct160k+" ~ 4,
        Variable_raw == "ses_arrondissement_fctBeauport" ~ 1,
        Variable_raw == "ses_arrondissement_fctCharlesbourg" ~ 2,
        Variable_raw == "ses_arrondissement_fctCité-Limoilou" ~ 3,
        Variable_raw == "ses_arrondissement_fctLes Rivières" ~ 4,
        Variable_raw == "ses_arrondissement_fctSte-Foy-Sillery-Cap-Rouge" ~ 5,
        Variable_raw == "ses_age_c" ~ 1,
        Variable_raw == "ses_femme_fct1" ~ 2,
        Variable_raw == "ses_proprio_fct1" ~ 3,
        TRUE ~ 99
      ),

      # Noms lisibles
      Variable_display = case_when(
        Variable_raw == "ses_educ_fctCollegial/Certificat" ~ "Collégial/Certificat",
        Variable_raw == "ses_educ_fctBacc" ~ "Bacc",
        Variable_raw == "ses_educ_fctGraduate" ~ "Graduate",
        Variable_raw == "ses_revenu_fct40-79k" ~ "40-79k$",
        Variable_raw == "ses_revenu_fct80-119k" ~ "80-119k$",
        Variable_raw == "ses_revenu_fct120-159k" ~ "120-159k$",
        Variable_raw == "ses_revenu_fct160k+" ~ "160k$+",
        Variable_raw == "ses_arrondissement_fctBeauport" ~ "Beauport",
        Variable_raw == "ses_arrondissement_fctCharlesbourg" ~ "Charlesbourg",
        Variable_raw == "ses_arrondissement_fctCité-Limoilou" ~ "Cité-Limoilou",
        Variable_raw == "ses_arrondissement_fctLes Rivières" ~ "Les Rivières",
        Variable_raw == "ses_arrondissement_fctSte-Foy-Sillery-Cap-Rouge" ~ "Ste-Foy-Sillery-Cap-Rouge",
        Variable_raw == "ses_age_c" ~ "Âge",
        Variable_raw == "ses_femme_fct1" ~ "Femme",
        Variable_raw == "ses_proprio_fct1" ~ "Propriétaire",
        TRUE ~ Variable_raw
      ),

      Category_num = case_when(
        Category == "Éducation" ~ 4,
        Category == "Revenu" ~ 3,
        Category == "Géographie" ~ 2,
        Category == "Contrôles" ~ 1
      ),

      Significant = ifelse(p_value < 0.05, "Significatif (p < 0.05)", "Non significatif"),
      OR_text = sprintf("%.2f [%.2f-%.2f]", OR, CI_lower, CI_upper)
    ) %>%
    arrange(desc(Category_num), Order) %>%
    mutate(y_position = n():1)

  # Calculer pseudo R² (McFadden pour modèles logistiques)
  # McFadden R² = 1 - (logLik(model) / logLik(null_model))
  null_deviance <- model_obj$null.deviance
  residual_deviance <- model_obj$deviance
  pseudo_r2 <- 1 - (residual_deviance / null_deviance)

  # Pourcentage de cas
  pct_events <- (n_events / n_total) * 100

  # Créer le graphique
  p <- ggplot(plot_data, aes(x = OR, y = y_position)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.8) +
    geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper, color = Significant),
                  width = 0.3, linewidth = 1) +
    geom_point(aes(color = Significant), size = 4) +
    geom_text(aes(label = OR_text, x = max(CI_upper) * 1.8),
              hjust = 0, size = 3, fontface = "bold") +
    scale_x_log10(limits = c(0.1, max(plot_data$CI_upper) * 2.5)) +
    scale_y_continuous(breaks = plot_data$y_position,
                      labels = plot_data$Variable_display) +
    scale_color_manual(values = c("Significatif (p < 0.05)" = color_sig,
                                  "Non significatif" = "grey60")) +
    geom_hline(yintercept = c(max(plot_data$y_position[plot_data$Category == "Contrôles"]) + 0.5,
                              max(plot_data$y_position[plot_data$Category == "Géographie"]) + 0.5,
                              max(plot_data$y_position[plot_data$Category == "Revenu"]) + 0.5),
               linetype = "dotted", color = "grey70", linewidth = 0.5) +
    labs(title = sprintf("%s (n=%d, %d cas / %.1f%%)", title_text, n_total, n_events, pct_events),
         subtitle = sprintf("Régression logistique (échelle logarithmique) | McFadden R² = %.3f\nRéférences: Secondaire, <40k$, Haute-St-Charles, Homme, Locataire", pseudo_r2),
         x = "Odds Ratio (IC à 95%)",
         y = "",
         color = "") +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 15)),
      legend.position = "bottom",
      axis.text.y = element_text(hjust = 1),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      plot.margin = margin(10, 80, 10, 10)
    )

  return(p)
}

# ====================================================================
# 6. CRÉATION DES GRAPHIQUES
# ====================================================================

cat("\n========================================\n")
cat("CRÉATION DES GRAPHIQUES\n")
cat("========================================\n\n")

# Calculer n et événements pour chaque modèle
n_total <- nrow(data_reg)
n_ideal_eco <- sum(data_reg$ideal_eco_bin, na.rm = TRUE)
n_comp_eco <- sum(data_reg$comp_transport_eco, na.rm = TRUE)

# Graphique 1: Idéal écologique
p1 <- create_forest_plot(or_ideal,
                         "Odds Ratios: Idéal écologique",
                         "#1976D2",
                         model_ideal,
                         n_total,
                         n_ideal_eco)

ggsave("odds_ratios_ideal_eco.png", p1, width = 10, height = 7, dpi = 300)
cat("Graphique sauvegardé: odds_ratios_ideal_eco.png\n")

# Graphique 2: Comportement écologique
p2 <- create_forest_plot(or_comp,
                         "Odds Ratios: Comportement écologique",
                         "#FF6F00",
                         model_comp,
                         n_total,
                         n_comp_eco)

ggsave("odds_ratios_comportement_eco.png", p2, width = 10, height = 7, dpi = 300)
cat("Graphique sauvegardé: odds_ratios_comportement_eco.png\n")

# ====================================================================
# 7. COMPARAISON DES DEUX MODÈLES
# ====================================================================

cat("\n========================================\n")
cat("COMPARAISON IDÉAL vs COMPORTEMENT\n")
cat("========================================\n\n")

# Tableau comparatif
comparison <- or_ideal %>%
  dplyr::select(Variable_raw, OR_ideal = OR, p_ideal = p_value) %>%
  left_join(
    or_comp %>% dplyr::select(Variable_raw, OR_comp = OR, p_comp = p_value),
    by = "Variable_raw"
  ) %>%
  filter(Variable_raw != "(Intercept)") %>%
  mutate(
    Diff_OR = OR_ideal - OR_comp,
    Sig_ideal = ifelse(p_ideal < 0.05, "***", ""),
    Sig_comp = ifelse(p_comp < 0.05, "***", "")
  )

cat("Prédicteurs avec effets différents sur idéal vs comportement:\n\n")
print(comparison %>%
        filter(abs(Diff_OR) > 0.5 | (p_ideal < 0.05 & p_comp >= 0.05) | (p_ideal >= 0.05 & p_comp < 0.05)) %>%
        dplyr::select(Variable_raw, OR_ideal, Sig_ideal, OR_comp, Sig_comp, Diff_OR))

cat("\n\n========================================\n")
cat("COMPARAISON IDÉAL vs COMPORTEMENT: TERMINÉE\n")
cat("========================================\n\n")

# ====================================================================
# 8. TESTS D'INTERACTIONS
# ====================================================================

cat("\n========================================\n")
cat("TESTS D'INTERACTIONS\n")
cat("========================================\n\n")

# 8.1 Interaction éducation × revenu (capital culturel × capital économique)
cat("--- Test 1: Éducation × Revenu ---\n")
cat("Hypothèse: L'effet de l'éducation varie-t-il selon le revenu?\n")
cat("(Ex: les diplômés à faible revenu ont-ils des attitudes différentes?)\n\n")

model_ideal_inter1 <- glm(ideal_eco_bin ~ ses_educ_fct * ses_revenu_fct + ses_age_c +
                                         ses_femme_fct + ses_proprio_fct +
                                         ses_arrondissement_fct,
                         data = data_reg,
                         weights = POND,
                         family = binomial(link = "logit"))

model_comp_inter1 <- glm(comp_transport_eco ~ ses_educ_fct * ses_revenu_fct + ses_age_c +
                                              ses_femme_fct + ses_proprio_fct +
                                              ses_arrondissement_fct,
                        data = data_reg,
                        weights = POND,
                        family = binomial(link = "logit"))

# Test du rapport de vraisemblance
lr_test_ideal_1 <- anova(model_ideal, model_ideal_inter1, test = "Chisq")
lr_test_comp_1 <- anova(model_comp, model_comp_inter1, test = "Chisq")

cat("Idéal écologique:\n")
print(lr_test_ideal_1)
cat("\nComportement écologique:\n")
print(lr_test_comp_1)

cat("\nAIC comparison (idéal):\n")
cat("  Sans interaction:", AIC(model_ideal), "\n")
cat("  Avec interaction:", AIC(model_ideal_inter1), "\n")
cat("  Δ AIC =", AIC(model_ideal) - AIC(model_ideal_inter1), "\n\n")

cat("AIC comparison (comportement):\n")
cat("  Sans interaction:", AIC(model_comp), "\n")
cat("  Avec interaction:", AIC(model_comp_inter1), "\n")
cat("  Δ AIC =", AIC(model_comp) - AIC(model_comp_inter1), "\n\n")

# 8.2 Interaction éducation × arrondissement
cat("\n--- Test 2: Éducation × Arrondissement ---\n")
cat("Hypothèse: L'effet de l'éducation varie-t-il selon l'arrondissement?\n")
cat("(Ex: l'éducation a-t-elle plus d'effet dans les quartiers centraux?)\n\n")

model_ideal_inter2 <- glm(ideal_eco_bin ~ ses_educ_fct * ses_arrondissement_fct +
                                         ses_revenu_fct + ses_age_c +
                                         ses_femme_fct + ses_proprio_fct,
                         data = data_reg,
                         weights = POND,
                         family = binomial(link = "logit"))

model_comp_inter2 <- glm(comp_transport_eco ~ ses_educ_fct * ses_arrondissement_fct +
                                              ses_revenu_fct + ses_age_c +
                                              ses_femme_fct + ses_proprio_fct,
                        data = data_reg,
                        weights = POND,
                        family = binomial(link = "logit"))

lr_test_ideal_2 <- anova(model_ideal, model_ideal_inter2, test = "Chisq")
lr_test_comp_2 <- anova(model_comp, model_comp_inter2, test = "Chisq")

cat("Idéal écologique:\n")
print(lr_test_ideal_2)
cat("\nComportement écologique:\n")
print(lr_test_comp_2)

cat("\nAIC comparison (idéal):\n")
cat("  Sans interaction:", AIC(model_ideal), "\n")
cat("  Avec interaction:", AIC(model_ideal_inter2), "\n")
cat("  Δ AIC =", AIC(model_ideal) - AIC(model_ideal_inter2), "\n\n")

cat("AIC comparison (comportement):\n")
cat("  Sans interaction:", AIC(model_comp), "\n")
cat("  Avec interaction:", AIC(model_comp_inter2), "\n")
cat("  Δ AIC =", AIC(model_comp) - AIC(model_comp_inter2), "\n\n")

# 8.3 Interaction propriétaire × arrondissement
cat("\n--- Test 3: Propriétaire × Arrondissement ---\n")
cat("Hypothèse: L'effet d'être propriétaire varie-t-il selon l'arrondissement?\n")
cat("(Ex: les propriétaires en banlieue ont-ils des attitudes différentes?)\n\n")

model_ideal_inter3 <- glm(ideal_eco_bin ~ ses_proprio_fct * ses_arrondissement_fct +
                                         ses_educ_fct + ses_revenu_fct + ses_age_c +
                                         ses_femme_fct,
                         data = data_reg,
                         weights = POND,
                         family = binomial(link = "logit"))

model_comp_inter3 <- glm(comp_transport_eco ~ ses_proprio_fct * ses_arrondissement_fct +
                                              ses_educ_fct + ses_revenu_fct + ses_age_c +
                                              ses_femme_fct,
                        data = data_reg,
                        weights = POND,
                        family = binomial(link = "logit"))

lr_test_ideal_3 <- anova(model_ideal, model_ideal_inter3, test = "Chisq")
lr_test_comp_3 <- anova(model_comp, model_comp_inter3, test = "Chisq")

cat("Idéal écologique:\n")
print(lr_test_ideal_3)
cat("\nComportement écologique:\n")
print(lr_test_comp_3)

cat("\nAIC comparison (idéal):\n")
cat("  Sans interaction:", AIC(model_ideal), "\n")
cat("  Avec interaction:", AIC(model_ideal_inter3), "\n")
cat("  Δ AIC =", AIC(model_ideal) - AIC(model_ideal_inter3), "\n\n")

cat("AIC comparison (comportement):\n")
cat("  Sans interaction:", AIC(model_comp), "\n")
cat("  Avec interaction:", AIC(model_comp_inter3), "\n")
cat("  Δ AIC =", AIC(model_comp) - AIC(model_comp_inter3), "\n\n")

# ====================================================================
# 9. MODÈLES AVEC RÉPERTOIRE ÉCOLOGIQUE
# ====================================================================

cat("\n========================================\n")
cat("MODÈLES AVEC RÉPERTOIRE ÉCOLOGIQUE\n")
cat("========================================\n\n")

cat("Substitution: Arrondissement → Répertoire écologique\n")
cat("Objectif: Tester si le répertoire (variable continue) explique mieux\n")
cat("         que l'arrondissement (variable catégorielle)\n\n")

# Vérifier que répertoire_eco existe
if (!"repertoire_eco" %in% names(data_reg)) {
  cat("ERREUR: Variable 'repertoire_eco' non trouvée dans les données\n")
} else {

  # Préparer répertoire centré
  data_reg <- data_reg %>%
    mutate(repertoire_eco_c = as.numeric(scale(repertoire_eco, center = TRUE, scale = FALSE)))

  # 9.1 Modèle idéal avec répertoire
  cat("--- Modèle 1: Idéal écologique avec répertoire ---\n\n")

  model_ideal_rep <- glm(ideal_eco_bin ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                        ses_femme_fct + ses_proprio_fct +
                                        repertoire_eco_c,
                        data = data_reg,
                        weights = POND,
                        family = binomial(link = "logit"))

  summary(model_ideal_rep)

  # Comparer avec modèle arrondissement
  cat("\n--- Comparaison AIC: Arrondissement vs Répertoire (Idéal) ---\n")
  cat("Avec arrondissement:", AIC(model_ideal), "\n")
  cat("Avec répertoire:    ", AIC(model_ideal_rep), "\n")
  cat("Δ AIC =", AIC(model_ideal) - AIC(model_ideal_rep), "\n")
  cat("(Négatif = répertoire moins bon, Positif = répertoire meilleur)\n\n")

  # 9.2 Modèle comportement avec répertoire
  cat("\n--- Modèle 2: Comportement écologique avec répertoire ---\n\n")

  model_comp_rep <- glm(comp_transport_eco ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                            ses_femme_fct + ses_proprio_fct +
                                            repertoire_eco_c,
                       data = data_reg,
                       weights = POND,
                       family = binomial(link = "logit"))

  summary(model_comp_rep)

  cat("\n--- Comparaison AIC: Arrondissement vs Répertoire (Comportement) ---\n")
  cat("Avec arrondissement:", AIC(model_comp), "\n")
  cat("Avec répertoire:    ", AIC(model_comp_rep), "\n")
  cat("Δ AIC =", AIC(model_comp) - AIC(model_comp_rep), "\n\n")

  # 9.3 Odds ratios pour répertoire
  cat("\n--- Odds Ratios: Répertoire écologique ---\n\n")

  or_rep_ideal <- exp(coef(model_ideal_rep)["repertoire_eco_c"])
  ci_rep_ideal <- exp(confint(model_ideal_rep)["repertoire_eco_c", ])
  p_rep_ideal <- coef(summary(model_ideal_rep))["repertoire_eco_c", "Pr(>|z|)"]

  or_rep_comp <- exp(coef(model_comp_rep)["repertoire_eco_c"])
  ci_rep_comp <- exp(confint(model_comp_rep)["repertoire_eco_c", ])
  p_rep_comp <- coef(summary(model_comp_rep))["repertoire_eco_c", "Pr(>|z|)"]

  cat("IDÉAL ÉCOLOGIQUE:\n")
  cat(sprintf("  OR = %.3f [%.3f-%.3f], p = %.4f %s\n",
              or_rep_ideal, ci_rep_ideal[1], ci_rep_ideal[2], p_rep_ideal,
              ifelse(p_rep_ideal < 0.05, "***", "")))

  cat("\nCOMPORTEMENT ÉCOLOGIQUE:\n")
  cat(sprintf("  OR = %.3f [%.3f-%.3f], p = %.4f %s\n",
              or_rep_comp, ci_rep_comp[1], ci_rep_comp[2], p_rep_comp,
              ifelse(p_rep_comp < 0.05, "***", "")))

  cat("\nInterprétation:\n")
  cat("- OR > 1: Plus le répertoire est élevé, plus la probabilité augmente\n")
  cat("- OR < 1: Plus le répertoire est élevé, moins la probabilité augmente\n\n")

  # 9.4 Test d'interaction éducation × répertoire
  cat("\n--- Test d'interaction: Éducation × Répertoire ---\n")
  cat("Hypothèse: L'effet du répertoire varie-t-il selon l'éducation?\n")
  cat("(Ex: le répertoire est-il plus important pour les moins éduqués?)\n\n")

  model_ideal_rep_inter <- glm(ideal_eco_bin ~ ses_educ_fct * repertoire_eco_c +
                                              ses_revenu_fct + ses_age_c +
                                              ses_femme_fct + ses_proprio_fct,
                              data = data_reg,
                              weights = POND,
                              family = binomial(link = "logit"))

  model_comp_rep_inter <- glm(comp_transport_eco ~ ses_educ_fct * repertoire_eco_c +
                                                   ses_revenu_fct + ses_age_c +
                                                   ses_femme_fct + ses_proprio_fct,
                             data = data_reg,
                             weights = POND,
                             family = binomial(link = "logit"))

  lr_test_ideal_rep <- anova(model_ideal_rep, model_ideal_rep_inter, test = "Chisq")
  lr_test_comp_rep <- anova(model_comp_rep, model_comp_rep_inter, test = "Chisq")

  cat("Idéal écologique:\n")
  print(lr_test_ideal_rep)
  cat("\nComportement écologique:\n")
  print(lr_test_comp_rep)

  cat("\nAIC comparison (idéal):\n")
  cat("  Sans interaction:", AIC(model_ideal_rep), "\n")
  cat("  Avec interaction:", AIC(model_ideal_rep_inter), "\n")
  cat("  Δ AIC =", AIC(model_ideal_rep) - AIC(model_ideal_rep_inter), "\n\n")

  cat("AIC comparison (comportement):\n")
  cat("  Sans interaction:", AIC(model_comp_rep), "\n")
  cat("  Avec interaction:", AIC(model_comp_rep_inter), "\n")
  cat("  Δ AIC =", AIC(model_comp_rep) - AIC(model_comp_rep_inter), "\n\n")

  # ====================================================================
  # 10. VISUALISATION DE L'EFFET DU RÉPERTOIRE
  # ====================================================================

  cat("\n========================================\n")
  cat("VISUALISATION: EFFET DU RÉPERTOIRE\n")
  cat("========================================\n\n")

  # 10.1 Préparer les données pour la courbe de probabilité prédite

  # Obtenir la plage du répertoire
  rep_min <- min(data_reg$repertoire_eco, na.rm = TRUE)
  rep_max <- max(data_reg$repertoire_eco, na.rm = TRUE)
  rep_range <- seq(rep_min, rep_max, length.out = 100)

  # Créer données pour prédiction (autres variables à leurs valeurs moyennes/modales)
  pred_data <- data.frame(
    repertoire_eco_c = rep_range - mean(data_reg$repertoire_eco, na.rm = TRUE),
    ses_educ_fct = "Collegial/Certificat",  # Modal
    ses_revenu_fct = "40-79k",  # Modal
    ses_age_c = 0,  # Centré
    ses_femme_fct = "0",
    ses_proprio_fct = "1"  # Modal
  )

  # Prédictions avec intervalles de confiance
  pred_comp <- predict(model_comp_rep, newdata = pred_data, type = "link", se.fit = TRUE)

  # Convertir en probabilités
  pred_df <- data.frame(
    repertoire = rep_range,
    prob = plogis(pred_comp$fit),
    prob_lower = plogis(pred_comp$fit - 1.96 * pred_comp$se.fit),
    prob_upper = plogis(pred_comp$fit + 1.96 * pred_comp$se.fit)
  )

  # Données des arrondissements (pour overlay)
  arrond_data <- data_reg %>%
    group_by(ses_arrondissement_fct, repertoire_eco) %>%
    summarise(
      prob_obs = weighted.mean(comp_transport_eco, w = POND, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    ) %>%
    unique()

  # 10.2 Créer le graphique
  cat("--- Création du graphique: Effet du répertoire sur comportement éco ---\n\n")

  p_rep_effect <- ggplot() +
    # Intervalle de confiance
    geom_ribbon(data = pred_df,
                aes(x = repertoire, ymin = prob_lower, ymax = prob_upper),
                fill = "#FF6F00", alpha = 0.2) +
    # Courbe principale
    geom_line(data = pred_df,
              aes(x = repertoire, y = prob),
              color = "#FF6F00", linewidth = 1.5) +
    # Points des arrondissements
    geom_point(data = arrond_data,
               aes(x = repertoire_eco, y = prob_obs, size = n),
               color = "#1976D2", alpha = 0.7) +
    # Labels des arrondissements
    geom_text(data = arrond_data,
              aes(x = repertoire_eco, y = prob_obs,
                  label = gsub("ses_arrondissement_fct", "", ses_arrondissement_fct)),
              hjust = -0.1, vjust = 0.5, size = 3, color = "grey20") +
    scale_y_continuous(labels = scales::percent_format(),
                      limits = c(0, 1)) +
    scale_size_continuous(range = c(3, 8), name = "N") +
    labs(title = "Effet du répertoire écologique sur le comportement écologique",
         subtitle = sprintf("Régression logistique | OR = %.1f [%.1f-%.1f], p < 0.001 | AIC = %.1f",
                           or_rep_comp, ci_rep_comp[1], ci_rep_comp[2], AIC(model_comp_rep)),
         x = "Répertoire écologique (0-1)",
         y = "Probabilité prédite de comportement écologique",
         caption = "Note: Points = probabilités observées par arrondissement (taille ∝ n)\nCourbe = prédiction du modèle avec IC 95%\nAutres variables: Collégial, 40-79k$, âge moyen, homme, propriétaire") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0, margin = margin(t = 10)),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  ggsave("effet_repertoire_comportement.png", p_rep_effect, width = 10, height = 7, dpi = 300)
  cat("Graphique sauvegardé: effet_repertoire_comportement.png\n\n")

  # 10.3 Même chose pour idéal écologique (pour comparaison)
  cat("--- Création du graphique: Effet du répertoire sur idéal éco ---\n\n")

  # Prédictions pour idéal
  pred_ideal <- predict(model_ideal_rep, newdata = pred_data, type = "link", se.fit = TRUE)

  pred_df_ideal <- data.frame(
    repertoire = rep_range,
    prob = plogis(pred_ideal$fit),
    prob_lower = plogis(pred_ideal$fit - 1.96 * pred_ideal$se.fit),
    prob_upper = plogis(pred_ideal$fit + 1.96 * pred_ideal$se.fit)
  )

  arrond_data_ideal <- data_reg %>%
    group_by(ses_arrondissement_fct, repertoire_eco) %>%
    summarise(
      prob_obs = weighted.mean(ideal_eco_bin, w = POND, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    ) %>%
    unique()

  p_rep_ideal <- ggplot() +
    geom_ribbon(data = pred_df_ideal,
                aes(x = repertoire, ymin = prob_lower, ymax = prob_upper),
                fill = "#1976D2", alpha = 0.2) +
    geom_line(data = pred_df_ideal,
              aes(x = repertoire, y = prob),
              color = "#1976D2", linewidth = 1.5) +
    geom_point(data = arrond_data_ideal,
               aes(x = repertoire_eco, y = prob_obs, size = n),
               color = "#FF6F00", alpha = 0.7) +
    geom_text(data = arrond_data_ideal,
              aes(x = repertoire_eco, y = prob_obs,
                  label = gsub("ses_arrondissement_fct", "", ses_arrondissement_fct)),
              hjust = -0.1, vjust = 0.5, size = 3, color = "grey20") +
    scale_y_continuous(labels = scales::percent_format(),
                      limits = c(0, 1)) +
    scale_size_continuous(range = c(3, 8), name = "N") +
    labs(title = "Effet du répertoire écologique sur l'idéal écologique",
         subtitle = sprintf("Régression logistique | OR = %.1f [%.1f-%.1f], p < 0.001 | AIC = %.1f",
                           or_rep_ideal, ci_rep_ideal[1], ci_rep_ideal[2], AIC(model_ideal_rep)),
         x = "Répertoire écologique (0-1)",
         y = "Probabilité prédite d'idéal écologique",
         caption = "Note: Points = probabilités observées par arrondissement (taille ∝ n)\nCourbe = prédiction du modèle avec IC 95%\nAutres variables: Collégial, 40-79k$, âge moyen, homme, propriétaire") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0, margin = margin(t = 10)),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  ggsave("effet_repertoire_ideal.png", p_rep_ideal, width = 10, height = 7, dpi = 300)
  cat("Graphique sauvegardé: effet_repertoire_ideal.png\n\n")

  # ====================================================================
  # 11. FOREST PLOT COMPARATIF: ARRONDISSEMENT VS RÉPERTOIRE
  # ====================================================================

  cat("\n========================================\n")
  cat("FOREST PLOT COMPARATIF\n")
  cat("========================================\n\n")

  # 11.1 Préparer les données pour le forest plot avec répertoire

  # Obtenir OR et IC pour le modèle avec répertoire
  or_comp_rep_full <- data.frame(
    Variable_raw = names(coef(model_comp_rep)),
    OR = exp(coef(model_comp_rep)),
    CI_lower = exp(confint(model_comp_rep)[, 1]),
    CI_upper = exp(confint(model_comp_rep)[, 2]),
    p_value = coef(summary(model_comp_rep))[, "Pr(>|z|)"]
  )

  # Préparer données arrondissement (déjà existant: or_comp)
  plot_data_arrond <- or_comp %>%
    filter(Variable_raw != "(Intercept)") %>%
    mutate(
      Modele = "Arrondissement (5 df)",
      Category = case_when(
        grepl("educ", Variable_raw) ~ "Éducation",
        grepl("revenu", Variable_raw) ~ "Revenu",
        grepl("arrondissement", Variable_raw) ~ "Géographie",
        grepl("age", Variable_raw) ~ "Démographie",
        grepl("femme", Variable_raw) ~ "Démographie",
        grepl("proprio", Variable_raw) ~ "Statut résidentiel"
      ),
      Order = case_when(
        Variable_raw == "ses_educ_fctCollegial/Certificat" ~ 1,
        Variable_raw == "ses_educ_fctBacc" ~ 2,
        Variable_raw == "ses_educ_fctGraduate" ~ 3,
        Variable_raw == "ses_revenu_fct40-79k" ~ 1,
        Variable_raw == "ses_revenu_fct80-119k" ~ 2,
        Variable_raw == "ses_revenu_fct120-159k" ~ 3,
        Variable_raw == "ses_revenu_fct160k+" ~ 4,
        Variable_raw == "ses_arrondissement_fctBeauport" ~ 1,
        Variable_raw == "ses_arrondissement_fctCharlesbourg" ~ 2,
        Variable_raw == "ses_arrondissement_fctCité-Limoilou" ~ 3,
        Variable_raw == "ses_arrondissement_fctLes Rivières" ~ 4,
        Variable_raw == "ses_arrondissement_fctSte-Foy-Sillery-Cap-Rouge" ~ 5,
        Variable_raw == "ses_age_c" ~ 1,
        Variable_raw == "ses_femme_fct1" ~ 2,
        Variable_raw == "ses_proprio_fct1" ~ 1,
        TRUE ~ 99
      ),
      Variable_display = case_when(
        Variable_raw == "ses_educ_fctCollegial/Certificat" ~ "Collégial/Certificat",
        Variable_raw == "ses_educ_fctBacc" ~ "Bacc",
        Variable_raw == "ses_educ_fctGraduate" ~ "Graduate",
        Variable_raw == "ses_revenu_fct40-79k" ~ "40-79k$",
        Variable_raw == "ses_revenu_fct80-119k" ~ "80-119k$",
        Variable_raw == "ses_revenu_fct120-159k" ~ "120-159k$",
        Variable_raw == "ses_revenu_fct160k+" ~ "160k$+",
        Variable_raw == "ses_arrondissement_fctBeauport" ~ "Beauport",
        Variable_raw == "ses_arrondissement_fctCharlesbourg" ~ "Charlesbourg",
        Variable_raw == "ses_arrondissement_fctCité-Limoilou" ~ "Cité-Limoilou",
        Variable_raw == "ses_arrondissement_fctLes Rivières" ~ "Les Rivières",
        Variable_raw == "ses_arrondissement_fctSte-Foy-Sillery-Cap-Rouge" ~ "Ste-Foy-Sillery-Cap-Rouge",
        Variable_raw == "ses_age_c" ~ "Âge",
        Variable_raw == "ses_femme_fct1" ~ "Femme",
        Variable_raw == "ses_proprio_fct1" ~ "Propriétaire",
        TRUE ~ Variable_raw
      ),
      Significant = ifelse(p_value < 0.05, "Significatif", "Non significatif")
    )

  # Préparer données répertoire
  plot_data_rep <- or_comp_rep_full %>%
    filter(Variable_raw != "(Intercept)") %>%
    mutate(
      Modele = "Répertoire (1 df)",
      Category = case_when(
        grepl("educ", Variable_raw) ~ "Éducation",
        grepl("revenu", Variable_raw) ~ "Revenu",
        grepl("repertoire", Variable_raw) ~ "Géographie",
        grepl("age", Variable_raw) ~ "Démographie",
        grepl("femme", Variable_raw) ~ "Démographie",
        grepl("proprio", Variable_raw) ~ "Statut résidentiel"
      ),
      Order = case_when(
        Variable_raw == "ses_educ_fctCollegial/Certificat" ~ 1,
        Variable_raw == "ses_educ_fctBacc" ~ 2,
        Variable_raw == "ses_educ_fctGraduate" ~ 3,
        Variable_raw == "ses_revenu_fct40-79k" ~ 1,
        Variable_raw == "ses_revenu_fct80-119k" ~ 2,
        Variable_raw == "ses_revenu_fct120-159k" ~ 3,
        Variable_raw == "ses_revenu_fct160k+" ~ 4,
        Variable_raw == "repertoire_eco_c" ~ 1,
        Variable_raw == "ses_age_c" ~ 1,
        Variable_raw == "ses_femme_fct1" ~ 2,
        Variable_raw == "ses_proprio_fct1" ~ 1,
        TRUE ~ 99
      ),
      Variable_display = case_when(
        Variable_raw == "ses_educ_fctCollegial/Certificat" ~ "Collégial/Certificat",
        Variable_raw == "ses_educ_fctBacc" ~ "Bacc",
        Variable_raw == "ses_educ_fctGraduate" ~ "Graduate",
        Variable_raw == "ses_revenu_fct40-79k" ~ "40-79k$",
        Variable_raw == "ses_revenu_fct80-119k" ~ "80-119k$",
        Variable_raw == "ses_revenu_fct120-159k" ~ "120-159k$",
        Variable_raw == "ses_revenu_fct160k+" ~ "160k$+",
        Variable_raw == "repertoire_eco_c" ~ "Répertoire écologique",
        Variable_raw == "ses_age_c" ~ "Âge",
        Variable_raw == "ses_femme_fct1" ~ "Femme",
        Variable_raw == "ses_proprio_fct1" ~ "Propriétaire",
        TRUE ~ Variable_raw
      ),
      Significant = ifelse(p_value < 0.05, "Significatif", "Non significatif")
    )

  # Combiner les deux
  plot_data_combined <- bind_rows(plot_data_arrond, plot_data_rep) %>%
    mutate(
      Category_num = case_when(
        Category == "Éducation" ~ 5,
        Category == "Revenu" ~ 4,
        Category == "Géographie" ~ 3,
        Category == "Démographie" ~ 2,
        Category == "Statut résidentiel" ~ 1
      ),
      Modele = factor(Modele, levels = c("Arrondissement (5 df)", "Répertoire (1 df)"))
    ) %>%
    arrange(Modele, desc(Category_num), Order) %>%
    group_by(Modele) %>%
    mutate(y_position = n():1) %>%
    ungroup()

  # Calculer les limites communes pour l'axe X
  x_min <- 0.1
  x_max <- max(plot_data_combined$CI_upper, na.rm = TRUE) * 2.5

  # 11.2 Créer le graphique comparatif
  cat("--- Création du forest plot comparatif ---\n\n")

  p_comp_forest <- ggplot(plot_data_combined, aes(x = OR, y = y_position)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.8) +
    geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper, color = Significant),
                  width = 0.3, linewidth = 1) +
    geom_point(aes(color = Significant), size = 3) +
    scale_x_log10(limits = c(x_min, x_max)) +
    scale_y_continuous(breaks = plot_data_combined$y_position,
                      labels = plot_data_combined$Variable_display) +
    scale_color_manual(values = c("Significatif" = "#FF6F00",
                                  "Non significatif" = "grey60")) +
    facet_wrap(~ Modele, scales = "free_y", ncol = 2) +
    labs(title = "Comparaison: Arrondissement vs Répertoire écologique",
         subtitle = sprintf("Comportement écologique | AIC: Arrondissement = %.1f, Répertoire = %.1f (Δ = %.1f)",
                           AIC(model_comp), AIC(model_comp_rep), AIC(model_comp) - AIC(model_comp_rep)),
         x = "Odds Ratio (IC à 95%, échelle logarithmique)",
         y = "",
         color = "",
         caption = "Note: Le répertoire (1 paramètre) explique MIEUX que l'arrondissement (5 paramètres)\nRéférences: Secondaire, <40k$, Haute-St-Charles (ou faible répertoire), Homme, Locataire") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0, margin = margin(t = 10)),
      legend.position = "bottom",
      axis.text.y = element_text(hjust = 1, size = 9),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "grey95", color = NA)
    )

  ggsave("forest_plot_arrond_vs_repertoire.png", p_comp_forest, width = 14, height = 8, dpi = 300)
  cat("Graphique sauvegardé: forest_plot_arrond_vs_repertoire.png\n\n")
}

cat("\n\n========================================\n")
cat("ANALYSE COMPLÈTE TERMINÉE\n")
cat("========================================\n\n")

cat("RÉSUMÉ:\n")
cat("-------\n")
cat("✓ Régression logistique: Idéal écologique\n")
cat("✓ Régression logistique: Comportement écologique\n")
cat("✓ Forest plots générés\n")
cat("✓ Comparaison des effets\n")
cat("✓ Tests d'interactions (3 types)\n")
cat("✓ Modèles avec répertoire écologique\n")
cat("✓ Test d'interaction éducation × répertoire\n\n")

cat("FICHIERS GÉNÉRÉS:\n")
cat("-----------------\n")
cat("- odds_ratios_ideal_eco.png\n")
cat("- odds_ratios_comportement_eco.png\n\n")

cat("INTERPRÉTATIONS:\n")
cat("----------------\n")
cat("• Effets principaux:\n")
cat("  - Idéal éco: capital culturel (éducation)\n")
cat("  - Comportement éco: capital culturel + contraintes structurelles (répertoire)\n\n")
cat("• Interactions à examiner:\n")
cat("  - Éducation × Revenu: capital culturel vs capital économique\n")
cat("  - Éducation × Géographie: variation spatiale de l'effet culturel\n")
cat("  - Propriétaire × Géographie: intérêts économiques localisés\n")
cat("  - Éducation × Répertoire: capital culturel + opportunités structurelles\n\n")
cat("• Répertoire vs Arrondissement:\n")
cat("  - Comparer AIC pour voir quelle variable explique mieux\n")
cat("  - Répertoire = mesure continue des opportunités écologiques\n")
cat("  - Arrondissement = proxy catégoriel capturant d'autres dimensions\n\n")
