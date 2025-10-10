library(dplyr)
library(ggplot2)
library(MASS)  # Pour polr (regression ordinale)
library(broom)
library(marginaleffects)  # Pour probabilites predites

# ====================================================================
# 1. CHARGEMENT DES DONNEES
# ====================================================================

data_culture <- readRDS("_SharedFolder_culture_carbone/data/data_culture.rds")

cat("\n========================================\n")
cat("ANALYSE DE REGRESSION\n")
cat("VD: Position sur le tramway (op_tram_num)\n")
cat("========================================\n\n")

# ====================================================================
# 2. PREPARATION DES DONNEES
# ====================================================================

# La variable op_tram_num est ordinale avec 4 niveaux: 0, 0.33, 0.67, 1
# On va creer une version factorielle pour la regression ordinale
data_reg <- data_culture %>%
  filter(!is.na(op_tram_num)) %>%
  mutate(
    # Variable dependante ordinale
    op_tram_ord = factor(op_tram_num,
                        levels = c(0, 0.33, 0.67, 1),
                        labels = c("Oppose", "Plutot oppose",
                                  "Plutot favorable", "Favorable"),
                        ordered = TRUE),

    # Variable binaire pour regression logistique (oppose vs favorable)
    op_tram_bin = ifelse(op_tram_num >= 0.67, 1, 0),

    # Variables independantes - SES
    # Education (reference: Secondaire)
    ses_educ_fct = relevel(factor(ses_educ), ref = "Secondaire"),

    # Revenu (reference: <40k - le plus faible)
    ses_revenu_fct = relevel(factor(ses_revenu_grouped), ref = "<40k"),

    # Age (continue, centree pour interpretation)
    ses_age_c = scale(ses_age, center = TRUE, scale = FALSE),

    # Sexe (0 = homme, 1 = femme)
    ses_femme_fct = factor(ses_femme),

    # Statut proprietaire (0 = locataire, 1 = proprietaire)
    ses_proprio_fct = factor(ses_proprio),

    # Arrondissement (reference: Haute-St-Charles - le plus car-dependent)
    ses_arrondissement_fct = relevel(factor(ses_arrondissement),
                                    ref = "Haute-St-Charles")
  )

# Statistiques descriptives
cat("Distribution de la VD:\n")
print(table(data_reg$op_tram_ord))
cat("\n")

# ====================================================================
# 3. MODeLE 1: Regression logistique ordinale (proportional odds)
# ====================================================================

cat("\n========================================\n")
cat("MODeLE 1: ReGRESSION ORDINALE\n")
cat("========================================\n\n")

# Modele complet avec tous les controles SES
model_ord <- polr(op_tram_ord ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                ses_femme_fct + ses_proprio_fct +
                                ses_arrondissement_fct,
                  data = data_reg,
                  Hess = TRUE)

# Resume du modele
summary(model_ord)

# Test de significativite globale
cat("\n--- Tests de significativite (Wald) ---\n")
coef_table <- coef(summary(model_ord))
p_values_all <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2
results <- cbind(coef_table, "p-value" = p_values_all)
print(results)

# Garder seulement les p-values des coefficients (pas les intercepts)
n_coef <- nrow(coef_table) - 3  # 3 intercepts dans le modele ordinal
p_values <- p_values_all[1:n_coef]

# ====================================================================
# 4. ODDS RATIOS ET INTERVALLES DE CONFIANCE
# ====================================================================

cat("\n========================================\n")
cat("ODDS RATIOS (Regression ordinale)\n")
cat("========================================\n\n")

# Calculer les odds ratios
or_ord <- exp(coef(model_ord))
ci_ord_full <- confint(model_ord)

# Extraire seulement les CI des coefficients (en excluant les intercepts)
# Les intercepts sont a la fin dans confint()
coef_names <- names(or_ord)
ci_ord <- ci_ord_full[coef_names, ]
ci_ord <- exp(ci_ord)

# Creer un tableau des OR avec IC e 95%
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
# 5. MODeLE 2: Regression logistique binaire (oppose vs favorable)
# ====================================================================

cat("\n========================================\n")
cat("MODeLE 2: ReGRESSION LOGISTIQUE BINAIRE\n")
cat("VD: Favorable (e0.67) vs Oppose (<0.67)\n")
cat("========================================\n\n")

model_bin <- glm(op_tram_bin ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                               ses_femme_fct + ses_proprio_fct +
                               ses_arrondissement_fct,
                data = data_reg,
                family = binomial(link = "logit"))

summary(model_bin)

# Odds ratios pour modele binaire
cat("\n--- Odds Ratios (Regression binaire) ---\n")
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
# 6. PROBABILITeS PReDITES
# ====================================================================

cat("\n========================================\n")
cat("PROBABILITeS PReDITES\n")
cat("========================================\n\n")

# 6.1 Probabilites par niveau d'education (autres variables a leur valeur modale/moyenne)
cat("--- Par niveau d'education ---\n")

pred_educ <- predictions(
  model_ord,
  newdata = datagrid(
    ses_educ_fct = unique(data_reg$ses_educ_fct),
    ses_revenu_fct = "40-79k",  # Revenu modal
    ses_age_c = 0,  # Age moyen (centré)
    ses_femme_fct = "0",  # Homme (reference)
    ses_proprio_fct = "1",  # Proprietaire (modal)
    ses_arrondissement_fct = "Cite-Limoilou"  # Arrondissement modal
  ),
  type = "probs"
)

print(as.data.frame(pred_educ))

# 6.2 Probabilites par arrondissement (autres variables a leur valeur modale/moyenne)
cat("\n--- Par arrondissement ---\n")

pred_arrond <- predictions(
  model_ord,
  newdata = datagrid(
    ses_educ_fct = "Collegial/Certificat",  # Education modale
    ses_revenu_fct = "40-79k",  # Revenu modal
    ses_age_c = 0,  # Age moyen (centré)
    ses_femme_fct = "0",  # Homme (reference)
    ses_proprio_fct = "1",  # Proprietaire (modal)
    ses_arrondissement_fct = unique(data_reg$ses_arrondissement_fct)
  ),
  type = "probs"
)

print(as.data.frame(pred_arrond))

# ====================================================================
# 7. GRAPHIQUES
# ====================================================================

cat("\n========================================\n")
cat("CReATION DES GRAPHIQUES\n")
cat("========================================\n\n")

# 7.1 Forest plot des odds ratios
or_plot_data <- or_table %>%
  filter(!grepl("Intercept", Variable)) %>%
  mutate(
    Variable = gsub("ses_educ_fct", "education: ", Variable),
    Variable = gsub("ses_arrondissement_fct", "", Variable),
    Significant = ifelse(p_value < 0.05, "Sig (p<0.05)", "NS")
  )

plot_or <- ggplot(or_plot_data, aes(x = OR, y = reorder(Variable, OR))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_point(aes(color = Significant), size = 3) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Significant),
                height = 0.2) +
  scale_color_manual(values = c("Sig (p<0.05)" = "#1976D2", "NS" = "grey50")) +
  labs(title = "Odds Ratios: Effets sur la position tramway",
       subtitle = "Modele de regression ordinale",
       x = "Odds Ratio (IC 95%)",
       y = "",
       color = "Significativite") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("_SharedFolder_culture_carbone/graph/output/odds_ratios_tramway.png",
       plot_or, width = 10, height = 6, dpi = 300)

cat("Graphique sauvegarde: odds_ratios_tramway.png\n")

# 7.2 Probabilites predites par education
pred_educ_df <- as.data.frame(pred_educ) %>%
  mutate(
    Education = gsub("ses_educ_fct", "", ses_educ_fct),
    Outcome = factor(group,
                    levels = c("Oppose", "Plutet oppose",
                              "Plutet favorable", "Favorable"))
  )

plot_pred_educ <- ggplot(pred_educ_df,
                        aes(x = Education, y = estimate, fill = Outcome)) +
  geom_col(position = "stack") +
  geom_text(aes(label = sprintf("%.0f%%", estimate * 100)),
           position = position_stack(vjust = 0.5),
           size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(title = "Probabilites predites de position sur le tramway par niveau d'education",
       subtitle = "Arrondissement de reference: Cite-Limoilou",
       x = "Niveau d'education",
       y = "Probabilite predite",
       fill = "Position") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave("_SharedFolder_culture_carbone/graph/output/prob_pred_education.png",
       plot_pred_educ, width = 10, height = 6, dpi = 300)

cat("Graphique sauvegarde: prob_pred_education.png\n")

# 7.3 Probabilites predites par arrondissement
pred_arrond_df <- as.data.frame(pred_arrond) %>%
  mutate(
    Arrondissement = ses_arrondissement_fct,
    Outcome = factor(group,
                    levels = c("Oppose", "Plutet oppose",
                              "Plutet favorable", "Favorable"))
  )

plot_pred_arrond <- ggplot(pred_arrond_df,
                          aes(x = Arrondissement, y = estimate, fill = Outcome)) +
  geom_col(position = "stack") +
  geom_text(aes(label = sprintf("%.0f%%", estimate * 100)),
           position = position_stack(vjust = 0.5),
           size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(title = "Probabilites predites de position sur le tramway par arrondissement",
       subtitle = "Niveau d'education de reference: Collegial/Certificat",
       x = "Arrondissement",
       y = "Probabilite predite",
       fill = "Position") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave("_SharedFolder_culture_carbone/graph/output/prob_pred_arrondissement.png",
       plot_pred_arrond, width = 10, height = 6, dpi = 300)

cat("Graphique sauvegarde: prob_pred_arrondissement.png\n")

cat("\n========================================\n")
cat("ANALYSE TERMINeE\n")
cat("========================================\n")
