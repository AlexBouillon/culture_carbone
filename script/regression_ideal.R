# Analyse des déterminants du transport idéal - Culture Carbone
# Régression logistique multinomiale pour identifier les facteurs
# qui distinguent les cultures écologique vs carbone

# === PRÉPARATION ===

# Charger les packages nécessaires
if (!require(nnet, quietly = TRUE)) install.packages("nnet")
if (!require(dplyr, quietly = TRUE)) install.packages("dplyr")
if (!require(ggplot2, quietly = TRUE)) install.packages("ggplot2")
if (!require(stargazer, quietly = TRUE)) install.packages("stargazer")
if (!require(broom, quietly = TRUE)) install.packages("broom")
if (!require(MASS, quietly = TRUE)) install.packages("MASS")
if (!require(effects, quietly = TRUE)) install.packages("effects")

library(nnet)
library(dplyr)
library(ggplot2)
library(stargazer)
library(broom)
library(MASS)
library(effects)
library(ggthemes)

# Charger les données
data <- readRDS("_SharedFolder_culture_carbone/data/data_clean.rds")

# Créer le dossier pour les résultats s'il n'existe pas
if (!dir.exists("_SharedFolder_culture_carbone/results")) {
  dir.create("_SharedFolder_culture_carbone/results")
}

# === PRÉPARATION DES VARIABLES ===

# Nettoyer et préparer les données
data_reg <- data %>%
  filter(!is.na(ideal)) %>%
  mutate(
    # Variable dépendante: Marche comme référence (neutre conceptuellement)
    ideal_clean = relevel(as.factor(ideal), ref = "Marche"),

    # Variables sociodémographiques
    age_group = as.factor(ses_age),
    genre = factor(ses_femme, levels = c(0, 1), labels = c("Homme", "Femme")),
    education = as.factor(ses_educ),
    revenus = as.factor(ses_revenu_grouped),
    arrondissement = as.factor(ses_arrondissement),
    proprietaire = factor(ses_proprio, levels = c(0, 1), labels = c("Locataire", "Propriétaire")),

    # Variables de transport actuel
    transport_actuel = as.factor(main_transport_3),
    freq_bus = as.factor(freq_bus3),

    # Variables d'attitude (continues - centrées)
    ouv_tc_c = scale(ouverture_tc)[,1],
    mobil_active_c = scale(mobilite_active)[,1],
    dev_durable_c = scale(dev_durable)[,1],
    dev_economique_c = scale(dev_economique)[,1],
    densification_c = scale(densification)[,1],
    sante_transport_c = scale(sante_transport_actif)[,1],

    # Variables d'opinion
    opinion_rtc_c = scale(op_rtc_num)[,1],
    opinion_velo_c = scale(op_velo_num)[,1],
    opinion_tram_c = scale(op_tram_num)[,1]
  )

# === MODÈLES PROGRESSIFS ===

# Modèle 1: Variables sociodémographiques seulement
model1 <- multinom(
  ideal_clean ~ age_group + genre + education + revenus + arrondissement + proprietaire,
  data = data_reg,
  weights = data_reg$POND,
  trace = FALSE
)

# Modèle 2: + Transport actuel
model2 <- multinom(
  ideal_clean ~ age_group + genre + education + revenus + arrondissement + proprietaire +
    transport_actuel + freq_bus,
  data = data_reg,
  weights = data_reg$POND,
  trace = FALSE
)

# Modèle 3: + Attitudes environnementales
model3 <- multinom(
  ideal_clean ~ age_group + genre + education + revenus + arrondissement + proprietaire +
    transport_actuel + freq_bus +
    ouv_tc_c + mobil_active_c + dev_durable_c + dev_economique_c + densification_c,
  data = data_reg,
  weights = data_reg$POND,
  trace = FALSE
)

# Modèle 4: Modèle avec seulement les attitudes (pas d'opinions pour éviter les problèmes)
model4 <- model3  # Pour l'instant, gardons le modèle 3 comme modèle "complet"

# === COMPARAISON DES MODÈLES ===

# Fonction pour extraire AIC de manière sécurisée
safe_aic <- function(model) {
  if(is.null(model) || inherits(model, "try-error")) return(NA)
  return(AIC(model))
}

# Fonction pour extraire le nombre d'observations
safe_nobs <- function(model) {
  if(is.null(model) || inherits(model, "try-error")) return(0)
  if(is.null(model$model)) return(length(model$fitted.values) / length(levels(model$lev)))
  return(nrow(model$model))
}

# AIC des modèles
aic_comparison <- data.frame(
  Modele = c("M1: Sociodémographique", "M2: + Transport", "M3: + Attitudes", "M4: Complet"),
  AIC = c(safe_aic(model1), safe_aic(model2), safe_aic(model3), safe_aic(model4)),
  N_obs = c(safe_nobs(model1), safe_nobs(model2), safe_nobs(model3), safe_nobs(model4))
)

# Test de rapport de vraisemblance (seulement si les modèles sont valides)
if(!is.null(model1) && !is.null(model2) && safe_nobs(model1) == safe_nobs(model2)) {
  lrt_1_2 <- try(anova(model1, model2, test = "Chisq"), silent = TRUE)
}

if(!is.null(model2) && !is.null(model3) && safe_nobs(model2) == safe_nobs(model3)) {
  lrt_2_3 <- try(anova(model2, model3, test = "Chisq"), silent = TRUE)
}

# === RÉSULTATS DU MEILLEUR MODÈLE ===

# Sélectionner le meilleur modèle basé sur l'AIC (en excluant les NA)
valid_models <- which(!is.na(aic_comparison$AIC))
if(length(valid_models) > 0) {
  best_model_idx <- valid_models[which.min(aic_comparison$AIC[valid_models])]
  best_model <- get(paste0("model", best_model_idx))
  best_model_name <- aic_comparison$Modele[best_model_idx]
} else {
  # Fallback au modèle 1 si tous échouent
  best_model <- model1
  best_model_name <- "M1: Sociodémographique"
}

# Calculer les odds ratios
odds_ratios <- exp(coef(best_model))

# === VISUALISATIONS ===

# 1. Graphique des odds ratios pour les variables d'attitude
attitude_vars <- c("ouv_tc_c", "mobil_active_c", "dev_durable_c", "dev_economique_c", "densification_c")

# Extraire coefficients et erreurs standard
coef_data <- broom::tidy(best_model, conf.int = TRUE, exponentiate = TRUE)

# Filtrer pour les variables d'attitude seulement
attitude_coef <- coef_data %>%
  filter(term %in% attitude_vars) %>%
  mutate(
    variable = case_when(
      term == "ouv_tc_c" ~ "Ouverture TC",
      term == "mobil_active_c" ~ "Mobilité active",
      term == "dev_durable_c" ~ "Développement durable",
      term == "dev_economique_c" ~ "Développement économique",
      term == "densification_c" ~ "Densification"
    )
  )

# Forest plot des odds ratios
p_forest <- ggplot(attitude_coef, aes(x = estimate, y = variable, color = y.level)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.3), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.7) +
  scale_x_log10() +
  theme_fivethirtyeight() +
  labs(
    title = "Odds Ratios: Déterminants attitudinaux du transport idéal",
    subtitle = "Référence: Transport idéal = Marche",
    x = "Odds Ratio (échelle log)",
    color = "Transport idéal"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("_SharedFolder_culture_carbone/results/forest_plot_attitudes.png", p_forest, width = 12, height = 8)

# 1b. Graphique des odds ratios pour les variables sociodémographiques
ses_vars <- c("genre", "education", "revenus", "proprietaire", "arrondissement")

# Extraire coefficients pour les variables SES
ses_coef <- coef_data %>%
  filter(grepl(paste(ses_vars, collapse = "|"), term)) %>%
  mutate(
    variable_type = case_when(
      grepl("genre", term) ~ "Genre",
      grepl("education", term) ~ "Éducation",
      grepl("revenus", term) ~ "Revenus",
      grepl("proprietaire", term) ~ "Propriété",
      grepl("arrondissement", term) ~ "Arrondissement",
      TRUE ~ "Autre"
    ),
    variable_clean = gsub("^[a-z_]+", "", term)
  )

# Forest plot des odds ratios pour les variables SES
p_forest_ses <- ggplot(ses_coef, aes(x = estimate, y = term, color = y.level)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                position = position_dodge(width = 0.5), width = 0.3,
                orientation = "y") +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.7) +
  scale_x_log10() +
  theme_fivethirtyeight() +
  labs(
    title = "Odds Ratios: Déterminants sociodémographiques du transport idéal",
    subtitle = "Référence: Transport idéal = Marche",
    x = "Odds Ratio (échelle log)",
    color = "Transport idéal"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.y = element_text(size = 10)
  ) +
  facet_wrap(~ variable_type, scales = "free_y", ncol = 2)

ggsave("_SharedFolder_culture_carbone/results/forest_plot_ses.png", p_forest_ses, width = 14, height = 10)

# 2. Probabilités prédites par profil
# Créer des profils types basés sur les variables du meilleur modèle
model_vars <- colnames(coef(best_model))

# Profil de base
profil_base <- data.frame(
  age_group = factor(levels(data_reg$age_group)[2], levels = levels(data_reg$age_group)),
  genre = factor("Femme", levels = levels(data_reg$genre)),
  education = factor(levels(data_reg$education)[3], levels = levels(data_reg$education)),
  revenus = factor(levels(data_reg$revenus)[2], levels = levels(data_reg$revenus)),
  arrondissement = factor(levels(data_reg$arrondissement)[1], levels = levels(data_reg$arrondissement)),
  proprietaire = factor("Propriétaire", levels = levels(data_reg$proprietaire)),
  transport_actuel = factor(levels(data_reg$transport_actuel)[1], levels = levels(data_reg$transport_actuel)),
  freq_bus = factor(levels(data_reg$freq_bus)[1], levels = levels(data_reg$freq_bus))
)

# Ajouter les variables d'attitude si elles sont dans le modèle
if("ouv_tc_c" %in% model_vars) {
  profil_base$ouv_tc_c <- 0
}
if("mobil_active_c" %in% model_vars) {
  profil_base$mobil_active_c <- 0
}
if("dev_durable_c" %in% model_vars) {
  profil_base$dev_durable_c <- 0
}
if("dev_economique_c" %in% model_vars) {
  profil_base$dev_economique_c <- 0
}
if("densification_c" %in% model_vars) {
  profil_base$densification_c <- 0
}

# Profils écologique et carbone
profil_eco <- profil_base
profil_carbone <- profil_base

# Ajuster les attitudes selon le profil
if("ouv_tc_c" %in% names(profil_base)) {
  profil_eco$ouv_tc_c <- 1
  profil_carbone$ouv_tc_c <- -1
}
if("mobil_active_c" %in% names(profil_base)) {
  profil_eco$mobil_active_c <- 1
  profil_carbone$mobil_active_c <- -1
}
if("dev_durable_c" %in% names(profil_base)) {
  profil_eco$dev_durable_c <- 1
  profil_carbone$dev_durable_c <- -1
}
if("dev_economique_c" %in% names(profil_base)) {
  profil_eco$dev_economique_c <- 0
  profil_carbone$dev_economique_c <- 1
}
if("densification_c" %in% names(profil_base)) {
  profil_eco$densification_c <- 1
  profil_carbone$densification_c <- -1
}

# Prédictions
pred_eco <- predict(best_model, profil_eco, type = "probs")
pred_carbone <- predict(best_model, profil_carbone, type = "probs")

# Créer dataframe pour visualisation
pred_data <- rbind(
  data.frame(Profile = "Écologique", Transport = names(pred_eco), Probabilite = as.numeric(pred_eco)),
  data.frame(Profile = "Carbone", Transport = names(pred_carbone), Probabilite = as.numeric(pred_carbone))
)

# Graphique des probabilités prédites
p_pred <- ggplot(pred_data, aes(x = Transport, y = Probabilite, fill = Profile)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_fivethirtyeight() +
  labs(
    title = "Probabilités prédites par profil culturel",
    subtitle = "Comparaison profils 'Écologique' vs 'Carbone'",
    y = "Probabilité prédite"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  ) +
  scale_fill_manual(values = c("Écologique" = "green", "Carbone" = "orange"))

ggsave("_SharedFolder_culture_carbone/results/predicted_probabilities.png", p_pred, width = 10, height = 6)

# Sauvegarder les résultats
save(model1, model2, model3, model4, best_model, odds_ratios, aic_comparison,
     pred_data, attitude_coef,
     file = "_SharedFolder_culture_carbone/results/regression_models.RData")
