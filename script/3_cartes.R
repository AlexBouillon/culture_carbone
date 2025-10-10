library(tidyverse)
library(sf)

# ====================================================================
# 1. CHARGEMENT DES DONNÉES
# ====================================================================

shp <- read_sf("_SharedFolder_culture_carbone/data/shapefiles/vdq-arrondissement/vdq-arrondissement.shp")
data_culture <- readRDS("_SharedFolder_culture_carbone/data/data_culture.rds")
data_sondage <- readRDS("_SharedFolder_culture_carbone/data/data_clean.rds")
data_scores <- readRDS("_SharedFolder_culture_carbone/data/data_scores.rds")

# ====================================================================
# 2. CALCUL DES INDICATEURS PAR ARRONDISSEMENT
# ====================================================================

# 2.1 Indicateurs socio-économiques
data_ses <- data_culture %>%
  group_by(ses_arrondissement) %>%
  summarise(
    # Revenu médian (converti en numérique depuis les catégories)
    # On prend le point milieu de chaque catégorie
    revenu_median = median(case_when(
      ses_revenu == "-20k" ~ 15,
      ses_revenu == "20_39k" ~ 30,
      ses_revenu == "40_59k" ~ 50,
      ses_revenu == "60_79k" ~ 70,
      ses_revenu == "80_99k" ~ 90,
      ses_revenu == "100_119k" ~ 110,
      ses_revenu == "120_139k" ~ 130,
      ses_revenu == "140_159k" ~ 150,
      ses_revenu == "160k+" ~ 180,
      TRUE ~ NA_real_
    ), na.rm = TRUE),

    # % diplôme universitaire (Bacc + Graduate)
    pct_diplome_univ = sum(ses_educ %in% c("Bacc", "Graduate"), na.rm = TRUE) /
                       sum(!is.na(ses_educ)) * 100,

    # % idéal écologique
    pct_ideal_eco = sum(ideal_eco_bin == 1, na.rm = TRUE) /
                    sum(!is.na(ideal_eco_bin)) * 100,

    # % comportement écologique
    pct_comp_eco = sum(comp_transport_eco == 1, na.rm = TRUE) / n() * 100,

    # Sense-making moyen
    sense_making_moyen = mean(sense_making, na.rm = TRUE),

    # Support tramway moyen
    support_tram_moyen = mean(op_tram_num, na.rm = TRUE),

    # Effectif total
    n_total = n(),

    .groups = 'drop'
  )

# 2.2 Répertoire écologique (déjà calculé dans data_culture)
data_repertoire <- data_culture %>%
  group_by(ses_arrondissement) %>%
  summarise(
    repertoire_eco = first(repertoire_eco),
    repertoire_cat = first(repertoire_cat),
    .groups = 'drop'
  )

# 2.3 Distribution des profils culturels
data_profils <- data_culture %>%
  filter(!is.na(profil_culture)) %>%
  group_by(ses_arrondissement, profil_culture) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(ses_arrondissement) %>%
  mutate(
    pct = n / sum(n) * 100,
    total = sum(n)
  )

# 2.4 Profil culturel dominant par arrondissement
data_dominant <- data_profils %>%
  group_by(ses_arrondissement) %>%
  slice_max(order_by = pct, n = 1) %>%
  rename(profil_dominant = profil_culture,
         pct_profil_dominant = pct) %>%
  select(ses_arrondissement, profil_dominant, pct_profil_dominant)

# 2.5 Fusionner tous les indicateurs
data_carte <- data_ses %>%
  left_join(data_repertoire, by = "ses_arrondissement") %>%
  left_join(data_dominant, by = "ses_arrondissement")

# Harmoniser les noms d'arrondissement avec le shapefile
data_carte <- data_carte %>%
  mutate(
    NOM = case_when(
      ses_arrondissement == "Beauport" ~ "Beauport",
      ses_arrondissement == "Charlesbourg" ~ "Charlesbourg",
      ses_arrondissement == "Cité-Limoilou" ~ "La Cité-Limoilou",
      ses_arrondissement == "Haute-St-Charles" ~ "La Haute-Saint-Charles",
      ses_arrondissement == "Les Rivières" ~ "Les Rivières",
      ses_arrondissement == "Ste-Foy-Sillery-Cap-Rouge" ~ "Sainte-Foy–Sillery–Cap-Rouge",
      TRUE ~ ses_arrondissement
    )
  )

# Joindre avec le shapefile
shp_data <- shp %>%
  left_join(data_carte, by = "NOM")

# ====================================================================
# 3. OPTION A: CARTE À BULLES PROPORTIONNELLES (Profils culturels)
# ====================================================================

# Préparer les données pour les bulles (centroïdes des arrondissements)
centroides <- st_centroid(shp_data)

# Joindre les profils culturels aux centroïdes
profils_map <- data_profils %>%
  mutate(
    NOM = case_when(
      ses_arrondissement == "Beauport" ~ "Beauport",
      ses_arrondissement == "Charlesbourg" ~ "Charlesbourg",
      ses_arrondissement == "Cité-Limoilou" ~ "La Cité-Limoilou",
      ses_arrondissement == "Haute-St-Charles" ~ "La Haute-Saint-Charles",
      ses_arrondissement == "Les Rivières" ~ "Les Rivières",
      ses_arrondissement == "Ste-Foy-Sillery-Cap-Rouge" ~ "Sainte-Foy–Sillery–Cap-Rouge",
      TRUE ~ ses_arrondissement
    )
  ) %>%
  left_join(
    centroides %>% select(NOM, geometry),
    by = "NOM"
  ) %>%
  st_as_sf()

# Créer une palette de couleurs pour les 8 profils
profil_colors <- c(
  # Profils éco (verts, du plus foncé au plus clair)
  "Éco normalisé" = "#0d4711",      # Vert très foncé (répertoire élevé)
  "Éco limité" = "#008b09",         # Vert moyen (répertoire modéré)
  "Éco résistant" = "#00da0b",      # Vert clair (répertoire faible)
  "Éco latent" = "#8ca38d",         # Vert très clair (potentiel non réalisé)
  "Éco contraint" = "#78b37f",      # Vert pâle (empêché/bloqué)
  # Profils carbone (rouges/oranges)
  "Carbone résistant" = "#8b0e0e",  # Rouge très foncé (résistance active)
  "Carbone normalisé" = "#E57373",  # Rouge/rose moyen (cohérence carbone)
  "Carbone atypique" = "#f7dc83"    # Orange pâle (rare, atypique)
)

# Carte A: Bulles proportionnelles
carte_A <- ggplot() +
  # Fond de carte (arrondissements)
  geom_sf(data = shp_data, fill = "grey95", color = "grey50", size = 0.5) +
  # Bulles proportionnelles colorées par profil
  geom_sf(data = profils_map,
          aes(size = pct, fill = profil_culture),
          shape = 21, color = "black", alpha = 0.7) +
  scale_size_continuous(name = "% du profil",
                       range = c(1, 15),
                       breaks = c(10, 30, 50, 70)) +
  scale_fill_manual(name = "Profil culturel",
                   values = profil_colors) +
  labs(title = "Distribution des profils culturels par arrondissement",
       subtitle = "Taille des bulles = pourcentage du profil dans l'arrondissement") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

# Sauvegarder
ggsave("_SharedFolder_culture_carbone/graph/output/carte_A_bulles_profils.png",
       carte_A, width = 12, height = 8, dpi = 300)

cat("\nCarte A sauvegardée: carte_A_bulles_profils.png\n")

# ====================================================================
# 4. OPTION B: CARTE CHOROPLÈTHE (Profil dominant)
# ====================================================================

carte_B <- ggplot(shp_data) +
  geom_sf(aes(fill = profil_dominant), color = "white", size = 0.8) +
  scale_fill_manual(name = "Profil culturel\ndominant",
                   values = profil_colors,
                   na.value = "grey80") +
  # Ajouter les noms d'arrondissement
  geom_sf_text(aes(label = NOM), size = 3, fontface = "bold") +
  labs(title = "Profil culturel dominant par arrondissement",
       subtitle = "Profil le plus fréquent dans chaque arrondissement") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Sauvegarder
ggsave("_SharedFolder_culture_carbone/graph/output/carte_B_profil_dominant.png",
       carte_B, width = 10, height = 8, dpi = 300)

cat("Carte B sauvegardée: carte_B_profil_dominant.png\n")

# ====================================================================
# 5. CARTE MULTIVARIÉE (Répertoire + Idéal + SES)
# ====================================================================

# Carte avec fond choroplèthe (répertoire écologique)
# + bulles proportionnelles (% idéal éco)
# + bordures colorées (niveau d'éducation)

# Créer des catégories pour l'éducation
shp_data <- shp_data %>%
  mutate(
    educ_cat = cut(pct_diplome_univ,
                  breaks = c(0, 30, 40, 50, 100),
                  labels = c("Faible (<30%)", "Moyen (30-40%)",
                            "Élevé (40-50%)", "Très élevé (>50%)"),
                  include.lowest = TRUE)
  )

# Préparer les centroïdes pour les bulles (% idéal éco)
centroides_ideal <- centroides %>%
  select(NOM, pct_ideal_eco, geometry)

carte_multivar <- ggplot() +
  # Fond: répertoire écologique (gradient)
  geom_sf(data = shp_data,
          aes(fill = repertoire_eco * 100),
          color = "white", size = 1) +
  scale_fill_gradient2(name = "Répertoire\nécologique\n(score)",
                      low = "#D32F2F", mid = "#FFF59D", high = "#388E3C",
                      midpoint = 50,
                      limits = c(0, 100),
                      breaks = c(0, 25, 50, 75, 100)) +
  # Bulles: % idéal écologique
  geom_sf(data = centroides_ideal,
          aes(size = pct_ideal_eco),
          shape = 21, fill = "#1976D2", color = "white",
          alpha = 0.7, stroke = 1.5) +
  scale_size_continuous(name = "% Idéal\nécologique",
                       range = c(2, 12),
                       breaks = c(20, 40, 60, 80)) +
  # Ajouter les noms d'arrondissement
  geom_sf_text(data = shp_data,
               aes(label = NOM),
               size = 2.5, fontface = "bold", color = "black",
               nudge_y = -0.01) +
  labs(title = "Carte multivariée: Répertoire écologique, Idéal écologique et Contexte socio-économique",
       subtitle = "Fond = score de répertoire écologique | Bulles = % population avec idéal écologique") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Sauvegarder
ggsave("_SharedFolder_culture_carbone/graph/output/carte_multivar.png",
       carte_multivar, width = 12, height = 8, dpi = 300)

cat("Carte multivariée sauvegardée: carte_multivar.png\n")

# ====================================================================
# 6. CARTES SUPPLÉMENTAIRES
# ====================================================================

# 6.1 Carte du revenu médian
carte_revenu <- ggplot(shp_data) +
  geom_sf(aes(fill = revenu_median), color = "white", size = 0.8) +
  scale_fill_gradient2(name = "Revenu médian\n(k$/an)",
                      low = "#1565C0", mid = "#FFF59D", high = "#2E7D32",
                      midpoint = 70,
                      breaks = seq(0, 150, 30)) +
  geom_sf_text(aes(label = NOM), size = 3, fontface = "bold") +
  labs(title = "Revenu médian par arrondissement") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("_SharedFolder_culture_carbone/graph/output/carte_revenu.png",
       carte_revenu, width = 10, height = 8, dpi = 300)

# 6.2 Carte de l'éducation
carte_educ <- ggplot(shp_data) +
  geom_sf(aes(fill = pct_diplome_univ), color = "white", size = 0.8) +
  scale_fill_gradient2(name = "% Diplôme\nuniversitaire",
                      low = "#C62828", mid = "#FFF59D", high = "#1565C0",
                      midpoint = 40,
                      breaks = seq(0, 80, 20)) +
  geom_sf_text(aes(label = NOM), size = 3, fontface = "bold") +
  labs(title = "Niveau d'éducation par arrondissement",
       subtitle = "% de la population avec un diplôme universitaire (Bacc ou +)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("_SharedFolder_culture_carbone/graph/output/carte_education.png",
       carte_educ, width = 10, height = 8, dpi = 300)

# 6.3 Carte du support au tramway
carte_tram <- ggplot(shp_data) +
  geom_sf(aes(fill = support_tram_moyen), color = "white", size = 0.8) +
  scale_fill_gradient2(name = "Support moyen\nau tramway\n(échelle 0-1)",
                      low = "#C62828", mid = "#FFF59D", high = "#1565C0",
                      midpoint = 0.5,
                      breaks = seq(0, 1, 0.2)) +
  geom_sf_text(aes(label = NOM), size = 3, fontface = "bold") +
  labs(title = "Support au tramway par arrondissement",
       subtitle = "Score moyen de support au projet de tramway") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("_SharedFolder_culture_carbone/graph/output/carte_tramway.png",
       carte_tram, width = 10, height = 8, dpi = 300)

cat("\nToutes les cartes ont été créées avec succès!\n")
cat("Fichiers sauvegardés dans: _SharedFolder_culture_carbone/output/\n")
