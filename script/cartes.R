library(tidyverse)
library(sf)

shp <- read_sf("_SharedFolder_culture_carbone/data/shapefiles/vdq-arrondissement/vdq-arrondissement.shp")

data <- ...

dataCarte <- data |> 
  mutate(ses_arrondissement = recode(ses_arrondissement,
                                     "beauport" = "Beauport",
                                     "charlesbourg" = "Charlesbourg",
                                     "cite_limoilou" = "La Cité-Limoilou",
                                     "haute_saint_charles" = "La Haute-Saint-Charles",
                                     "les_rivieres" = "Les Rivières",
                                     "sainte_foy" = "Sainte-Foy–Sillery–Cap-Rouge")) |> 
  group_by(ses_arrondissement) |> 
  summarise(
    total_votes = n(),  # Nombre total de votes
    vote_marchand = sum(vd_vote_intent == "marchand", na.rm = TRUE),  # Nombre de votes pour Marchand
    prop_marchand = vote_marchand / total_votes * 100 
  )