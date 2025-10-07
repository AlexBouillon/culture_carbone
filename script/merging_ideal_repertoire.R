
library(readxl)

data_scores <- read_excel("_SharedFolder_culture_carbone/data/walk_scores.xlsx") 



score par arrondissement




df <- df %>%
  mutate(
    transit_score = transit_score / 100,
    walk_score = walk_score / 100,
    bike_score = bike_score / 100
  )

# 1. Calcul du répertoire écologique pondéré
df <- df %>%
  mutate(repertoire_eco = 0.4 * transit_score + 
                          0.3 * walk_score + 
                          0.3 * bike_score)


