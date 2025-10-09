# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(haven)   
library(lubridate)
# Load raw data -------------------------------------------------------------------
df_raw <- read.csv("_SharedFolder_culture_carbone/data/dataset.csv", 
                 sep = ";", 
                 fileEncoding = "UTF-8")

#cleaning

df_clean <- data.frame(
  id = 1:nrow(df_raw)
)

# SES --------------------------------------------------------------------
## Arrondissement
table(df_raw$SECT)
df_clean$ses_arrondissement <- NA
df_clean$ses_arrondissement[df_raw$SECT == 1] <- "Beauport"
df_clean$ses_arrondissement[df_raw$SECT == 2] <- "Charlesbourg"
df_clean$ses_arrondissement[df_raw$SECT == 3] <- "Cité-Limoilou"
df_clean$ses_arrondissement[df_raw$SECT == 4] <- "Ste-Foy-Sillery-Cap-Rouge"
df_clean$ses_arrondissement[df_raw$SECT == 5] <- "Haute-St-Charles"
df_clean$ses_arrondissement[df_raw$SECT == 6] <- "Les Rivières"
df_clean$ses_arrondissement <- factor(df_clean$ses_arrondissement)
table(df_clean$ses_arrondissement)

## age
table(df_raw$age)
df_clean$ses_age <- NA
df_clean$ses_age[df_raw$age == 0] <- "-18"
df_clean$ses_age[df_raw$age == 1] <- "18_24"
df_clean$ses_age[df_raw$age == 2] <- "25_34"
df_clean$ses_age[df_raw$age == 3] <- "35_44"
df_clean$ses_age[df_raw$age == 4] <- "45_54"
df_clean$ses_age[df_raw$age == 5] <- "55_64"
df_clean$ses_age[df_raw$age == 6] <- "65_74"
df_clean$ses_age[df_raw$age == 7] <- "75+"
df_clean$ses_age <- factor(df_clean$ses_age,
  levels = c("-18", "18_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75+"))
table(df_clean$ses_age)

## genre
table(df_raw$GENRE)
df_clean$ses_femme <- NA
df_clean$ses_femme[df_raw$GENRE == 1] <- 0
df_clean$ses_femme[df_raw$GENRE == 2] <- 1
df_clean$ses_femme[df_raw$GENRE == 3] <- 0
table(df_clean$ses_femme)

## occupation
table(df_raw$EMPLO2)
df_clean$ses_occup <- NA
df_clean$ses_occup[df_raw$EMPLO2 == 1] <- "Travail"
df_clean$ses_occup[df_raw$EMPLO2 == 2] <- "Travail"
df_clean$ses_occup[df_raw$EMPLO2 == 3] <- "Travail"
df_clean$ses_occup[df_raw$EMPLO2 == 4] <- "Études"
df_clean$ses_occup[df_raw$EMPLO2 == 5] <- "Études"
df_clean$ses_occup[df_raw$EMPLO2 == 6] <- "Sans emploi"
df_clean$ses_occup[df_raw$EMPLO2 == 7] <- "Retraite"
df_clean$ses_occup <- factor(df_clean$ses_occup)
table(df_clean$ses_occup)

## proprio
table(df_raw$PROP)
df_clean$ses_proprio <- NA
df_clean$ses_proprio[df_raw$PROP == 1] <- 1
df_clean$ses_proprio[df_raw$PROP == 2] <- 0
table(df_clean$ses_proprio)

## educ
table(df_raw$SCOL)
df_clean$ses_educ <- NA
df_clean$ses_educ[df_raw$SCOL %in% c(1,2)] <- "Secondaire"
df_clean$ses_educ[df_raw$SCOL %in% c(3,4)] <- "Collegial/Certificat"
df_clean$ses_educ[df_raw$SCOL == 5] <- "Bacc"
df_clean$ses_educ[df_raw$SCOL %in% c(6,7)] <- "Graduate"
df_clean$ses_educ <- factor(df_clean$ses_educ,
  levels = c("Secondaire", "Collegial/Certificat", "Bacc", "Graduate"))
table(df_clean$ses_educ)

## revenu
table(df_raw$REVEN)
df_clean$ses_revenu <- NA
df_clean$ses_revenu[df_raw$REVEN == 1] <- "-20k"
df_clean$ses_revenu[df_raw$REVEN == 2] <- "20_39k"
df_clean$ses_revenu[df_raw$REVEN == 3] <- "40_59k"
df_clean$ses_revenu[df_raw$REVEN == 4] <- "60_79k"
df_clean$ses_revenu[df_raw$REVEN == 5] <- "80_99k"
df_clean$ses_revenu[df_raw$REVEN == 6] <- "100_119k"
df_clean$ses_revenu[df_raw$REVEN == 7] <- "120_139k"
df_clean$ses_revenu[df_raw$REVEN == 8] <- "140_159k"
df_clean$ses_revenu[df_raw$REVEN == 9] <- "160k+"
df_clean$ses_revenu <- factor(df_clean$ses_revenu,
  levels = c("-20k", "20_39k", "40_59k", "60_79k", "80_99k", "100_119k", "120_139k", "140_159k", "160k+"))
table(df_clean$ses_revenu)

df_clean$ses_revenu_grouped[df_raw$REVEN %in% c(1,2)] <- "<40k"
df_clean$ses_revenu_grouped[df_raw$REVEN %in% c(3,4)] <- "40-79k"
df_clean$ses_revenu_grouped[df_raw$REVEN %in% c(5,6)] <- "80-119k"
df_clean$ses_revenu_grouped[df_raw$REVEN %in% c(7,8)] <- "120-159k"
df_clean$ses_revenu_grouped[df_raw$REVEN == 9] <- "160k+"
df_clean$ses_revenu_grouped <- factor(df_clean$ses_revenu_grouped,
  levels = c("<40k", "40-79k", "80-119k", "120-159k", "160k+"))
table(df_clean$ses_revenu_grouped)

# Transport --------------------------------------------------------------
modes <- c("Q1r1" = "Auto conducteur",
           "Q1r2" = "Auto passager",
           "Q1r3" = "Autobus",
           "Q1r4" = "Marche",
           "Q1r5" = "Vélo personnel",
           "Q1r6" = "Vélo-partage",
           "Q1r7" = "Flexibus",
           "Q1r8" = "Autopartage",
           "Q1r9" = "Taxi",
           "Q1r10" = "Moto")
for (q in names(modes)) {
  df_clean$main_transport[df_raw[[q]] == 1] <- modes[q]
  df_clean$second_transport[df_raw[[q]] == 2] <- modes[q]
  df_clean$third_transport[df_raw[[q]] == 3] <- modes[q]
}
df_clean$main_transport <- factor(df_clean$main_transport)
df_clean$second_transport <- factor(df_clean$second_transport)
df_clean$third_transport <- factor(df_clean$third_transport)
table(df_clean$main_transport)
table(df_clean$second_transport)
table(df_clean$third_transport)

df_clean$main_transport_3 <- NA
df_clean$main_transport_3[df_clean$main_transport %in% c("Auto conducteur","Auto passager", "Moto")] <- "Auto"
df_clean$main_transport_3[df_clean$main_transport %in% c("Autobus","Flexibus","Taxi","Autopartage")] <- "Transport collectif"
df_clean$main_transport_3[df_clean$main_transport %in% c("Marche","Vélo personnel","Vélo-partage")] <- "Actif"
df_clean$main_transport_3 <- factor(df_clean$main_transport_3)
table(df_clean$main_transport_3)

df_clean$comp_transport_eco <- NA
df_clean$comp_transport_eco[df_clean$main_transport_3 == "Auto"] <- 0
df_clean$comp_transport_eco[df_clean$main_transport_3 %in% c("Transport collectif", "Actif")] <- 1
table(df_clean$comp_transport_eco)

table(df_raw$Q3)
df_clean$freq_bus3 <- NA
df_clean$freq_bus3[df_raw$Q3 %in% c(1,2,3)] <- "Régulier"
df_clean$freq_bus3[df_raw$Q3 %in% c(4,5)] <- "Occasionnel"
df_clean$freq_bus3[df_raw$Q3 == 6] <- "Jamais"
df_clean$freq_bus3 <- factor(df_clean$freq_bus3,
  levels = c("Régulier", "Occasionnel", "Jamais"))
table(df_clean$freq_bus3)


# Ideal ------------------------------------------------------------------
table(df_raw$Q11)
df_clean$ideal <- NA
df_clean$ideal[df_raw$Q11 == 1] <- "Auto"
df_clean$ideal[df_raw$Q11 == 2] <- "Transport en commun"
df_clean$ideal[df_raw$Q11 == 3] <- "Marche"
df_clean$ideal[df_raw$Q11 == 4] <- "Vélo"
df_clean$ideal <- factor(df_clean$ideal)
table(df_clean$ideal)


# Ideal_bin --------------------------------------------------------------

df_clean$ideal_eco_bin <- NA
df_clean$ideal_eco_bin[df_raw$Q11 == 1] <- 0
df_clean$ideal_eco_bin[df_raw$Q11 %in% c(2, 3, 4)] <- 1
table(df_clean$ideal_eco_bin)

# Ouverture --------------------------------------------------------------
table(df_raw$Q12r1)
df_clean$ouverture_tc <- sondr::clean_likert_numeric_vector(
  raw_vector = df_raw$Q12r1,
  na_values  = 99,
  revert     = TRUE
)
table(df_clean$ouverture_tc, useNA = "ifany")

table(df_raw$Q12r2)
df_clean$ouverture_actif <- sondr::clean_likert_numeric_vector(
  raw_vector = df_raw$Q12r2,
  na_values  = 99,
  revert     = TRUE
)
table(df_clean$ouverture_actif, useNA = "ifany")

table(df_raw$Q12r3)
df_clean$ouverture_combine <- sondr::clean_likert_numeric_vector(
  raw_vector = df_raw$Q12r3,
  na_values  = 99,
  revert     = TRUE
)
table(df_clean$ouverture_combine, useNA = "ifany")


# Priorite ---------------------------------------------------------------
table(df_raw$Q15r1)
df_clean$mobilite_active <- sondr::clean_likert_numeric_vector(
  raw_vector = df_raw$Q15r1,
  na_values  = 99,
  revert     = TRUE
)
table(df_clean$mobilite_active)

table(df_raw$Q15r2)
df_clean$dev_durable <- sondr::clean_likert_numeric_vector(
  raw_vector = df_raw$Q15r2,
  na_values  = 99,
  revert     = TRUE
)
table(df_clean$dev_durable)

table(df_raw$Q15r3)
df_clean$dev_economique <- sondr::clean_likert_numeric_vector(
  raw_vector = df_raw$Q15r3,
  na_values  = 99,
  revert     = TRUE
)
table(df_clean$dev_economique)

table(df_raw$Q15r4)
df_clean$densification <- sondr::clean_likert_numeric_vector(
  raw_vector = df_raw$Q15r4,
  na_values  = 99,
  revert     = TRUE
)
table(df_clean$densification)

table(df_raw$Q15r5)
df_clean$sante_transport_actif <- sondr::clean_likert_numeric_vector(
  raw_vector = df_raw$Q15r5,
  na_values  = 99,
  revert     = TRUE
)
table(df_clean$sante_transport_actif)


# Opinion ----------------------------------------------------------------

# Map des items
items <- c(rtc = "Q10r1", velo = "Q10r2", tram = "Q10r3")

for (nm in names(items)) {
  q <- items[[nm]]

  # 1) Ordinale numérique (4=Très bonne ... 1=Très mauvaise), 97/98 -> NA
  df_clean[[paste0("op_", nm, "_num")]] <- sondr::clean_likert_numeric_vector(
    raw_vector = df_raw[[q]],
    na_values  = c(97, 98),
    revert     = TRUE   # rend 4=Très bonne, 3=Plutôt bonne, 2=Plutôt mauvaise, 1=Très mauvaise
  )
  
  # 2) Binaire Favorable / Défavorable (97/98 -> NA)
  df_clean[[paste0("op_", nm, "_bin")]] <- case_when(
    df_raw[[q]] %in% c(1, 2) ~ "Favorable",
    df_raw[[q]] %in% c(3, 4) ~ "Défavorable",
    df_raw[[q]] %in% c(97,98) ~ NA_character_,
    TRUE ~ NA_character_
  ) |> factor(levels = c("Défavorable", "Favorable"))

  # 3) Connaissance (1 = a une opinion 1-4, 0 = 97/98, NA = autre valeur manquante)
  df_clean[[paste0("know_", nm)]] <- ifelse(
    df_raw[[q]] %in% 1:4, 1L,
    ifelse(df_raw[[q]] %in% c(97,98), 0L, NA_integer_)
  )
}

# Vérifications rapides
# RTC
table(df_clean$op_rtc_num, useNA = "ifany")
# Vélo cité
table(df_clean$op_velo_num, useNA = "ifany")
# Tramway
table(df_clean$op_tram_num, useNA = "ifany")

# RTC
table(df_clean$op_rtc_bin, useNA = "ifany")
# Vélo cité
table(df_clean$op_velo_bin, useNA = "ifany")
# Tramway
table(df_clean$op_tram_bin, useNA = "ifany")

# RTC
table(df_clean$know_rtc, useNA = "ifany")
# Vélo cité
table(df_clean$know_velo, useNA = "ifany")
# Tramway
table(df_clean$know_tram, useNA = "ifany")


# Pondération ------------------------------------------------------------
df_clean$POND <- df_raw$POND


# Garder les +18
df_clean <- df_clean %>% filter(ses_age != "-18")

# save data --------------------------------------------------------------
saveRDS(df_clean, "_SharedFolder_culture_carbone/data/data_clean.rds")
