library(readxl)
library(dplyr)
library(stringr)

# Charger les données
data_pol <- read_excel("_SharedFolder_culture_carbone/data/bodet/MyData_FINAL.xlsx",
                       sheet = "Réponses au formulaire 1")

# Renommer les colonnes pour simplifier
data_pol <- data_pol %>%
  rename(
    A1 = `A1. Êtes-vous en faveur du projet de tramway à Québec dans sa forme actuelle?`,
    A2 = `A2. Êtes-vous en faveur de la construction d'un troisième lien entre Québec et sa Rive-Sud?`,
    A3 = `A3. Êtes-vous en faveur de la gratuité des transports collectifs pour l'ensemble de la population de la Ville de Québec?`,
    A4 = `A4. En quelques mots, quel est pour vous l'enjeu le plus important de cette élection?`,
    B1 = `B1. Êtes-vous impliqué.e dans un comité citoyen, un organisme communautaire ou une association locale dans la Ville de Québec?`,
    B2 = `B2. Quel est votre niveau d'intérêt pour les élections municipales à Québec?`,
    C1 = `C1. Lors de cette élection municipale, quelle candidature à la Mairie comptez-vous appuyer?`,
    C2 = `C2. Comptez-vous appuyer la ou le représentant du même parti au poste de conseiller?`,
    D1 = `D1. Quel âge avez-vous?`,
    D2 = `D2. À quel genre vous identifiez-vous?`,
    D3 = `D3. Quel est votre revenu familial?`,
    D4 = `D4. Combien de personnes (adultes et enfants) habitent avec vous?`,
    D5 = `D5. Êtes-vous propriétaire ou locataire?`,
    D6 = `D6. En général, quel moyen de transport utilisez-vous régulièrement pour aller au travail, aux études, faire vos courses, etc.? Vous pouvez cocher plus d'une case.`,
    D7 = `D7. Depuis combien d'années résidez-vous à Québec (ou du moins dans ses frontières actuelles)?`,
    D8 = `D8. Lors de l'élection municipale de 2017, pour quelle candidature avez-vous voté à la Mairie?`,
    D9 = `D9. Lors de l'élection municipale de 2017, pour quel parti avez-vous voté pour le poste de conseiller municipal?`,
    D10 = `D10. Lors de l'élection provinciale de 2018, pour quel parti avez-vous voté?`,
    D11 = `D11. Quels sont les trois premiers caractères de votre code postal?`
  )

# Supprimer le questionnaire test
data_pol <- data_pol %>%
  filter(D11 != "w3g")

# Convertir toutes les colonnes de A1 à D11 en minuscules
data_pol <- data_pol %>%
  mutate(across(A1:D11, ~tolower(as.character(.))))

# ======== Q18 District (Montcalm) ========
data_pol <- data_pol %>%
  mutate(
    D11 = str_replace_all(D11, " ", ""),
    montcalm = case_when(
      D11 %in% c("g2l", "g1g", "g1h", "3m8") ~ 0,  # non
      D11 %in% c("g1s", "3a5") ~ 1,  # oui
      D11 == "neveuxpasrépondre" ~ 2,  # ne veux pas répondre
      TRUE ~ NA_real_
    )
  )

# ======== Q1 Tramway ========
data_pol <- data_pol %>%
  mutate(
    tramway = case_when(
      A1 == "non" ~ 0,
      A1 == "oui" ~ 1,
      A1 == "ne veux pas répondre" ~ 2,
      TRUE ~ NA_real_
    )
  )

# ======== Q2 Troisième lien ========
data_pol <- data_pol %>%
  mutate(
    troisiemelien = case_when(
      A2 == "non" ~ 0,
      A2 == "oui" ~ 1,
      A2 == "ne veux pas répondre" ~ 2,
      TRUE ~ NA_real_
    )
  )

# ======== Q3 Gratuité transport ========
data_pol <- data_pol %>%
  mutate(
    gratuit = case_when(
      A3 == "non" ~ 0,
      A3 == "oui" ~ 1,
      A3 == "ne veux pas répondre" ~ 2,
      TRUE ~ NA_real_
    )
  )

# ======== Q4 Enjeux ========
# Supprimer la colonne A4E et renommer A4a en enjeux
data_pol <- data_pol %>%
  select(-contains("A4Enquelquesmotsquelestp")) %>%
  rename(enjeux = A4a)
# Les catégories: 1=mobilité, 2=environnement, 3=fiscalité et services,
# 4=gouvernance, 5=autres, 6=aucun

# ======== Q7 Intention de vote ========
data_pol <- data_pol %>%
  mutate(
    vote = case_when(
      C1 == "bruno marchand – québec forte et fière" ~ 1,  # qff
      C1 == "marie-josée savard – Équipe m-j savard" ~ 2,  # mjs_rg
      C1 == "jean-françois gosselin – québec 21" ~ 3,  # qc21
      C1 == "jackie smith – transition québec" ~ 4,  # tq
      C1 == "jean rousseau – démocratie québec" ~ 5,  # dq
      C1 == "autre candidature" ~ 6,  # autre
      TRUE ~ NA_real_
    )
  )

# ======== Q8 Même conseiller ========
data_pol <- data_pol %>%
  mutate(
    memeconseiller = case_when(
      C2 == "non" ~ 0,
      C2 == "oui" ~ 1,
      TRUE ~ NA_real_
    )
  )

# ======== Q9 Âge ========
data_pol <- data_pol %>%
  mutate(
    age = str_extract(D1, "^\\d+"),
    age = as.numeric(age)
  )

# ======== Q10 Genre ========
data_pol <- data_pol %>%
  mutate(
    femme = as.numeric(D2 == "femme")
  )

# ======== Q11 Revenu familial ========
data_pol <- data_pol %>%
  mutate(
    revenu = case_when(
      D3 == "moins de 40 000$" ~ 0,  # bas
      D3 == "entre 40 000$ et 90 000$" ~ 1,  # moyen
      D3 == "plus de 90 000$" ~ 2,  # élevé
      TRUE ~ NA_real_
    )
  )

# ======== Q12 Propriétaire ========
data_pol <- data_pol %>%
  mutate(
    proprietaire = case_when(
      D5 == "locataire" ~ 0,
      D5 == "propriétaire" ~ 1,
      TRUE ~ NA_real_
    )
  )

# ======== Q13 Mode de transport ========
data_pol <- data_pol %>%
  mutate(
    mode = case_when(
      D6 == "en voiture" ~ 1,  # voiture
      D6 %in% c("en transport en commun", "en transport en commun, à pied",
                "en transport en commun, à vélo", "en transport en commun, à vélo, à pied",
                "à pied", "à vélo", "à vélo, à pied") ~ 2,  # actif
      TRUE ~ 0  # hybride
    )
  )

# ======== Q15 Vote Régis Labeaume en 2017 ========
data_pol <- data_pol %>%
  mutate(
    voteregis = as.numeric(D8 == "régis labeaume")
  )

# ======== Q17 Vote Québec 2018 ========
data_pol <- data_pol %>%
  mutate(
    voteqc = case_when(
      D10 == "coalition avenir québec" ~ 1,  # caq
      D10 == "parti libéral du québec" ~ 2,  # plq
      D10 == "parti québécois" ~ 3,  # pq
      D10 == "québec solidaire" ~ 4,  # qs
      D10 %in% c("autre parti", "parti conservateur du québec", "n'ai pas voté",
                 "ne veux pas répondre / ne sais pas", "p") ~ 5,  # autre
      TRUE ~ NA_real_
    )
  )
