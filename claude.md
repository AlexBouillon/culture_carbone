# Culture carbone - Contexte théorique et méthodologique

## Vue d'ensemble du projet

Ce projet analyse si le **transport** constitue un **clivage social et politique** (cleavage) dans la ville de Québec, en s'inspirant du cadre théorique classique de Bartolini et Mair (1990) sur les structures de clivages en Europe occidentale.

**Question centrale:** Le transport peut-il être considéré comme un clivage structurant, créant des divisions sociales durables qui s'expriment par des identités collectives distinctes et potentiellement par des mobilisations politiques?

---

## Cadre théorique: Les clivages selon Bartolini et Mair (1990)

### Définition d'un clivage

Bartolini et Mair (1990) définissent un **clivage** (cleavage) comme une structure conflictuelle qui doit remplir **trois critères simultanément**:

#### 1. Critère socio-structural
- Il existe une **division sociale objective** basée sur des caractéristiques démographiques, économiques ou géographiques
- Cette division crée des groupes sociaux distincts avec des positions structurelles différentes dans la société
- Exemples classiques: classe sociale, religion, centre-périphérie, urbain-rural

#### 2. Critère normatif (identitaire)
- Les groupes divisés développent des **identités collectives** et des **systèmes de valeurs** distincts
- Il y a une conscience partagée d'appartenir à un groupe et de se distinguer d'autres groupes
- Ces identités sont **durables** et structurent les attitudes et comportements
- On parle parfois de "cultures" distinctes (ex: culture de classe, culture religieuse)

#### 3. Critère organisationnel
- Le clivage est **politisé** par des organisations distinctes, notamment des **partis politiques**
- Ces organisations mobilisent les identités collectives et les traduisent en action politique
- Il y a une **représentation politique différenciée** des deux côtés du clivage

### Clivage complet vs clivage latent

Un **clivage complet** remplit les trois critères. Un **clivage latent** ou **cleavage en formation** peut remplir les deux premiers critères (socio-structural et normatif) sans encore être pleinement organisé politiquement.

### Liens avec Rokkan (1970) et Lijphart (1999)

- **Rokkan (1970)**: A identifié les clivages historiques qui ont structuré les systèmes de partis européens (centre-périphérie, État-Église, rural-urbain, propriétaires-travailleurs)
- **Lijphart (1999)**: Distingue les clivages **recoupants** (overlapping) où plusieurs divisions sociales se renforcent mutuellement, des clivages **transversaux** (crosscutting) où elles se croisent, créant des loyautés multiples et complexes

---

## Application au cas du transport à Québec

### Les trois hypothèses de recherche

Cette étude teste si le transport constitue un clivage en examinant les deux premiers critères (socio-structural et normatif). Le critère organisationnel pourrait être étudié ultérieurement via l'analyse de l'alignement partisan.

#### H1: Division centre-périphérie sur la mobilité (critère socio-structural)
**Hypothèse:** Il existe une division géographique entre les arrondissements centraux et périphériques sur les attitudes envers le transport écologique (tramway).

**Opérationalisation:**
- Variable indépendante: `ses_arrondissement` (6 arrondissements de Québec)
- Variables dépendantes:
  - `op_tram_ord` (opinion sur le tramway, ordinale: Opposé, Plutôt opposé, Plutôt favorable, Favorable)
  - `op_tram_bin` (favorable vs opposé, binaire)
  - `profil_culture` (8 profils culturels)
- Modèle: Régression logistique ordinale et binaire avec arrondissement comme prédicteur
- Arrondissement de référence: Haute-St-Charles

**Résultats:** ✅ **Hypothèse validée**

**Effets des arrondissements (vs Haute-St-Charles, référence):**

| Arrondissement | OR (ordinal) | p-value | Interprétation |
|---|---|---|---|
| **Ste-Foy-Sillery-Cap-Rouge** | **2.42** | **< 0.001*** | **LE PLUS FAVORABLE** (2.4× plus de chances d'être favorable) |
| **Cité-Limoilou** | **1.63** | **0.03*** | **Favorable** (1.6× plus de chances) |
| Charlesbourg | 1.07 | 0.76 | Non significatif (similaire à référence) |
| Beauport | 1.03 | 0.91 | Non significatif (similaire à référence) |
| Les Rivières | 0.93 | 0.77 | Non significatif (similaire à référence) |

**Probabilités prédites d'être "Favorable" au tramway:**
- Ste-Foy-Sillery-Cap-Rouge: **23.5%** (le plus haut)
- Cité-Limoilou: 17.2%
- Charlesbourg: 12.0%
- Beauport: 11.5%
- Haute-St-Charles: 11.3%
- Les Rivières: **10.5%** (le plus bas)

**Interprétation:**
- Il existe une **division centre-périphérie claire** avec Ste-Foy-Sillery-Cap-Rouge comme arrondissement le plus pro-tramway, suivi de Cité-Limoilou
- Les arrondissements périphériques (Beauport, Charlesbourg, Les Rivières, Haute-St-Charles) sont significativement moins favorables
- Cette division géographique valide le critère socio-structural du clivage

#### H2: Division éducationnelle sur la mobilité (critère socio-structural)
**Hypothèse:** Il existe une division selon le niveau d'éducation sur les attitudes envers le transport écologique.

**Opérationalisation:**
- Variable indépendante: `ses_educ` (Secondaire, Collégial/Certificat, Bacc, Études graduées)
- Variables dépendantes: identiques à H1
- Modèle: Régression logistique avec éducation comme prédicteur
- Catégorie de référence: Secondaire

**Résultats:** ✅ **Hypothèse validée**

**Effets de l'éducation (vs Secondaire, référence):**

| Niveau d'éducation | OR (ordinal) | p-value | Interprétation |
|---|---|---|---|
| Collégial/Certificat | 1.21 | 0.25 | Non significatif |
| **Bacc** | **2.81** | **< 0.001*** | **2.8× plus de chances d'être favorable** |
| **Études graduées** | **3.88** | **< 0.001*** | **3.9× plus de chances d'être favorable** |

**Probabilités prédites d'être "Favorable" au tramway:**
- Études graduées: **40.0%** (le plus haut)
- Bacc: 32.6%
- Collégial/Certificat: 17.2%
- Secondaire: 14.7%

**Interprétation:**
- Il existe un **effet dose-réponse clair**: plus le niveau d'éducation augmente, plus l'attitude est favorable au tramway
- L'éducation universitaire (Bacc et plus) crée une division majeure: les diplômés universitaires sont 3-4× plus favorables que ceux avec un diplôme secondaire
- Cette division éducationnelle valide le critère socio-structural du clivage

#### H3: Cultures carbone et écologique structurées par géographie ET éducation (critère normatif)
**Hypothèse:** Au-delà des attitudes ponctuelles, il existe des **cultures de mobilité** distinctes (identités collectives) qui sont structurées conjointement par la géographie et l'éducation.

**Opérationalisation:**
- Variable dépendante: `profil_simple` (4 profils basés sur idéal × comportement)

  **Profils écologiques** (idéal écologique):
  - **Éco aligné**: idéal éco + comportement éco (alignement total)
  - **Éco contraint**: idéal éco + comportement carbone (aspiration non réalisée)

  **Profils carbone** (idéal automobile):
  - **Carbone aligné**: idéal carbone + comportement carbone (alignement carbone)
  - **Carbone atypique**: idéal carbone + comportement éco (pratique écologique non assumée)

- Variables indépendantes: `ses_educ × ses_arrondissement` (interaction) + `repertoire_eco` (testé comme prédicteur)
- Modèle: Régression logistique multinomiale avec et sans interaction

**Résultats:** ✅ **Hypothèse validée**

**Distribution des profils culturels (n=992):**
- **Carbone aligné: 49.5%** (profil modal - majorité silencieuse)
- **Éco contraint: 26.2%** (aspiration écologique non réalisée)
- **Éco aligné: 20.1%** (alignement écologique total)
- **Carbone atypique: 4.2%** (rare - pratique écologique sans idéal)

**Effets principaux significatifs:**

1. **Éducation** (p < 0.001 pour profils "Éco aligné" et "Éco contraint"):
   - Les diplômés universitaires sont massivement plus susceptibles d'avoir des profils écologiques
   - Probabilité "Éco aligné": **Études graduées (59%)** vs Secondaire (38%)
   - Inversion complète: Graduate favorise Éco aligné, Secondaire divisé également

2. **Géographie** (p < 0.001 pour "Éco aligné" et "Éco contraint"):
   - **Cité-Limoilou**: 30% Éco aligné (le plus élevé)
   - **Ste-Foy-Sillery-Cap-Rouge**: 14% Éco aligné + **39% Éco contraint** (dissonance de classe!)
   - **Arrondissements périphériques**: 60-67% Carbone aligné (domination carbone)

3. **Répertoire écologique** (effet sur les profils):
   - **Éco aligné**: répertoire moyen = 0.576 (le plus élevé)
   - **Éco contraint**: répertoire moyen = 0.495 (modéré)
   - **Carbone aligné**: répertoire moyen = 0.457 (le plus faible)
   - → Le répertoire **facilite** l'alignement écologique mais ne l'explique pas totalement

**Interaction éducation × arrondissement:**
- Test du rapport de vraisemblance: **non significatif** (p = 0.80)
- AIC plus élevé avec interaction (1949 vs 1896) → modèle additif préférable
- Effets **indépendants**: l'éducation et la géographie ont des effets additifs, pas multiplicatifs

**Interprétation:**
- Les profils culturels sont **clairement structurés** par l'éducation (très fort) et la géographie (modéré)
- Ces profils suggèrent l'émergence d'**identités collectives distinctes** autour de la mobilité
- L'absence d'interaction suggère des effets **additifs** plutôt que synergiques
- **Paradoxe de Ste-Foy**: Arrondissement pro-tramway mais avec dissonance massive (39% Éco contraint)
  - Valeurs écolos + pratiques carbones = hypocrisie structurelle de classe
- Cela valide le critère normatif du clivage (identités en formation)

---

## Implications théoriques

### Transport comme clivage latent?

Les résultats suggèrent que le transport à Québec pourrait constituer un **clivage latent** (ou cleavage en formation):

1. ✅ **Critère socio-structural validé**: Divisions objectives selon la géographie (centre-périphérie) et l'éducation
   - Ste-Foy-Sillery-Cap-Rouge et Cité-Limoilou vs arrondissements périphériques
   - Diplômés universitaires vs diplômés secondaires

2. ✅ **Critère normatif partiellement validé**: Émergence de cultures de mobilité distinctes (profils écologiques vs carbone)
   - 8 profils distincts basés sur idéal-comportement-répertoire
   - Structuration claire par éducation et géographie
   - Suggère des identités collectives en formation

3. ❓ **Critère organisationnel non testé**: Reste à vérifier si ces divisions se traduisent par des alignements partisans distincts
   - Si oui → clivage complet
   - Si non → clivage latent

### Clivages recoupants ou additifs?

Les résultats suggèrent que les divisions géographiques et éducationnelles se **recoupent partiellement** mais avec des effets principalement **additifs**:

- Les arrondissements centraux (Ste-Foy-Sillery-Cap-Rouge, Cité-Limoilou) tendent à avoir:
  - Des populations plus éduquées
  - Des répertoires écologiques plus élevés
  - Plus d'attitudes favorables au transport écologique

- Les arrondissements périphériques tendent à avoir:
  - Des populations moins éduquées
  - Des répertoires écologiques plus faibles
  - Plus d'attitudes favorables à l'automobile

**Cependant**, l'absence d'interaction significative (H3) suggère que ces effets sont **additifs** plutôt que **multiplicatifs**:
- Une personne avec études graduées à Haute-St-Charles reste plus favorable qu'une personne avec secondaire à Cité-Limoilou
- L'effet de l'éducation ne dépend pas de l'arrondissement (et vice-versa)
- Cela limite la polarisation et crée des "zones grises" (éduqués en périphérie, peu éduqués au centre)

Selon **Lijphart (1999)**, cette configuration **atténue** la polarisation sociale en créant des loyautés croisées, ce qui pourrait limiter l'intensité du clivage.

### Lien avec les clivages classiques

Ce clivage émergent du transport pourrait être une **expression contemporaine** de clivages classiques:

1. **Centre-périphérie** (Rokkan 1970):
   - Historiquement: relation capitale-régions
   - Ici: réinterprétation à l'échelle urbaine (centre-ville vs banlieue)
   - Reflète des intérêts matériels distincts (accès transport collectif, temps de déplacement, coûts)

2. **Classe sociale / éducation**:
   - L'éducation comme marqueur de classe (capital culturel)
   - Diplômés universitaires: professions intellectuelles, valeurs postmatérialistes
   - Peut refléter des divisions socio-économiques plus larges

3. **Matérialiste-postmatérialiste** (Inglehart 1977):
   - L'écologie comme valeur postmatérialiste
   - Plus présente chez les éduqués et urbains
   - Reflète un changement générationnel et culturel

**Question ouverte:** Le transport est-il un clivage **autonome** ou une **expression** de clivages existants (classe, urbain-rural, valeurs)?

---

## Considérations méthodologiques

### Forces de l'approche

1. **Triangulation des mesures**:
   - Opinion ponctuelle (tramway) ET profils culturels durables
   - Validation croisée des hypothèses avec différentes variables dépendantes

2. **Pondération**:
   - Données pondérées (`POND`) pour représentativité de la population de Québec
   - Assure que les estimations reflètent la population réelle

3. **Interaction testée**:
   - Examen de l'effet conjoint éducation × géographie (H3)
   - Test rigoureux du rapport de vraisemblance

4. **Richesse théorique**:
   - Typologie en 8 profils capture la complexité des tensions idéal-comportement-contexte
   - Permet d'identifier des profils minoritaires mais théoriquement importants (ex: Éco résistant, Carbone résistant)

### Limites et problèmes identifiés

#### 1. Problème de circularité dans les profils culturels ⚠️

**Problème:** La variable `repertoire_eco` (répertoire écologique de l'arrondissement) est utilisée pour **construire** `profil_culture`, puis on régresse `profil_culture` sur `arrondissement`.

**Construction de repertoire_eco (script 2_indice_culture.R, lignes 12-35):**
```r
repertoire_arrondissement <- data_scores %>%
  group_by(arrondissement) %>%
  summarise(
    walk_score = sum(walk_score * population) / sum(population),
    transit_score = sum(transit_score * population) / sum(population),
    bike_score = sum(bike_score * population) / sum(population)
  ) %>%
  mutate(
    repertoire_eco = 0.4 * transit_norm + 0.3 * walk_norm + 0.3 * bike_norm
  )
```

**Construction de profil_culture (lignes 60-116):**
```r
profil_culture = case_when(
  ideal_eco_bin == 1 & repertoire_cat == "Élevé" & comp_transport_eco == 1 ~ "Éco normalisé",
  # repertoire_cat dépend directement de l'arrondissement
  ...
)
```

**Régression (script 4_regression.R, lignes 420-425):**
```r
model_profils <- multinom(profil_culture ~ ... + ses_arrondissement_fct, ...)
```

**Implication:** L'effet de l'arrondissement est partiellement "forcé" par construction. On ne peut pas vraiment tester si les profils sont structurés par la géographie puisque la géographie (via repertoire_eco) est déjà dans leur définition.

**Solution potentielle:**
- Construire les profils **uniquement** sur idéal × comportement (4 profils au lieu de 8)
- Traiter `repertoire_eco` comme **médiateur** plutôt que comme composante du profil
- Tester le modèle de médiation: arrondissement → repertoire_eco → cohérence idéal-comportement
- Cela permettrait de tester le mécanisme causal plutôt que de l'assumer

#### 2. Sur-paramétrage du modèle d'interaction ⚠️

**Problème:** Le modèle avec interaction éducation × arrondissement génère **18 termes d'interaction** pour environ 1000 observations (après exclusion des valeurs manquantes).

**Calcul:** 3 niveaux d'éducation (Collégial, Bacc, Graduate vs Secondaire) × 5 arrondissements (vs Haute-St-Charles) × 7 comparaisons (8 profils - 1 référence) = 105 degrés de liberté

**Implication:**
- Manque de puissance statistique (trop de paramètres pour la taille d'échantillon)
- Risque d'instabilité des estimations (coefficients très larges observés)
- Difficulté d'interprétation (105 termes!)
- Peut expliquer pourquoi l'interaction est non significative

**Solution potentielle:**
1. **Simplifier la typologie**: Passer de 8 à 4 profils
   - Éco cohérent (idéal + comportement alignés écologiques)
   - Carbone cohérent (idéal + comportement alignés carbone)
   - Aspiration contrainte (idéal éco + comportement carbone)
   - Pratique non assumée (idéal carbone + comportement éco)

2. **Dichotomiser la géographie**:
   - Centre (Cité-Limoilou, Ste-Foy-Sillery-Cap-Rouge) vs Périphérie (autres)
   - Réduit de 5 à 1 degré de liberté pour l'arrondissement

3. **Modèles binaires plus parcimonieux**:
   - Prédire la cohérence (alignement idéal-comportement) plutôt que 8 profils
   - Interaction devient 3 × 1 = 3 termes au lieu de 105

#### 3. Mécanismes causaux non testés ⚠️

**Problème:** Les analyses montrent des **associations** (corrélations) mais ne testent pas les **mécanismes causaux** qui expliquent ces associations.

**Questions sans réponse:**
1. **Pourquoi l'éducation prédit les attitudes écologiques?**
   - Valeurs postmatérialistes? (Inglehart)
   - Capital culturel? (Bourdieu)
   - Exposition à l'information scientifique?
   - Auto-sélection (qui choisit de faire des études)?

2. **Pourquoi la géographie prédit les attitudes?**
   - Intérêt matériel (coûts/bénéfices du transport)?
   - Socialisation locale (normes du quartier)?
   - Auto-sélection résidentielle (choix du lieu de résidence)?
   - Exposition aux infrastructures?

3. **Le répertoire écologique est-il vraiment un médiateur?**
   - Hypothèse: arrondissement → repertoire_eco → comportement
   - Non testé formellement avec analyse de médiation

**Solution potentielle:**
1. **Analyses de médiation** (si variables médiatrices disponibles):
   - Tester si valeurs environnementales médiatisent l'effet de l'éducation
   - Tester si repertoire_eco médiatise l'effet de l'arrondissement
   - Utiliser méthode de Baron & Kenny ou approche par équations structurelles

2. **Modèles conditionnels** (pour tester contraintes structurelles):
   - Parmi ceux avec idéal écologique, qu'est-ce qui prédit la réalisation comportementale?
   - Permet de tester si le répertoire est une vraie contrainte ou si d'autres facteurs importent

3. **Analyses multiniveaux** (si données individuelles géolocalisées disponibles):
   - Niveau 1: individus
   - Niveau 2: secteurs/quartiers
   - Permet de décomposer variance individuelle vs contextuelle

#### 4. Alternative: Score continu vs profils catégoriels

**Débat:** Utiliser un score continu (0 = 100% carbone, 1 = 100% éco) plutôt que 8 catégories?

**Argument CONTRE le score continu:**
- **Perte de distinctions théoriques** entre types de tensions
  - "Éco contraint" (veut mais ne peut pas) ≠ "Éco latent" (peut mais ne veut pas vraiment)
  - Ces deux profils auraient un score similaire (~0.5) mais représentent des dynamiques très différentes
- **Assume une linéarité fausse**
  - Est-ce qu'augmenter de 0.3 à 0.4 a le même sens que de 0.7 à 0.8?
  - Les transitions qualitatives (seuils) sont ignorées
- **Gomme la richesse conceptuelle** de l'approche culturelle
  - L'intérêt théorique est justement dans les **types** de cultures, pas un continuum
- **Difficile à interpréter pour l'identité**
  - Une identité est-elle un score 0.6? C'est quoi une identité "moyenne"?

**Argument POUR le score continu:**
- **Parcimonie** (1 coefficient vs 7 catégories)
- **Plus de puissance statistique** (surtout pour interactions)
- **Facilite l'interprétation** des effets marginaux
- **Évite cellules vides** (certains profils ont n < 50)

**Recommandation:** Garder l'approche catégorielle mais **simplifier à 4 profils** basés sur cohérence vs tension:
1. **Éco cohérent**: idéal éco + comportement éco (n ≈ 199)
2. **Carbone cohérent**: idéal carbone + comportement carbone (n ≈ 491)
3. **Aspiration contrainte**: idéal éco + comportement carbone (n ≈ 260)
4. **Pratique non assumée**: idéal carbone + comportement éco (n ≈ 42)

Cette simplification:
- Préserve les distinctions théoriques clés (cohérence vs dissonance)
- Augmente les effectifs par catégorie (puissance statistique)
- Facilite l'interprétation
- Permet de tester interactions avec plus de puissance

#### 5. Validité temporelle et causale ⚠️

**Problème:** Données transversales (un seul point dans le temps) limitent les inférences causales.

**Questions:**
1. **Direction de la causalité**: Est-ce que l'arrondissement influence les attitudes, ou les gens choisissent leur arrondissement selon leurs attitudes?
   - Auto-sélection résidentielle probable

2. **Durabilité des identités**: Les profils culturels sont-ils stables ou sensibles au contexte politique?
   - Dépendent-ils du débat sur le tramway (conjoncturel)?
   - Ou reflètent-ils des identités durables (critère normatif)?

3. **Généralisation**: Ces résultats s'appliquent-ils à d'autres villes? À d'autres enjeux de transport?

**Solution potentielle:**
- Données longitudinales (panel) si disponibles
- Enquête follow-up pour tester stabilité des profils
- Comparaisons avec d'autres villes (Montréal, Toronto)

---

## Analyses futures suggérées

### 1. Test du critère organisationnel (prioritaire pour théorie du clivage)

**Objectif:** Vérifier si les cultures de mobilité s'alignent avec des familles de partis distinctes.

**Analyses:**
- Régresser l'intention de vote ou l'identification partisane sur les profils culturels
- Examiner si les cultures écologiques vs carbone s'alignent avec des partis distincts
- **Si oui** → clivage complet (les 3 critères remplis)
- **Si non** → clivage latent (seulement socio-structural + normatif)

**Variables nécessaires:**
- Intention de vote (prochain scrutin municipal/provincial/fédéral)
- Identification partisane
- Vote passé

### 2. Analyses de médiation (pour comprendre mécanismes)

**Objectif:** Tester les **mécanismes causaux** qui relient géographie/éducation aux attitudes.

**Modèles à tester:**
1. **Médiation par le répertoire:**
   - H: arrondissement → repertoire_eco → cohérence idéal-comportement
   - Permet de quantifier l'effet direct vs indirect de la géographie

2. **Médiation par les valeurs:**
   - H: éducation → valeurs postmatérialistes → attitudes écologiques
   - Nécessite mesures de valeurs (si disponibles dans le sondage)

**Méthode:** Approche de Baron & Kenny ou méthode de médiation moderne (Hayes, 2013)

### 3. Modèles conditionnels (pour tester contraintes structurelles)

**Objectif:** Comprendre **qui peut réaliser** ses préférences et **pourquoi**.

**Analyses:**
1. **Parmi ceux avec idéal écologique**, qu'est-ce qui prédit la réalisation comportementale?
   - Est-ce vraiment le repertoire_eco (contrainte)?
   - Ou d'autres facteurs (coût, temps, habitudes)?

2. **Parmi ceux avec comportement écologique**, qu'est-ce qui prédit l'affirmation identitaire (idéal)?
   - Effet de l'arrondissement? De l'éducation?
   - Socialisation vs pragmatisme?

**Méthode:** Régressions logistiques stratifiées par idéal ou par comportement

### 4. Simplification de la typologie (recommandé avant autres analyses)

**Objectif:** Résoudre le problème de circularité et augmenter la puissance statistique.

**Nouvelle typologie proposée (4 profils sans répertoire):**
```r
profil_culture_simple = case_when(
  ideal_eco_bin == 1 & comp_transport_eco == 1 ~ "Éco cohérent",
  ideal_eco_bin == 0 & comp_transport_eco == 0 ~ "Carbone cohérent",
  ideal_eco_bin == 1 & comp_transport_eco == 0 ~ "Aspiration contrainte",
  ideal_eco_bin == 0 & comp_transport_eco == 1 ~ "Pratique non assumée"
)
```

**Ensuite:**
- Tester si `repertoire_eco` prédit l'appartenance aux profils (surtout "Aspiration contrainte")
- Tester interaction éducation × arrondissement avec plus de puissance
- Analyses de médiation: arrondissement → repertoire_eco → profil

### 5. Analyse de la polarisation affective (dimension émotionnelle)

**Objectif:** Mesurer l'intensité du clivage au-delà des attitudes.

**Questions:**
- Y a-t-il de l'**hostilité** entre groupes (éco vs carbone)?
- Les gens se **définissent-ils** par rapport à leur mode de transport?
- Y a-t-il des **stéréotypes** associés aux différentes cultures?

**Variables nécessaires:**
- Sentiment thermometer (attitudes envers les "cyclistes", "automobilistes", etc.)
- Questions d'identification ("Je me considère comme...")
- Perceptions des groupes opposés

### 6. Comparaisons spatiales et temporelles

**Objectif:** Tester la généralisation et la durabilité du clivage.

**Analyses:**
1. **Comparaisons avec d'autres villes** (Montréal, Toronto, Vancouver)
   - Le clivage est-il spécifique à Québec ou généralisable?

2. **Analyse longitudinale** (si données panel disponibles)
   - Les profils culturels sont-ils stables dans le temps?
   - Le clivage se renforce-t-il ou s'atténue-t-il?

3. **Analyse d'événements** (avant/après tramway)
   - Le débat sur le tramway a-t-il créé ou révélé le clivage?

---

## Références théoriques clés

### Articles dans `_SharedFolder_culture_carbone/theory`

1. **Bartolini, S., & Mair, P. (1990).** *Identity, Competition and Electoral Availability: The Stabilisation of European Electorates 1885-1985.* Cambridge University Press.
   - **Contribution:** Définition canonique des trois critères du clivage (socio-structural, normatif, organisationnel)
   - **Distinction clé:** Clivage complet vs latent
   - **Relevance:** Cadre théorique principal de cette étude

2. **"Lipset and Rokkan meet data: The electoral structuring of traditional cleavages 1870-1967.pdf"**
   - **Contribution:** Approche empirique et quantitative des clivages classiques de Rokkan
   - **Méthodologie:** Comment tester la structuration électorale des divisions sociales
   - **Relevance:** Modèle méthodologique pour opérationnaliser les clivages

3. **"Bringing Rokkan into the twenty-first century: The cleavage structure of Western Europe.pdf"**
   - **Contribution:** Réinterprétation contemporaine des clivages classiques
   - **Discussion:** Nouveaux clivages émergents (environnement, postmatérialisme, immigration)
   - **Relevance:** Situe le transport comme clivage moderne potentiel, dans la lignée des transformations post-industrielles

4. **Hooghe_Marks_2025.pdf**
   - **Contribution:** Recherche récente sur les structures de clivage
   - **Possible contenu:** Clivages GAL-TAN (Green/Alternative/Libertarian vs Traditional/Authoritarian/Nationalist)
   - **Relevance:** Cadre contemporain pour comprendre les divisions culturelles autour de l'environnement

### Autres références conceptuelles importantes

- **Rokkan, S. (1970).** *Citizens, Elections, Parties.* Oslo: Universitetsforlaget.
  - **Clivages historiques fondateurs:** centre-périphérie, État-Église, rural-urbain, propriétaires-travailleurs
  - **Contribution:** Théorie des "frozen cleavages" (clivages gelés) qui structurent durablement la politique européenne
  - **Relevance:** Le centre-périphérie réinterprété à l'échelle urbaine (H1)

- **Lijphart, A. (1999).** *Patterns of Democracy.* New Haven: Yale University Press.
  - **Contribution:** Distinction clivages recoupants (overlapping) vs transversaux (crosscutting)
  - **Impact:** Clivages recoupants → polarisation accrue; clivages transversaux → modération
  - **Relevance:** Analyse de l'interaction géographie × éducation (H3) et implications pour polarisation

- **Inglehart, R. (1977).** *The Silent Revolution: Changing Values and Political Styles Among Western Publics.* Princeton University Press.
  - **Théorie:** Transition matérialiste-postmatérialiste dans les sociétés post-industrielles
  - **Contribution:** L'écologie comme valeur postmatérialiste, liée à sécurité économique et éducation
  - **Relevance:** Explique pourquoi l'éducation prédit les attitudes écologiques (H2)

- **Bourdieu, P. (1979).** *La Distinction. Critique sociale du jugement.* Paris: Éditions de Minuit.
  - **Théorie:** Capital culturel et habitus de classe
  - **Contribution:** Les goûts et pratiques (dont la mobilité) comme marqueurs de classe sociale
  - **Relevance:** L'éducation comme proxy du capital culturel; choix de transport comme distinction sociale

---

## Résumé des fichiers de données et scripts

### Données principales

| Fichier | Description | Variables clés |
|---|---|---|
| `_SharedFolder_culture_carbone/data/dataset.csv` | Données brutes du sondage | Variables originales (Q1, Q3, Q10, Q11, Q12, Q15, SECT, etc.) |
| `_SharedFolder_culture_carbone/data/data_clean.rds` | Données nettoyées (output de `0.1_cleaning.R`) | `ses_*`, `ideal_eco_bin`, `comp_transport_eco`, `POND` |
| `_SharedFolder_culture_carbone/data/data_scores.rds` | Scores de mobilité par secteur | `walk_score`, `bike_score`, `transit_score` par arrondissement |
| `_SharedFolder_culture_carbone/data/data_culture.rds` | Données avec profils culturels (output de `2_indice_culture.R`) | `profil_culture`, `repertoire_eco`, `sense_making` |

### Scripts d'analyse

#### 1. `script/0.1_cleaning.R` - Nettoyage et recodage
**Objectif:** Transformer les données brutes en variables analytiques.

**Transformations principales:**
- **SES:** Recodage des variables sociodémographiques (âge, genre, éducation, revenu, propriété, arrondissement)
- **Transport:**
  - `main_transport_3`: Catégorisation en 3 modes (Auto, Transport collectif, Actif)
  - `comp_transport_eco`: Binaire (1 = TC ou Actif, 0 = Auto)
- **Idéal:**
  - `ideal`: Mode idéal en 4 catégories
  - `ideal_eco_bin`: Binaire (1 = TC/Marche/Vélo, 0 = Auto)
- **Attitudes:**
  - `op_tram_num`, `op_tram_bin`: Opinion sur le tramway
  - `op_rtc_*`, `op_velo_*`: Opinions sur autres infrastructures
- **Pondération:** `POND` (préservé du sondage original)

**Output:** `data_clean.rds` (n ≈ 1000 observations)

#### 2. `script/2_indice_culture.R` - Construction des profils culturels
**Objectif:** Créer les profils culturels basés sur idéal × comportement × répertoire.

**Étapes principales:**

1. **Calcul du répertoire écologique par arrondissement (lignes 12-35):**
```r
repertoire_arrondissement <- data_scores %>%
  group_by(arrondissement) %>%
  summarise(
    walk_score = sum(walk_score * population) / sum(population),
    transit_score = sum(transit_score * population) / sum(population),
    bike_score = sum(bike_score * population) / sum(population)
  ) %>%
  mutate(
    repertoire_eco = 0.4 * transit_norm + 0.3 * walk_norm + 0.3 * bike_norm
  )
```

2. **Catégorisation du répertoire (lignes 64-74):**
```r
repertoire_cat = case_when(
  repertoire_eco >= 0.70 ~ "Élevé",
  repertoire_eco >= 0.50 ~ "Modéré",
  repertoire_eco < 0.50 ~ "Faible"
)
```

3. **Création de la typologie culturelle (lignes 78-116):**
- 8 profils basés sur `ideal_eco_bin × comp_transport_eco × repertoire_cat`
- Voir section H3 pour détails

4. **Score de sense-making (lignes 132-166):**
```r
sense_making = congruence * facilitation
```
- `congruence`: 1 si idéal = comportement, 0 sinon
- `facilitation`: Score (0-1) selon compatibilité avec répertoire

**Output:** `data_culture.rds`

#### 3. `script/4_regression.R` - Analyses de régression principale
**Objectif:** Tester H1, H2, et H3 avec différents modèles.

**Modèles implémentés:**

**Modèle 1: Régression logistique ordinale (lignes 74-79)**
```r
model_ord <- polr(op_tram_ord ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                ses_femme_fct + ses_proprio_fct + ses_arrondissement_fct,
                  data = data_reg,
                  weights = POND,
                  Hess = TRUE)
```
- VD: `op_tram_ord` (4 niveaux ordonnés: Opposé, Plutôt opposé, Plutôt favorable, Favorable)
- Package: `MASS::polr`
- Test H1 et H2 sur attitude ordinale

**Modèle 2: Régression logistique binaire (lignes 134-139)**
```r
model_bin <- glm(op_tram_bin ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                               ses_femme_fct + ses_proprio_fct + ses_arrondissement_fct,
                data = data_reg,
                weights = POND,
                family = binomial(link = "logit"))
```
- VD: `op_tram_bin` (Favorable vs Opposé)
- Test H1 et H2 avec dichotomisation

**Modèle 3: Régression logistique multinomiale (lignes 420-425)**
```r
model_profils <- multinom(profil_culture ~ ses_educ_fct + ses_revenu_fct + ses_age_c +
                                           ses_femme_fct + ses_proprio_fct + ses_arrondissement_fct,
                          data = data_profils,
                          weights = POND,
                          trace = FALSE)
```
- VD: `profil_culture` (8 catégories)
- Package: `nnet::multinom`
- Catégorie de référence: "Carbone normalisé" (modal)
- Test H3 avec effets additifs

**Modèle 4: Modèle 3 avec interaction (lignes 439-445)**
```r
model_profils_inter <- multinom(profil_culture ~ ses_educ_fct * ses_arrondissement_fct +
                                                 ses_revenu_fct + ses_age_c +
                                                 ses_femme_fct + ses_proprio_fct,
                                data = data_profils,
                                weights = POND,
                                trace = FALSE,
                                maxit = 500)
```
- Test H3 avec interaction éducation × arrondissement
- Test du rapport de vraisemblance pour comparer avec Modèle 3

**Graphiques générés:**
1. `odds_ratios_tramway.png`: Forest plot des OR (Modèle 1)
2. `prob_pred_education.png`: Probabilités prédites par éducation (Modèle 1)
3. `prob_pred_arrondissement.png`: Probabilités prédites par arrondissement (Modèle 1)
4. `profils_culture_education.png`: Distribution des profils par éducation (Modèle 3)
5. `profils_culture_arrondissement.png`: Distribution des profils par arrondissement (Modèle 3)

### Variables clés

#### Variables sociodémographiques (SES)
| Variable | Type | Niveaux/Description | Source |
|---|---|---|---|
| `ses_arrondissement` | Factor | Haute-St-Charles, Beauport, Charlesbourg, Cité-Limoilou, Les Rivières, Ste-Foy-Sillery-Cap-Rouge | SECT |
| `ses_age` | Ordered Factor | -18, 18_24, 25_34, 35_44, 45_54, 55_64, 65_74, 75+ | age |
| `ses_age_c` | Numeric | Âge centré (pas standardisé) pour régression | Transformé |
| `ses_femme` | Binary | 0 = Homme/Autre, 1 = Femme | GENRE |
| `ses_educ` | Ordered Factor | Secondaire, Collegial/Certificat, Bacc, Graduate | SCOL |
| `ses_revenu` | Ordered Factor | 9 catégories (-20k à 160k+) | REVEN |
| `ses_revenu_grouped` | Ordered Factor | 5 catégories (<40k, 40-79k, 80-119k, 120-159k, 160k+) | Regroupé |
| `ses_proprio` | Binary | 0 = Locataire, 1 = Propriétaire | PROP |
| `ses_occup` | Factor | Travail, Études, Sans emploi, Retraite | EMPLO2 |

#### Variables de transport
| Variable | Type | Niveaux/Description | Source |
|---|---|---|---|
| `main_transport` | Factor | 10 modes (Auto conducteur, Auto passager, Autobus, Marche, Vélo personnel, etc.) | Q1r1-Q1r10 |
| `main_transport_3` | Factor | Auto, Transport collectif, Actif | Regroupé |
| `comp_transport_eco` | Binary | 0 = Auto, 1 = TC ou Actif | Dérivé |
| `freq_bus3` | Ordered Factor | Régulier, Occasionnel, Jamais | Q3 |

#### Variables d'idéal et d'attitudes
| Variable | Type | Niveaux/Description | Source |
|---|---|---|---|
| `ideal` | Factor | Auto, Transport en commun, Marche, Vélo | Q11 |
| `ideal_eco_bin` | Binary | 0 = Auto, 1 = TC/Marche/Vélo | Dérivé |
| `op_tram_num` | Numeric | 1-4 (Très mauvaise à Très bonne) | Q10r3 |
| `op_tram_ord` | Ordered Factor | Opposé, Plutôt opposé, Plutôt favorable, Favorable | Dérivé |
| `op_tram_bin` | Factor | Défavorable, Favorable | Dérivé |
| `know_tram` | Binary | 0 = Ne sait pas, 1 = A une opinion | Dérivé |

#### Variables de répertoire et culture
| Variable | Type | Description | Source |
|---|---|---|---|
| `repertoire_eco` | Numeric | Score 0-1 (répertoire écologique de l'arrondissement) | Calculé (data_scores) |
| `repertoire_cat` | Ordered Factor | Faible, Modéré, Élevé | Dérivé (seuils 0.5, 0.7) |
| `walk_score` | Numeric | Score de marchabilité (0-100) | data_scores |
| `transit_score` | Numeric | Score de transport collectif (0-100) | data_scores |
| `bike_score` | Numeric | Score de cyclabilité (0-100) | data_scores |
| `profil_culture` | Factor | 8 profils (voir H3) | Calculé (2_indice_culture.R) |
| `sense_making` | Numeric | Score 0-1 de cohérence idéal-comportement-répertoire | Calculé |
| `sense_making_cat` | Ordered Factor | Très faible, Faible, Modéré, Fort | Dérivé |

#### Variable de pondération
| Variable | Type | Description |
|---|---|---|
| `POND` | Numeric | Poids du sondage pour représentativité de la population |

---

## Interprétation des résultats principaux

### Graphique 1: Odds Ratios (Forest Plot)
**Fichier:** `odds_ratios_tramway.png`

**Interprétation factuelle:**
- **Éducation (vs Secondaire):**
  - Bacc: OR = 2.81 → Les détenteurs d'un baccalauréat ont **2.8 fois plus de chances** d'être favorables au tramway
  - Graduate: OR = 3.88 → Les détenteurs d'études graduées ont **3.9 fois plus de chances** d'être favorables
  - **Effet dose-réponse clair:** Plus l'éducation augmente, plus l'attitude est favorable

- **Arrondissement (vs Haute-St-Charles):**
  - **Ste-Foy-Sillery-Cap-Rouge: OR = 2.42** (le plus fort effet géographique)
  - **Cité-Limoilou: OR = 1.63**
  - Tous les autres arrondissements: non significatifs

- **Propriété:**
  - Propriétaire: OR = 0.62 → Les propriétaires sont significativement **moins favorables** au tramway
  - Peut refléter des préoccupations sur l'impact du chantier, la taxation, ou la valeur immobilière

- **Revenu:** Aucun effet significatif après contrôle de l'éducation
  - Suggère que c'est l'éducation (capital culturel) plutôt que le revenu (capital économique) qui structure les attitudes

### Graphique 2: Probabilités prédites par éducation
**Fichier:** `prob_pred_education.png`

**Interprétation factuelle:**
- **Gradient éducationnel marqué:**
  - Études graduées: 40% "Favorable" (le plus haut)
  - Bacc: 33% "Favorable"
  - Collégial: 17% "Favorable"
  - Secondaire: 15% "Favorable"

- **Polarisation selon l'éducation:**
  - Secondaire: 47% "Opposé" vs 15% "Favorable" → majorité opposée
  - Graduate: 19% "Opposé" vs 40% "Favorable" → majorité favorable
  - **Inversion complète** des attitudes selon l'éducation

### Graphique 3: Probabilités prédites par arrondissement
**Fichier:** `prob_pred_arrondissement.png`

**Interprétation factuelle:**
- **Ste-Foy-Sillery-Cap-Rouge: 23.5% "Favorable"** (le plus haut)
  - Arrondissement le plus pro-tramway
  - Population éduquée, aisée, services de proximité

- **Cité-Limoilou: 17.2% "Favorable"**
  - Deuxième arrondissement le plus favorable
  - Quartier central, dense, bien desservi

- **Les Rivières: 10.5% "Favorable"** (le plus bas)
  - Arrondissement le moins favorable
  - Périphérique, dépendant de l'auto

- **Division centre-périphérie claire:**
  - Centre (Ste-Foy, Cité-Limoilou): 17-24% favorables
  - Périphérie (autres): 10-12% favorables

### Graphique 4: Distribution des profils par éducation
**Fichier:** `profils_culture_education.png`

**Interprétation factuelle:**
- **Secondaire et Collégial:** Dominés par "Carbone normalisé" et "Éco contraint"
  - Majorité a un idéal carbone OU est empêchée de réaliser son idéal écologique

- **Bacc et Graduate:** Plus de diversité, forte présence de "Éco normalisé"
  - Graduate: 56.5% dans profils écologiques cohérents
  - Capacité et volonté de réaliser un idéal écologique

- **Profil "Éco contraint":** Présent dans tous les niveaux d'éducation
  - Même chez les éduqués, certains veulent mais ne peuvent pas (contraintes contextuelles)

### Graphique 5: Distribution des profils par arrondissement
**Fichier:** `profils_culture_arrondissement.png`

**Interprétation factuelle:**
- **Arrondissements périphériques** (Haute-St-Charles, Beauport, Charlesbourg, Les Rivières):
  - 60-67% "Carbone normalisé" (idéal et comportement carbone)
  - Cohérence idéal-comportement due à faible répertoire écologique

- **Ste-Foy-Sillery-Cap-Rouge:**
  - Plus de diversité de profils
  - Présence notable de "Éco limité" (23%) → veulent et peuvent, malgré répertoire modéré
  - Plus faible proportion de "Carbone normalisé" (37%)

- **Cité-Limoilou:**
  - Plus de profils écologiques cohérents
  - Mais aussi "Carbone résistant" (45%) → résistance active malgré répertoire élevé

---

## Conclusions principales

### Ce que nous savons maintenant

1. **Il existe un clivage socio-structural clair** (H1 et H2 validées)
   - **Division géographique:** Ste-Foy-Sillery-Cap-Rouge et Cité-Limoilou vs arrondissements périphériques
   - **Division éducationnelle:** Diplômés universitaires vs diplômés secondaires
   - Ces divisions sont **robustes** (observées dans tous les modèles)

2. **Il existe des cultures de mobilité distinctes** (H3 partiellement validée)
   - 8 profils identifiés basés sur idéal × comportement × répertoire
   - Ces profils sont **structurés** par l'éducation et la géographie (effets additifs)
   - Suggère l'émergence d'**identités collectives** autour de la mobilité

3. **Le transport pourrait être un clivage latent à Québec**
   - Critère socio-structural: ✅ validé
   - Critère normatif: ✅ partiellement validé (identités en formation)
   - Critère organisationnel: ❓ non testé (nécessite analyse de l'alignement partisan)

### Ce que nous ne savons pas encore

1. **Les mécanismes causaux:**
   - **Pourquoi** l'éducation structure les attitudes? (valeurs? information? auto-sélection?)
   - **Pourquoi** la géographie structure les attitudes? (intérêt? socialisation? auto-sélection?)
   - Le répertoire est-il vraiment un **médiateur** ou juste un corrélat?

2. **La durabilité des identités:**
   - Les profils culturels sont-ils **stables** dans le temps?
   - Sont-ils spécifiques au débat sur le tramway ou reflètent-ils des identités durables?

3. **L'alignement politique:**
   - Les cultures de mobilité s'alignent-elles avec des partis distincts?
   - Si oui → clivage complet; si non → reste latent

4. **La polarisation affective:**
   - Y a-t-il de l'**hostilité** entre groupes (éco vs carbone)?
   - Les identités sont-elles émotionnellement investies?

### Implications pour la théorie des clivages

1. **Le transport comme nouveau clivage post-industriel:**
   - S'inscrit dans la transformation des structures de clivage au 21e siècle
   - Lié aux valeurs postmatérialistes (Inglehart) et à l'éducation (capital culturel)
   - Pourrait être une **expression contemporaine** du clivage centre-périphérie de Rokkan

2. **Structure additive plutôt que multiplicative:**
   - Géographie + Éducation (effets additifs) plutôt que Géographie × Éducation (interaction)
   - Selon Lijphart: **atténue la polarisation** (loyautés croisées possibles)
   - Mais crée aussi des "perdants doubles" (peu éduqués en périphérie) et "gagnants doubles" (éduqués au centre)

3. **Importance du contexte urbain:**
   - Le clivage est **spatialisé** à l'échelle de la ville
   - Le répertoire écologique (infrastructures) structure les possibilités d'action
   - Pose la question de l'équité spatiale et de l'accès aux choix de mobilité

---

## Notes méthodologiques pour analyses futures

### Améliorations prioritaires

1. ✅ **Simplifier la typologie culturelle** (8 → 4 profils)
   - Résout le problème de circularité
   - Augmente la puissance statistique
   - Préserve les distinctions théoriques clés

2. ✅ **Tester la médiation par le répertoire**
   - Modèle: arrondissement → repertoire_eco → cohérence
   - Permet de quantifier mécanismes causaux

3. ✅ **Tester l'alignement partisan** (critère organisationnel)
   - Nécessaire pour conclure sur clivage complet vs latent

4. ⚠️ **Attention aux inférences causales**
   - Données transversales → associations, pas causalité
   - Auto-sélection résidentielle probable (endogénéité)
   - Besoin de données longitudinales ou quasi-expérimentales pour causalité forte

### Bonnes pratiques confirmées

1. ✅ **Pondération systématique** (POND dans tous les modèles)
2. ✅ **Triangulation des mesures** (ordinale, binaire, multinomiale)
3. ✅ **Tests rigoureux** (rapport de vraisemblance, intervalles de confiance)
4. ✅ **Visualisations claires** (forest plots, probabilités prédites, distributions)

---

*Document créé: 2025-10-11*
*Dernière mise à jour: 2025-10-11*
*Version: 1.0 (corrigée pour exactitude factuelle)*
