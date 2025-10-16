# Conversion Odds Ratios → Marginal Effects

## 📊 Modifications effectuées

Tous les scripts de régression ont été modifiés pour utiliser des **Average Marginal Effects (AME)** au lieu des odds ratios.

---

## 🎯 Pourquoi les marginal effects ?

### Problèmes avec les odds ratios
1. **Interprétation non intuitive** : OR = 2.5 ne signifie PAS "2.5× plus de probabilité"
2. **Échelle non linéaire** : Nécessite une échelle logarithmique pour visualiser
3. **Non-collapsibilité** : Les OR changent selon les contrôles inclus
4. **Difficulté de communication** : Peu accessible pour les décideurs publics

### Avantages des marginal effects
1. **Interprétation directe** : Changement en points de pourcentage de probabilité
2. **Échelle linéaire** : Facile à visualiser et comparer
3. **Communication claire** : "L'éducation universitaire augmente la probabilité de +23%"
4. **Comparabilité** : On peut directement comparer l'ampleur des effets

---

## 📁 Fichiers modifiés

### 1. `4_regression.R` - Position tramway (ordinale)
**Avant** : Odds ratios avec échelle logarithmique
**Après** : Average Marginal Effects en points de pourcentage

**Changements** :
- Section 4 : Calcul des AME avec `slopes()` pour modèle ordinal
- Section 5 : AME avec `avg_slopes()` pour modèle binaire
- Section 7.1 : Forest plot avec AME (axe X linéaire, centré sur 0)

**Graphiques générés** :
- `marginal_effects_tramway.png` (au lieu de `odds_ratios_tramway.png`)

**Exemple d'interprétation** :
```
AME = +18.5 [12.3, 24.7]
→ "Avoir un diplôme universitaire (vs secondaire) augmente la probabilité
   d'être favorable au tramway de 18.5 points de pourcentage"
```

---

### 2. `4.2_regression_compl.R` - Idéal & Comportement (binaires)
**Avant** : Odds ratios avec échelle logarithmique
**Après** : Average Marginal Effects en points de pourcentage

**Changements** :
- Calcul des AME pour `ideal_eco_bin` et `comp_transport_eco`
- Fonction `create_forest_plot()` réécrite pour AME
- Comparaison idéal vs comportement avec différences d'AME

**Graphiques générés** :
- `marginal_effects_ideal_eco.png`
- `marginal_effects_comportement_eco.png`

**Exemple d'interprétation** :
```
AME idéal = +15.2 [9.8, 20.6]
AME comportement = +8.3 [3.1, 13.5]
Différence = +6.9 points de %

→ "L'éducation a un effet plus fort sur l'idéal (+15%) que sur le
   comportement (+8%), suggérant une contrainte structurelle"
```

---

### 3. `5_profils_simplifies.R` - Profils culturels (multinomial)
**Avant** : Odds ratios par profil avec échelle logarithmique
**Après** : Average Marginal Effects par profil en points de pourcentage

**Changements** :
- Fonction `create_forest_plot()` utilise maintenant `avg_slopes()` avec `by = "group"`
- Calcul des AME spécifiques pour chaque profil culturel
- Forest plots avec axe X linéaire

**Graphiques générés** :
- `marginal_effects_eco_aligne.png`
- `marginal_effects_eco_contraint.png`
- `marginal_effects_carbone_atypique.png`

**Exemple d'interprétation** :
```
Profil "Éco aligné" :
AME Graduate = +12.4 [7.8, 17.0]

→ "Avoir un diplôme d'études graduées (vs secondaire) augmente la
   probabilité d'être dans le profil 'Éco aligné' de 12.4 points de %"
```

---

## 🔧 Utilisation des fonctions `marginaleffects`

### ⚠️ IMPORTANT : Utiliser `avg_comparisons()` pour TOUS les modèles !

La fonction `avg_comparisons()` fonctionne universellement pour tous les types de modèles.

### Pour modèles binaires (logit / glm)
```r
ame <- avg_comparisons(model,
                       variables = list(
                         educ = "reference",  # Compare chaque niveau à la référence
                         age = "sd"  # Effet d'1 écart-type
                       ),
                       type = "response")  # Probabilités
```

### Pour modèles ordinaux (polr)
```r
ame <- avg_comparisons(model,
                       variables = list(
                         educ = "reference",
                         age = "sd"
                       ),
                       type = "probs",
                       by = "group") %>%  # Par niveau de VD
  filter(group == "Favorable")  # Filtrer le niveau d'intérêt
```

### Pour modèles multinomiaux (multinom)
```r
ame <- avg_comparisons(model,
                       variables = list(
                         educ = "reference",
                         age = "sd"
                       ),
                       type = "probs",
                       by = "group") %>%  # Par catégorie
  filter(group == "Éco aligné")  # Filtrer la catégorie
```

### ⚠️ Pourquoi `avg_comparisons()` et pas `avg_slopes()` ?

- `avg_slopes()` ne fonctionne qu'avec des variables continues
- Pour les variables catégorielles, il faut utiliser `avg_comparisons()`
- `avg_comparisons()` fonctionne pour tous les types de variables → **plus simple et universel**

### ⚠️ Conflit MASS::select
Attention ! Le package `MASS` (chargé pour `polr`) masque `dplyr::select`.
**Toujours utiliser `dplyr::select()` explicitement** dans les pipelines !

---

## 📊 Interprétation des AME

### Lecture du graphique
- **Axe X** : Changement de probabilité en **points de pourcentage** (-100 à +100)
- **Ligne de référence** : 0 (pas d'effet)
- **Barre d'erreur** : Intervalle de confiance à 95%
- **Significativité** : Si l'IC ne croise pas 0 → p < 0.05

### Exemple concret
```
Variable : Graduate (vs Secondaire)
AME = +18.5 [12.3, 24.7]

Interprétation :
"Par rapport à quelqu'un avec un diplôme secondaire, une personne
avec un diplôme d'études graduées a une probabilité 18.5 points de
pourcentage plus élevée d'être favorable au tramway (toutes choses
égales par ailleurs : revenu, arrondissement, âge, etc.)"

Si probabilité de base = 40% pour secondaire
→ Probabilité pour Graduate = 58.5%
```

---

## ✅ Vérification

Pour vérifier que les modifications fonctionnent, exécute chaque script :

```r
source("script/4_regression.R")
source("script/4.2_regression_compl.R")
source("script/5_profils_simplifies.R")
```

Tu devrais obtenir :
- Des tableaux d'AME en points de pourcentage
- Des forest plots avec axe X linéaire centré sur 0
- Des graphiques plus faciles à interpréter

---

## 📚 Ressources

- Documentation `marginaleffects` : https://vincentarelbundock.github.io/marginaleffects/
- Article de référence : Mize, T. D., Doan, L., & Long, J. S. (2019). A general framework for comparing predictions and marginal effects across models. *Sociological Methodology*, 49(1), 152-189.

---

## 💡 Prochaines étapes suggérées

1. **Exécuter les scripts** pour générer les nouveaux graphiques
2. **Comparer** visuellement avec les anciens odds ratios
3. **Rédiger l'interprétation** pour ton mémoire/article
4. **Discuter** avec ton directeur de recherche

---

Bonne analyse ! 🎓
