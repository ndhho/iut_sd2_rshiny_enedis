# Étude du DPE et des consommations électriques de logements type studio à Lyon vs Lille

## 🎯 Titre du projet
**Analyse comparative des consommations électriques selon le DPE des studios à Lyon et Lille**

---

## 🧩 Problématique
Dans quelle mesure la **classe énergétique (DPE)** influence-t-elle les **consommations électriques moyennes** des studios situés à **Lyon** et **Lille** selon le **type de logement** (neuf / ancien) ?

---

## 🎯 Objectifs du projet

1. **Comparer** les consommations électriques moyennes des studios selon leur **classe DPE** entre Lyon et Lille.  
2. **Identifier** les différences **climatiques** entre les deux villes pouvant expliquer les écarts observés.  
3. **Visualiser** les résultats sous forme de **KPI**, **graphiques interactifs** et **cartes** à l’aide de l’application **R Shiny**.  
4. **Modéliser** une **régression simple** entre la consommation et la classe énergétique, illustrée par un **boxplot**.

---

## 🧠 Méthodologie
- Données issues des **API ADEME** :
  - [DPE logements neufs](https://data.ademe.fr/datasets/dpe02neuf)
  - [DPE logements existants](https://data.ademe.fr/datasets/dpe03existant)
- Filtrage des **logements de type studio** localisés à **Lyon (69)** et **Lille (59)**.  
- Calcul et comparaison des **consommations moyennes (kWh/m².an)** par **classe DPE**.  
- Analyse statistique et **visualisation** sous **R Markdown** et **R Shiny**.  

---

## 📈 Livrable – Chapitre 4 : Rapport d’étude (R Markdown)
Le rapport présente :
- Une **analyse descriptive** des consommations électriques selon le DPE.  
- Des **graphiques comparatifs** (histogrammes, boxplots, nuages de points).  
- Une **modélisation par régression linéaire simple** entre la consommation et la classe énergétique.  
- Une **interprétation des résultats** en lien avec les différences climatiques entre Lyon et Lille.  

Le rapport est rédigé en **R Markdown**, exporté au format **HTML**, et intégré dans le dépôt GitHub du projet.

---

## ⚙️ Technologies utilisées
- **R / RStudio**
- **R Markdown**
- **R Shiny** (déploiement sur [shinyapps.io](https://www.shinyapps.io))
- **GitHub Desktop**
- **Draw.io** (schéma d’architecture)
- **CapCut** (captation vidéo de démonstration)
- **ChatGPT** (assistance rédaction et code)

---

## 🧾 Auteurs
Projet réalisé dans le cadre du module **RShiny – IUT SD2 2024/2025 – GreenTech Solutions**  
- **Équipe :** Anthony & [Nom du binôme]  
- **Client fictif :** Enedis  

---

> _« Vers une meilleure compréhension de la performance énergétique des logements étudiants dans un contexte de sobriété énergétique. »_
