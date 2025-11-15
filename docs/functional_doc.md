# Documentation Fonctionnelle : Analyse DPE Lyon vs Lille

Ce document présente l'intérêt de chaque page et les fonctionnalités majeures de l'application Shiny d'analyse des DPE pour les métropoles de Lyon (69) et Lille (59).

## 1. Fonctionnalités Majeures

L'application est conçue comme un tableau de bord interactif permettant une analyse complète et dynamique.

* **Sécurisation :** L'accès à l'application est contrôlé par un **écran d'authentification** (via `shinymanager`) qui requiert un nom d'utilisateur et un mot de passe.
* **Filtrage Global :** Un panneau latéral persistant permet à l'utilisateur d'affiner l'ensemble des données analysées dans tous les onglets selon trois axes :
    * **Géographique** (sélection de Lyon, Lille, ou les deux).
    * **Temporel** (logements neufs ou existants).
    * **Structurel** (slider pour la surface habitable en m²).
* **Analyse Multi-Graphiques :** L'application propose **5 types de graphiques** (`Diagramme en barres`, `Diagramme circulaire`, `Histogramme`, `Boîte à moustaches`, `Nuage de points`) répartis dans des onglets thématiques.
* **Interactivité Visuelle :**
    * **Carte Interactive :** L'onglet "Overview" propose une carte dynamique (`leaflet`) affichant un échantillon de logements.
    * **Analyse de Corrélation :** L'onglet "Analyse Statistique" permet à l'utilisateur de choisir n'importe quelle variable X et Y pour générer un nuage de points avec une droite de régression et le coefficient de corrélation.
* **Export :** L'utilisateur peut **télécharger les données et les graphiques**. Chaque graphique dispose d'un bouton d'export en `.png`, et les tables de données peuvent être exportées en `.csv`.
* **Personnalisation :** Un sélecteur de thème (`shinythemes`) permet à l'utilisateur d'ajuster l'apparence de l'application.

## 2. Intérêt de Chaque Page (Onglets)

L'application est structurée en cinq onglets, chacun répondant à un besoin d'analyse spécifique.

### 2.1. Onglet "Contexte"

* **Objectif :** Présenter le projet, ses objectifs et ses partenaires.
* **Contenu :**
    * **Objectif de l'application :** Un résumé du "Pourquoi" de l'application.
    * **Données :** Une description de la source des données (ADEME).
    * **Aperçu des données :** Une table interactive (`DT`) affichant un échantillon de 100 logements. L'utilisateur peut filtrer cette table et exporter le résultat.
    * **Partenaires :** Affiche les logos des commanditaires (ENEDIS, ADEME) et de l'établissement (IUT).

### 2.2. Onglet "Overview (Carte & KPIs)"

* **Objectif :** Fournir une synthèse visuelle et géographique de haut niveau des données filtrées.
* **Contenu :**
    * **KPIs (Indicateurs Clés) :** Trois `valueBox` affichent les chiffres essentiels de la sélection :
        1.  Nombre total de logements.
        2.  Taux de "passoires thermiques" (DPE F ou G).
        3.  Coût énergétique total moyen par m².
    * **Carte Interactive :** Une carte `leaflet` affichant un échantillon de logements (`sample_size`). La **couleur** des points représente le DPE (de vert à rouge), et la **taille** représente le coût total (calculé par échelle logarithmique).

### 2.3. Onglet "Analyse DPE"

* **Objectif :** Analyser en détail la performance énergétique (DPE) et la répartition des sources d'énergie.
* **Contenu :**
    * **Répartition des étiquettes DPE :** Un diagramme en barres comparant la proportion de chaque classe DPE (A à G) entre Lyon et Lille.
    * **Focus sur l'électricité :** Quatre diagrammes circulaires (Lyon/Lille vs Neuf/Existant) montrant le pourcentage de logements chauffés à l'électricité par rapport à "Autres Energies".

### 2.4. Onglet "Analyse des Coûts"

* **Objectif :** Comprendre l'impact financier du DPE pour les logements existants (le cœur du problème de rénovation).
* **Contenu :**
    * **Coût Total Moyen par m² :** Un diagramme en barres comparant le coût moyen par m² (toutes énergies confondues) pour chaque classe DPE, à Lyon et à Lille.
    * **Coût du Chauffage ÉLECTRIQUE par m² :** Le graphique clé de l'analyse. Il isole uniquement les logements existants chauffés à l'électricité et compare leur coût de chauffage au m² par classe DPE.

### 2.5. Onglet "Analyse Statistique"

* **Objectif :** Fournir des outils d'exploration statistique avancés pour répondre aux exigences du cahier des charges.
* **Contenu :**
    * **Histogramme :** Affiche la distribution (densité) des coûts au m² pour voir la concentration des valeurs.
    * **Boîte à moustaches :** Montre la médiane, les quartiles et les outliers du coût au m² pour chaque classe DPE.
    * **Nuage de points :** Visualise la relation entre la surface habitable et le coût total, en utilisant l'échantillon de la carte.
    * **Analyse de Corrélation :** Un module interactif où l'utilisateur sélectionne X et Y parmi les variables numériques clés (coûts, surface) pour générer un nuage de points, sa droite de régression et le coefficient de corrélation (r).