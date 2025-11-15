-----

### `Technical Documentation`

````markdown
# Documentation Technique : Analyse DPE Lyon vs Lille

Ce document détaille l'architecture technique, les dépendances et la procédure d'installation de l'application Shiny d'analyse des DPE.

## 1. Schéma de l'architecture

L'application repose sur une architecture "gated" (à accès contrôlé) où la logique principale du serveur n'est chargée qu'après une authentification réussie. Les données sont chargées au démarrage depuis des sources externes (GitHub), et les coordonnées sont converties une seule fois.

Le schéma ci-dessous illustre le flux d'interaction entre l'utilisateur, l'interface (UI), le serveur (Server) et les sources de données :

```mermaid
graph TD
    subgraph "Utilisateur (Navigateur)"
        A[Interface UI - Connexion] -- Identifiants --> S(Serveur)
        B[Interface UI - Dashboard] <--> S
        C[Filtres (Inputs)] --> S
        S -- Met à jour --> D[Graphiques (Outputs)]
        S -- Met à jour --> E[Carte (Outputs)]
    end
    
    subgraph "Serveur (app.R)"
        S -- 1. Authentifie --> S1[Logique de Connexion]
        S -- 2. Exécute --> S2[Logique Applicative]
        S2 -- Lit --> F[Données (data_combined)]
    end
    
    subgraph "Source de Données (Externe)"
        G[CSV Lyon sur GitHub] --> F
        H[CSV Lille sur GitHub] --> F
    end
````

## 2\. Packages Nécessaires

L'application repose sur les 9 packages R suivants :

  * **shiny** : Framework principal de l'application.
  * **shinydashboard** : Structure le tableau de bord (header, sidebar, body).
  * **dplyr** : Utilise pour toutes les manipulations de données (filtres, `group_by`, `summarise`).
  * **ggplot2** : Moteur de génération pour tous les graphiques (histogrammes, barres, nuages de points...).
  * **scales** : Permet de formater les axes et les étiquettes (ex: `percent()`).
  * **leaflet** : Génère la carte interactive (points, légendes, popups).
  * **DT** : Affiche les tables de données interactives (onglet "Contexte").
  * **sf** : Requis pour la conversion des coordonnées géographiques (Lambert-93 vers WGS84).
  * **shinythemes** : Permet le changement de thème par l'utilisateur.

## 3\. Guide d'installation

Voici les étapes pour installer et lancer l'application sur un poste local.

### 3.1. Prérequis

  * **R** (version 4.0 ou supérieure).
  * **RStudio** (recommandé).

### 3.2. Installation des packages

1.  Ouvrez RStudio et copiez-collez la commande suivante dans la console pour installer tous les packages requis :

    ```r
    install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", 
                       "scales", "leaflet", "DT", "sf", 
                       "shinythemes"))
    ```

2.  **Note importante pour `sf`** : Ce package peut avoir des dépendances système (comme `GDAL` ou `GEOS`). Si l'installation échoue, suivez les instructions spécifiques à votre système d'exploitation (Windows, macOS, ou Linux) disponibles sur [la page du package `sf`](https://www.google.com/search?q=https://r-spatial.github.io/sf/%23installing).

### 3.3. Lancement de l'application

1.  Créez un nouveau dossier pour votre projet (ex: `MonProjetShiny`).

2.  À l'intérieur de ce dossier, créez un fichier nommé `app.R` et copiez-y l'intégralité du script de l'application.

3.  Dans ce même dossier, créez un sous-dossier nommé **`www`**.

4.  À l'intérieur du dossier `www`, créez un fichier nommé **`style.css`**.

5.  Copiez le code CSS suivant dans `www/style.css` (Ceci est la charte graphique Enedis) :

    ```css
    /* --- Charte Visuelle "ENEDIS" --- */
    .skin-blue .main-header .navbar { background-color: #005DB9; }
    .skin-blue .main-header .logo { background-color: #004B9A; color: #FFFFFF; font-weight: bold; }
    .skin-blue .main-header .logo:hover { background-color: #003A75; }
    .skin-blue .main-sidebar { background-color: #2F2F2F; }
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { background-color: #222222; border-left-color: #92C814; }
    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover { background-color: #3e3e3e; border-left-color: #005DB9; }
    .box.box-solid.box-primary > .box-header { background: #005DB9; border-color: #005DB9; }
    .box.box-solid.box-info > .box-header { background: #9E9E9E; border-color: #9E9E9E; }
    .box.box-solid.box-warning > .box-header { background: #92C814; border-color: #92C814; color: #2F2F2F; }
    .small-box.bg-blue { background-color: #005DB9 !important; }
    .small-box.bg-green { background-color: #92C814 !important; color: #2F2F2F !important; }
    ```

6.  Ouvrez le fichier `app.R` dans RStudio.

7.  Cliquez sur le bouton **"Run App"** en haut à droite de l'éditeur de script.

8.  L'application se lancera et vous présentera l'écran de connexion.

### 3.4. Identifiants de connexion

  * **Utilisateur :** `admin`
  * **Mot de passe :** `admin`

<!-- end list -->

```
```