# Dashboard Shiny - Analyse DPE Lyon (69) vs Lille (59)

Ce dépôt contient le code source complet et la documentation du projet de dashboard Shiny réalisé pour l'IUT Lumière Lyon 2, à la demande d'**ENEDIS** et basé sur les données ouvertes de l'**ADEME**.

L'objectif de ce projet est de comparer la performance énergétique (DPE) de l'ensemble des appartements neufs et existants entre les métropoles de Lyon et Lille.

**Auteurs :** HO Huy, TRAN Cloélia

---

## Application en Direct

Vous pouvez accéder à l'application déployée ici :

**[https://akaishuychi.shinyapps.io/dpe-dashboard/](https://akaishuychi.shinyapps.io/dpe-dashboard/)**

### Identifiants de connexion
* **Utilisateur :** `admin`
* **Mot de passe :** `admin`

---

## Navigation du Dépôt

Ce dépôt contient plusieurs dossiers clés pour comprendre le projet. Pour une explication détaillée de l’application, veuillez consulter les documentations dédiées :

* **`/app/`**
    Contient le code source final de l'application Shiny (`app.R`) ainsi que les assets visuels (`www/`) et les fichiers de déploiement (`rsconnect/`).

* **`/docs/`**
    **C'est ici que se trouve la documentation complète du projet.**
    * **[technical_doc.md](https://github.com/ndhho/iut_sd2_rshiny_enedis/blob/main/docs/technical_doc.md)** :  Contient le guide d'installation, l'architecture et la liste des packages.
    * **[functional_doc.md](https://github.com/ndhho/iut_sd2_rshiny_enedis/blob/main/docs/functional_doc.md)** : Explique l'intérêt de chaque page et fonctionnalité de l'application.

* **`/rapport/`**
    Contient l'analyse statistique initiale (`rapport_statistique.Rmd`) qui a servi de base à la création du dashboard.

* **`/data_preparation/`**
    Contient les scripts R (ex: `extraction_api.R`) utilisés pour le nettoyage initial et la préparation des données.

* **`/data/`**
    Contient les jeux de données bruts et nettoyés (`logements_59.csv`, `logements_69.csv`).

## Lancement Rapide (Local)

Pour lancer l'application sur votre machine :

1.  **Télécharger le projet :**

      * Allez sur la page principale du dépôt [GitHub](https://github.com/ndhho/iut_sd2_rshiny_enedis)
      * Cliquez sur le bouton vert **`< > Code`** et sélectionnez **"Download ZIP"**.
      * Décompressez le fichier `.zip` sur votre ordinateur.

2.  **Installer les packages :**

      * Ouvrez RStudio.
      * Installez les dépendances (listées dans la [Documentation Technique](https://www.google.com/search?q=./docs/technical_doc.html)) en copiant leur `install.packages(...)` dans la console R.

3.  **Lancer l’application :**

      * Dans RStudio, naviguez (`File > Open File...`) et ouvrez le fichier `app/app.R` qui se trouve dans le dossier que vous venez de décompresser.
      * Cliquez sur le bouton **"Run App"** en haut à droite de l'éditeur.
      * Utilisez les identifiants `admin` / `admin` pour vous connecter.

Pour des instructions d'installation plus détaillées veuillez consulter la **[Documentation Technique](https://www.google.com/search?q=./docs/technical_doc.html)**.