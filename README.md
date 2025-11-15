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

  * **[Documentation Technique](https://www.google.com/search?q=./docs/technical_doc.html)**
  * **[Documentation Fonctionnelle](https://www.google.com/search?q=./docs/functional_doc.html)**

*(Assurez-vous que vous avez bien exporté vos fichiers `.md` en `.html` et qu'ils sont présents dans le dossier `/docs` de votre dépôt.)*

* **`/app/`**
    Contient le code source final de l'application Shiny (`app.R`) ainsi que les assets visuels (`www/`) et les fichiers de déploiement (`rsconnect/`).

* **`/docs/`**
    **C'est ici que se trouve la documentation complète du projet.**
    * `technical_doc.md` : **Documentation Technique.** Contient le guide d'installation, l'architecture et la liste des packages.
    * `Documentation_Fonctionnelle.md` : **Documentation Fonctionnelle.** Explique l'intérêt de chaque page et fonctionnalité de l'application.

* **`/rapport/`**
    Contient l'analyse statistique initiale (`rapport_statistique_v2.Rmd`) qui a servi de base à la création du dashboard.

* **`/data_preparation/`**
    Contient les scripts R (ex: `extraction_api.R`) utilisés pour le nettoyage initial et la préparation des données.

* **`/data/`**
    Contient les jeux de données bruts et nettoyés (`logements_59.csv`, `logements_69.csv`).

## Lancement Rapide (Local)

Pour lancer l'application sur votre machine :

1.  **Cloner le dépôt :**
    ```bash
    git clone [https://github.com/ndhho/iut_sd2_rshiny_enedis.git](https://github.com/ndhho/iut_sd2_rshiny_enedis.git)
    cd iut_sd2_rshiny_enedis
    ```

2.  **Installer les packages :**
    Ouvrez RStudio et installez les dépendances (listées dans `/docs/technical_doc.md`).

3.  **Lancer l'application :**
    Ouvrez le fichier `app/app.R` et cliquez sur **"Run App"**.

Pour des instructions d'installation détaillées, veuillez consulter la [Documentation Technique](/docs/technical_doc.md).