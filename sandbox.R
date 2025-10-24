#--- UNCOMMENT POUR INSTALLER LES PACKAGES NECESSAIRES ----

#install.packages(c("httr", "jsonlite","glue"))

#--- LOAD PACKAGES --- 
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

#--- GITHUB URL POUR LES FICHIERS CSV ---
file_path_69_csv <- "https://raw.githubusercontent.com/ndhho/iut_sd2_rshiny_enedis/refs/heads/main/codepostal_59_69/adresses_69.csv"

#--- LOAD DATAFRAME AVEC LES FICHIERS ---
df_adresse69 <- read.csv(file = file_path_69_csv, header = TRUE, sep = ";", dec = ".")

#--- RECUPERATION DES CODES POSTAUX DU DEPARTEMENT 69 ---
code_postal_69 <- unique(df_adresse69$code_postal)

#--- INITIALISATION DATAFRAME ---
logements_existants_69 <- data.frame()

#--- CALL API DPE LOGEMENTS EXISTANTS --- 
base_url_existants <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"

#--- BOUCLE SUR LES CODES POSTAUX 69 ---
for(code in code_postal_69){
  
  # 1️⃣ Requête générale
  params <- list(
    size = 10000,
    select = "type_batiment",
    qs = glue("code_postal_ban:{code} AND type_batiment:appartement")
  )
  
  url_encoded <- modify_url(base_url_existants, query = params)
  response <- GET(url_encoded)
  
  if(response$status_code == 200){
    content <- fromJSON(rawToChar(response$content), flatten = FALSE)
    
    # Récupération du total
    total_logements <- content$total
    print(glue("Code postal {code} → total = {total_logements}"))
    
    # Si total <= 10 000, on ajoute directement
    if(total_logements <= 10000){
      if(length(content$result) > 0){
        logements_existants_69 <- bind_rows(logements_existants_69, content$result)
      }
    } else {
      # 2️⃣ Total > 10 000 → boucle par année 2021 à 2025
      for(annee in 2021:2025){
        qs_year <- glue("code_postal_ban:{code} AND type_batiment:appartement AND date_reception_dpe:[{annee}-01-01 TO {annee}-12-31]")
        
        params_year <- list(
          size = 10000,
          select = "type_batiment",
          qs = qs_year
        )
        
        url_year <- modify_url(base_url_existants, query = params_year)
        response_year <- GET(url_year)
        
        if(response_year$status_code == 200){
          content_year <- fromJSON(rawToChar(response_year$content), flatten = FALSE)
          if(length(content_year$result) > 0){
            logements_existants_69 <- bind_rows(logements_existants_69, content_year$result)
          }
        } else {
          print(glue("Erreur API pour code {code} année {annee} : {response_year$status_code}"))
        }
      }
    }
    
  } else {
    print(glue("Erreur API pour code {code} : {response$status_code}"))
  }
}
