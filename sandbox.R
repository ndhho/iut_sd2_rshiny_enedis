#--- UNCOMMENT POUR INSTALLER LES PACKAGES NECESSAIRES ----
#install.packages(c("httr", "jsonlite","glue","lubridate","dplyr"))
#--- LOAD PACKAGES --- 
library(httr)
library(jsonlite)
library(glue)
library(lubridate)
library(dplyr)



get_data = function(base_url,code_postal) {
  #--- INITIALISATION DATAFRAME ---
  df = data.frame()
  # Colonnes valides pour dpe03existant et dpe02neufs
  colonnes = "type_batiment,etiquette_dpe,etiquette_ges,surface_habitable_logement,code_postal_ban,code_departement_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban,cout_total_5_usages,type_energie_principale_chauffage,type_energie_principale_ecs,cout_chauffage"
  #--- BOUCLE SUR LES CODES POSTAUX 69 ---
  for(code in code_postal){
    # Requête générale
    params = list(
      size = 10000,
      select = colonnes,
      qs = glue("code_postal_ban:{code} AND type_batiment:appartement")
    )
    url_encoded = modify_url(base_url, query = params)
    response = GET(url_encoded)
    
    if(response$status_code == 200){
      content = fromJSON(rawToChar(response$content), flatten = FALSE)
      
      # Récupération du total
      total_logements = content$total
      
      # Si total <= 10 000, on ajoute directement
      if(total_logements <= 10000){
        if(length(content$result) > 0){
          df = bind_rows(df, content$result)
        }
      } else {
        # Total > 10 000 => boucle par année de 2021 à 2025
        for(annee in 2021:2025){
          qs_year = glue("code_postal_ban:{code} AND type_batiment:appartement AND date_reception_dpe:[{annee}-01-01 TO {annee}-12-31]")
          
          params_year = list(
            size = 10000,
            select = colonnes,
            qs = qs_year
          )
          
          url_year = modify_url(base_url_existants, query = params_year)
          response_year = GET(url_year)
          
          if(response_year$status_code == 200){
            content_year = fromJSON(rawToChar(response_year$content), flatten = FALSE)
            if(length(content_year$result) > 0){
              df = bind_rows(df, content_year$result)
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
  return(df)}

#--- GITHUB URL POUR LES FICHIERS CSV ---

file_path_69_csv = "https://raw.githubusercontent.com/ndhho/iut_sd2_rshiny_enedis/refs/heads/main/data/codepostal_59_69/adresses_69.csv"
file_path_59_csv = "https://raw.githubusercontent.com/ndhho/iut_sd2_rshiny_enedis/refs/heads/main/data/codepostal_59_69/adresses_59.csv"

#--- LOAD DATAFRAME AVEC LES FICHIERS ---
df_adresse69 = read.csv(file = file_path_69_csv, header = TRUE, sep = ";", dec = ".")
df_adresse59 = read.csv(file = file_path_59_csv, header = TRUE, sep = ";", dec = ".")

#--- RECUPERATION DES CODES POSTAUX DU DEPARTEMENT 69 ---
code_postal_69 = unique(df_adresse69$code_postal)
code_postal_59 = unique(df_adresse59$code_postal)

base_url_existants = "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
base_url_neufs     = "https://data.ademe.fr/data-fair/api/v1/datasets/dpe02neuf/lines"

df_existants_69 = get_data(base_url_existants,code_postal_69)
df_neufs_69 = get_data(base_url_neufs,code_postal_69)

df_existants_59 = get_data(base_url_existants,code_postal_59)
df_neufs_59 = get_data(base_url_neufs,code_postal_59)

df_existants_59$flag = "existant"
df_neufs_59$flag = "neuf"

df_existants_69$flag = "existant"
df_neufs_69$flag = "neuf"

logements_69 = rbind(df_existants_69, df_neufs_69)
logements_59 = rbind(df_existants_59, df_neufs_59)

write.csv2(logements_69, file = "logements_69.csv",row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(logements_59, file = "logements_59.csv",row.names = FALSE, fileEncoding = "UTF-8")
