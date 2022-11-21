rm(list=ls())
gc()

library(data.table)
library(dplyr)

#_________________________________

# STATINS = starts with C10AA
ATC_regex <- '^C10AA' 
vnr_map <- fread('/data/projects/project_mferro/adherence_and_disease_onset/data/vnr_dictionary.csv') %>%
  mutate(VNRO = as.integer(VNRO))

#_________________________________

#### FUNCTION ####

extract_ATC_kela <- function(from_path, to_path, filename, ATC_regex) {
  print(filename)
  t = Sys.time()
  
  #FETCH DATA
  df <- fread(paste0(from_path, filename))
  df <- df %>% 
    #select column of interest
    select(HETU, ATC, OSTOPV, PLKM, VNRO, RKPV, RGTNO, ASKU, ANJA) %>%
    #filter for chosen drug
    filter( grepl(ATC_regex, ATC) ) %>%
    #apply correct format to some variables
    mutate( VNRO = as.integer(VNRO)) %>%
    mutate( RGTNO = if_else( is.na(as.integer(RGTNO)), VNRO, as.integer(RGTNO))  ) %>%
    #join pack size information
    left_join(vnr_map)
  
  #SAVE RESULT
  name <- strsplit(filename,".csv")[[1]][1]
  new_name <- paste0(to_path, name,'.rds')
  saveRDS(df, new_name)
  
  print(Sys.time() - t)
  gc()
}


#____________________________________

#### EXECUTE CODE FOR KELA ####
#NB: code takes approx 40min

from_path <- "/data/processed_data/kela_purchase/"
to_path <- "/data/projects/project_mferro/kela_kanta_reliability/data/statins_purchases/"

files <- system( paste0("ls ", from_path), intern = T)
for (file in files){
  extract_ATC_kela(from_path, to_path, file, ATC_regex)
}

rm(list=ls())
gc()

