rm(list=ls())
gc()

library(data.table)
library(dplyr)

#_________________________________

# STATINS = starts with C10AA
ATC_regex <- '^C10AA' 

#_________________________________

#### FUNCTION ####

extract_ATC_kanta <- function(from_path, to_path, filename, ATC_regex) {
  print(filename)
  t = Sys.time()
  
  #FETCH DATA
  df <- fread(paste0(from_path, filename))
  df <- df %>% 
    #select columns of interest 
    select(PATIENT_ID, CREATION_DATE, DOC_GROUP_MD5HASH,
           ATC_CODE, PRODUCT_CODE1, 
           DOSE_DISTRIBUTION, MED_EXCHANGED, RESEPTISTATUS) %>%
    #filter for chosen drug
    filter( grepl(ATC_regex, ATC_CODE)) %>%
    #apply correct format to some variables
    mutate(PRODUCT_CODE1 = if_else(PRODUCT_CODE1=="",NA_integer_,as.integer(PRODUCT_CODE1)) ) 
  
  #SAVE RESULT  
  name <- strsplit(filename,".csv")[[1]][1]
  new_name <- paste0(to_path, name,'.rds')
  saveRDS(df, new_name)
  
  print(Sys.time() - t)
  gc()
}


#____________________________________

#### EXECUTE CODE ####
#NB: code takes approx 2hour

from_path <- "/data/processed_data/kela_kanta/"
to_path <- "/data/projects/project_mferro/kela_kanta_reliability/data/statins_deliveries/"
files <- system( paste0("ls ", from_path), intern = T)
files <- files[grepl('deliveries', files)]


for (file in files){
  extract_ATC_kanta(from_path,to_path, file, ATC_regex)
}

rm(list=ls())
gc()


