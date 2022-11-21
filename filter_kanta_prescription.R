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
    select(PATIENT_ID, CREATION_DATE, PRODUCT_CODE,  
           CDA_ID_MD5HASH, CDA_SET_ID_MD5HASH, DOC_GROUP_MD5HASH, 
           DOC_TYPE_CODE, DOC_VERSION,
           ATC_CODE, DOSAGE_INSTRUCTION, DOSE_DISTRIBUTION, PURPOSE_OF_USE) %>%
    #filter for chosen drug
    filter( grepl(ATC_regex, ATC_CODE) ) 
  
  #subset all revocated prescription
  hash_to_eliminate = df[df$DOC_TYPE_CODE==2,]$CDA_SET_ID_MD5HASH
  row_to_eliminate = df$CDA_SET_ID_MD5HASH %in% hash_to_eliminate
  df = df[!row_to_eliminate,]
  
  #keep only last prescription (for those updated)
  df <- df %>% 
    group_by(DOC_GROUP_MD5HASH) %>% 
    arrange( desc(DOC_VERSION), .by_group = TRUE) %>% 
    filter(row_number()==1) %>%
    ungroup()
  
  print(Sys.time() - t)
  
  #prescribed dosage extraction
  #TODO
  
  #SAVE RESULTS
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
to_path <- "/data/projects/project_mferro/kela_kanta_reliability/data/statins_prescriptions/"
files <- system( paste0("ls ", from_path), intern = T)
files <- files[grepl('prescription', files)]


for (file in files){
  extract_ATC_kanta(from_path,to_path, file, ATC_regex)
}

rm(list=ls())
gc()



