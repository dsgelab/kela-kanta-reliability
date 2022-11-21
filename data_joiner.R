rm(list=ls())
gc()

library(data.table)
library(dplyr)

#______________________________________
####  JOIN ALL STATINS PURCHASES   ####

path = '/data/projects/project_mferro/kela_kanta_reliability/data/statins_purchases/'
files <- system( paste0('ls ',path), intern = T)

# NB: decided to join only data from 2014 to 2020 for quality reasons (could be changed in future)
file_year <- c()
for(file in files){
  year <- strsplit(file,'LAAKEOSTOT')[[1]][2]
  file_year = cbind(file_year,year)
}
files <- files[ grepl('2014|2015|2016|2017|2018|2019|2020', file_year) ]

#code runs in approx 30 mins
statins_df = data.frame()
for (file in files){
  print(file)
  temp_df = readRDS( paste0(path,file))
  statins_df = rbind(statins_df, temp_df)
  print('added')
}

#save results 
saveRDS(statins_df,'/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')


rm(list=ls())
gc()


#_______________________________________
###  JOIN ALL STATINS PRESCRIPTION  ####

path = '/data/projects/project_mferro/kela_kanta_reliability/data/statins_prescriptions/'
files <- system( paste0('ls ',path), intern = T)

# NB: decided to join only data from 2014 to 2020 for quality reasons (could be changed in future)
files <- files[ grepl('2014|2015|2016|2017|2018|2019|2020', files) ]

#code runs in approx 30 mins
statins_df = data.frame()
for (file in files){
  print(file)
  temp_df = readRDS( paste0(path,file))
  statins_df = rbind(statins_df, temp_df)
  print('added')
}

#apply correct structure and save results 
statins_df <- data.table(statins_df)
saveRDS(statins_df,'/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')


rm(list=ls())
gc()


#______________________________________
####  JOIN ALL STATINS DELIVERIES  ####

path = '/data/projects/project_mferro/kela_kanta_reliability/data/statins_deliveries/'
files <- system( paste0('ls ',path), intern = T)

# NB: decided to join only data from 2014 to 2020 for quality reasons (could be changed in future)
files <- files[ grepl('2014|2015|2016|2017|2018|2019|2020', files) ]

#code runs in approx 30 mins
statins_df = data.frame()
for (file in files){
  print(file)
  temp_df = readRDS( paste0(path,file))
  statins_df = rbind(statins_df, temp_df)
  print('added')
}

#save results 
saveRDS(statins_df,'/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds')


rm(list=ls())
gc()


#____________________________
####  FINAL JOIN : FULL  ####

print('start data fetching')
kela_purchase <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds') 
kanta_prescription <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds') 
kanta_deliveries <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds') 

print('start joining')
final_df <- kela_purchase %>% 
  full_join(kanta_prescription, by = c('HETU'='PATIENT_ID','RKPV'='CREATION_DATE')) %>%
  full_join(kanta_deliveries, by = c('CDA_ID_MD5HASH'='DOC_GROUP_MD5HASH'), suffix = c('.pres','.del')) 


#save results 
print('start saving')
saveRDS(final_df,'/data/projects/project_mferro/kela_kanta_reliability/data/final_full_df.rds')


#_____________________________
####  FINAL JOIN : INNER  ####
#NB: decided not to use prescription's VNR code as key while joining kela and kanta

print('start data fetching')
kela_purchase <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')
kanta_prescription <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')
kanta_deliveries <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds')

print('start joining')
final_df <- kela_purchase %>% 
  inner_join(kanta_prescription, by = c('HETU'='PATIENT_ID','RKPV'='CREATION_DATE')) %>%
  inner_join(kanta_deliveries, by = c('CDA_ID_MD5HASH'='DOC_GROUP_MD5HASH'), suffix = c('.pres','.del'))

#save results
print('start saving')
saveRDS(final_df,'/data/projects/project_mferro/kela_kanta_reliability/data/final_inner_df.rds')

rm(list=ls())
gc()












