
rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

#_________________________________________________________
#### DISTRIBUTION PLOTS (2014-2020) ####

#...................................
##### PURCHASE LAG DISTRIBUTION ####
purch = readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')

print('start procesing ...')
temp = purch %>%
  group_by(HETU) %>%
  arrange(OSTOPV) %>%
  mutate(lag_days = time_length( difftime(lead(OSTOPV),OSTOPV),unit = "days")) 
head(temp,20)
#CHECKPOINT: save dataset
print('saving ...')
saveRDS(temp,'/data/projects/project_mferro/kela_kanta_reliability/data/plot_purch_dist.rds')


#.......................................
##### PRESCRIPTION LAG DISTRIBUTION ####
rm(list=ls())
presc = readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')

print('start procesing ...')
temp = presc %>%
  group_by(PATIENT_ID) %>%
  arrange(CREATION_DATE) %>%
  mutate(lag_days = time_length( difftime(lead(CREATION_DATE),CREATION_DATE),unit = "days")) 

#CHECKPOINT: save dataset
print('saving ...')
saveRDS(temp,'/data/projects/project_mferro/kela_kanta_reliability/data/plot_presc_dist.rds')


#...................................
##### DELIVERY LAG DISTRIBUTION ####
rm(list=ls())
deliv = readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds')

print('start procesing ...')
temp = deliv %>%
  group_by(PATIENT_ID) %>%
  arrange(CREATION_DATE) %>%
  mutate(lag_days = time_length( difftime(lead(CREATION_DATE),CREATION_DATE),unit = "days")) 

#CHECKPOINT: save dataset
print('saving ...')
saveRDS(temp,'/data/projects/project_mferro/kela_kanta_reliability/data/plot_deliv_dist.rds')


rm(list=ls())
gc()
