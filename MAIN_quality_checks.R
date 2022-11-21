rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(gridExtra)
library(grid)

#install.packages('UpSetR',repos='file://data/cran/')
library(UpSetR)




#_________________________
####  DATASET INFO  ####

kela_purchase <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')
nrow(kela_purchase)
length( unique(kela_purchase$HETU) )

#also check typical pack size in statins
temp <- kela_purchase %>% group_by(HETU,PKOKO) %>% filter(row_number()==1)
100*table(temp$PKOKO,useNA = 'ifany')/nrow(temp)
#check how many people take the 90 and 250 package
length( temp[temp$PKOKO==90,]$HETU %>% unique() )
length( temp[temp$PKOKO==250,]$HETU %>% unique() )


kanta_prescription <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')
nrow(kanta_prescription)
length( unique(kanta_prescription$PATIENT_ID) )

kanta_delivery <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds')
nrow(kanta_delivery)
length( unique(kanta_delivery$PATIENT_ID) )

final_inner_df <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/final_inner_df.rds')
nrow(final_inner_df)
length( unique(final_inner_df$HETU) )


rm(list=ls())
gc()

#________________________
####  FINGENN INFO   ####

#join dataset with minimal phenotype to check info about indexed patients
index_person <- fread('/data/processed_data/minimal_phenotype/minimal_phenotype_2022-03-28.csv') %>% select(FINREGISTRYID,index_person)
kela_purchase <- readRDS('/data/projects/project_mferro/extra/joined_statins_from1995.rds')
new_df <- kela_purchase %>% left_join(index_person,by=c('HETU'='FINREGISTRYID'))

#plot info by year
new_df <- new_df %>% mutate(YEAR = format(OSTOPV,'%Y')) 
slice1 <- new_df %>% filter(index_person==1) %>% distinct(HETU,YEAR) %>% group_by(YEAR) %>% summarise(TOT=n())
slice2 <- new_df %>% filter(index_person==0) %>% distinct(HETU,YEAR) %>% group_by(YEAR) %>% summarise(TOT=n())

p1 <- ggplot(slice1,aes(YEAR)) + geom_col(aes(y=TOT)) + theme(axis.text.x = element_text(angle = 90))
p2 <- ggplot(slice2,aes(YEAR)) + geom_col(aes(y=TOT)) + theme(axis.text.x = element_text(angle = 90)) + ylim(0,8e+05) 
grid.arrange(p1,p2,ncol=2,top='INDEX  vs NOT INDEX')


#_______________________
####  UPSET PLOTS  #####

kela_purchase <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')
kanta_prescription <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')
kanta_delivery <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds')

#...........................
# upset plot for individuals
to_plot = list(purchase = unique(kela_purchase$HETU),
               prescription = unique(kanta_prescription$PATIENT_ID),
               delivery = unique(kanta_delivery$PATIENT_ID))

upset(UpSetR::fromList(to_plot),
      mainbar.y.label = 'IN SUBGROUP',
      sets.x.label = 'IN DATASET')
grid.text("UpsetPlot by patient ID", x = 0.65, y = 0.95, gp = gpar(fontsize = 15))

#........................................................................
#upset plot for 1st join (based on double key between kela and kanta)
key1 <- tidyr::unite(kela_purchase[,c('HETU','RKPV')],two_way_key)
key2 <- tidyr::unite(kanta_prescription[,c('PATIENT_ID','CREATION_DATE')],two_way_key)
to_plot = list(purchases = key1$two_way_key, prescription = key2$two_way_key)

upset(UpSetR::fromList(to_plot),
      mainbar.y.label = 'IN SUBGROUP',
      sets.x.label = 'IN DATASET')
grid.text("UpsetPlot by (patient,prescription) key", x = 0.65, y = 0.95,gp = gpar(fontsize = 15))

#....................................................
# upset plot for 2nd join (based on kanta document id)
to_plot = list(prescription = unique(kanta_prescription$CDA_SET_ID_MD5HASH),
               delivery = unique(kanta_delivery$DOC_GROUP_MD5HASH))

upset(UpSetR::fromList(to_plot),
      mainbar.y.label = 'IN SUBGROUP',
      sets.x.label = 'IN DATASET')
grid.text("UpsetPlot by prescription ID", x = 0.65, y = 0.95, gp = gpar(fontsize = 15))


rm(list=ls())
gc()

#___________________________
####  CHECK DUPLICATES  #### 

kela_purchase <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')
kela_purchase[duplicated(kela_purchase),]
# 986 duplicates found!
unique( kela_purchase[duplicated(kela_purchase),]$PLKM )
# can be a problem of registration: multiple package purchase not in PLKM but in multiple rows 


#...............................

kanta_prescription <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')
kanta_prescription[duplicated(kanta_prescription),]
# no duplicates found!
# but we know there are some 0 lag prescription .. they are going to disappear while joining with kela

#NB: for more info look in PLOT_distributions.R


#...............................

kanta_delivery <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds')
kanta_delivery[duplicated(kanta_delivery),]
# 13013 duplicates found!


rm(list=ls())
gc()

#___________________________________
#### PRESCRIPTION AND PURCHASES ####

kela_purchase <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')
kanta_prescription <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')

#join them on 3 keys
temp <- kela_purchase %>% full_join(kanta_prescription, by = c('HETU'='PATIENT_ID','RGTNO'='PRODUCT_CODE','RKPV'='CREATION_DATE'), keep=T) 
nrow(temp)

#try not to use vnr code for the join
temp1 <- kela_purchase %>% full_join(kanta_prescription, by = c('HETU'='PATIENT_ID','RKPV'='CREATION_DATE'), keep=T) 
nrow(temp1)
#we lose 268267 rows .. check special cases:
temp1[temp1$RGTNO!=temp1$PRODUCT_CODE,]


##### ANALYSIS ####

#TEST date completeness
sum( is.na(kanta_prescription$CREATION_DATE) )
sum( is.na(kela_purchase$RKPV) )
# no NAs present

#now lets see with dataset joined
sum( is.na(temp$CREATION_DATE) )
sum( is.na(temp$RKPV) )
#a lot of NAs appearing


#____________________________________
#### PRESCRIPTION AND DELIVERIES ####
rm(list=ls())
gc()

kanta_delivery <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds')
kanta_prescription <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')

#join them
temp <- kanta_prescription %>% 
  full_join(kanta_delivery, by = c('CDA_SET_ID_MD5HASH'='DOC_GROUP_MD5HASH'), suffix=c('.del','.pres')) %>%
  group_by(PATIENT_ID) %>% 
  arrange(CREATION_DATE.pres, .by_group = T)
head(temp,20)

##### ANALYSIS ####

#........................
#TEST 1
sum( is.na(kanta_prescription$CREATION_DATE) )
sum( is.na(kela_purchase$RKPV) )
# no NAs present

#now lets see with kela joined
sum( is.na(temp$CREATION_DATE) )
#NAs but probably due to missing years from kanta?


#____________________________________
#### ANJA and DOSE_DISTRIBUTION  ####

rm(list=ls())
final_inner_df <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/final_inner_df.rds')

#extract list of patient using ANJA service
pat_list <- final_inner_df[final_inner_df$ANJA %in% c('U','B','K'),]$HETU %>% unique()
pat_list2 <- final_inner_df[final_inner_df$DOSE_DISTRIBUTION.pres==1,]$PATIENT_ID %>% unique()
pat_list3 <- final_inner_df[final_inner_df$DOSE_DISTRIBUTION.del==1,]$PATIENT_ID %>% unique()

#check if KANTA is reliable
length(pat_list2)
length(pat_list3)
#check KELA
length(pat_list)

#patient in common between kanta and kela
100*sum(pat_list2 %in% pat_list)/length(pat_list2)
100*sum(pat_list3 %in% pat_list)/length(pat_list3)
#check kanta in common patients
100*sum(pat_list2 %in% pat_list3)/length(pat_list2)

#CONFIRMED: kela 'anja' and kanta 'dose_distribution' are the same thing!

rm(list=ls())
gc()













