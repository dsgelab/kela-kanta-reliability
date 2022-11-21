rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

#_________________________________________________________
#### DISTRIBUTION PLOTS (2014-2020) ####

#NB: 
# data processing codes take a lot .. use the pre-saved checkpoints 
# created with preprocess_plot_distribution.R 


#...................................
##### PURCHASE LAG DISTRIBUTION ####
temp = readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/plot_purch_dist.rds')

#check max values:
apex = sort(table(temp$lag_days),decreasing = T)
names(apex)[1:5]

#PLOT:
ggplot(data=temp, aes(x=lag_days)) + 
  geom_histogram(binwidth=5) +
  xlab('days between two purchases') + ylab('total') +
  labs(title='') +
  geom_vline(xintercept = c(14,28,98), colour='blue',size=0.2) +
  scale_x_continuous(breaks = c(0,14,28,98,200,300),labels = c('0','14','28','98','200','300'),limits = c(-5,300))

#check 14 days lag example
to_keep <- temp[temp$lag_days==14 & !is.na(temp$lag_days),]$HETU
temp[temp$HETU %in% to_keep,]
#seems ANJA is related to this
sub <- temp[temp$lag_days==14 & !is.na(temp$lag_days),]
sub <- sub %>% arrange(HETU)
#View(sub)

#check how many 14 days lag are due to ANJA service
100*sum(sub$ANJA!='')/nrow(sub)
#almost all of them!

#check 28 days lag example
to_keep <- temp[temp$lag_days==28 & !is.na(temp$lag_days),]$HETU
temp[temp$HETU %in% to_keep,]
#seems ANJA is related to this
sub <- temp[temp$lag_days==28 & !is.na(temp$lag_days),]
sub <- sub %>% arrange(HETU)
#View(sub)

#check how many 28 days lag are due to ANJA service
100*sum(sub$ANJA!='')/nrow(sub)
#half of them



#.......................................
##### PRESCRIPTION LAG DISTRIBUTION ####
rm(list=ls())
temp = readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/plot_presc_dist.rds')

#check max values:
apex = sort(table(temp$lag_days),decreasing = T)
names(apex)[1:3]

#PLOT:
ggplot(data=temp, aes(x=lag_days)) + 
  geom_histogram(binwidth=10) +
  xlab('number of days between two prescriptions') + ylab('total') +
  labs(title='') +
  geom_vline(xintercept = c(0,365,730), colour='blue',size=0.2) +
  scale_x_continuous(breaks = c(0,365,730,1000,1500),labels = c('0','365','730','1000','1500'),limits = c(-5,1500))

#check 0 lag prescription
to_keep <- temp[temp$lag_days==0 & !is.na(temp$lag_days),]$PATIENT_ID
sub <- temp[temp$PATIENT_ID %in% to_keep,] %>% arrange(PATIENT_ID)
# View(sub)
# seems like there are some duplicate prescription, don't know why


#...................................
##### DELIVERY LAG DISTRIBUTION ####
rm(list=ls())
temp = readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/plot_deliv_dist.rds')

#check max values:
apex = sort(table(temp$lag_days),decreasing = T)
names(apex)[1:5]

#PLOT:
ggplot(data=temp, aes(x=lag_days)) + 
  geom_histogram(binwidth=5) +
  xlab('number of days between two deliveries') + ylab('total') +
  labs(title='') +
  geom_vline(xintercept = c(14,98), colour='blue',size=0.2) +
  scale_x_continuous(breaks = c(0,14,28,98,200,300,500),labels = c('0','14','28','98','200','300','500'),limits = c(-5,500))

#check 0 lag delivery
to_keep <- temp[temp$lag_days==0 & !is.na(temp$lag_days),]$PATIENT_ID
sub <- temp[temp$PATIENT_ID %in% to_keep,]
sub <- sub %>% arrange(PATIENT_ID) 
#View(sub)
# seems like there are some duplicate deliveries 

#check 14 days lag
temp[temp$lag_days==14,]
#a lot of NAs
sub <- temp[temp$lag_days==14,] %>% arrange(PATIENT_ID)
#View(sub)



#_________________________________________________________
#### SPLIT DISTRIBUTIONS  ####

#...................................
##### ANJA PURCHASE ####
rm(list=ls())
temp <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/plot_purch_dist.rds') %>% ungroup()

split1 <- temp[!temp$ANJA %in% c('K','U','B'),]
split2 <- temp[temp$ANJA %in% c('K','U','B'),]

p1 <-ggplot(data=split1, aes(x=lag_days)) + 
  geom_histogram(binwidth=5) +
  xlab('number of days between two purchases') + ylab('total') +
  labs(title='') +
  geom_vline(xintercept = c(28,98), colour='blue',size=0.2) +
  scale_x_continuous(breaks = c(0,14,28,98,200,300),labels = c('0','14','28','98','200','300'),limits = c(0,300))

p2 <- ggplot(data=split2, aes(x=lag_days)) + 
  geom_histogram(binwidth=5) +
  xlab('number of days between two purchases') + ylab('total') +
  labs(title='') +
  geom_vline(xintercept = 14, colour='blue',size=0.2) +
  scale_x_continuous(breaks = c(0,14,28,98,200,300),labels = c('0','14','28','98','200','300'),limits = c(0,300))

grid.arrange(p1, p2, ncol = 2)

#if we get rid of those?
nrow(split2)
100*nrow(split2)/nrow(split1)
#2.9 mil rows (19% of dataset)
length(unique(split2$HETU))
100*length(unique(split2$HETU))/length(unique(split1$HETU))
#67k patients (6.4% of patients)


#.....................................
##### DOSE_DISTRIBUTION DELIVERY  ####
rm(list=ls())
temp <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/plot_deliv_dist.rds') %>% ungroup()

split1 <- temp[temp$DOSE_DISTRIBUTION==0,]
split2 <- temp[temp$DOSE_DISTRIBUTION==1,]

p1 <-ggplot(data=split1, aes(x=lag_days)) + 
  geom_histogram(binwidth=5) +
  xlab('number of days between two deliveries') + ylab('total') +
  labs(title='') +
  geom_vline(xintercept = c(14,45,98), colour='blue',size=0.2) +
  scale_x_continuous(breaks = c(0,14,45,98,200,300,500),labels = c('0','14','45','98','200','300','500'),limits = c(-5,500)) 

p2 <- ggplot(data=split2, aes(x=lag_days)) + 
  geom_histogram(binwidth=5) +
  xlab('number of days between two deliveries') + ylab('total') +
  labs(title='') +
  geom_vline(xintercept = 14, colour='blue',size=0.2) +
  scale_x_continuous(breaks = c(0,14,45,98,200,300,500),labels = c('0','14','45','98','200','300','500'),limits = c(-5,500)) 

grid.arrange(p1, p2, ncol = 2)

#if we get rid of those?
nrow(split2)
100*nrow(split2)/nrow(split1)
#2.5 mil rows (17% of dataset)
length(unique(split2$PATIENT_ID))
100*length(unique(split2$PATIENT_ID))/length(unique(split1$PATIENT_ID))
#62k patients (5.9% of patients)


rm(list=ls())
gc()


